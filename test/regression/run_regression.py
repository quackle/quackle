#!/usr/bin/env python3
"""Regression driver for libquackle_test.

libquackle_test (built from test/testharness.cpp) exposes the reproducible,
scriptable corners of libquackle -- move generation, static leaves, anagramming,
rack enumeration, game reports, and self-play -- through its --mode lever. This
script drives those levers with fixed inputs and a fixed RNG seed, then compares
the output against recorded golden files, reporting each test individually.

Only the Speedy Player produces reproducible move choices, so it is the only
computer player exercised here. Everything that draws tiles (positions with
unknown racks, reports, self-play) is made deterministic by passing --seed;
testharness.cpp applies that seed before dispatching to any mode.

Tests are grouped into "suites", one per dictionary+alphabet combination
(e.g. nwl23-english). A suite lives in suites/<name>/ with:
    suite.json      -- the lever definitions (see that file for the schema)
    inputs/         -- input files referenced by tests (rack/leave lists)
    expected/       -- golden output, one <test>.out per test (created by --record)
    manifest.json   -- environment fingerprint captured when goldens were recorded

Usage:
    run_regression.py                 verify every suite
    run_regression.py nwl23-english   verify one suite
    run_regression.py --record        (re)record golden files for every suite
    run_regression.py --record nwl23-english
    run_regression.py --list          list suites and their tests
    run_regression.py -v              echo each exact libquackle_test invocation
    run_regression.py --no-gaddag     run degraded (skip gaddag generation)

Exit status is non-zero if any test fails (or if a golden file is missing).

Where a lexicon has no gaddag in the data dir, one is generated from its dawg
(via worddump + makegaddag from the build tree) and cached, so Speedy Player
runs at full strength; see README.md. Move generation is identical with or
without a gaddag, so goldens don't depend on it.
"""

import argparse
import hashlib
import json
import os
import re
import shlex
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path

# When True, echo each libquackle_test invocation before running it.
VERBOSE = False

# ---------------------------------------------------------------------------
# Paths
# ---------------------------------------------------------------------------

SCRIPT_DIR = Path(__file__).resolve().parent          # test/regression
REPO_ROOT = SCRIPT_DIR.parent.parent                  # quackle/
SUITES_DIR = SCRIPT_DIR / "suites"
POSITIONS_DIR = REPO_ROOT / "test" / "positions"      # shared .gcg corpus
DATA_DIR = REPO_ROOT / "data"

BIN_NAME = "libquackle_test"
# Build-tree layouts we'll probe for the binary, relative to the repo root.
# Covers in-source ./build plus common out-of-source / IDE conventions.
BUILD_DIR_CANDIDATES = ["build", "Build", "out", "cmake-build-debug",
                        "cmake-build-release", "build-debug", "build-release"]


def resolve_binary(cli_bin):
    """Locate libquackle_test, most specific source first.

    Precedence:
      1. --bin PATH               (a binary, or a build dir containing test/<bin>)
      2. $LIBQUACKLE_TEST         (same interpretation)
      3. probe <repo>/<candidate>/test/<bin> for common build-dir names
    Returns a resolved Path, or None if nothing was found.
    """
    def as_binary(p):
        """Accept either the binary itself or a build directory holding it."""
        p = Path(p).expanduser()
        if p.is_dir():
            cand = p / "test" / BIN_NAME
            return cand if cand.exists() else None
        return p if p.exists() else None

    for source in (cli_bin, os.environ.get("LIBQUACKLE_TEST")):
        if source:
            # An explicit request that doesn't resolve is a hard error, so the
            # caller isn't silently fooled by a stale probe below.
            found = as_binary(source)
            if found:
                return found.resolve()
            sys.exit(f"{BIN_NAME} not found at requested path: {source}")

    for name in BUILD_DIR_CANDIDATES:
        cand = REPO_ROOT / name / "test" / BIN_NAME
        if cand.exists():
            return cand.resolve()
    return None

# ANSI colors (disabled when stdout is not a tty)
if sys.stdout.isatty():
    C_RED, C_GREEN, C_YELLOW, C_BLUE, C_RESET = (
        "\033[31m", "\033[32m", "\033[33m", "\033[34m", "\033[0m")
else:
    C_RED = C_GREEN = C_YELLOW = C_BLUE = C_RESET = ""


# ---------------------------------------------------------------------------
# Output normalization
# ---------------------------------------------------------------------------
#
# libquackle_test's stdout has a leading, environment-dependent banner (how many
# data files loaded, whether a gaddag was present) followed by a "using seed N"
# line, and then the real test output. It also stamps a wall-clock duration into
# self-play summaries. We strip both so goldens depend only on libquackle's
# behavior, not on the host or the data build.

_SEED_LINE = re.compile(r"^using seed \d+$")
_TIMING = re.compile(r"played in \d+ seconds")


def normalize(stdout: str) -> str:
    """Drop the startup banner and neutralize wall-clock timings."""
    lines = stdout.splitlines()

    # Everything up to and including the "using seed N" line is startup banner.
    start = 0
    for i, line in enumerate(lines):
        if _SEED_LINE.match(line):
            start = i + 1
            break

    body = "\n".join(lines[start:])
    body = _TIMING.sub("played in <N> seconds", body)
    # Ensure a single trailing newline for stable file contents / digests.
    return body.rstrip("\n") + "\n"


def gaddag_present(raw_output: str) -> bool:
    return "couldn't open gaddag" not in raw_output


def strategy_present(raw_output: str) -> bool:
    # startUp() emits "Could not open  to load syn2" etc. when strategy files
    # for the lexicon can't be found.
    return "to load syn2" not in raw_output


# ---------------------------------------------------------------------------
# Gaddag generation
# ---------------------------------------------------------------------------
#
# The data dir ships each lexicon's .dawg but not its .gaddag (a generated file
# the repo intentionally doesn't carry). Without a gaddag, Speedy Player's move
# generation produces identical results -- it's just slower -- so goldens don't
# depend on it. But building one lets tests run at full strength and noticeably
# faster. When the data dir has no gaddag for a lexicon, we build one from its
# dawg (worddump -> makegaddag, both from the target build tree) and inject it
# through the harness's cwd-relative "lexica/" lookup, which shadows only the
# gaddag and leaves the dawg/alphabet/strategy served from ../data. Generated
# gaddags are cached under the temp dir (keyed by dawg content) so repeat runs
# are fast; nothing is ever written into the repo.

GADDAG_CACHE = Path(tempfile.gettempdir()) / "quackle_regression_gaddags"


def makegaddag_path(binary):
    """makegaddag sits beside libquackle_test in the same build tree."""
    cand = binary.parent.parent / "makegaddag" / "makegaddag"
    return cand if cand.exists() else None


def dawg_fingerprint(lexicon):
    dawg = DATA_DIR / "lexica" / f"{lexicon}.dawg"
    if not dawg.exists():
        return None
    return hashlib.sha256(dawg.read_bytes()).hexdigest()[:16]


def build_gaddag(binary, lexicon, alphabet, work_root):
    """Return a path to a gaddag for `lexicon`, building + caching if needed.

    Returns None when the data dir already ships one (nothing to inject) or when
    generation isn't possible (makegaddag or dawg missing). Both tools run with
    cwd = work_root/run so their "../data" resolves to the repo data dir.
    """
    if (DATA_DIR / "lexica" / f"{lexicon}.gaddag").exists():
        return None                                  # already served by ../data
    fp = dawg_fingerprint(lexicon)
    mg = makegaddag_path(binary)
    if fp is None or mg is None:
        return None

    GADDAG_CACHE.mkdir(parents=True, exist_ok=True)
    cached = GADDAG_CACHE / f"{lexicon}-{alphabet}-{fp}.gaddag"
    if cached.exists():
        return cached

    run_dir = work_root / "run"
    dump = subprocess.run(
        [str(binary), "--mode=worddump",
         f"--lexicon={lexicon}", f"--alphabet={alphabet}"],
        cwd=run_dir, capture_output=True, text=True, timeout=600)
    prefix = "wordDump: "
    words = [ln[len(prefix):] for ln in dump.stdout.splitlines()
             if ln.startswith(prefix)]
    if not words:
        return None
    words_file = work_root / f"{lexicon}.words"
    words_file.write_text("\n".join(words) + "\n")

    # Write to a temp name then rename, so an interrupted build can't leave a
    # truncated file that a later run would mistake for a valid cache entry.
    partial = cached.with_name(cached.name + ".partial")
    gen = subprocess.run(
        [str(mg), "-f", str(words_file), "-o", str(partial),
         f"--alphabet={alphabet}"],
        cwd=run_dir, capture_output=True, text=True, timeout=600)
    if not partial.exists():
        sys.stderr.write(gen.stdout + gen.stderr)
        return None
    partial.replace(cached)
    return cached


def ensure_gaddags(binary, suites, work_root):
    """Build/locate a gaddag for every lexicon the selected suites use.

    Returns {lexicon: gaddag_path_or_None}, printing a one-line status per
    lexicon that needed generation.
    """
    result = {}
    for lexicon, alphabet in sorted({(s["lexicon"], s["alphabet"]) for s in suites}):
        if lexicon in result:
            continue
        shipped = (DATA_DIR / "lexica" / f"{lexicon}.gaddag").exists()
        path = build_gaddag(binary, lexicon, alphabet, work_root)
        result[lexicon] = path
        if shipped:
            continue                                 # ../data already has it
        if path:
            print(f"{C_BLUE}gaddag{C_RESET}  {lexicon}: {path}")
        else:
            print(f"{C_YELLOW}gaddag  {lexicon}: unavailable "
                  f"(makegaddag not built?); Speedy Player runs degraded "
                  f"-- slower, identical results.{C_RESET}")
    return result


# ---------------------------------------------------------------------------
# Running one test
# ---------------------------------------------------------------------------

class TestResult:
    def __init__(self, name):
        self.name = name
        self.status = None       # "pass" | "fail" | "error" | "missing"
        self.detail = ""         # diff or error text
        self.gaddag = None
        self.strategy = None


def build_argv(binary, suite, test, run_dir, gaddags):
    """Construct the libquackle_test command line for one test.

    Positions and input files are staged into run_dir with stable relative
    names so that nothing machine-specific (absolute paths) leaks into output.
    Returns (argv, staged), where `staged` describes the cwd files the harness
    reads that don't appear in argv (input files, injected gaddag) -- surfaced
    by --verbose so the printed invocation tells the whole story.
    """
    lexicon = suite["lexicon"]
    alphabet = suite["alphabet"]
    seed = test.get("seed", suite.get("seed", 1))
    staged = []

    argv = [
        str(binary),
        f"--mode={test['mode']}",
        f"--lexicon={lexicon}",
        f"--alphabet={alphabet}",
        f"--seed={seed}",
    ]

    # Inject a generated gaddag via the harness's cwd-relative "lexica/" lookup
    # (checked before ../data), so Speedy Player runs at full strength.
    gaddag = gaddags.get(lexicon)
    if gaddag:
        lexica = run_dir / "lexica"
        lexica.mkdir(exist_ok=True)
        (lexica / f"{lexicon}.gaddag").symlink_to(gaddag)
        staged.append(f"lexica/{lexicon}.gaddag -> {gaddag}")

    # Stage input files (e.g. the 'racks' file for staticleaves, 'leaves' for
    # leavecalc) under the exact filename the harness opens in its cwd.
    for dest_name, src_rel in test.get("inputs", {}).items():
        src = (SUITES_DIR / suite["_name"] / src_rel).resolve()
        shutil.copy(src, run_dir / dest_name)
        staged.append(f"{dest_name} (from suites/{suite['_name']}/{src_rel})")

    # Stage .gcg positions and reference them relatively.
    positions = test.get("positions", [])
    if positions:
        pos_dir = run_dir / "positions"
        pos_dir.mkdir(exist_ok=True)
        for gcg in positions:
            src = POSITIONS_DIR / gcg
            shutil.copy(src, pos_dir / gcg)
            argv.append(f"--position=positions/{gcg}")

    argv.extend(test.get("args", []))
    return argv, staged


def run_test(binary, suite, test, work_root, gaddags):
    """Run one test and return (normalized_output, raw_output, argv)."""
    run_dir = work_root / "run"
    if run_dir.exists():
        shutil.rmtree(run_dir)
    run_dir.mkdir()

    argv, staged = build_argv(binary, suite, test, run_dir, gaddags)

    if VERBOSE:
        print(f"  {C_BLUE}> {test['name']}{C_RESET}")
        print(f"    (cd {shlex.quote(str(run_dir))} && {shlex.join(argv)})")
        for s in staged:
            print(f"      staged: {s}")

    proc = subprocess.run(
        argv, cwd=run_dir, capture_output=True, text=True, timeout=600)

    raw = proc.stdout + proc.stderr
    return normalize(proc.stdout), raw, argv


def golden_content(normalized, digest):
    """What we store on disk for a test's expected output."""
    if digest:
        h = hashlib.sha256(normalized.encode()).hexdigest()
        nlines = normalized.count("\n")
        return f"sha256:{h}\nlines:{nlines}\n"
    return normalized


# ---------------------------------------------------------------------------
# Suite discovery
# ---------------------------------------------------------------------------

def load_suite(name):
    suite_json = SUITES_DIR / name / "suite.json"
    if not suite_json.exists():
        sys.exit(f"No suite '{name}' (expected {suite_json})")
    with open(suite_json) as f:
        suite = json.load(f)
    suite["_name"] = name
    return suite


def discover_suites():
    if not SUITES_DIR.is_dir():
        return []
    return sorted(
        p.name for p in SUITES_DIR.iterdir()
        if (p / "suite.json").exists())


# ---------------------------------------------------------------------------
# Commands
# ---------------------------------------------------------------------------

def setup_work_root():
    """Create a temp dir whose sibling 'data' symlink points at repo data.

    testharness.cpp hardcodes its app-data directory to "../data", so the
    binary's cwd must have a sibling 'data'. We build that layout in a temp
    area to avoid depending on where the script is run from.
    """
    work_root = Path(tempfile.mkdtemp(prefix="quackle_regress_"))
    (work_root / "data").symlink_to(DATA_DIR)
    (work_root / "run").mkdir()
    return work_root


def record_suite(binary, suite, work_root, gaddags):
    name = suite["_name"]
    expected_dir = SUITES_DIR / name / "expected"
    expected_dir.mkdir(exist_ok=True)

    print(f"{C_BLUE}Recording suite {name}{C_RESET} "
          f"({suite['lexicon']}-{suite['alphabet']})")

    any_gaddag = True
    any_strategy = True
    for test in suite["tests"]:
        normalized, raw, _ = run_test(binary, suite, test, work_root, gaddags)
        any_gaddag = any_gaddag and gaddag_present(raw)
        any_strategy = any_strategy and strategy_present(raw)
        content = golden_content(normalized, test.get("digest", False))
        out_path = expected_dir / f"{test['name']}.out"
        out_path.write_text(content)
        print(f"  recorded {test['name']:<28} ({len(content)} bytes)")

    manifest = {
        "lexicon": suite["lexicon"],
        "alphabet": suite["alphabet"],
        "git_commit": git_commit(),
        "gaddag_present": any_gaddag,
        "strategy_present": any_strategy,
    }
    (SUITES_DIR / name / "manifest.json").write_text(
        json.dumps(manifest, indent=2) + "\n")
    if not any_gaddag:
        print(f"  {C_YELLOW}note: recorded WITHOUT a gaddag "
              f"(Speedy Player ran in degraded mode).{C_RESET}")
    if not any_strategy:
        print(f"  {C_YELLOW}note: recorded WITHOUT strategy files for "
              f"'{suite['lexicon']}'.{C_RESET}")


def verify_suite(binary, suite, work_root, gaddags):
    name = suite["_name"]
    expected_dir = SUITES_DIR / name / "expected"
    manifest_path = SUITES_DIR / name / "manifest.json"
    manifest = json.loads(manifest_path.read_text()) if manifest_path.exists() else {}

    print(f"{C_BLUE}Suite {name}{C_RESET} "
          f"({suite['lexicon']}-{suite['alphabet']})")

    results = []
    run_gaddag = True
    run_strategy = True
    for test in suite["tests"]:
        r = TestResult(test["name"])
        out_path = expected_dir / f"{test['name']}.out"
        try:
            normalized, raw, argv = run_test(binary, suite, test, work_root, gaddags)
        except subprocess.TimeoutExpired:
            r.status, r.detail = "error", "timed out"
            results.append(r)
            print(f"  {C_RED}ERROR{C_RESET} {test['name']}: timed out")
            continue

        run_gaddag = run_gaddag and gaddag_present(raw)
        run_strategy = run_strategy and strategy_present(raw)

        if not out_path.exists():
            r.status = "missing"
            r.detail = f"no golden at {out_path} (run with --record)"
            print(f"  {C_YELLOW}MISSING{C_RESET} {test['name']}")
            results.append(r)
            continue

        expected = out_path.read_text()
        actual = golden_content(normalized, test.get("digest", False))
        if actual == expected:
            r.status = "pass"
            print(f"  {C_GREEN}PASS{C_RESET}  {test['name']}")
        else:
            r.status = "fail"
            r.detail = make_diff(expected, actual, test.get("digest", False),
                                 normalized)
            print(f"  {C_RED}FAIL{C_RESET}  {test['name']}")
        results.append(r)

    # Warn if the environment changed in a way that would invalidate goldens.
    if manifest.get("gaddag_present") is not None and \
            manifest.get("gaddag_present") != run_gaddag:
        print(f"  {C_YELLOW}WARNING: gaddag availability changed since "
              f"recording (was {manifest['gaddag_present']}, now {run_gaddag}). "
              f"Move-generation goldens likely need re-recording.{C_RESET}")
    if manifest.get("strategy_present") is not None and \
            manifest.get("strategy_present") != run_strategy:
        print(f"  {C_YELLOW}WARNING: strategy-file availability changed since "
              f"recording (was {manifest['strategy_present']}, now "
              f"{run_strategy}).{C_RESET}")

    return results


def make_diff(expected, actual, digest, normalized):
    import difflib
    if digest:
        return (f"    expected {expected.strip()}\n"
                f"    actual   {actual.strip()}\n"
                f"    (digest mismatch; full output has "
                f"{normalized.count(chr(10))} lines)")
    diff = difflib.unified_diff(
        expected.splitlines(), actual.splitlines(),
        fromfile="expected", tofile="actual", lineterm="")
    lines = list(diff)
    if len(lines) > 60:
        lines = lines[:60] + [f"... ({len(lines) - 60} more diff lines)"]
    return "\n".join("    " + l for l in lines)


def git_commit():
    try:
        return subprocess.check_output(
            ["git", "-C", str(REPO_ROOT), "rev-parse", "--short", "HEAD"],
            text=True).strip()
    except Exception:
        return "unknown"


def cmd_list(suites):
    for name in suites:
        suite = load_suite(name)
        print(f"{C_BLUE}{name}{C_RESET} "
              f"({suite['lexicon']}-{suite['alphabet']}, "
              f"seed={suite.get('seed', 1)})")
        for test in suite["tests"]:
            extra = " ".join(test.get("args", []))
            pos = ",".join(test.get("positions", []))
            bits = [f"mode={test['mode']}"]
            if pos:
                bits.append(f"positions={pos}")
            if extra:
                bits.append(extra)
            print(f"    {test['name']:<28} {' '.join(bits)}")


# ---------------------------------------------------------------------------
# main
# ---------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(description=__doc__,
                                     formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("suites", nargs="*",
                        help="suite name(s) to run (default: all)")
    parser.add_argument("--record", action="store_true",
                        help="record golden files instead of verifying")
    parser.add_argument("--list", action="store_true",
                        help="list suites and their tests, then exit")
    parser.add_argument("--bin", default=None, metavar="PATH",
                        help="path to libquackle_test, or to a build directory "
                             "containing test/libquackle_test. Overrides the "
                             "$LIBQUACKLE_TEST env var and the build-dir search.")
    parser.add_argument("--no-gaddag", action="store_true",
                        help="skip generating a gaddag; run Speedy Player in "
                             "degraded mode (slower, but produces identical "
                             "results).")
    parser.add_argument("-v", "--verbose", action="store_true",
                        help="echo the exact libquackle_test invocation (with "
                             "cwd and staged files) before running each test.")
    args = parser.parse_args()

    global VERBOSE
    VERBOSE = args.verbose

    available = discover_suites()
    if args.list:
        cmd_list(args.suites or available)
        return 0

    binary = resolve_binary(args.bin)
    if binary is None:
        looked = ", ".join(f"{n}/test/{BIN_NAME}" for n in BUILD_DIR_CANDIDATES)
        sys.exit(f"{BIN_NAME} not found. Build it first, e.g.\n"
                 f"    cmake --build build --target {BIN_NAME}\n"
                 f"or point at it with --bin PATH or $LIBQUACKLE_TEST.\n"
                 f"Searched under {REPO_ROOT}: {looked}")

    selected = args.suites or available
    if not selected:
        sys.exit(f"No suites found under {SUITES_DIR}")

    suites = [load_suite(name) for name in selected]
    work_root = setup_work_root()
    try:
        gaddags = {} if args.no_gaddag else ensure_gaddags(binary, suites, work_root)

        if args.record:
            for suite in suites:
                record_suite(binary, suite, work_root, gaddags)
            print(f"\n{C_GREEN}Recorded {len(suites)} suite(s).{C_RESET}")
            return 0

        all_results = []
        for suite in suites:
            all_results.extend(verify_suite(binary, suite, work_root, gaddags))

        npass = sum(1 for r in all_results if r.status == "pass")
        failures = [r for r in all_results if r.status != "pass"]

        print()
        for r in failures:
            print(f"{C_RED}--- {r.status.upper()}: {r.name} ---{C_RESET}")
            if r.detail:
                print(r.detail)
        print(f"{npass}/{len(all_results)} passed", end="")
        if failures:
            print(f", {C_RED}{len(failures)} not passing{C_RESET}")
            return 1
        print(f"  {C_GREEN}all good{C_RESET}")
        return 0
    finally:
        shutil.rmtree(work_root, ignore_errors=True)


if __name__ == "__main__":
    sys.exit(main())
