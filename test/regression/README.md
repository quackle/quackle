# libquackle regression tests

`run_regression.py` drives the `libquackle_test` harness (built from
`test/testharness.cpp`) across its reproducible `--mode` levers, comparing output
against recorded golden files and reporting each test individually. It's the
automated counterpart to the human-driven test apps: fixed inputs, a fixed RNG
seed, and stable expected output.

## Running

```sh
# build the harness first
cmake --build build --target libquackle_test

# verify everything
python3 test/regression/run_regression.py

# one suite, or list what's defined
python3 test/regression/run_regression.py nwl23-english
python3 test/regression/run_regression.py --list

# (re)record golden files after an intended behavior change
python3 test/regression/run_regression.py --record
```

Exit status is non-zero if any test fails or a golden is missing.

### Pointing at a build tree

By default the script probes common build-directory names under the repo root
(`build`, `out`, `cmake-build-debug`, …) for `test/libquackle_test`. To use a
different build tree, either pass `--bin` or set `$LIBQUACKLE_TEST`; both accept
the binary itself or a build directory containing `test/libquackle_test`:

```sh
python3 test/regression/run_regression.py --bin ../my-build
LIBQUACKLE_TEST=/path/to/libquackle_test python3 test/regression/run_regression.py
```

## Layout

Tests are grouped into **suites**, each in `suites/<name>/`:

```
suites/<name>/
  suite.json      lever definitions (see the schema comment inside)
  inputs/         input files referenced by tests (rack / leave lists)
  expected/       golden output, one <test>.out per test  (from --record)
  manifest.json   environment fingerprint captured at record time
```

There are two kinds:

- **Per-combo suites**, named `<lexicon>-<alphabet>` (e.g. `nwl23-english`).
  These hold only the **language-agnostic** levers (`anagram`, `enumerate`,
  `staticleaves`, `leavecalc`, `selfplay`, `playability`) — the ones worth
  replicating across dictionaries/languages. To add a combination, create
  `suites/<lex>-<alpha>/suite.json`, adjust the inputs to that language, and run
  `--record <name>`.

- **The `common` suite.** The position-specific levers (`positions`, `report`,
  `htmlreport`) load `.gcg` boards laid out with English words scored against
  `nwl23`, so they're meaningful only for the default dictionary/language. They
  live in one place, fixed to nwl23-english, and are **not** replicated per
  combo.

Running with no arguments runs every suite; the `common` suite runs alongside
whatever per-combo suites exist.

## What is (and isn't) tested

Only the **Speedy Player** produces reproducible move choices, so it's the only
computer player exercised. Everything that draws tiles — positions with unknown
racks, game reports, self-play — is made deterministic by passing `--seed`;
`testharness.cpp` applies that seed before dispatching to any mode.

Levers covered: `anagram`, `enumerate`, `staticleaves`, `leavecalc`,
`positions`, `report`, `htmlreport`, `selfplay` (including Speedy-vs-Speedy at a
fixed seed), and `playability`. `randomracks` (infinite) isn't regression-testable
and is left out. `worddump` isn't asserted on directly — its output shape depends
on whether a gaddag is present — but it's used internally to build gaddags (below).

## Gaddags

The data dir ships each lexicon's `.dawg` but not its `.gaddag` (a generated
file the repo doesn't carry). Move generation produces **identical** results
either way, so goldens don't depend on the gaddag — but with one, Speedy Player
runs at full strength and noticeably faster.

When a lexicon has no gaddag in the data dir, the driver builds one from the dawg
(`libquackle_test --mode=worddump` → `makegaddag`, both from the target build
tree) and injects it via the harness's cwd-relative `lexica/` lookup — nothing is
written into the repo. Generated gaddags are cached under the system temp dir
(keyed by dawg content), so only the first run pays the ~10s build cost.

- `makegaddag` must be built (it's beside `libquackle_test` in the build tree).
  If it isn't, the driver notes so and runs degraded — slower, identical results.
- `--no-gaddag` forces degraded mode.
- `manifest.json` records `gaddag_present`; a change since recording triggers a
  warning (move-generation goldens *may* need re-recording, though in practice a
  gaddag alone doesn't change them).

## Notes

- Output is normalized before comparison: the startup banner (up to the
  `using seed N` line) and wall-clock timings (`played in N seconds`) are
  stripped, so goldens depend only on libquackle behavior.
- Large outputs (`enumerate`, 3.2M lines) are compared by SHA-256 digest rather
  than stored verbatim; set `"digest": true` on such a test.
- `manifest.json` records whether a gaddag and lexicon strategy files were
  present when goldens were recorded. If that changes, `run_regression.py` warns
  that move-generation goldens likely need re-recording — building an `nwl23`
  gaddag, for instance, changes Speedy Player's move generation.
