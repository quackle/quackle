# AGENTS.md

## Project layout

Quackle is a cross-platform (macOS, Windows, Linux) crossword game AI/analysis tool, built with CMake across three components:

- `CMakeLists.txt` (root) - builds `libquackle`, the engine. No Qt dependency.
- `quackleio/CMakeLists.txt` - builds `libquackleio`. Depends on Qt.
- `quacker/CMakeLists.txt` - builds the `Quackle` application. Depends on libquackle and libquackleio; is the top-level build (it `add_subdirectory`s the other two, plus `test/` and the standalone utilities below).

Other top-level directories (`encodeleaves/`, `makegaddag/`, `makeminidawg/`, `gaddagize/`) build standalone one-off utilities, each with its own `CMakeLists.txt` pulled in by `quacker/CMakeLists.txt`.

## Build status

Builds work on all platforms: macOS, Windows, and Linux.

Qt5, Qt6.2.4, and Qt6.12.0 all build and run on macOS (Apple Silicon), including
universal (`x86_64;arm64`) builds.

- Qt 5.15.2 (`~/dev/qt/5.15.2`) - working
- Qt 6.2.4 (`~/dev/qt/6.2.4`) - working
- Qt 6.12.0 (`~/dev/qt/6.12.0`) - working

Linux uses the same Qt locations as macOS (`~/dev/qt/<ver>/...`).

Windows is only built against Qt 6.10.3, installed at `C:\dev\qt\<ver>\<arch>` (e.g. `C:\dev\qt\6.10.3\msvc2022_64`).

## Building (macOS)

From a build directory, configure against `quacker/` (which pulls in libquackle and libquackleio) and build with Ninja. `QT_VERSION` defaults to `6`. For Qt6:

```
cmake --fresh -DCMAKE_PREFIX_PATH=~/dev/qt/6.2.4/macos/ -GNinja -DCMAKE_OSX_ARCHITECTURES=x86_64 ../quacker/
ninja
```

For a universal build, set `-DCMAKE_OSX_ARCHITECTURES="x86_64;arm64"`.

For Qt5, add `-DQT_VERSION=5` and use the `clang_64/` subdirectory instead of `macos/`, e.g. `-DCMAKE_PREFIX_PATH=~/dev/qt/5.15.2/clang_64/`.

### Qt6 + AGL.framework on newer Xcode

Qt6 releases up through roughly 6.4 link `-framework AGL` (via `FindWrapOpenGL.cmake`) even though nothing actually calls into it. Xcode 15+ dropped `AGL.framework` from the SDK, so linking fails there, even though the framework still resolves fine at runtime via the dyld shared cache. `quacker/CMakeLists.txt` works around this by searching known Command Line Tools SDK locations for a copy of `AGL.framework` and adding it to the linker's framework search path when building Qt6 on Apple. If no such SDK is found on a given machine, install the Xcode Command Line Tools (which bundles an older SDK) to unblock the link.

## Sanitizers

Three CMake options, all `OFF` by default, defined in `Settings.cmake` (included by every
subproject, so they apply to the whole tree):

- `QUACKLE_SANITIZE_ADDRESS` - AddressSanitizer
- `QUACKLE_SANITIZE_UNDEFINED` - UndefinedBehaviorSanitizer
- `QUACKLE_SANITIZE_THREAD` - ThreadSanitizer

Their values are printed on every `cmake` configure, so check the configure output rather than
guessing what a build tree has enabled.

Address and Thread are mutually exclusive (configure fails if both are set); Address and Undefined
combine fine. On non-MSVC these add `-fsanitize=...` to compile and link, plus
`-fno-omit-frame-pointer -g` for usable stack traces. MSVC supports only AddressSanitizer;
requesting UBSan or TSan there is a configure error.

Set these in a **fresh** build directory. Toggling them in an existing tree only recompiles what
CMake considers dirty and can leave a half-instrumented binary.

```
cmake --fresh -DCMAKE_PREFIX_PATH=~/dev/qt/6.2.4/macos/ -GNinja \
    -DCMAKE_BUILD_TYPE=RelWithDebInfo -DQUACKLE_SANITIZE_ADDRESS=ON ../quacker/
```

## Regression testing

`test/regression/README.md` is the reference; read it before touching the suites. In short:
`test/regression/run_regression.py` drives the `libquackle_test` harness across its `--mode` levers
at a fixed seed and diffs against recorded goldens.

```
cmake --build build --target libquackle_test makegaddag
python3 test/regression/run_regression.py          # all suites; non-zero exit on failure
python3 test/regression/run_regression.py --list
```

Build `makegaddag` too: without it the driver runs degraded (same results, much slower).

**Run regression tests against a Release or RelWithDebInfo build, with `QUACKLE_SANITIZE_ADDRESS=ON`
where the toolchain allows it.** The suites are slow enough that an unoptimized Debug build wastes
real time, and they exercise a lot of engine code paths — which makes them the best ASan coverage
available. RelWithDebInfo is generally the better of the two, since a sanitizer report is far more
useful with debug info. Note that ASan itself slows things down; if a run is only checking goldens
and not hunting for memory errors, plain Release is fine.

Goldens are behavior assertions, not scratch files. Only re-record (`--record`) when a behavior
change is intended, and say so explicitly when reporting results — never re-record to make a
failing test pass.

## Code formatting

Run `clang-format` (config in `.clang-format`) on source files you add or modify.

## Comments

Comment the surprising, the non-obvious, and the *why* the code can't state itself (an external
constraint, a workaround for someone else's bug, a subtle invariant). This applies everywhere —
source, CMake, CI YAML.

Do **not** narrate history: what broke, the symptoms observed, how it was diagnosed, or how a
value was arrived at belongs in the commit message, not the source. A comment reciting a bug's
backstory is noise that makes the code harder to read. Keep comments short — one line naming the
constraint beats a paragraph retelling the investigation.
