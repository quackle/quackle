# AGENTS.md

## Project layout

Quackle is a cross-platform (macOS, Windows, Linux) crossword game AI/analysis tool, built with CMake across three components:

- `CMakeLists.txt` (root) - builds `libquackle`, the engine. No Qt dependency.
- `quackleio/CMakeLists.txt` - builds `libquackleio`. Depends on Qt.
- `quacker/CMakeLists.txt` - builds the `Quackle` application. Depends on libquackle and libquackleio; is the top-level build (it `add_subdirectory`s the other two).

Other top-level directories (`makeminidawg/`, `makegaddag/`, etc.) build standalone one-off utilities and aren't a build concern right now.

## Build status

Currently Qt5-only. Qt6 compatibility is in progress. Once builds work on Qt6, verify against all three installed Qt versions:

- Qt 5.15.2 (`~/dev/qt/5.15.2`)
- Qt 6.2.4 (`~/dev/qt/6.2.4`)
- Qt 6.12.0 (`~/dev/qt/6.12.0`)

Only macOS (Intel) is currently working. Windows/Linux build setups aren't addressed yet.

## Building (macOS, Qt5)

From a build directory, configure against `quacker/` (which pulls in libquackle and libquackleio) and build with Ninja:

```
cmake --fresh -DCMAKE_PREFIX_PATH=~/dev/qt/5.15.2/clang_64/ -GNinja -DCMAKE_OSX_ARCHITECTURES=x86_64 ../quacker/
ninja
```

For Qt6, use the `macos/` subdirectory instead of `clang_64/`, e.g. `-DCMAKE_PREFIX_PATH=~/dev/qt/6.2.4/macos/`.
