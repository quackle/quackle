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

## Code formatting

Run `clang-format` (config in `.clang-format`) on source files you add or modify.
