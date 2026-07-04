# AGENTS.md

## Project layout

Quackle is a cross-platform (macOS, Windows, Linux) crossword game AI/analysis tool, built with CMake across three components:

- `CMakeLists.txt` (root) - builds `libquackle`, the engine. No Qt dependency.
- `quackleio/CMakeLists.txt` - builds `libquackleio`. Depends on Qt.
- `quacker/CMakeLists.txt` - builds the `Quackle` application. Depends on libquackle and libquackleio; is the top-level build (it `add_subdirectory`s the other two).

Other top-level directories (`makeminidawg/`, `makegaddag/`, etc.) build standalone one-off utilities and aren't a build concern right now.

## Build status

Qt5 and Qt6.2.4 both build and run on macOS (Apple Silicon), including universal
(`x86_64;arm64`) builds. Qt 6.12.0 not yet verified.

- Qt 5.15.2 (`~/dev/qt/5.15.2`) - working
- Qt 6.2.4 (`~/dev/qt/6.2.4`) - working
- Qt 6.12.0 (`~/dev/qt/6.12.0`) - not yet verified

Only macOS is currently working. Windows/Linux build setups aren't addressed yet.

## Building (macOS)

From a build directory, configure against `quacker/` (which pulls in libquackle and libquackleio) and build with Ninja. For Qt5:

```
cmake --fresh -DCMAKE_PREFIX_PATH=~/dev/qt/5.15.2/clang_64/ -GNinja -DCMAKE_OSX_ARCHITECTURES=x86_64 ../quacker/
ninja
```

For Qt6, add `-DQT_VERSION=6` and use the `macos/` subdirectory instead of `clang_64/`, e.g. `-DCMAKE_PREFIX_PATH=~/dev/qt/6.2.4/macos/`. For a universal build, set `-DCMAKE_OSX_ARCHITECTURES="x86_64;arm64"`.

### Qt6 + AGL.framework on newer Xcode

Qt6 releases up through roughly 6.4 link `-framework AGL` (via `FindWrapOpenGL.cmake`) even though nothing actually calls into it. Xcode 15+ dropped `AGL.framework` from the SDK, so linking fails there, even though the framework still resolves fine at runtime via the dyld shared cache. `quacker/CMakeLists.txt` works around this by searching known Command Line Tools SDK locations for a copy of `AGL.framework` and adding it to the linker's framework search path when building Qt6 on Apple. If no such SDK is found on a given machine, install the Xcode Command Line Tools (which bundles an older SDK) to unblock the link.
