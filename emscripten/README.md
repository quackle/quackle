### Building libquackle for emscripten...

Note: These steps must be followed in the root directory; we are building libquackle here.

There doesn't seem to be an easy way to compile anything Qt-related into emscripten.

- qmake -spec linux-llvm (or maybe just `qmake`)
- Edit Makefile manually and replace g++ with em++ and gcc with emcc (only one spot each in the first few lines)
- Also replace ar with emar in the first few lines
- emmake make


------------

### Building Javascript interface (basically api.cpp here)

- Compile libquackle as above
- Compile here as above. It will fail at the end
- Copy the needed files from the root's data directory to a data directory here.
- `emcc -O2 --bind obj/release/api.o obj/release/trademarkedboards.o obj/release/non_qt_gcgio.o ../lib/release/libquackle.a -o scriptackle.js --preload-file data@/ -s ALLOW_MEMORY_GROWTH=1 -s WASM=1`


-------------

Notes:

- Had to add application/wasm to etc/mime.types. Will this be required for nginx, etc?
- See https://github.com/kripken/emscripten/wiki/WebAssembly for more tips