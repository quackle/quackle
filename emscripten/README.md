### Building libquackle for emscripten...

Note: These steps must be followed in the root directory; we are building libquackle here.

- qmake -spec linux-llvm (or maybe just `qmake`)
- Edit Makefile manually and replace g++ with em++ and gcc with emcc (only one spot each in the first few lines)
- Also replace ar with emar in the first few lines
- emmake make

------------

### Building Javascript interface (basically api.cpp here)

- Run `make` in this directory. This should generate scriptackle.{data, js, wasm} files. These files can then be copied out.

-------------

Notes:

- Had to add application/wasm to etc/mime.types. Will this be required for nginx, etc?
- See https://github.com/kripken/emscripten/wiki/WebAssembly for more tips
- It seems this is VERY finiking for right now. This combination of versions works for sure, some others may cause freezing or strange behavior...

  - MacOS High Sierra (10.13.4)
  - emsdk sdk-1.38.5-64bit
  - Qt 5.10.1 (qmake 3.1)
  - qmake should be called with qmake -spec linux-llvm 
  
  This should be built in the future with something like CircleCI to improve repeatability. 