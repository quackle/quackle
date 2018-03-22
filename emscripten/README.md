Building libquackle for emscripten...

Note: These steps must be followed in the root directory; we are building libquackle here.

There doesn't seem to be an easy way to compile anything Qt-related into emscripten.

- qmake -spec linux-llvm
- Edit Makefile manually and replace g++ with em++ and gcc with emcc (only one spot each in the first few lines)
- (I think also replace ar with emar in the first few lines)
- emmake make
- It'll fail at the end but all the .o files are generated in /obj/release (note it won't fail if you do the emar replacement)
- cd to that directory and do `emcc -O2 *.o -o libquackle.js`
- Now we have a library! (That I have no idea how to use...)

----------------
Things to implement:
- GCG loading; this is implemented in quackleio, which is Qt-dependent. May not need straight GCG loading as much as some way to incrementally load moves. This might be a better approach, actually.

- Whatever flexiblealphabet does (flexure?)

Avenues to approach for using this library:

- Use extern "C" and embind, etc to export classes, instances, etc

- Build a C api (or even a JS api) and export these functions. JS api might sound like the cleanest way.
    - Need to enter moves, generate, and simulate for some number of iterations, pause simulation, add moves, etc. The very basics.


------------

Progress:

- *MUST COMPILE FILES WITH SAME OPTIMIZATION OPTIONS AS THE FINAL LINK BELOW*
- Compile libquackle as above, except skip emcc step
- Compile here as above. It will fail at the end
- Run `emcc --bind obj/release/api.o obj/release/trademarkedboards.o ../obj/release/*.o -o scripty.js` here. That creates scripty.js, which seems to have the symbols needed to run Quackle.
- `emcc -O2 --bind obj/release/api.o obj/release/trademarkedboards.o obj/release/non_qt_gcgio.o ../obj/release/*.o -o scripty.js --preload-file data@/ -s ALLOW_MEMORY_GROWTH=1 -s WASM=1`

- Figure out how to do this with wasm. Chrome doesn't support asm.js as fast as it could.
    - Had to add application/wasm to etc/mime.types. Will this be required for nginx, etc?