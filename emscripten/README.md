Building libquackle for emscripten...

- qmake -spec linux-llvm
- Edit Makefile manually and replace g++ with em++ and gcc with emcc (only one spot each in the first few lines)
- (I think also replace ar with emar in the first few lines)
- emmake make
- It'll fail at the end but all the .o files are generated in /obj/release (note it won't fail if you do the emar replacement)
- cd to that directory and do `emcc -O2 *.o -o libquackle.js`
- Now we have a library! (That I have no idea how to use...)