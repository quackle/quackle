set -eu

#################### Generate Python bindings
ln -sf ../../lib/release/libquackle.so.0 python/libquackle.so.0
ln -sf ../../quackleio/lib/release/libquackleio.so.0 python/libquackleio.so.0
swig -c++ -o python/pyquackle.cxx -I../ `pkg-config QtCore --cflags` -python quackle.i
g++ -c -std=c++11 -fPIC python/pyquackle.cxx `pkg-config QtCore --cflags` `pkg-config python2 --cflags` -I../ -o python/pyquackle.o
g++ -std=c++11 -shared ../lib/release/libquackle.so ../quackleio/lib/release/libquackleio.so python/pyquackle.o -o python/_quackle.so

#################### Generate Go bindings
mkdir go
ln -sf ../../lib/release/libquackle.so.0 go/libquackle.so.0
ln -sf ../../quackleio/lib/release/libquackleio.so.0 go/libquackleio.so.0
swig -c++ -o go/quackle_wrap.cxx -I../ `pkg-config QtCore --cflags` -go -cgo -intgosize 64 quackle.i

#################### Generate Lua bindings
mkdir lua
ln -sf ../../lib/release/libquackle.so.0 lua/libquackle.so.0
ln -sf ../../quackleio/lib/release/libquackleio.so.0 lua/libquackleio.so.0
swig -c++ -o lua/quackle_wrap.cxx -I../ `pkg-config QtCore --cflags` -lua quackle.i
g++ -std=c++11 -fPIC `pkg-config lua52 --cflags` `pkg-config QtCore --cflags` -I../ -c lua/quackle_wrap.cxx -o lua/quackle_wrap.o
g++ -std=c++11 -shared `pkg-config lua52 --libs` ../lib/release/libquackle.so ../quackleio/lib/release/libquackleio.so lua/quackle_wrap.o -o lua/quackle.so
