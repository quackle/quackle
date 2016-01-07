CC=g++

QTFLAGS := $(shell pkg-config QtCore --cflags)
PYTHONFLAGS := $(shell pkg-config python2 --cflags)
LUAFLAGS := $(shell pkg-config lua52 --cflags)

INCLUDES=-I..

python/quackle_wrap.cxx:
	@test -d python || mkdir python
	swig -c++ -o $@ $(INCLUDES) $(QTFLAGS) -python quackle.i

python/quackle_wrap.o: python/quackle_wrap.cxx
	$(CC) -c -std=c++11 -fPIC $< $(QTFLAGS) $(PYTHONFLAGS) $(INCLUDES) -o $@

python: python/quackle_wrap.o
	ln -sf ../../lib/release/libquackle.so.0 python/libquackle.so.0
	ln -sf ../../quackleio/lib/release/libquackleio.so.0 python/libquackleio.so.0
	$(CC) -std=c++11 -shared ../lib/release/libquackle.so ../quackleio/lib/release/libquackleio.so $< -o python/_quackle.so

go:
	@test -d go || mkdir go
	ln -sf ../../lib/release/libquackle.so.0 go/libquackle.so.0
	ln -sf ../../quackleio/lib/release/libquackleio.so.0 go/libquackleio.so.0
	swig -c++ -o go/quackle_wrap.cxx -I../ $(QTFLAGS) -go -cgo -intgosize 64 quackle.i

lua/quackle_wrap.cxx:
	@test -d lua || mkdir lua
	swig -c++ -o $@ $(INCLUDES) $(QTFLAGS) -lua quackle.i

lua/quackle_wrap.o: lua/quackle_wrap.cxx
	$(CC) -std=c++11 -fPIC $(LUAFLAGS) $(QTFLAGS) $(INCLUDES) -c $< -o $@

lua: lua/quackle_wrap.o
	ln -sf ../../lib/release/libquackle.so.0 lua/libquackle.so.0
	ln -sf ../../quackleio/lib/release/libquackleio.so.0 lua/libquackleio.so.0
	$(CC) -std=c++11 -shared $(LUAFLAGS) ../lib/release/libquackle.so ../quackleio/lib/release/libquackleio.so $< -o lua/quackle.so

.PHONY: clean

clean:
	-rm -rf python/libquackle.*
	-rm -rf python/libquackleio.*
	-rm -rf python/quackle.py
	-rm -rf lua/libquackle.*
	-rm -rf lua/libquackleio.*
	-rm -rf go/libquackle.*
	-rm -rf go/libquackleio.*
	-rm -rf */*_wrap.cxx
	-rm -rf */*.o
	-rm -rf */*.pyc
	-rm -rf */*.so
	-rm -rf lua
	-rm -rf go
