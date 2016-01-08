CC=g++

QTFLAGS := $(shell pkg-config QtCore --cflags)
QTLIBS := $(shell pkg-config QtCore --libs)
PYTHONFLAGS := $(shell pkg-config python2 --cflags)
LUAFLAGS := $(shell pkg-config lua52 --cflags)

INCLUDES=-I..
QUACKLELIBS=../lib/release/libquackle.a ../quackleio/lib/release/libquackleio.a

python/quackle_wrap.cxx:
	@test -d python || mkdir python
	swig -c++ -o $@ $(INCLUDES) $(QTFLAGS) -python quackle.i

python/quackle_wrap.o: python/quackle_wrap.cxx
	$(CC) -std=c++11 -fPIC $(QTFLAGS) $(PYTHONFLAGS) $(INCLUDES) -c $< -o $@

python: python/quackle_wrap.o
	$(CC) -std=c++11 -shared -Wl,--whole-archive $(QUACKLELIBS) -Wl,--no-whole-archive $(QTLIBS) $< -o python/_quackle.so

go:
	ln -sf ../quackle.i go/quackle.swigcxx
	go build ./go/...

lua/quackle_wrap.cxx:
	@test -d lua || mkdir lua
	swig -c++ -o $@ $(INCLUDES) $(QTFLAGS) -lua quackle.i

lua/quackle_wrap.o: lua/quackle_wrap.cxx
	$(CC) -std=c++11 -fPIC $(LUAFLAGS) $(QTFLAGS) $(INCLUDES) -c $< -o $@

lua: lua/quackle_wrap.o
	$(CC) -std=c++11 -shared $(LUAFLAGS) -Wl,--whole-archive $(QUACKLELIBS) -Wl,--no-whole-archive $(QTLIBS) $< -o lua/quackle.so

.PHONY: clean go

clean:
	-rm -rf python/quackle.py
	-rm -rf */*_wrap.cxx
	-rm -rf */*.o
	-rm -rf */*.so
	-rm -rf */*.pyc
	-rm -rf lua
	-rm -rf go/quackle.swigcxx
