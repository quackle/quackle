CC=g++

#---------------------------------
# Package versions for pkg-config

# QT=QtCore # Qt4
QT=Qt5Core # Qt5
LUA=lua

#---------------------------------
# Flags

QTFLAGS := $(shell pkg-config $(QT) --cflags)
QTLIBS := $(shell pkg-config $(QT) --libs)
PYTHON2FLAGS := $(shell pkg-config python2 --cflags)
PYTHON3FLAGS := $(shell pkg-config python3 --cflags)
PHPFLAGS := $(shell php-config --includes)
PHPLIBS := $(shell php-config --libs)
LUAFLAGS := $(shell pkg-config $(LUA) --cflags)

INCLUDES=-I..
QUACKLELIBS=../lib/release/libquackle.a ../quackleio/lib/release/libquackleio.a

#---------------------------------
# PHP

php/quackle_wrap.cxx:
	@test -d php || mkdir php
	swig -c++ -o $@ $(INCLUDES) $(QTFLAGS) -php quackle.i

php/quackle_wrap.o: php/quackle_wrap.cxx
	$(CC) -std=c++11 -fPIC $(QTFLAGS) $(PHPFLAGS) $(PHPLIBS) $(INCLUDES) -c $< -o $@

php: php/quackle_wrap.o
	$(CC) -std=c++11 -shared -Wl,--whole-archive $(QUACKLELIBS) -Wl,--no-whole-archive $(QTLIBS) $< -o php/quackle.so

#---------------------------------
# Python2

python2/quackle_wrap.cxx:
	@test -d python2 || mkdir python2
	swig -c++ -o $@ $(INCLUDES) $(QTFLAGS) -python quackle.i

python2/quackle_wrap.o: python2/quackle_wrap.cxx
	$(CC) -std=c++11 -fPIC $(QTFLAGS) $(PYTHON2FLAGS) $(INCLUDES) -c $< -o $@

python2: python2/quackle_wrap.o
	$(CC) -std=c++11 -shared -Wl,--whole-archive $(QUACKLELIBS) -Wl,--no-whole-archive $(QTLIBS) $< -o python2/_quackle.so

#---------------------------------
# Python3

python3/quackle_wrap.cxx:
	@test -d python3 || mkdir python3
	swig -c++ -o $@ $(INCLUDES) $(QTFLAGS) -python -py3 quackle.i

python3/quackle_wrap.o: python3/quackle_wrap.cxx
	$(CC) -std=c++11 -fPIC $(QTFLAGS) $(PYTHON3FLAGS) $(INCLUDES) -c $< -o $@

python3: python3/quackle_wrap.o
	$(CC) -std=c++11 -shared -Wl,--whole-archive $(QUACKLELIBS) -Wl,--no-whole-archive $(QTLIBS) $< -o python3/_quackle.so

#---------------------------------
# Go

go:
	ln -sf ../quackle.i go/quackle.swigcxx
	go build ./go/...


#---------------------------------
# Lua

lua/quackle_wrap.cxx:
	@test -d lua || mkdir lua
	swig -c++ -o $@ $(INCLUDES) $(QTFLAGS) -lua quackle.i

lua/quackle_wrap.o: lua/quackle_wrap.cxx
	$(CC) -std=c++11 -fPIC $(LUAFLAGS) $(QTFLAGS) $(INCLUDES) -c $< -o $@

lua: lua/quackle_wrap.o
	$(CC) -std=c++11 -shared $(LUAFLAGS) -Wl,--whole-archive $(QUACKLELIBS) -Wl,--no-whole-archive $(QTLIBS) $< -o lua/quackle.so

#---------------------------------

.PHONY: clean go

clean:
	-rm -rf python2/quackle.py
	-rm -rf python3/quackle.py
	-rm -rf python3/__pycache__
	-rm -rf php/*php*
	-rm -rf */*_wrap.cxx
	-rm -rf */*.o
	-rm -rf */*.so
	-rm -rf */*.pyc
	-rm -rf go/quackle.swigcxx
