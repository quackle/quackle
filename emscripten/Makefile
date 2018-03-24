CXX = em++
CXXFLAGS = -pipe -Wno-unknown-warning-option -Wno-deprecated-register -std=c++14 -O2 -std=gnu++14 -Wall -W -D_REENTRANT -fPIC
INCPATH = -I. -I..
LINK = emcc
LFLAGS = -O2 --bind --preload-file data@/ -s ALLOW_MEMORY_GROWTH=1 -s WASM=1
LIBS = ../lib/release/libquackle.a

SOURCES = api.cpp \
          ../test/trademarkedboards.cpp \
          non_qt_gcgio.cpp

OBJECTS = obj/release/api.o \
          obj/release/trademarkedboards.o \
          obj/release/non_qt_gcgio.o

TARGET = scriptackle.js

default: $(TARGET)


$(TARGET): $(OBJECTS)
	$(LINK) $(LFLAGS)  -o $(TARGET) $(OBJECTS) $(LIBS)

clean:
	rm -rf $(OBJECTS)
	rm -rf scriptackle.*

obj/release/api.o: api.cpp  # Maybe other h files here too?
	$(CXX) -c $(CXXFLAGS) $(INCPATH) -o obj/release/api.o api.cpp

obj/release/trademarkedboards.o:
	$(CXX) -c $(CXXFLAGS) $(INCPATH) -o obj/release/trademarkedboards.o ../test/trademarkedboards.cpp

obj/release/non_qt_gcgio.o: non_qt_gcgio.cpp
	$(CXX) -c $(CXXFLAGS) $(INCPATH) -o obj/release/non_qt_gcgio.o non_qt_gcgio.cpp

