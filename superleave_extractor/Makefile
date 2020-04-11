CXX = g++
CXXFLAGS = -pipe -Wno-unknown-warning-option -Wno-deprecated-register -std=c++14 -O2 -std=gnu++14 -Wall -W -D_REENTRANT -fPIC
INCPATH = -I. -I..
LINK = g++
LFLAGS = -O2
LIBS = ../lib/release/libquackle.a

SOURCES = main.cpp

OBJECTS = obj/release/main.o

TARGET = extractor

default: $(TARGET)


$(TARGET): $(OBJECTS) $(LIBS)
	$(LINK) $(LFLAGS)  -o $(TARGET) $(OBJECTS) $(LIBS)

clean:
	rm -rf $(OBJECTS)

obj/release/main.o: main.cpp  # Maybe other h files here too?
	$(CXX) -c $(CXXFLAGS) $(INCPATH) -o obj/release/main.o main.cpp

