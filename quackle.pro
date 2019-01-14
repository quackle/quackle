TEMPLATE = lib
INCLUDEPATH += .
DEPENDPATH += .
VERSION = 0.99
QT -= gui core
win32:!win32-g++ { # VS solutions don't like having two projects named "quackle"
  TARGET = libquackle
}

debug {
  OBJECTS_DIR = obj/debug
  DESTDIR = lib/debug
}

release {
  OBJECTS_DIR = obj/release
  DESTDIR = lib/release
}

# enable/disable debug symbols
#CONFIG += debug staticlib
CONFIG += release staticlib c++14
CONFIG -= x11

# Input
HEADERS += *.h

SOURCES += $$files(*.cpp)
SOURCES -= \
	loaddawg.cpp \
	loadgaddag.cpp \
	makedawg.cpp \
	quackletest.cpp

macx {
	CONFIG += x86	
  QMAKE_MACOSX_DEPLOYMENT_TARGET = 10.9
}

linux { # old unixes/Qt distribs running around...most notably on Travis-CI
  QMAKE_CXXFLAGS += -std=c++1y
}
