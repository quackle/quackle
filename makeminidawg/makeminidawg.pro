TEMPLATE = app
DEPENDPATH += .. ../quackleio
INCLUDEPATH += . ..

# enable/disable debug symbols
# CONFIG += debug
CONFIG += release

CONFIG += console
CONFIG -= x11

debug {
  OBJECTS_DIR = obj/debug
}

release {
  OBJECTS_DIR = obj/release
}

LIBS += -lquackle -lquackleio

QMAKE_LFLAGS_RELEASE += -L../lib/release -L../quackleio/lib/release
QMAKE_LFLAGS_DEBUG += -L../lib/debug -L../quackleio/lib/debug

# Input
HEADERS += minidawgmaker.h
SOURCES += minidawgmaker.cpp makeminidawgmain.cpp


win32:!win32-g++ {
	QMAKE_CFLAGS_DEBUG     ~= s/-MDd/-MTd/
	QMAKE_CXXFLAGS_DEBUG   ~= s/-MDd/-MTd/
	QMAKE_CFLAGS_RELEASE   ~= s/-MD/-MT/
	QMAKE_CXXFLAGS_RELEASE ~= s/-MD/-MT/
}

macx-g++ {
    QMAKE_CXXFLAGS += -fpermissive
}
