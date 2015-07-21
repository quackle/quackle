TEMPLATE = app
DEPENDPATH += .. ../quackleio
INCLUDEPATH += . ..

# enable/disable debug symbols
# CONFIG += debug

CONFIG += console
CONFIG -= x11
CONFIG -= app_bundle
CONFIG += release

debug {
  OBJECTS_DIR = obj/debug
}

release {
  OBJECTS_DIR = obj/release
}

LIBS += -lquackleio -lquackle

QMAKE_LFLAGS_RELEASE += -L../lib/release -L../quackleio/lib/release
QMAKE_LFLAGS_DEBUG += -L../lib/debug -L../quackleio/lib/debug

# Input
HEADERS += testharness.h trademarkedboards.h
SOURCES += testharness.cpp testmain.cpp trademarkedboards.cpp


macx-g++ {
    QMAKE_CXXFLAGS += -fpermissive
}
