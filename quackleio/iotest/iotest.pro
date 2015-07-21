TEMPLATE = app
DEPENDPATH += .
INCLUDEPATH += . .. ../..

# enable/disable debug symbols
#CONFIG += debug
CONFIG += release

debug {
  OBJECTS_DIR = obj/debug
}

release {
  OBJECTS_DIR = obj/release
}

LIBS += -lquackleio -lquackle

QMAKE_LFLAGS_RELEASE += -L../../lib/release -L../../quackleio/lib/release
QMAKE_LFLAGS_DEBUG += -L../../lib/debug -L../../quackleio/lib/debug

# Input
HEADERS += trademarkedboards.h
SOURCES += iotest.cpp trademarkedboards.cpp

macx-g++ {
    QMAKE_CXXFLAGS += -fpermissive
}
