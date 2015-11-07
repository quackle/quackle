TEMPLATE = app
DEPENDPATH += .. ../quackleio
INCLUDEPATH += . ..

# enable/disable debug symbols
# CONFIG += debug
CONFIG += release

CONFIG += console
CONFIG -= x11
CONFIG -= app_bundle

debug {
  OBJECTS_DIR = obj/debug
  QMAKE_LIBDIR += ../lib/debug ../quackleio/lib/debug
}

release {
  OBJECTS_DIR = obj/release
  QMAKE_LIBDIR += ../lib/release ../quackleio/lib/release
}

win32:!win32-g++ {
  LIBS += -lquackleio -llibquackle
} else {
  LIBS += -lquackleio -lquackle
}

QMAKE_CXXFLAGS += -std=c++11
QMAKE_CXXFLAGS:!win32-msvc2013 += -Wno-unknown-warning-option -Wno-deprecated-register


# Input
HEADERS += 
SOURCES += makeminidawgmain.cpp


macx-g++ {
    QMAKE_CXXFLAGS += -fpermissive
}
