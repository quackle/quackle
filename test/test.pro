TEMPLATE = app
DEPENDPATH += .. ../quackleio
INCLUDEPATH += . ..

# enable/disable debug symbols
# CONFIG += debug

CONFIG += console c++14
CONFIG -= x11
CONFIG -= app_bundle
CONFIG += release
CONFIG -= debug

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

!msvc {
  QMAKE_CXXFLAGS += -Wno-unknown-warning-option -Wno-deprecated-register
}

# Input
HEADERS += testharness.h trademarkedboards.h
SOURCES += testharness.cpp testmain.cpp trademarkedboards.cpp


macx-g++ {
    QMAKE_CXXFLAGS += -fpermissive
}

linux { # old unixes/Qt distribs running around...most notably on Travis-CI
  QMAKE_CXXFLAGS += -std=c++1y
}
