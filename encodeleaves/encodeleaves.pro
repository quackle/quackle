TEMPLATE = app
DEPENDPATH += .. ../quackleio
INCLUDEPATH += . ..

# enable/disable debug symbols
# CONFIG += debug
CONFIG += release
CONFIG -= debug

CONFIG += console c++14
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


# Input
SOURCES += encodeleaves.cpp


macx-g++ {
    QMAKE_CXXFLAGS += -fpermissive
}

linux { # old unixes/Qt distribs running around...most notably on Travis-CI
  QMAKE_CXXFLAGS += -std=c++1y
}
