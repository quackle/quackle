TEMPLATE = app
DEPENDPATH += .. ../quackleio
INCLUDEPATH += . ..

# enable/disable debug symbols
# CONFIG += debug
CONFIG += release

CONFIG += console
CONFIG -= app_bundle

debug {
  OBJECTS_DIR = obj/debug
}

release {
  OBJECTS_DIR = obj/release
}

QMAKE_CXXFLAGS:!win32-msvc2013 += -std=c++11 -Wno-unknown-warning-option -Wno-deprecated-register

# Input
SOURCES += gaddagize.cpp


macx-g++ {
    QMAKE_CXXFLAGS += -fpermissive
}
