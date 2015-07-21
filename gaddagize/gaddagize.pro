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

# Input
SOURCES += gaddagize.cpp


macx-g++ {
    QMAKE_CXXFLAGS += -fpermissive
}
