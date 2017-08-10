TEMPLATE = app
DEPENDPATH += .. ../quackleio
INCLUDEPATH += . ..

# enable/disable debug symbols
# CONFIG += debug
CONFIG += release

CONFIG += console c++14
CONFIG -= app_bundle

debug {
  OBJECTS_DIR = obj/debug
}

release {
  OBJECTS_DIR = obj/release
}

!msvc {
  QMAKE_CXXFLAGS += -Wno-unknown-warning-option -Wno-deprecated-register
}

# Input
SOURCES += gaddagize.cpp


macx-g++ {
    QMAKE_CXXFLAGS += -fpermissive
}

linux { # old unixes/Qt distribs running around...most notably on Travis-CI
  QMAKE_CXXFLAGS += -std=c++1y
}
