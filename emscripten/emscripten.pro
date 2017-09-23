# Qt .pro file for emscripten integration.

TEMPLATE = app
DEPENDPATH += .. ../quackleio
INCLUDEPATH += . ..
CONFIG += release
CONFIG -= debug

release {
  OBJECTS_DIR = obj/release
  QMAKE_LIBDIR += ../lib/release ../quackleio/lib/release
}

MOC_DIR = moc

CONFIG += console c++14
CONFIG -= app_bundle

LIBS += -lquackleio -lquackle

!msvc {
  QMAKE_CXXFLAGS += -Wno-unknown-warning-option -Wno-deprecated-register
}

SOURCES += js_interface.cpp

macx-g++ {
    QMAKE_CXXFLAGS += -fpermissive
}

# TODO: Later add linux, etc flags. see makegaddag.pro for examples.
