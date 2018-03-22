# Qt .pro file for emscripten integration.

TEMPLATE = app
DEPENDPATH += ..
INCLUDEPATH += . ..
CONFIG += release
CONFIG -= debug

QT -= gui core

release {
  OBJECTS_DIR = obj/release
  QMAKE_LIBDIR += ../lib/release
}

MOC_DIR = moc

CONFIG += release c++14
CONFIG -= x11

LIBS += -lquackle

!msvc {
  QMAKE_CXXFLAGS += -Wno-unknown-warning-option -Wno-deprecated-register
}
HEADERS += non_qt_gcgio.h
SOURCES += tester.cpp ../test/trademarkedboards.cpp non_qt_gcgio.cpp

macx-g++ {
    QMAKE_CXXFLAGS += -fpermissive
}

# TODO: Later add linux, etc flags. see makegaddag.pro for examples.

linux {
  QMAKE_CXXFLAGS += -std=c++14
}