TEMPLATE = lib
INCLUDEPATH += . ..
DEPENDPATH += . ..
VERSION = 0.9
QT -= gui
debug {
  OBJECTS_DIR = obj/debug
  DESTDIR = lib/debug
}

release {
  OBJECTS_DIR = obj/release
  DESTDIR = lib/release
}

MOC_DIR = moc

# enable/disable debug symbols
#CONFIG += debug staticlib
CONFIG += release staticlib c++14
CONFIG -= x11

!msvc {
  QMAKE_CXXFLAGS += -Wno-unknown-warning-option -Wno-deprecated-register
}

# Input
HEADERS += *.h

SOURCES += *.cpp

macx {
  CONFIG += x86	
  QMAKE_MACOSX_DEPLOYMENT_TARGET = 10.9
}

unix:!macx {
	QMAKE_CXXFLAGS += -Wno-unused-local-typedefs
}

linux { # old unixes/Qt distribs running around...most notably on Travis-CI
  QMAKE_CXXFLAGS += -std=c++1y
}
