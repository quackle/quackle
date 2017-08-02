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
CONFIG += release staticlib
CONFIG -= x11

QMAKE_CXXFLAGS += -std=c++11
#QMAKE_CXXFLAGS:!win32-msvc2013 += -Wno-unknown-warning-option -Wno-deprecated-register

# Input
HEADERS += *.h

SOURCES += *.cpp

macx {
	CONFIG += x86	
  QMAKE_MACOSX_DEPLOYMENT_TARGET = 10.8
}

unix:!macx {
	QMAKE_CXXFLAGS += -Wno-unused-local-typedefs
}
