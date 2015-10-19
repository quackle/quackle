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

QMAKE_CXXFLAGS += -std=c++11

# enable/disable debug symbols
#CONFIG += debug staticlib
CONFIG += release staticlib
CONFIG -= x11

# Input
HEADERS += *.h

SOURCES += *.cpp

macx-g++ {
    QMAKE_CXXFLAGS += -fpermissive
}

macx-xcode {
	CONFIG += x86	
}

unix:!macx {
	QMAKE_CXXFLAGS += -Wno-unused-local-typedefs
}
