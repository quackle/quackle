TEMPLATE = app
VERSION = 0.97
DEPENDPATH += .. ../quackleio
INCLUDEPATH += . ..

MOC_DIR = moc

# enable/disable debug symbols
#CONFIG += debug
CONFIG += release

debug {
  OBJECTS_DIR = obj/debug
}

release {
  OBJECTS_DIR = obj/release
}

QMAKE_LFLAGS_RELEASE += -L../lib/release -L../quackleio/lib/release
QMAKE_LFLAGS_DEBUG += -L../lib/debug -L../quackleio/lib/debug

LIBS += -lquackleio -lquackle

# Input
HEADERS += *.h
SOURCES += *.cpp

win32 {
	RC_FILE = quacker.rc
}

win32:!win32-g++ {
	QMAKE_CFLAGS_DEBUG     ~= s/-MDd/-MTd/
	QMAKE_CXXFLAGS_DEBUG   ~= s/-MDd/-MTd/
	QMAKE_CFLAGS_RELEASE   ~= s/-MD/-MT/
	QMAKE_CXXFLAGS_RELEASE ~= s/-MD/-MT/
}

macx {
	DEFINES += FORCE_SECONDARY_ARROW_GLYPHS=1
}

macx-g++ {
    QMAKE_CXXFLAGS += -fpermissive
}