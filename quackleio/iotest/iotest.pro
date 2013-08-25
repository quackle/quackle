TEMPLATE = app
DEPENDPATH += .
INCLUDEPATH += . .. ../..

# enable/disable debug symbols
#CONFIG += debug

debug {
  OBJECTS_DIR = obj/debug
  win32 { LIBS += -L../debug -L../../debug }
}

release {
  OBJECTS_DIR = obj/release
  win32 { LIBS += -L../release -L../../release }
}

LIBS += -L.. -L../.. -lquackle -lquackleio

# Input
HEADERS += trademarkedboards.h
SOURCES += iotest.cpp trademarkedboards.cpp

win32:!win32-g++ {
	QMAKE_CFLAGS_DEBUG     ~= s/-MDd/-MTd/
	QMAKE_CXXFLAGS_DEBUG   ~= s/-MDd/-MTd/
	QMAKE_CFLAGS_RELEASE   ~= s/-MD/-MT/
	QMAKE_CXXFLAGS_RELEASE ~= s/-MD/-MT/
}
