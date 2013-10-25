TEMPLATE = app
DEPENDPATH += .. ../quackleio
INCLUDEPATH += . ..

# enable/disable debug symbols
# CONFIG += debug
CONFIG += release

CONFIG += console

debug {
  OBJECTS_DIR = obj/debug
}

release {
  OBJECTS_DIR = obj/release
}

# Input
SOURCES += gaddagize.cpp


win32:!win32-g++ {
	QMAKE_CFLAGS_DEBUG     ~= s/-MDd/-MTd/
	QMAKE_CXXFLAGS_DEBUG   ~= s/-MDd/-MTd/
	QMAKE_CFLAGS_RELEASE   ~= s/-MD/-MT/
	QMAKE_CXXFLAGS_RELEASE ~= s/-MD/-MT/
}

macx-g++ {
    QMAKE_CXXFLAGS += -fpermissive
}
