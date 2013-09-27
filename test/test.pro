TEMPLATE = app
DEPENDPATH += .. ../quackleio
INCLUDEPATH += . ..

# enable/disable debug symbols
# CONFIG += debug

CONFIG += console
CONFIG -= x11
CONFIG += release

build_pass:CONFIG(debug, debug|release) {
	LIBS += -L../debug -L../quackleio/debug
}

build_pass:CONFIG(release, debug|release) {
	LIBS += -L../release -L../quackleio/release
}

LIBS += -L.. -L../quackleio -lquackle -lquackleio

# Input
HEADERS += testharness.h trademarkedboards.h
SOURCES += testharness.cpp testmain.cpp trademarkedboards.cpp


win32:!win32-g++ {
	QMAKE_CFLAGS_DEBUG     ~= s/-MDd/-MTd/
	QMAKE_CXXFLAGS_DEBUG   ~= s/-MDd/-MTd/
	QMAKE_CFLAGS_RELEASE   ~= s/-MD/-MT/
	QMAKE_CXXFLAGS_RELEASE ~= s/-MD/-MT/
}

macx-g++ {
    QMAKE_CXXFLAGS += -fpermissive
}
