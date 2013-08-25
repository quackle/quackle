TEMPLATE = app
DEPENDPATH += .. ../quackleio
INCLUDEPATH += . ..

# enable/disable debug symbols
# CONFIG += debug

CONFIG += console

build_pass:CONFIG(debug, debug|release) {
	LIBS += -L../debug -L../quackleio/debug
}

build_pass:CONFIG(release, debug|release) {
	LIBS += -L../release -L../quackleio/release
}

LIBS += -L.. -L../quackleio -lquackle -lquackleio

# Input
SOURCES += encodeleaves.cpp


win32:!win32-g++ {
	QMAKE_CFLAGS_DEBUG     ~= s/-MDd/-MTd/
	QMAKE_CXXFLAGS_DEBUG   ~= s/-MDd/-MTd/
	QMAKE_CFLAGS_RELEASE   ~= s/-MD/-MT/
	QMAKE_CXXFLAGS_RELEASE ~= s/-MD/-MT/
}
