TEMPLATE = app
DEPENDPATH += .. ../quackleio
INCLUDEPATH += . ..

# enable/disable debug symbols
# CONFIG += debug

CONFIG += console

#LIBS += -L.. -L../quackleio -lquackle -lquackleio

# Input
SOURCES += gaddagize.cpp


win32:!win32-g++ {
	QMAKE_CFLAGS_DEBUG     ~= s/-MDd/-MTd/
	QMAKE_CXXFLAGS_DEBUG   ~= s/-MDd/-MTd/
	QMAKE_CFLAGS_RELEASE   ~= s/-MD/-MT/
	QMAKE_CXXFLAGS_RELEASE ~= s/-MD/-MT/
}
