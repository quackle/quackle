TEMPLATE = app
DEPENDPATH += .. ../quackleio
INCLUDEPATH += . ..
CONFIG += release

debug {
  OBJECTS_DIR = obj/debug
}

release {
  OBJECTS_DIR = obj/release
}

MOC_DIR = moc

# enable/disable debug symbols
# CONFIG += debug

CONFIG += console

LIBS += -lquackleio -lquackle

QMAKE_LFLAGS_RELEASE += -L../lib/release -L../quackleio/lib/release
QMAKE_LFLAGS_DEBUG += -L../lib/debug -L../quackleio/lib/debug

# Input
SOURCES += makegaddag.cpp

win32 {
	DEFINES += QUACKLE_USE_WCHAR_FOR_USER_VISIBLE=0
# Following 2 lines are turned on only if the above #define is true
#	INCLUDEPATH += $(STLPORTDIR)/stlport
#	LIBS += -L$(STLPORTDIR)/lib -lstlport_mingw32_static
	QMAKE_CXXFLAGS_DEBUG += -mthreads
	QMAKE_CXXFLAGS_RELEASE += -mthreads
}

macx-g++ {
    QMAKE_CXXFLAGS += -fpermissive
}
