TEMPLATE = app
DEPENDPATH += .. ../quackleio
INCLUDEPATH += . ..
debug {
  OBJECTS_DIR = obj/debug
  win32 { LIBS += -L../debug -L../quackleio/debug }
}

release {
  OBJECTS_DIR = obj/release
  win32 { LIBS += -L../release -L../quackleio/release }
}

MOC_DIR = moc

# enable/disable debug symbols
# CONFIG += debug

CONFIG += console

LIBS += -L.. -L../quackleio -lquackle -lquackleio

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
