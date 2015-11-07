TEMPLATE = app
DEPENDPATH += .. ../quackleio
INCLUDEPATH += . ..
CONFIG += release

debug {
  OBJECTS_DIR = obj/debug
  QMAKE_LIBDIR += ../lib/debug ../quackleio/lib/debug
}

release {
  OBJECTS_DIR = obj/release
  QMAKE_LIBDIR += ../lib/release ../quackleio/lib/release
}

MOC_DIR = moc

# enable/disable debug symbols
# CONFIG += debug

CONFIG += console
CONFIG -= app_bundle

win32:!win32-g++ {
  LIBS += -lquackleio -llibquackle
} else {
  LIBS += -lquackleio -lquackle
}

QMAKE_CXXFLAGS += -std=c++11
QMAKE_CXXFLAGS:!win32-msvc2013 += -Wno-unknown-warning-option -Wno-deprecated-register

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
