TEMPLATE = lib
INCLUDEPATH += . ..
DEPENDPATH += . ..
VERSION = 0.9
QT -= gui
debug {
  OBJECTS_DIR = obj/debug
  win32 { LIBS += -L../debug }
}

release {
  OBJECTS_DIR = obj/release
  win32 { LIBS += -L../release }
}

MOC_DIR = moc

# enable/disable debug symbols
#CONFIG += debug staticlib
CONFIG += release staticlib
CONFIG -= x11

LIBS += -L.. -lquackle

# Input
HEADERS += gcgio.h logania.h queenie.h streamingreporter.h flexiblealphabet.h util.h froggetopt.h dict.h dictfactory.h dictimplementation.h
SOURCES += gcgio.cpp queenie.cpp streamingreporter.cpp flexiblealphabet.cpp util.cpp froggetopt.cpp dict.cpp dictfactory.cpp dictimplementation.cpp 

win32:!win32-g++ {
	QMAKE_CFLAGS_DEBUG     ~= s/-MDd/-MTd/
	QMAKE_CXXFLAGS_DEBUG   ~= s/-MDd/-MTd/
	QMAKE_CFLAGS_RELEASE   ~= s/-MD/-MT/
	QMAKE_CXXFLAGS_RELEASE ~= s/-MD/-MT/
}
