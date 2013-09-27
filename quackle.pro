TEMPLATE = lib
INCLUDEPATH += .
DEPENDPATH += .
VERSION = 0.9
QT -= gui core
debug {
  OBJECTS_DIR = obj/debug
}

release {
  OBJECTS_DIR = obj/release
}

# enable/disable debug symbols
#CONFIG += debug staticlib
CONFIG += release staticlib
CONFIG -= x11

# Input
HEADERS += alphabetparameters.h \
	bag.h \
	board.h \
	boardparameters.h \
	bogowinplayer.h \
	catchall.h \
	clock.h \
	computerplayer.h \
	computerplayercollection.h \
	datamanager.h \
	endgame.h \
	endgameplayer.h \
	enumerator.h \
	evaluator.h \
	fixedstring.h \
	gaddag.h \
	game.h \
	gameparameters.h \
	generator.h \
	lexiconparameters.h \
	move.h \
	player.h \
	playerlist.h \
	preendgame.h \
	rack.h \
	reporter.h \
	resolvent.h \
	sim.h \
	strategyparameters.h \
	uv.h 

SOURCES += bogowinplayer.cpp \
	alphabetparameters.cpp \
	bag.cpp \
	board.cpp \
	boardparameters.cpp \
	catchall.cpp \
	clock.cpp \
	computerplayer.cpp \
	computerplayercollection.cpp \
	datamanager.cpp \
	endgame.cpp \
	endgameplayer.cpp \
	enumerator.cpp \
	evaluator.cpp \
	game.cpp \
	gameparameters.cpp \
	generator.cpp \
	lexiconparameters.cpp \
	move.cpp \
	player.cpp \
	playerlist.cpp \
	preendgame.cpp \
	rack.cpp \
	reporter.cpp \
	resolvent.cpp \
	sim.cpp \
	strategyparameters.cpp 

win32:!win32-g++ {
	QMAKE_CFLAGS_DEBUG     ~= s/-MDd/-MTd/
	QMAKE_CXXFLAGS_DEBUG   ~= s/-MDd/-MTd/
	QMAKE_CFLAGS_RELEASE   ~= s/-MD/-MT/
	QMAKE_CXXFLAGS_RELEASE ~= s/-MD/-MT/
}

macx-g++ {
    QMAKE_CXXFLAGS += -fpermissive
}
