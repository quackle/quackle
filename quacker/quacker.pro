TEMPLATE = app
VERSION = 0.98
TARGET = Quackle
DEPENDPATH += .. ../quackleio
INCLUDEPATH += . ..

APP_ALPHABETS_FILES.files = $$files(../data/alphabets/*)
APP_LEXICA_FILES.files = $$files(../data/lexica/*)
APP_STRATEGY_FILESods5.files = $$files(../data/strategy/ods5/*)
APP_STRATEGY_FILEStwl06.files = $$files(../data/strategy/twl06/*)
APP_STRATEGY_FILEStwl98.files = $$files(../data/strategy/twl98/*)
APP_THEME_FILES.files = $$files(../data/themes/*)

MOC_DIR = moc

# enable/disable debug symbols
#CONFIG += debug
CONFIG += release

debug {
  OBJECTS_DIR = obj/debug
}

release {
  OBJECTS_DIR = obj/release
}

QMAKE_LFLAGS_RELEASE += -L../lib/release -L../quackleio/lib/release
QMAKE_LFLAGS_DEBUG += -L../lib/debug -L../quackleio/lib/debug

LIBS += -lquackleio -lquackle

# Input
HEADERS += *.h
SOURCES += *.cpp

win32 {
	RC_FILE = quacker.rc
}

win32:!win32-g++ {
	QMAKE_CFLAGS_DEBUG     ~= s/-MDd/-MTd/
	QMAKE_CXXFLAGS_DEBUG   ~= s/-MDd/-MTd/
	QMAKE_CFLAGS_RELEASE   ~= s/-MD/-MT/
	QMAKE_CXXFLAGS_RELEASE ~= s/-MD/-MT/
}

macx {
	DEFINES += FORCE_SECONDARY_ARROW_GLYPHS=1
	ICON = quacker.icns
	BUNDLEID = com.Quackle.Quackle
	QMAKE_INFO_PLIST = Quackle.plist
	QMAKE_MACOSX_DEPLOYMENT_TARGET = 10.8

	# copy data/ directory into app bundle
	APP_ALPHABETS_FILES.path = Contents/MacOS/data/alphabets
	APP_LEXICA_FILES.path = Contents/MacOS/data/lexica
	APP_STRATEGY_FILESods5.path = Contents/MacOS/data/strategy/ods5
	APP_STRATEGY_FILEStwl06.path = Contents/MacOS/data/strategy/twl06
	APP_STRATEGY_FILEStwl98.path = Contents/MacOS/data/strategy/twl98
	APP_THEME_FILES.path = Contents/MacOS/data/themes

	QMAKE_BUNDLE_DATA += APP_ALPHABETS_FILES APP_LEXICA_FILES APP_STRATEGY_FILESods5 APP_STRATEGY_FILEStwl06 APP_STRATEGY_FILEStwl98 APP_THEME_FILES

	# plist gymnastics
	QMAKE_POST_LINK += ;cp -n $$PWD/quacker.plist $${OUT_PWD}/$${TARGET}.app/Contents
	QMAKE_POST_LINK += /usr/libexec/PlistBuddy -c \"Set :GIT_COMMIT_HASH $${HC_GITHASH}\" $${OUT_PWD}/$${TARGET}.app/Contents/Info.plist
}

macx-g++ {
    QMAKE_CXXFLAGS += -fpermissive
}

unix:!macx {
	QMAKE_CXXFLAGS += -Wno-unused-local-typedefs
}
