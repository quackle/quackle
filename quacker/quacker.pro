TEMPLATE = app
VERSION = 0.99
TARGET = Quackle
DEPENDPATH += .. ../quackleio
INCLUDEPATH += . ..

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

debug {
	QMAKE_LIBDIR += ../lib/debug ../quackleio/lib/debug
}
release {
	QMAKE_LIBDIR += ../lib/release ../quackleio/lib/release
}

win32:!win32-g++ {
  LIBS += -lquackleio -llibquackle
} else {
  LIBS += -lquackleio -lquackle
}
macx:LIBS += -framework CoreFoundation

# Input
HEADERS += *.h
SOURCES += *.cpp

win32 {
	RC_FILE = quacker.rc
}

macx {
	DEFINES += FORCE_SECONDARY_ARROW_GLYPHS=1
	ICON = quacker.icns
	BUNDLEID = com.Quackle.Quackle
	QMAKE_INFO_PLIST = Quackle.plist
	QMAKE_MACOSX_DEPLOYMENT_TARGET = 10.8

	# copy data/ directory into app bundle
	APP_ALPHABETS_FILES.files = ../data/alphabets
	APP_LEXICA_FILES.files = ../data/lexica
	APP_STRATEGY_FILES.files = ../data/strategy
	APP_THEME_FILES.files = ../data/themes

	APP_ALPHABETS_FILES.path = Contents/Resources/data
	APP_LEXICA_FILES.path = Contents/Resources/data
	APP_STRATEGY_FILES.path = Contents/Resources/data
	APP_THEME_FILES.path = Contents/Resources/data

	QMAKE_BUNDLE_DATA += APP_ALPHABETS_FILES APP_LEXICA_FILES APP_STRATEGY_FILES APP_THEME_FILES

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
