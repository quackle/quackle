TEMPLATE = app
VERSION = 1.0.4
TARGET = Quackle
DEPENDPATH += .. ../quackleio
INCLUDEPATH += . ..
QT += widgets core gui

MOC_DIR = moc

# enable/disable debug symbols
#CONFIG += debug
CONFIG += release

#Um, why is this necessary?  I don't know.  But if this isn't here,
#qmake messes up resulting Visual Studio project files.
CONFIG -= debug

CONFIG += c++14

debug {
  OBJECTS_DIR = obj/debug
  QMAKE_LIBDIR += ../lib/debug ../quackleio/lib/debug
}

release {
  OBJECTS_DIR = obj/release
  QMAKE_LIBDIR += ../lib/release ../quackleio/lib/release
}

win32:!win32-g++ {
  LIBS += -lquackleio -llibquackle
} else {
  LIBS += -lquackleio -lquackle
}
macx:LIBS += -framework CoreFoundation

!msvc {
  QMAKE_CXXFLAGS += -Wno-unknown-warning-option -Wno-deprecated-register
}

# Input
HEADERS += *.h
SOURCES += *.cpp

win32 {
	RC_FILE = quacker.rc
}

macx {
	DEFINES += FORCE_SECONDARY_ARROW_GLYPHS=1
	BUNDLEID = com.Quackle.Quackle
	QMAKE_INFO_PLIST = Quackle.plist
	QMAKE_ASSET_CATALOGS = $$PWD/Images.xcassets
	QMAKE_ASSET_CATALOGS_APP_ICON = "AppIcon"
	QMAKE_MACOSX_DEPLOYMENT_TARGET = 10.9

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
}

macx-g++ {
    QMAKE_CXXFLAGS += -fpermissive
}

unix:!macx {
	QMAKE_CXXFLAGS += -Wno-unused-local-typedefs
}

linux { # old unixes/Qt distribs running around...most notably on Travis-CI
  QMAKE_CXXFLAGS += -std=c++1y
}
