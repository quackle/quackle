TEMPLATE = app
VERSION = 0.97
DEPENDPATH += .. ../quackleio
INCLUDEPATH += . ..

MOC_DIR = moc

# enable/disable debug symbols
#CONFIG += debug
CONFIG += release

build_pass:CONFIG(debug, debug|release) {
	LIBS += -L../debug -L../quackleio/debug
}

build_pass:CONFIG(release, debug|release) {
	LIBS += -L../release -L../quackleio/release
}

LIBS += -L.. -L../quackleio -lquackle -lquackleio

# Input
HEADERS += bagdisplay.h boarddisplay.h boardsetup.h boardsetupdialog.h brb.h configdialog.h configpages.h customqsettings.h dashboard.h geometry.h graphicalboard.h graphicalreporter.h history.h letterbox.h letterboxsettings.h lister.h movebox.h newgame.h noteeditor.h quacker.h quackersettings.h rackdisplay.h settings.h simviewer.h view.h widgetfactory.h oppothread.h oppothreadprogressbar.h
SOURCES += bagdisplay.cpp boarddisplay.cpp boardsetup.cpp boardsetupdialog.cpp brb.cpp configdialog.cpp configpages.cpp dashboard.cpp geometry.cpp graphicalboard.cpp graphicalreporter.cpp history.cpp letterbox.cpp letterboxsettings.cpp lister.cpp movebox.cpp newgame.cpp noteeditor.cpp quacker.cpp quackersettings.cpp rackdisplay.cpp settings.cpp simviewer.cpp view.cpp widgetfactory.cpp oppothread.cpp oppothreadprogressbar.cpp main.cpp 


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
}

macx-g++ {
    QMAKE_CXXFLAGS += -fpermissive
}
