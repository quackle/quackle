#include <QtCore>
#include <iostream>

#include "datamanager.h"
#include "computerplayercollection.h"
#include "alphabetparameters.h"
#include "boardparameters.h"
#include "lexiconparameters.h"
#include "game.h"
#include "player.h"
#include "strategyparameters.h"

// QuackleIO stuff
#include "quackleio/gcgio.h"

#define QUACKLE_DATAMANAGER Quackle::DataManager::self()

// General flow taken from Python bindings, quackletest.cpp,
// testharness.cpp, etc.
Quackle::DataManager startup() {
    Quackle::DataManager dataManager;
    dataManager.setAppDataDirectory("data");
    dataManager.setBackupLexicon("twl06");

    dataManager.lexiconParameters()->loadDawg(
        Quackle::LexiconParameters::findDictionaryFile("twl06.dawg"));
    dataManager.lexiconParameters()->loadGaddag(
        Quackle::LexiconParameters::findDictionaryFile("twl06.gaddag"));
    dataManager.strategyParameters()->initialize("twl06");
    dataManager.setBoardParameters(new Quackle::BoardParameters());
    dataManager.setComputerPlayers(
        Quackle::ComputerPlayerCollection::fullCollection());
    return dataManager;
}

void loadGameAndPlayers(Quackle::DataManager dataManager, char* gcgfilename) {

    // Set up alphabet -- assume it's english so we probably don't need to do this.
    // see `m_alphabetParameters = new EnglishAlphabetParameters;` in datamanager
    // string abc = Quackle::AlphabetParameters::findAlphabetFile("english");
    // strint abc2 = QuackleIO::Util::stdStringToQString


    // Let's load the GCG the way the Python file does, then create a GCG loader
    // function. MVP for this should be to provide a GCG loader and a speedy
    // player move generator.
    // Later add simming.
    Quackle::Game game;
    Quackle::Player player = dataManager.computerPlayers().playerForName(
        "Speedy Player");
    Quackle::ComputerPlayer *computerPlayer = player.computerPlayer();
    QuackleIO::GCGIO io;

    QFile file(QString(gcgfilename));
    if (!file.open(QIODevice::ReadOnly | QIODevice::Text)) {
        cout << "Could not open gcg " << gcgfilename;
        return;
    }
    QTextStream in(&file);
    Quackle::Game *game = io.read(in, QuackleIO::Logania::MaintainBoardPreparation);
    file.close();
    // We have a game now.
}

void main(int argc, char **argv) {
    Quackle::DataManager dataManager;
    dataManager = startup();
    loadGameAndPlayers(dataManager, argv[1]);
}
