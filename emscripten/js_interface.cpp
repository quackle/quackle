#include <QtCore>
#include <iostream>

#include "computerplayercollection.h"
#include "datamanager.h"
#include "game.h"
#include "move.h"
#include "computerplayer.h"
#include "alphabetparameters.h"
#include "boardparameters.h"
#include "lexiconparameters.h"
#include "strategyparameters.h"

// QuackleIO stuff
#include "quackleio/gcgio.h"

#define QUACKLE_DATAMANAGER Quackle::DataManager::self()

// General flow taken from Python bindings, quackletest.cpp,
// testharness.cpp, etc.
Quackle::DataManager startup() {
    Quackle::DataManager dataManager;
    dataManager.setAppDataDirectory("../data");
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

void loadGameAndPlayers(Quackle::DataManager dataManager, QString gcgfilename) {

    // Set up alphabet -- assume it's english so we probably don't need to do this.
    // see `m_alphabetParameters = new EnglishAlphabetParameters;` in datamanager
    // string abc = Quackle::AlphabetParameters::findAlphabetFile("english");
    // strint abc2 = QuackleIO::Util::stdStringToQString


    // Let's load the GCG the way the Python file does, then create a GCG loader
    // function. MVP for this should be to provide a GCG loader and a speedy
    // player move generator.
    // Later add simming.
    bool playerFound = false;
    cout << "OK" << endl;
    const Quackle::Player &player = dataManager.computerPlayers().playerForName(
        "Speedy Player", playerFound);
    if (!playerFound) {
        cout << "Could not get player";
        return;
    }
    Quackle::ComputerPlayer *computerPlayer = player.computerPlayer();
    QuackleIO::GCGIO io;
    cout << "HEHE" << endl;
    Quackle::Game *game = io.read(gcgfilename, QuackleIO::Logania::MaintainBoardPreparation);
    cout << "EEP" << endl;
    // We have a game now.
    Quackle::GamePosition position = game->currentPosition();
    computerPlayer->setPosition(position);
    Quackle::MoveList moves = computerPlayer->moves(15);
    cout << "HERE" << endl;
    for (Quackle::MoveList::const_iterator it = moves.begin(); it != moves.end(); ++it) {
        cout << *it << endl;
    }
}

int main(int argc, char **argv) {
    Quackle::DataManager dataManager;
    dataManager = startup();
    loadGameAndPlayers(dataManager, QString(argv[1]));

    return 0;
}
