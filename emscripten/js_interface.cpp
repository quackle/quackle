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
#include "../test/trademarkedboards.h"

// QuackleIO stuff
#include "quackleio/gcgio.h"
#include "quackleio/util.h"
#include "quackleio/flexiblealphabet.h"

// There's only one of these, it's a singleton.
Quackle::DataManager dataManager;

// General flow taken from Python bindings, quackletest.cpp,
// testharness.cpp, etc.
void startup() {
    
    dataManager.setAppDataDirectory("../data");
    dataManager.setBackupLexicon("twl06");

    QString alphabetFile = QuackleIO::Util::stdStringToQString(
        Quackle::AlphabetParameters::findAlphabetFile(
            QuackleIO::Util::qstringToStdString("english")));
    QuackleIO::FlexibleAlphabetParameters *flexure = new QuackleIO::FlexibleAlphabetParameters;
    if (flexure->load(alphabetFile))
    {
        dataManager.setAlphabetParameters(flexure);
    }
    else {
        cout << "Could not load alphabet";
        return;
    }
    dataManager.setBoardParameters(new ScrabbleBoard());

    dataManager.lexiconParameters()->loadDawg(
        Quackle::LexiconParameters::findDictionaryFile("twl06.dawg"));
    dataManager.lexiconParameters()->loadGaddag(
        Quackle::LexiconParameters::findDictionaryFile("twl06.gaddag"));
    dataManager.strategyParameters()->initialize("twl06");
    dataManager.setComputerPlayers(
        Quackle::ComputerPlayerCollection::fullCollection());
}

void loadGameAndPlayers(QString gcgfilename) {

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
    startup();
    loadGameAndPlayers(QString(argv[1]));

    return 0;
}
