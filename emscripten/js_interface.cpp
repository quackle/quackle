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

void loadGameAndPlayers(QString gcgfilename, int playerId, int turnNumber) {

    // Set up alphabet -- assume it's english so we probably don't need to do this.
    // see `m_alphabetParameters = new EnglishAlphabetParameters;` in datamanager
    // string abc = Quackle::AlphabetParameters::findAlphabetFile("english");
    // strint abc2 = QuackleIO::Util::stdStringToQString


    // Let's load the GCG the way the Python file does, then create a GCG loader
    // function. MVP for this should be to provide a GCG loader and a speedy
    // player move generator.
    // Later add simming.
    bool playerFound = false;
    const Quackle::Player &player = dataManager.computerPlayers().playerForName(
        "Speedy Player", playerFound);
    if (!playerFound) {
        cout << "Could not get player";
        return;
    }
    Quackle::ComputerPlayer *computerPlayer = player.computerPlayer();
    QuackleIO::GCGIO io;
    Quackle::Game *game = io.read(gcgfilename, QuackleIO::Logania::MaintainBoardPreparation);
    // We have a game now.

    Quackle::GamePosition position;
    if (playerId != -1) {
        position = game->history().positionAt(
            Quackle::HistoryLocation(playerId, turnNumber));
    } else {
        position = game->currentPosition();
    }
    // computerPlayer->setPosition(position);
    // Quackle::MoveList moves = computerPlayer->moves(15);
    // for (Quackle::MoveList::const_iterator it = moves.begin(); it != moves.end(); ++it) {
    //     cout << *it << endl;
    // }
    // //
    cout << "Position to sim: " << endl;
    cout << position << endl;
    const int kibitzLength = 3;

    position.kibitz(kibitzLength);
    cout << position.moves() << endl;
    Quackle::Simulator simulator;
    simulator.setLogfile("quackletest.simulation", false);
    simulator.setPosition(position);
    // simulator.simulate(2, 10);
    // cout << simulator.simmedMoves() << endl;
    // cout << "after " << simulator.iterations() << " iterations pruning to those within 5 pts";

    // simulator.pruneTo(5, kibitzLength);
    int iterationStep = 10;
    int iterationsToRun = 50;

    const int plies = 2;
    simulator.setIgnoreOppos(false);
    cout << "--------------" << endl;
    for (int iterations = 0; iterations < iterationsToRun; iterations += iterationStep) {
        simulator.simulate(plies, iterationStep);
        cout << "sim results after " << iterations + iterationStep << " iterations: " << endl;
        const Quackle::SimmedMoveList &moves = simulator.simmedMoves();

        for (Quackle::SimmedMoveList::const_iterator it = moves.begin(); it != moves.end(); ++it)
        {
            cout << *it << endl;
        }

        cout << endl;
    }

}

int main(int argc, char **argv) {
    startup();
    int playerId = -1;
    int turnNumber = -1;
    if (argc == 4) {
        playerId = atoi(argv[2]);
        turnNumber = atoi(argv[3]);
    }
    loadGameAndPlayers(QString(argv[1]), playerId, turnNumber);
    return 0;
}
