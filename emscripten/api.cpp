/**
 * This file defines a C API for libquackle. The implementation will
 * be done in Javascript for easy calling from a Javascript environment.
 */

#include <emscripten/bind.h>
#include <emscripten.h>

#include "computerplayercollection.h"
#include "computerplayer.h"
#include "game.h"
#include "datamanager.h"
#include "../test/trademarkedboards.h"
#include "lexiconparameters.h"
#include "strategyparameters.h"

#include "non_qt_gcgio.h"

Quackle::DataManager dataManager;

// These singletons are used for best interaction with JS environment.
// For example, see iterative simulator.
// XXX CHECK FOR MEMORY LEAKS
// does reassigning position free the last one properly? etc.
Quackle::Game *game = new Quackle::Game;
Quackle::GamePosition position;
Quackle::Simulator simulator;
int totalIterations = 0;

void startup()
{
    dataManager.setAppDataDirectory("/");
    dataManager.setBackupLexicon("default_english");
    dataManager.setBoardParameters(new ScrabbleBoard());

    dataManager.lexiconParameters()->loadDawg(
        Quackle::LexiconParameters::findDictionaryFile("owlymcowl.dawg"));
    dataManager.lexiconParameters()->loadGaddag(
        Quackle::LexiconParameters::findDictionaryFile("owlymcowl.gaddag"));
    dataManager.strategyParameters()->initialize("default_english");
    dataManager.setComputerPlayers(
        Quackle::ComputerPlayerCollection::fullCollection());
}

string loadGameAndPlayers(string gcgRepr, int playerID, int turnNumber) {
    bool playerFound = false;
    const Quackle::Player &player = dataManager.computerPlayers().playerForName(
        "Speedy Player", playerFound);
    if (!playerFound) {
        cout << "Could not get player";
        return "NOT FOUND";
    }

    Quackle::ComputerPlayer *computerPlayer = player.computerPlayer();
    EmQuackle::GCGIO io;
    game = io.readFromString(gcgRepr);

    Quackle::GamePosition position;
    if (playerID != -1) {
        position = game->history().positionAt(
            Quackle::HistoryLocation(playerID, turnNumber));
    } else {
        position = game->currentPosition();
    }
    std::stringstream buffer;
    buffer << position << std::endl;
    return buffer.str();
}

string kibitzTurn(int playerID, int turnID) {
    Quackle::GamePosition position;
    position = game->history().positionAt(Quackle::HistoryLocation(playerID,
                                                                   turnID));

    const int kibitzLength = 15;

    position.kibitz(kibitzLength);

    std::stringstream buffer;
    buffer << position.moves() << std::endl;
    return buffer.str();
}

void setupSimulator(int playerID, int turnID) {
    position = game->history().positionAt(Quackle::HistoryLocation(playerID,
                                                                   turnID));
    position.kibitz(15);
    totalIterations = 0;

    simulator.setPosition(position);
    simulator.setIgnoreOppos(false);
}

// This function is called by JS when needed.
string simulateIter(int iterationStep) {
    int plies = 2;
    std::stringstream buffer;
    simulator.simulate(plies, iterationStep);
    totalIterations += iterationStep;
    const Quackle::SimmedMoveList &moves = simulator.simmedMoves();
    for (Quackle::SimmedMoveList::const_iterator it = moves.begin();
            it != moves.end(); ++it) {

        buffer << *it << endl;
    }
    buffer << totalIterations << endl;
    return buffer.str();
}

void deleteGame() {
    delete game;
    game = new Quackle::Game;
}

EMSCRIPTEN_BINDINGS(module_funcs) {
    emscripten::function("startup", &startup);
    emscripten::function("loadGameAndPlayers", &loadGameAndPlayers);
    emscripten::function("deleteGame", &deleteGame);
    emscripten::function("kibitzTurn", &kibitzTurn);
    emscripten::function("setupSimulator", &setupSimulator);
    emscripten::function("simulateIter", &simulateIter);
}

int main() {
    return 0;
}

// EMSCRIPTEN_BINDINGS(game_class) {
//     emscripten::class_<Quackle::Game>("Game")
//         .constructor()
//         .function("currentPosition_const",
//             emscripten::select_overload<const Quackle::GamePosition&()const>
//             (&Quackle::Game::currentPosition))
//         .function("currentPosition",
//             emscripten::select_overload<Quackle::GamePosition&()>
//             (&Quackle::Game::currentPosition))
//         .function("setPlayers", &Quackle::Game::setPlayers)
//         .function("hasPositions", &Quackle::Game::hasPositions)
//         .function("commitCandidate", &Quackle::Game::commitCandidate)
//         .function("addPosition", &Quackle::Game::addPosition);
// }

// EMSCRIPTEN_BINDINGS(position_class) {
//     emscripten::class_<Quackle::GamePosition>("GamePosition")
//         .constructor
// }
