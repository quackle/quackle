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
Quackle::Game *game = new Quackle::Game;

// API (to be implemented in Javascript)
extern "C" {
    // gcj is my json-ified gcg with more stuff in it.
void outputSimResults(string);
}


void startup() {
    dataManager.setAppDataDirectory("/");
    dataManager.setBackupLexicon("twl06");
    dataManager.setBoardParameters(new ScrabbleBoard());

    dataManager.lexiconParameters()->loadDawg(
        Quackle::LexiconParameters::findDictionaryFile("twl06.dawg"));
    dataManager.lexiconParameters()->loadGaddag(
        Quackle::LexiconParameters::findDictionaryFile("twl06.gaddag"));
    dataManager.strategyParameters()->initialize("twl06");
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

void simulate(int playerID, int turnID) {
    Quackle::GamePosition position;
    position = game->history().positionAt(Quackle::HistoryLocation(playerID,
                                                                   turnID));
    position.kibitz(15);
    int iterations = 0;
    int iterationStep = 10;
    const int plies = 2;

    Quackle::Simulator simulator;
    simulator.setPosition(position);
    simulator.setIgnoreOppos(false);

    while(true) {
        std::stringstream buffer;
        simulator.simulate(plies, iterationStep);
        iterations += iterationStep;
        const Quackle::SimmedMoveList &moves = simulator.simmedMoves();
        for (Quackle::SimmedMoveList::const_iterator it = moves.begin();
             it != moves.end(); ++it) {

            buffer << *it << endl;
        }
        EM_ASM({
            simulationProgress(UTF8ToString($0), $1)
        },
               buffer.str().c_str(), iterations);

        int stop = EM_ASM_INT(stopSimulation());
        if (stop == 1) {
            break;
        }
    }
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
    emscripten::function("simulate", &simulate);
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
