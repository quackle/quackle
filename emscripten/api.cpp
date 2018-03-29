/**
 * This file defines a C API for libquackle.
 */

#include <emscripten/bind.h>
#include <emscripten.h>

#include "api.h"
#include "non_qt_gcgio.h"


API::API(){
    m_game = new Quackle::Game;
}

void API::startup()
{
    m_dataManager.setAppDataDirectory("/");
    m_dataManager.setBackupLexicon("default_english");
    m_dataManager.setBoardParameters(new ScrabbleBoard());

    m_dataManager.lexiconParameters()->loadDawg(
        Quackle::LexiconParameters::findDictionaryFile("owlymcowl.dawg"));
    m_dataManager.lexiconParameters()->loadGaddag(
        Quackle::LexiconParameters::findDictionaryFile("owlymcowl.gaddag"));
    m_dataManager.strategyParameters()->initialize("default_english");
    m_dataManager.setComputerPlayers(
        Quackle::ComputerPlayerCollection::fullCollection());
}

string API::loadGameAndPlayers() {
    // bool playerFound = false;
    // const Quackle::Player &player = dataManager.computerPlayers().playerForName(
    //     "Speedy Player", playerFound);
    // if (!playerFound) {
    //     cout << "Could not get player";
    //     return "NOT FOUND";
    // }

    // Quackle::ComputerPlayer *computerPlayer = player.computerPlayer();

    EmQuackle::GCGIO io;

    char *str = (char *)EM_ASM_INT({
        var jsString = Globals.getGameGCG();
        var lengthBytes = lengthBytesUTF8(jsString) + 1;
        // 'jsString.length' would return the length of the string as UTF-16 units,
        // but Emscripten C strings operate as UTF-8.
        var stringOnWasmHeap = _malloc(lengthBytes);
        stringToUTF8(jsString, stringOnWasmHeap, lengthBytes+1);
        return stringOnWasmHeap;
    });

    m_game = io.readFromString(str);
    free(str);

    m_position = m_game->currentPosition();

    std::stringstream buffer;
    buffer << m_position << std::endl;
    return buffer.str();
}

void API::kibitzPositionAt(int playerID, int turnNumber, int numMoves) {
    m_position = m_game->history().positionAt(Quackle::HistoryLocation(playerID,
                                                                     turnNumber));
    m_rack = m_position.currentPlayer().rack();
    m_position.kibitz(numMoves);
}

string API::letterStringToString(const Quackle::LetterString &ls) {
    return string(
        QUACKLE_ALPHABET_PARAMETERS->userVisible(ls).c_str());
}

string API::moveToString(const Quackle::Move &move) {
    UVOStringStream ss;

    switch (move.action) {
        case Quackle::Move::Pass:
            ss << "type:pass";
            break;
        case Quackle::Move::Exchange:
            ss << "type:exchange,tiles:" << QUACKLE_ALPHABET_PARAMETERS->userVisible(move.tiles());
            break;
        case Quackle::Move::BlindExchange:
            ss << "type:exchange,tiles:" << move.tiles().length();
            break;
        case Quackle::Move::TimePenalty:
            ss << "type:timepenalty,score:" << move.score;
            break;
        case Quackle::Move::UnusedTilesBonus:
        case Quackle::Move::UnusedTilesBonusError:
            ss << "type:unusedTiles,tiles:" << QUACKLE_ALPHABET_PARAMETERS->userVisible(move.tiles());
            break;
        case Quackle::Move::Place:
        case Quackle::Move::PlaceError:
            ss << "type:place,pos:" << move.positionString();
            ss << ",tiles:" << letterStringToString(move.prettyTiles());
            ss << ",leave:" << m_rack - move;
            break;
    }

    ss << ",score:" << move.score << ",equity:" << move.equity << ",win:" << move.win;
    if (move.scoreAddition() != 0) {
        ss << ",scoreAddition:" << move.scoreAddition();
    }
    return ss.str();
}

// A simple comma-separated format. Hope this is sufficient.
string API::serializeMoves(const Quackle::MoveList &moves) {
    std::stringstream buffer;
    for (Quackle::MoveList::const_iterator it = moves.begin(); it != moves.end(); ++it) {
        buffer << moveToString(*it) << endl;
    }
    return buffer.str();
}

string API::kibitzTurn(int playerID, int turnNumber) {
    kibitzPositionAt(playerID, turnNumber, 15);
    return serializeMoves(m_position.moves());
}

string API::setupSimulator(int playerID, int turnNumber) {
    kibitzPositionAt(playerID, turnNumber, 15);

    m_totalIterations = 0;

    m_simulator.setPosition(m_position);
    m_simulator.setIgnoreOppos(false);

    return serializeMoves(m_position.moves());
}

// This function is called by JS when needed.
string API::simulateIter(int iterationStep, int plies) {
    std::stringstream buffer;
    m_simulator.simulate(plies, iterationStep);
    m_totalIterations += iterationStep;
    // const Quackle::SimmedMoveList &moves = simulator.simmedMoves();
    // for (Quackle::SimmedMoveList::const_iterator it = moves.begin();
    //         it != moves.end(); ++it) {

    //     buffer << *it << endl;
    // }
    // simulator.moves args (prune, byWin)
    const Quackle::MoveList &moves = m_simulator.moves(false, true);

    buffer << serializeMoves(moves);

    buffer << m_totalIterations << endl;
    return buffer.str();
}

void API::deleteGame() {
    delete m_game;
    m_game = new Quackle::Game;
}

/**
 * The following functions emulate some of the quacker/movebox behavior.
 */





// Bindings so that Javascript can call these functions.
EMSCRIPTEN_BINDINGS(api) {
    emscripten::class_<API>("API")
        .constructor()
        .function("startup", &API::startup)
        .function("loadGameAndPlayers", &API::loadGameAndPlayers)
        .function("deleteGame", &API::deleteGame)
        .function("kibitzTurn", &API::kibitzTurn)
        .function("setupSimulator", &API::setupSimulator)
        .function("simulateIter", &API::simulateIter);
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
