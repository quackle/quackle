/*
 *  Quackle -- Crossword game artificial intelligence and analysis tool
 *  Copyright (C) 2005-2006 Jason Katz-Brown and John O'Laughlin.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 *  02110-1301  USA
 */

#ifndef QUACKLE_GAME_H
#define QUACKLE_GAME_H

#include <vector>
#include <map>

#include "alphabetparameters.h"
#include "bag.h"
#include "board.h"
#include "player.h"
#include "playerlist.h"

namespace Quackle
{

class ComputerPlayer;

class HistoryLocation
{
public:
	HistoryLocation(int playerId, int turnNumber);

	int playerId() const;
	int turnNumber() const;

private:
	int m_playerId;
	int m_turnNumber;
};

// comparison based on turn number then player id
bool operator<(const Quackle::HistoryLocation &hl1, const Quackle::HistoryLocation &hl2);

inline int HistoryLocation::playerId() const
{
	return m_playerId;
}

inline int HistoryLocation::turnNumber() const
{
	return m_turnNumber;
}

class GamePosition
{
public:
	// Constructs a new position with empty, prepared board
	// and full, prepared bag
	// using players playing the game
	// where the first player of players starts the game (at turn number 1)
	// after Game::addPosition() is called to get things going.
	// You *must* call increment() if you do not use Game::addPosition()
	// because this constructor leaves current player pointing to the end of
	// the player list, which is invalid.
	GamePosition(const PlayerList &players);

	// copy constructor that preserves current-player iterator
	// and includes copying the move list and moveMade
	// Natch, preserves board and bag.
	// MAKE SURE THAT ANY NEW FIELDS YOU ADD
	// ALSO GET COPIED!!!!!!!!!!!!!!!!!!!!!!
	GamePosition(const GamePosition &position);

	// empty constructor to appease value lists
	// empty prepared board, full prepared bag
	GamePosition();

	// operator= that preserves current-player iterator
	// and includes copying the move list and moveMade
	// Natch, preserves board and bag.
	// MAKE SURE THAT ANY NEW FIELDS YOU ADD
	// ALSO GET COPIED!!!!!!!!!!!!!!!!!!!!!!
	const GamePosition &operator=(const GamePosition &position);

	// kibitz up to nmoves best moves; stored in move list
	void kibitz(int nmoves = 10);

	// get what's in the move list
	const MoveList &moves() const;

	void setMoves(const MoveList &moves);

	// kibitz (destroying previous move list)
	// and return the best move based on static evaluation
	const Move &staticBestMove();

	// erase a move from move list that equals move
	void removeMove(const Move &move);

	// erase all moves from move list
	void removeAllMoves();

	// Add move to move list (so list is still ordered).
	// Does *not* check if the move is already in the move list,
	// so this could duplicate a move in the list.
	// Does nothing is move is a nonmove.
	// Ensures the move's prettiness and sanity of playthru tiles
	// in the tiles.
	void addMove(const Move &move);

	// for each move in list, add it to our list if we don't
	// already have it
	void makeSureMoveListContainsMoves(const MoveList &moves);

	// fill the move list with computerPlayer's moves, or
	// the current player's computer player's moves
	void kibitzAs(ComputerPlayer *computerPlayer, int nmoves);

	// if move does not have a pretty tiles field set, make it
	void ensureMovePrettiness(Move &move);

	// If the move's tiles are PANELINg, and the P is played through,
	// turn the tiles into .ANELINg
	void ensureMoveTilesDoNotIncludePlayThru(Move &move);

	// add move to move list and set as move made for this position
	void addAndSetMoveMade(const Move &move);

	// ValidMode - okeydokey
	// TooLateExchange - not enough tiles in the bag for an exchange
	// InvalidTiles - tiles to play or exchange aren't on rack
	// InvalidPlace - word does not connect to rest of board
	// InvalidOpeningPlace - word does not connect to start square
	//                       of empty board
	// UnacceptableWord - word is not in our lexicon
	// InvalidAction - type of move is not Place, Pass, or Exchange
	enum MoveValidity { ValidMove = 0x0000, InvalidTiles = 0x0001, TooLateExchange = 0x0002, InvalidPlace = 0x0004, InvalidOpeningPlace = 0x0008, UnacceptableWord = 0x0010, InvalidAction = 0x0020 };

	// Returns validity of this move for current player.
	// MoveValidity flags are OR'ed together into the return int.
	int validateMove(const Move &move) const;

	// returns whether the words formed by this move are all acceptable
	bool formsAcceptableWords(const Move &move) const;

        MoveList allWordsFormedBy(const Move &move) const;

	// returns whether the board is empty or tiles of move touch
	// at least one other square of the board
	bool isConnected(const Move &move) const;

	// returns whether the tiles of move are a word in our lexicon
	bool isAcceptableWord(const LetterString &word) const;

	// returns true if any exchange is allowed right now
	bool exchangeAllowed() const;

	enum OverdrawValidity { ValidOverdraw = 0x0000, InvalidOverdrawNumber = 0x0001, OverdrawnTilesNotUnseen = 0x0002};

	// computers letters that should be thrown back if specified letters
	// are the faceup tiles and returns if these faceup tiles are valid
	int handleOverdraw(const LetterString &letters, LetterString *throwback) const;

	// Make move on the board and add score of move
	// to current player's score.
	// If the move is an exchange, tosses the exchanged tiles
	// back in the bag.
	// If maintainBoard is false, the board can no longer be used
	// with kibitzing capabilities.
	void makeMove(const Move &move, bool maintainBoard = true);

	// Used when modifying the board without going through the motions,
	// or preparing a freshly-loaded-from-file board for analysis
	void ensureBoardIsPreparedForAnalysis();

	// score specified move and return a move with score field
	// filled in and equity field guessed at; score of
	// non-place moves is zero
	void scoreMove(Move &move);

	// equity value is a combination of static score, player consideration,
	// and shared consideration heuristics: (rack is assumed to be
	// current player's rack if not specified)
	double calculateEquity(const Move &move);

	// 1. score of play
	int calculateScore(const Move &move);

	// 2. definitionally, rack leave value
	double calculatePlayerConsideration(const Move &move);

	// just get me the leave value, please
	double leaveValue(const LetterString &leave) const;

	// 3. other considerations like board geometry
	double calculateSharedConsideration(const Move &move);

	void setBoard(const Board &board);
	const Board &board() const;

	// all tiles not on board or players'
	// (probably) filled racks
	const Bag &bag() const;

	// Set drawing order, starting from back of drawingOrder.
	// The drawing order is reset to randomness after letters
	// are drawn when incremented.
	// This only is meaningful if you don't
	// manually set racks and make only valid plays.
	// (Otherwise there may be letters in the drawing order
	// that are no longer in the bag. Incidentally the drawing
	// order is also followed if you do set rack.)
	void setDrawingOrder(const LetterString &drawingOrder);
	const LetterString &drawingOrder() const;

	// get tiles unseen to current player
	Bag unseenBag() const;
	Bag unseenBagFromPlayerPerspective(const Player &player) const;

	// this must be called if you call makeMove with a nonvalid-for-
	// current-player move
	void ensureProperBag() const;

	// reset board to be empty and prepared for a new game
	void setEmptyBoard();

	// current player *really* on turn
	const Player &currentPlayer() const;
	Player &currentPlayer();

	// Returns true if currentPlayer was set, false if no player with
	// this id was found.
	bool setCurrentPlayer(int playerID);

	// Fake player on turn.
	// Of all libquackle classes, only History uses this field.
	// Player on turn is reset to the current player whenever this
	// position is incremented.
	const Player &playerOnTurn() const;
	Player &playerOnTurn();

	// returns true if currentPlayer was set, false if no player with
	// this id was found
	bool setPlayerOnTurn(int playerID);

	// Returns the current player if he or she is human,
	// or the next human player if one exists,
	// or the current player if all players are nonhuman.
	const Player &humanPlayer() const;

	// returns a pointer to the next player
	PlayerList::const_iterator nextPlayer() const;

	// Returns a pointer to the next player of a type.
	// This starts its search on the player after the current player,
	// so the current player will only be returned if it is the only player
	// of the specified type.
	// If no player of that type exists, the current player is returned
	// and found is set to false.
	PlayerList::const_iterator nextPlayerOfType(Player::PlayerType type, bool &found) const;

	// Returns the player with id specified, or the current player
	// if not found.
	PlayerList::const_iterator playerWithId(int id, bool &found) const;

	const PlayerList &players() const;

	// If in the endgame, returns players with adjusted scores.
	// Otherwise returns players.
	PlayerList endgameAdjustedScores() const;

	// returns the players with the highest score
	PlayerList leadingPlayers() const;

	// returns point difference between current player and
	// highest-scoring other player
	int spread() const;

	// as above, but treating playerID as current player
	int spread(int playerID) const;

	// returns true when there is a player with an empty rack and the bag
	// is empty, so in all frames after the frame containing a player
	// making the final move, gameOver is true
	bool gameOver() const;

	// the move made will
	// be set to the proper UnusedTileBonus move.
	// The score of the bonus is *not* added to score of current player,
	// therefor this method is quite misnamed.
	void adjustScoresToFinishGame();

	// this is a little special: don't subtract score of bonus from
	// current player, but subtract it from other players; set move made
	// to a negative UnusedTilesBonus for the current player
	void adjustScoresToFinishPassedOutGame();

	// returns true if the game will be over after committing this move
	bool doesMoveEndGame(const Move &move) const;

	// if the game is over or will be after this turn,
	// returns how many extra points the current player will get,
	// and which tiles those are
	int deadwood(LetterString *tiles) const;

	// replenishes previousRack from bag and sets it as current player's rack
	void replenishAndSetRack(const Rack &previousRack);
	void replenishAndSetRack(const Rack &previousRack, Player &player);

	// set rack of current player.
	// This isn't simple because we must ensure the bag contains the proper tiles,
	// if adjustBag is true, by readding to bag tiles were on rack but no longer
	void setCurrentPlayerRack(const Rack &rack, bool adjustBag = true);

	// same as above for a given player
	void setPlayerRack(int playerID, const Rack &rack, bool adjustBag = true);

	// Set rack of current player's opponent.
	// This is broken for number of players != 2 and but I don't care much.
	// This isn't simple because we must ensure the bag contains the proper tiles,
	// if adjustBag is true, by readding to bag tiles were on rack but no longer
	void setOppRack(const Rack &rack, bool adjustBag = true);

	// return opponent's rack (for preendgame win% usage)
	Rack oppRack();

	// if we set player's rack to this, will there be more tiles in
	// play than before?
	bool canSetCurrentPlayerRackWithoutBagExpansion(const Rack &rack) const;
	bool canSetPlayerRackWithoutBagExpansion(int playerID, const Rack &rack) const;

	// set bag to a full bag of tiles
	void resetBag();

	// set bag to whatever you want
	void setBag(const Bag &bag);

	// move made FROM this position, or candidate play being considered
	void setMoveMade(const Move &move);
	const Move &moveMade() const;

	// Move committed from this position. Can be different from the moveMade
	// for example if the user loads a gcg file then looks at different
	// possible plays without committing them.
	void setCommittedMove(const Move &move);
	const Move &committedMove() const;

	// saves the current candidate as the committedMove.
	void prepareForCommit();

	// returns the board with the moveMade made on it.
	// Note that this board won't be ready to be fed to a Generator!
	// It should only be used to have a representation of a board for
	// example to display.
	Board boardAfterMoveMade() const;

	// set move made to the default, a nonmove
	void resetMoveMade();

	// The new position is made by incrementing
	// the current player.If the current player was the last in the player list,
	// turnNumber is incremented in the created position and true is returned,
	// otherwise false is returned.
	// Player on turn is set to the new current player.
	// If after incrementing, gameOver() is true, then the current player is
	// reset to be the player who made the last turn of the game.
	// If needed, current player at time of call's rack is refilled
	// from the bag based on move made.
	// Furthermove, any empty player's rack is refilled (this happens
	// at start of game.)
	// If applicable, this player's score is also incremented by score of move
	// made.
	bool incrementTurn();

	// Turn numbers in games start from 1.
	// A turn number of zero indicates a position that is pregame.
	int turnNumber() const;

	// nestedness of this position in a recursive calculation
	void setNestedness(unsigned int nestedness);
	void incrementNestedness();
	unsigned int nestedness() const;

	// returns a blank string that is one tab per nestedness
	UVString nestednessIndentation() const;

	// number of plays in a row that have been scoreless
	int scorelessTurnsInARow() const;

	HistoryLocation location() const;

	const UVString &explanatoryNote() const;
	void setExplanatoryNote(const UVString &explanatoryNote);

	// not recommended to use this!
	Board &underlyingBoardReference();

protected:
	PlayerList m_players;
	PlayerList::iterator m_currentPlayer;
	PlayerList::iterator m_playerOnTurn;

	// moves originally from kibitzer
	MoveList m_moves;

	// It is very important that these two variables almost always be
	// modified together, unless one is experimenting with different
	// candidates before deciding on the move to commit.
	// For instance, the end-of-game bonus move is stored in both fields.
	Move m_moveMade;
	Move m_committedMove;

	// I feel safer with this not being public
	void setTurnNumber(int turnNumber);
	int m_turnNumber;
	unsigned int m_nestedness;
	int m_scorelessTurnsInARow;
	bool m_gameOver;

	Quackle::Board m_board;

	// Keeping the bag holding the correct letters is VOUDOU!
	// If you break its behavior, I will be bemaddened!
	Quackle::Bag m_bag;

	LetterString m_drawingOrder;

	UVString m_explanatoryNote;

	// Use this instead of m_bag.removeTiles(); if the bag
	// doesn't contain the tiles to remove, it removes from
	// non-current player racks if they have tiles and then refills.
	// Returns false if one of the letters was found nowhere to be
	// removed from.
	bool removeLetters(const LetterString &letters);
};

inline const Player &GamePosition::currentPlayer() const
{
	return *m_currentPlayer;
};

inline Player &GamePosition::currentPlayer()
{
	return *m_currentPlayer;
};

inline const Player &GamePosition::playerOnTurn() const
{
	return *m_playerOnTurn;
};

inline Player &GamePosition::playerOnTurn()
{
	return *m_playerOnTurn;
};

inline const MoveList &GamePosition::moves() const
{
	return m_moves;
}

inline void GamePosition::setMoves(const MoveList &moves)
{
	m_moves = moves;
}

inline void GamePosition::setBoard(const Board &board)
{
	m_board = board;
}

inline const Board &GamePosition::board() const
{
	return m_board;
}

inline Board &GamePosition::underlyingBoardReference()
{
	return m_board;
}

inline const Bag &GamePosition::bag() const
{
	return m_bag;
}

inline void GamePosition::setBag(const Bag &bag)
{
	m_bag = bag;
}

inline void GamePosition::setDrawingOrder(const LetterString &drawingOrder)
{
	m_drawingOrder = drawingOrder;
}

inline const LetterString &GamePosition::drawingOrder() const
{
	return m_drawingOrder;
}

inline const PlayerList &GamePosition::players() const
{
	return m_players;
}

inline int GamePosition::spread() const
{
	return spread(currentPlayer().id());
}

inline void GamePosition::replenishAndSetRack(const Rack &previousRack)
{
	replenishAndSetRack(previousRack, currentPlayer());
}

inline bool GamePosition::gameOver() const
{
	return m_gameOver;
}

inline void GamePosition::setMoveMade(const Move &move)
{
	m_moveMade = move;
}

inline const Move &GamePosition::moveMade() const
{
	return m_moveMade;
}

inline void GamePosition::setCommittedMove(const Move &move)
{
	m_committedMove = move;
}

inline const Move &GamePosition::committedMove() const
{
	return m_committedMove;
}

inline void GamePosition::setTurnNumber(int turnNumber)
{
	m_turnNumber = turnNumber;
}

inline void GamePosition::setNestedness(unsigned int nestedness)
{
	m_nestedness = nestedness;
}

inline void GamePosition::incrementNestedness()
{
	++m_nestedness;
}

inline int GamePosition::scorelessTurnsInARow() const
{
	return m_scorelessTurnsInARow;
}

inline HistoryLocation GamePosition::location() const
{
	return HistoryLocation(currentPlayer().id(), turnNumber());
}

inline int GamePosition::turnNumber() const
{
	return m_turnNumber;
}

inline unsigned int GamePosition::nestedness() const
{
	return m_nestedness;
}

inline const UVString &GamePosition::explanatoryNote() const
{
	return m_explanatoryNote;
}

inline void GamePosition::setExplanatoryNote(const UVString &explanatoryNote)
{
	m_explanatoryNote = explanatoryNote;
}

// simple modifiable list of positions
typedef vector<GamePosition> PositionList;

class Game;

// unmodifiable list of positions in a game
class History : public PositionList
{
public:
	History()
		: /* a position in the past */ m_currentLocation(0, -1)
	{
	}

	const PlayerList &players() const;
	const Player &lastPlayer() const;
	const GamePosition &lastPosition() const;

	// returns number of levels of turns so far in this game
	// If this contains no positions, returns 0.
	// Remember turns are numbered starting at 1.
	int maximumTurnNumber() const;

	// location at [maximumTurnNumber(), lastPlayer().id()]
	HistoryLocation lastLocation() const;

	// current location and position -- usually the last one unless
	// it's been manually set with Game::setCurrentLocation.
	HistoryLocation currentLocation() const;
	const GamePosition &currentPosition() const;
	GamePosition &currentPosition();

	// all positions this player has been in
	PositionList positionsFacedBy(int playerID) const;

	// the next position after the current position
	const GamePosition &nextPosition(bool *exists = 0) const;

	// the next item of positionsFacedBy(playerID)
	const GamePosition &nextPositionFacedBy(int playerID, bool *exists = 0) const;

	// the previous position before the current position
	const GamePosition &previousPosition(bool *exists = 0) const;

	// the first position in the game
	const GamePosition &firstPosition(bool *exists = 0) const;

	const GamePosition &positionAt(const HistoryLocation &location, bool *exists = 0) const;

private:
	PlayerList m_players;
	void setPlayers(const PlayerList &list);

	HistoryLocation m_currentLocation;
	void setCurrentLocation(const HistoryLocation &location);

	// erase all positions that are after location gamewise
	void eraseAfter(const HistoryLocation &location);

	GamePosition &mutablePositionAt(const HistoryLocation &location, bool *exists = 0);
	GamePosition &lastPosition();

	friend class Game;
};

inline void History::setPlayers(const PlayerList &list)
{
	m_players = list;
}

inline const PlayerList &History::players() const
{
	return m_players;
}

inline const Player &History::lastPlayer() const
{
	return back().playerOnTurn();
}

inline const GamePosition &History::lastPosition() const
{
	return back();
}

inline GamePosition &History::lastPosition()
{
	return back();
}

inline HistoryLocation History::lastLocation() const
{
	return lastPosition().location();
}

inline HistoryLocation History::currentLocation() const
{
	return m_currentLocation;
}

inline const GamePosition &History::currentPosition() const
{
	return positionAt(currentLocation());
}

inline GamePosition &History::currentPosition()
{
	return mutablePositionAt(currentLocation());
}

inline void History::setCurrentLocation(const HistoryLocation &location)
{
	m_currentLocation = location;
}

class Game
{
public:
	Game();
	~Game();

	// return to like-new board and empty player list.
	// Use to make new game. See setPlayers() and addPosition();
	// both should each be called at least once manually
	// (with setPlayers() coming before addPosition())
	void reset();

	// game starts with first player of list.
	// This method sets the ids of the players to
	// numbers in ascending order, starting at 0.
	// Because the player IDs cannot be later changed,
	// this is a representation invariant.
	void setPlayers(const PlayerList &list);
	const PlayerList &players() const;

	// returns whether or not addPosition has been called after a
	// reset or being newly constructed
	bool hasPositions() const;

	// history of all positions encountered this game, including
	// the current position
	const History &history() const;

	// This should be called after players() to get the first player
	// started off with his or her first rack and the game underway.
	//
	// implementation details:
	// Create a new GamePosition that is a copy of the current position
	// and add it to the end of our positions list, making it the
	// current position.
	// The first position added after a reset() is the first player
	// going first to start the game.
	// In other cases, the new current position is the
	// GamePosition::incrementTurn() of the older position,
	// with racks replenished from suitable bag
	void addPosition();

	// don't call without having called addPosition!
	const GamePosition &currentPosition() const;
	GamePosition &currentPosition();

	// moves our current location pointer
	void setCurrentPosition(const HistoryLocation &location);

	// set current position to position and point current
	// position to this position (strange that this must
	// be explicit, but it must be)
	void setCurrentPosition(const GamePosition &position);

	// Sets the computer player that will play for player with
	// given playerId. Game does not take control of deleting this pointer.
	void associateComputerPlayer(int playerId, ComputerPlayer *computerPlayer);

	// returns the computer player associated with given playerId
	ComputerPlayer *computerPlayer(int playerId);

	// sets the computer player that will play for players for whom
	// a computer player has not been associated
	void setDefaultComputerPlayer(ComputerPlayer *computerPlayer);

	// associates computer players for players with known ComputerPlayer
	// type
	void associateKnownComputerPlayers();

	ComputerPlayer *defaultComputerPlayer();

	// Have quackle commit a move for the current player
	// using the specified computerPlayer, or, if computerPlayer is 0,
	// use the associated computer player,
	// or a default computer player if none has been associated.
	// Returns the move that is played.
	// Does nothing if the game is over.
	Move haveComputerPlay(ComputerPlayer *computerPlayer = 0);

	// haveComputerPlay() until the current player type is not
	// ComputerPlayer or the game is over
	void advanceToNoncomputerPlayer();

	// the candidate play is stored in currentPosition().moveMade()
	// and defaults to a nonmove.
	//
	// put candidate on platform, the current position
	void setCandidate(const Move &move);
	const Move &candidate() const;

	// reset candidate to a nonmove
	void resetCandidature();

	// Copy current candidate play as the moveMade in the current position
	// then addPosition and set up the new position with the play on the board.
	// Also makes sure the bag is full of only kosher letters afterward.
	// maintainBoard must be true if you want to keep using this game.
	// If the game is over, does nothing.
	void commitCandidate(bool maintainBoard = true);

	// convience to set move as candidate and then commit the candidate
	void commitMove(const Move &move);

	const UVString &description() const;
	void setDescription(const UVString &description);

	const UVString &title() const;
	void setTitle(const UVString &title);

protected:
	History m_positions;

	typedef map<int, ComputerPlayer *> ComputerPlayerMap;
	ComputerPlayer *m_defaultComputerPlayer;
	ComputerPlayerMap m_computerPlayers;

	UVString m_title;
	UVString m_description;

	// create a new GamePosition that is a copy of the current position
	// without incrementing. Adds it after the current position and
	// erases all later positions.
	void addClonePosition();
};

inline const PlayerList &Game::players() const
{
	return m_positions.players();
}

inline bool Game::hasPositions() const
{
	return !m_positions.empty();
}

inline const History &Game::history() const
{
	return m_positions;
}

inline void Game::setCandidate(const Move &move)
{
	currentPosition().setMoveMade(move);
}

inline const Move &Game::candidate() const
{
	return currentPosition().moveMade();
}

inline void Game::resetCandidature()
{
	currentPosition().resetMoveMade();
}

inline const GamePosition &Game::currentPosition() const
{
	return m_positions.currentPosition();
}

inline GamePosition &Game::currentPosition()
{
	return m_positions.currentPosition();
}

inline void Game::setCurrentPosition(const HistoryLocation &location)
{
	m_positions.setCurrentLocation(location);
}

inline const UVString &Game::description() const
{
	return m_description;
}

inline void Game::setDescription(const UVString &description)
{
	m_description = description;
}

inline const UVString &Game::title() const
{
	return m_title;
}

inline void Game::setTitle(const UVString &title)
{
	m_title = title;
}

}

bool operator==(const Quackle::HistoryLocation &hl1, const Quackle::HistoryLocation &hl2);

UVOStream& operator<<(UVOStream& o, const Quackle::GamePosition &position);
UVOStream& operator<<(UVOStream& o, const Quackle::PositionList &positions);
UVOStream& operator<<(UVOStream& o, const Quackle::HistoryLocation &historyLocation);

#endif
