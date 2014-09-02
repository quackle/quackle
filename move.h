/*
 *  Quackle -- Crossword game artificial intelligence and analysis tool
 *  Copyright (C) 2005-2014 Jason Katz-Brown and John O'Laughlin.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef QUACKLE_MOVE_H
#define QUACKLE_MOVE_H

#include <vector>

#include "alphabetparameters.h"

using namespace std;

namespace Quackle
{

// A move represents one of a Place, Exchange, Pass, Unused Tile Bonus,
// Time Penalty, Score Addition, or a lack of a move.
// Passes are special because they start at equity of -50
// (for this use createPassMove).
// Tiles to exchange are specified in tiles.
// Tiles of place word are also specified in tiles.
// tiles is empty for a pass.
// tiles contains unused tiles for an UnusedTilesBonus
// and score contains the bonus.
// A Nonmove represents absence of a choice of move and has extremely
// negative score and equity (so it is sorted below other moves)

class Move
{
public:
	enum Action { Place = 0, Exchange, Pass, UnusedTilesBonus, TimePenalty, Nonmove };

	// creates a pass move with 0 equity;
	// tiles is "", score and equity are zero
	Move();

	int score;
	bool isBingo;

	// 0 if this is a challenged phoney; score field otherwise
	int effectiveScore() const;

	double equity;
	double win;  // between 0 and 1 inclusive
	double possibleWin;

	Action action;

	bool horizontal;
	int startrow;
	int startcol;

	// returns whether this is not a Nonmove
	bool isAMove() const;

	// tiles like .ANELINg
	void setTiles(const LetterString &tiles);

	// tiles like (P)ANELINg
	void setPrettyTiles(const LetterString &prettyTiles);

	// returns tiles like (P)ANELINg
	const LetterString &prettyTiles() const;

	// Returns tiles like .ANELIN?.
	// QUACKLE_PLAYED_THRU_MARK will take place of a letter already on the board
	// and blanks are special -- so use isAlreadyOnBoard and
	// AlphabetParameters->isBlankLetter well.
	// Returns an empty string if this is a challenged phoney.
	LetterString usedTiles() const;

	// Returns tiles like PANELING (pretty tiles, nonblank,
	// without playthru markings)
	LetterString wordTiles() const;

	// Returns tiles like ANELING (pretty tiles, nonblank,
	// without playthru markings or played thru letters)
	LetterString wordTilesWithNoPlayThru() const;

	// returns tiles like .ANELINg
	const LetterString &tiles() const;

	bool isChallengedPhoney() const;
	void setIsChallengedPhoney(bool isChallengedPhoney);

	int scoreAddition() const;
	void setScoreAddition(int scoreAddition);

	// tests for equality to played-thru mark
	static bool isAlreadyOnBoard(Letter letter);

	UVString xml() const;

	// returns a unique identifier for this move (does not specify
	// whether move is challenged off)
	UVString toString() const;

	// returns string with all information (including score and equity)
	UVString debugString() const;

	// for a place move, a position like 8H
	UVString positionString() const;
	
	// eg place("8h", ".EaTY"); word is like setTiles(), and no
	// pretty tiles are set.
	static Move createPlaceMove(UVString placeString, LetterString word);
	static Move createPlaceMove(int zeroIndexedRow, int zeroIndexedColumn, bool horizontal, LetterString word);

	static Move createChallengedPhoney(UVString placeString, LetterString word);
	static Move createChallengedPhoney(int zeroIndexedRow, int zeroIndexedColumn, bool horizontal, LetterString word);

	static Move createExchangeMove(LetterString tilesToExchange);
	static Move createUnusedTilesBonus(LetterString unusedTiles, int bonus);
	static Move createTimePenalty(int penalty);
	static Move createPassMove();
	static Move createNonmove();

private:
	LetterString m_tiles;
	LetterString m_prettyTiles;
	bool m_isChallengedPhoney;
	int m_scoreAddition;
};

// comparison based on action, then tiles, then horizontalness, then startrow, then endcol
bool operator<(const Quackle::Move &move1, const Quackle::Move &move2);

class MoveList : public vector<Move>
{
public:
	MoveList();

	enum SortType { Equity, Score, Alphabetical, Win};

	// perform stable sort
	static void sort(MoveList &list, SortType type = Equity);

	// sort in opposite direction
	static void sortNonReverse(MoveList &list, SortType type = Equity);

	static bool winComparator(const Move &move1, const Move &move2);
	static bool equityComparator(const Move &move1, const Move &move2);
	static bool scoreComparator(const Move &move1, const Move &move2);
	static bool alphabeticalComparator(const Move &move1, const Move &move2);
	static bool wordPosComparator(const Move &move1, const Move &move2);

	bool contains(const Move &move) const;
	
private:
	static SortType m_sortType;

};

inline bool Move::isAMove() const
{
	return action != Nonmove;
}

inline int Move::effectiveScore() const
{
	return m_isChallengedPhoney? 0 : (score + m_scoreAddition);
}

inline void Move::setTiles(const LetterString &tiles)
{
	m_tiles = tiles;
}

inline void Move::setPrettyTiles(const LetterString &prettyTiles)
{
	m_prettyTiles = prettyTiles;
}

inline const LetterString &Move::prettyTiles() const
{
	return m_prettyTiles;
}

inline const LetterString &Move::tiles() const
{
	return m_tiles;
}

inline bool Move::isChallengedPhoney() const
{
	return m_isChallengedPhoney;
}

inline void Move::setIsChallengedPhoney(bool isChallengedPhoney)
{
	m_isChallengedPhoney = isChallengedPhoney;
}

inline int Move::scoreAddition() const
{
	return m_scoreAddition;
}

inline void Move::setScoreAddition(int scoreAddition)
{
	m_scoreAddition = scoreAddition;
}

inline bool Move::isAlreadyOnBoard(Letter letter)
{
	return letter == QUACKLE_PLAYED_THRU_MARK;
}

}

// we gotta overload so plays with diff equity 
// are equal
bool operator==(const Quackle::Move &move1, const Quackle::Move &move2);

UVOStream& operator<<(UVOStream& o, const Quackle::Move& m);
UVOStream& operator<<(UVOStream& o, const Quackle::MoveList& moves);

#endif
