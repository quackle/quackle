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

#ifndef QUACKLE_BOARD_H
#define QUACKLE_BOARD_H

#include <vector>
#include <bitset>

#include "alphabetparameters.h"
#include "bag.h"
#include "move.h"
#include "rack.h"

using namespace std;

typedef bitset<QUACKLE_MAXIMUM_ALPHABET_SIZE> LetterBitset;

#define QUACKLE_MAXIMUM_BOARD_SIZE LETTER_STRING_MAXIMUM_LENGTH
#define QUACKLE_MINIMUM_BOARD_SIZE 7

namespace Quackle
{

class Board
{
public:
    // create uninitialized board of size specified
    // by global BoardParameters
    Board();

    // create uninitialized width x height board.
    // Width and height must each be between 
    // QUACKLE_MINIMUM_BOARD_SIZE and QUACKLE_MAXIMUM_BOARD_SIZE.
    Board(int width, int height);

    // use this to start out your board for use
	void prepareEmptyBoard();

	int width() const { return m_width; }
	int height() const { return m_height; }

	Bag tilesNotOnBoard() const;
	Bag tilesOnBoard() const;

	bool isEmpty() const;

	void makeMove(const Move &move);

	// Returns all words formed when play is made.
	// If move.tiles() is only of length 1, specified move is not in the 
	// returned list; otherwise it is.
	MoveList allWordsFormedBy(const Move &move) const;

	// returns whether the board is empty or tiles of move touch
	// at least one other square of the board
	bool isConnected(const Move &move) const;

	// if the board is empty, does this move not hit the center square?
	bool isUnacceptableOpeningMove(const Move &move) const;

	// updates british status of all squares
	void updateBritishness();

	// Return score of move suitable for score field of move.
	// If isBingo is nonzero, whether or not the play is a bingo
	// is stored in isBingo.
	int score(const Move &move, bool *isBingo = 0) const;

	// Return string suitable for prettyTiles field of move.
	// If markPlayThruTiles is true, wrap tiles played thru in
	// parentheses
	LetterString prettyTilesOfMove(const Move &move, bool markPlayThruTiles = true) const;

	LetterString sanitizedTilesOfMove(const Move &move) const;

	UVString toString() const;
	UVString htmlBoard(const int tileSize) const;
	UVString htmlKey() const;

	enum TileType { LetterTile = 0, BonusSquareTile, NothingTile };
	enum BonusSquareType { LetterBonus = 0, WordBonus, NoBonus };

	struct TileInformation
	{
		TileInformation()
			: letter(QUACKLE_NULL_MARK), tileType(NothingTile), isBlank(false), isBritish(false), isStartLocation(false), bonusSquareType(NoBonus), bonusMultiplier(0), isOnRack(false)
		{
		}

		// Letter is always nonblank version! blankness is stored in isBlank.
		// If there is no letter, this is the null mark.
		Letter letter;

		TileType tileType;
		bool isBlank;
		bool isBritish;

		bool isStartLocation;

		BonusSquareType bonusSquareType;
		int bonusMultiplier;

        bool isOnRack;
	};

	TileInformation tileInformation(int row, int col) const;

	Letter letter(int row, int col) const;
	bool isBlank(int row, int col) const;
	bool isBritish(int row, int col) const;

	const LetterBitset &vcross(int row, int col) const;
	void setVCross(int row, int col, const LetterBitset &vcross);

	const LetterBitset &hcross(int row, int col) const;
	void setHCross(int row, int col, const LetterBitset &hcross);

protected:
	int m_width;
	int m_height;
	bool m_empty;

	Letter m_letters[QUACKLE_MAXIMUM_BOARD_SIZE][QUACKLE_MAXIMUM_BOARD_SIZE];
	bool m_isBlank[QUACKLE_MAXIMUM_BOARD_SIZE][QUACKLE_MAXIMUM_BOARD_SIZE];
	bool m_isBritish[QUACKLE_MAXIMUM_BOARD_SIZE][QUACKLE_MAXIMUM_BOARD_SIZE];

	LetterBitset m_vcross[QUACKLE_MAXIMUM_BOARD_SIZE][QUACKLE_MAXIMUM_BOARD_SIZE];
	LetterBitset m_hcross[QUACKLE_MAXIMUM_BOARD_SIZE][QUACKLE_MAXIMUM_BOARD_SIZE];

	inline bool isNonempty(int row, int column) const;
};

inline bool Board::isEmpty() const
{
	return m_empty;
}

inline Letter Board::letter(int row, int col) const
{
	return m_letters[row][col];
}

inline bool Board::isBlank(int row, int col) const
{
	return m_isBlank[row][col];
}

inline bool Board::isBritish(int row, int col) const
{
	return m_isBritish[row][col];
}

inline const LetterBitset &Board::vcross(int row, int col) const
{
	return m_vcross[row][col];
}

inline void Board::setVCross(int row, int col, const LetterBitset &vcross)
{
	m_vcross[row][col] = vcross;
}

inline const LetterBitset &Board::hcross(int row, int col) const
{
	return m_hcross[row][col];
}

inline void Board::setHCross(int row, int col, const LetterBitset &hcross)
{
	m_hcross[row][col] = hcross;
}

inline bool Board::isNonempty(int row, int column) const
{
	return m_letters[row][column] != QUACKLE_NULL_MARK;
}

}

UVOStream &operator<<(UVOStream &o, const Quackle::Board &board);

#endif
