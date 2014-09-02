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

#ifndef QUACKLE_BAG_H
#define QUACKLE_BAG_H

#include "alphabetparameters.h"
#include "rack.h"

using namespace std;

namespace Quackle
{

class Move;

class Bag
{
public:
	Bag();
	Bag(const LetterString &contents);

	void clear();

	void exch(const Move &move, Rack &rack);

	// removes and returns a random letter from bag
	Letter pluck();

	// returns true if all letters were in the bag before
	// and were removed
	bool removeLetters(const LetterString &letters);
	bool removeLetters(const LongLetterString &letters);

	// returns true if the letter was in the bag and was
	// removed
	bool removeLetter(Letter letter);

	// how many of each letter are in the bag
	void letterCounts(char *countsArray) const;

	// put letters back in the bag
	void toss(const LetterString &letters);
	void toss(const LongLetterString &letters);
	void toss(const Rack &rack);

	// Fill rack up with tiles from the bag picked in random order.
	// Alphabetizes rack.
	void refill(Rack &rack);

	// Fill rack up with tiles from the bag picked in drawingOrder,
	// starting from the back of the LetterString.
	// Returns letters from drawingOrder that weren't added to the bag.
	// Alphabetizes rack.
	LetterString refill(Rack &rack, const LetterString &drawingOrder);

	// use this to start out your bag for use
	void prepareFullBag();

	// whether there are no tiles left in the bag
	bool empty() const;

	// returns number of tiles left in the bag
	int size() const;

	const LongLetterString &tiles() const;
	
	// returns our tiles in a random order
	LongLetterString shuffledTiles() const;

	// returns as many of our tiles in a random order as will
	// fit in a regular LetterString
	LetterString someShuffledTiles() const;

	static double probabilityOfDrawingFromFullBag(const LetterString &letters);
	static double probabilityOfDrawingFromBag(const LetterString &letters, const Bag &bag);
	double probabilityOfDrawing(const LetterString &letters);

	UVString toString() const;

private:
	// remove letter from the bag
	Letter erase(int pos);

	LongLetterString m_tiles;
};

inline void Bag::toss(const Rack &rack)
{
	toss(rack.tiles());
}

inline bool Bag::empty() const
{
    return m_tiles.empty();
}

inline int Bag::size() const
{
    return m_tiles.size();
}

inline const LongLetterString &Bag::tiles() const
{
	return m_tiles;
}

}

UVOStream &operator<<(UVOStream &o, const Quackle::Bag &bag);

#endif
