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

#ifndef QUACKLE_RACK_H
#define QUACKLE_RACK_H

#include "alphabetparameters.h"

using namespace std;

namespace Quackle
{

class Move;

class Rack
{
public:
	// constructs a new empty rack
	Rack();

	// constructs a new rack containing letters in t
	Rack(const LetterString &tiles);

	// tiles like AEILNN?
	void setTiles(const LetterString &tiles);
	const LetterString &tiles() const;
	LetterString alphaTiles() const;

	// returns true if no tiles are on rack
	bool empty() const;

	// returns true if this rack has exactly the same letters as
	// the specified rack
	bool equals(const Rack &rack) const;

	// number of tiles on rack
	unsigned int size() const;

	// equivalent to operator-=(move.usedTiles())
	// and returns true if all tiles in used were found
	// in this rack and unloaded
	bool unload(const LetterString &used);

	// same as above but nonmutating
	bool contains(const LetterString &used) const;

	void shuffle();

	// sum of scores of letters on rack
	int score() const;

	UVString xml() const;
	UVString toString() const;

private:
	LetterString m_tiles;
};

inline Rack::Rack()
{
}

inline Rack::Rack(const LetterString &tiles)
{
    setTiles(tiles);
}

inline void Rack::setTiles(const LetterString &tiles)
{
	m_tiles = tiles;
}

inline const LetterString &Rack::tiles() const
{
	return m_tiles;
}

inline bool Rack::empty() const
{
	return m_tiles.empty();
}

}

const Quackle::Rack operator-(const Quackle::Rack &rack, const Quackle::Move &move);
const Quackle::Rack operator-(const Quackle::Rack &rack1, const Quackle::Rack &rack);

UVOStream &operator<<(UVOStream &o, const Quackle::Rack &rack);

#endif
