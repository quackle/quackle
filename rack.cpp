/*
 *  Quackle -- Crossword game artificial intelligence and analysis tool
 *  Copyright (C) 2005-2019 Jason Katz-Brown, John O'Laughlin, and John Fultz.
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

#include <algorithm>
#include <iostream>

#include "datamanager.h"
#include "move.h"
#include "rack.h"
#include "uv.h"

using namespace std;
using namespace Quackle;

LetterString Rack::alphaTiles() const
{
	return String::alphabetize(m_tiles);
}

bool Rack::equals(const Rack &rack) const
{
	return alphaTiles() == rack.alphaTiles();
}

bool Rack::unload(const LetterString &used)
{
	// UVcout << *this << ".unload(" << used << ")" << endl;

	// Blank out rack spots that correspond to used tiles.
	LetterString::const_iterator usedEnd(used.end());
	for (LetterString::const_iterator usedIt = used.begin(); usedIt != usedEnd; ++usedIt)
	{
		LetterString::iterator tileEnd(m_tiles.end());
		for (LetterString::iterator tileIt = m_tiles.begin(); tileIt != tileEnd; ++tileIt)
		{
			if (*tileIt == *usedIt)
			{
				*tileIt = QUACKLE_NULL_MARK;
				break;
			}
		}
	}

	// Compress all blank spots. (GCC can do the body of this loop branch-free.)
	LetterString::const_iterator tileEnd(m_tiles.end());
	LetterString::iterator newTileEnd(m_tiles.begin());
	for (LetterString::const_iterator tileIt = m_tiles.begin(); tileIt != tileEnd; ++tileIt)
	{
		*newTileEnd = *tileIt;
		if (*tileIt != QUACKLE_NULL_MARK)
			newTileEnd++;
	}

	size_t expected_remaining_tiles = m_tiles.size() - used.size();
	m_tiles.truncate(newTileEnd - m_tiles.begin());
	return m_tiles.size() == expected_remaining_tiles;
}

void Rack::load(const LetterString &tiles)
{
	for (LetterString::const_iterator it = tiles.begin(); it != tiles.end(); ++it)
	{
		if (it != QUACKLE_NULL_MARK)
			m_tiles += *it;
	}
}

bool Rack::contains(const LetterString &used) const
{
	return Rack(*this).unload(used);
}

void Rack::shuffle()
{
	DataManager::self()->shuffle(m_tiles);
}

int Rack::score() const
{
	int ret = 0;

	const LetterString::const_iterator end(m_tiles.end());
	for (LetterString::const_iterator it = m_tiles.begin(); it != end; ++it)
		ret += QUACKLE_ALPHABET_PARAMETERS->score(*it);

	return ret;
}

unsigned int Rack::size() const
{
	return m_tiles.size();
}

const Rack operator-(const Rack &rack, const Move &move)
{
	Rack ret(rack);
    ret.unload(move.usedTiles());
    return ret;
}

const Rack operator-(const Rack &rack1, const Rack &rack2)
{
	Rack ret(rack1);
    ret.unload(rack2.tiles());
    return ret;
}

UVString Rack::xml() const
{
	return MARK_UV("<rack tiles=\"") + QUACKLE_ALPHABET_PARAMETERS->userVisible(m_tiles) + MARK_UV("\" />");
}

UVString Rack::toString() const
{
	return QUACKLE_ALPHABET_PARAMETERS->userVisible(m_tiles);
}

UVOStream &operator<<(UVOStream &o, const Rack &rack)
{
    o << rack.toString();
    return o;
}

