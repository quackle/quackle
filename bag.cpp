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

#include "alphabetparameters.h"
#include "bag.h"
#include "datamanager.h"
#include "gameparameters.h"
#include "rack.h"
#include "move.h"

using namespace std;
using namespace Quackle;

Bag::Bag()
{
	prepareFullBag();
}

Bag::Bag(const LetterString &contents)
{
	toss(contents);
}

void Bag::clear()
{
	m_tiles.clear();
}

void Bag::prepareFullBag()
{
	// put stuff in here to fill the bag
	m_tiles.clear();

	// we start at 0 because we want to include blanks etcetera
	for (Letter letter = 0; letter <= QUACKLE_ALPHABET_PARAMETERS->lastLetter(); ++letter)
		for (int i = 0; i < QUACKLE_ALPHABET_PARAMETERS->count(letter); ++i)
			m_tiles.push_back(letter);
}

int Bag::fullBagTileCount()
{
	int tileCount = 0;
	// we start at 0 because we want to include blanks etcetera
	for (Letter letter = 0; letter <= QUACKLE_ALPHABET_PARAMETERS->lastLetter(); ++letter)
		tileCount += QUACKLE_ALPHABET_PARAMETERS->count(letter);
	return tileCount;
}

void Bag::toss(const LetterString &letters)
{
	const LetterString::const_iterator end(letters.end());
	for (LetterString::const_iterator it = letters.begin(); it != end; ++it)
		m_tiles.push_back(*it);
}

void Bag::toss(const LongLetterString &letters)
{
	const LongLetterString::const_iterator end(letters.end());
	for (LongLetterString::const_iterator it = letters.begin(); it != end; ++it)
		m_tiles.push_back(*it);
}

Letter Bag::erase(int pos)
{
	LongLetterString::iterator it = m_tiles.begin();

	for (int i = 0; i < pos; ++it, ++i) ;

	Letter ret = *it;
	m_tiles.erase(it);

	return ret;
}

void Bag::exch(const Move &move, Rack &rack)
{
	LetterString usedTiles(move.usedTiles());
	rack.unload(usedTiles);

	refill(rack);
	toss(usedTiles);                                                                                                                        
}

Letter Bag::pluck()
{
	return erase(DataManager::self()->randomInteger(0, (int)m_tiles.size() - 1));
}

bool Bag::removeLetters(const LetterString &letters)
{
	bool ret = true;

	const LetterString::const_iterator end(letters.end());
	for (LetterString::const_iterator it = letters.begin(); it != end; ++it)
		if (!removeLetter(*it))
			ret = false;

	return ret;
}

bool Bag::removeLetters(const LongLetterString &letters)
{
	bool ret = true;

	const LongLetterString::const_iterator end(letters.end());
	for (LongLetterString::const_iterator it = letters.begin(); it != end; ++it)
		if (!removeLetter(*it))
			ret = false;

	return ret;
}

void Bag::letterCounts(char *countsArray) const
{
	String::counts(m_tiles, countsArray);
}

bool Bag::removeLetter(Letter letter)
{
	LongLetterString::iterator it;
	const LongLetterString::iterator end(m_tiles.end());
	for (it = m_tiles.begin(); it != end; ++it)
		if (*it == letter)
			break;

	if (it == m_tiles.end())
		return false;

	m_tiles.erase(it);
	return true;
}

void Bag::refill(Rack &rack)
{
	for (int number = QUACKLE_PARAMETERS->rackSize() - rack.tiles().length(); number > 0 && !m_tiles.empty(); --number)
		rack.setTiles(String::alphabetize(rack.tiles() + pluck()));
}

LetterString Bag::refill(Rack &rack, const LetterString &drawingOrder)
{
	LetterString ret(drawingOrder);

	for (int number = QUACKLE_PARAMETERS->rackSize() - rack.tiles().length(); number > 0 && !m_tiles.empty(); --number)
	{
		if (drawingOrder.empty())
			rack.setTiles(String::alphabetize(rack.tiles() + pluck()));
		else
		{
			removeLetter(String::back(ret));
			rack.setTiles(String::alphabetize(rack.tiles() + String::back(ret)));
			String::pop_back(ret);
		}
	}

	return ret;
}

LongLetterString Bag::shuffledTiles() const
{
	LongLetterString ret(m_tiles);
	DataManager::self()->shuffle(ret);
	return ret;
}

LetterString Bag::someShuffledTiles() const
{
	LongLetterString shuffled(shuffledTiles());
	LetterString ret;
	int i = 0;
	for (LongLetterString::const_iterator it = shuffled.begin(); it != shuffled.end() && i < LETTER_STRING_MAXIMUM_LENGTH - 1; ++it, ++i)
		ret.push_back(*it);

	return ret;
}

int factorial(int n)
{
	if (n < 0)
		return 0;

	switch (n)
	{
	case 0:
	case 1:
		return 1;
	case 2:
		return 2;
	case 3:
		return 6;
	case 4:
		return 24;
	case 5:
		return 120;
	case 6:
		return 720;
	case 7:
		return 5040;
	case 8:
		return 40320;
	case 9:
		return 362880;
	case 10:
		return 3628800;
	case 11:
		return 39916800;
	case 12:
		return 479001600;
	
	default:
		return factorial(n - 1) * n;
	}
}

int nCr(int n, int r)
{
	// don't worry about blanks yet
	if (r > n)
		return 0;

	return factorial(n) / (factorial(r) * factorial(n - r));
}

double Bag::probabilityOfDrawingFromFullBag(const LetterString &letters)
{
	char counts[QUACKLE_FIRST_LETTER + QUACKLE_MAXIMUM_ALPHABET_SIZE];
	String::counts(String::clearBlankness(letters), counts);

	float ret = 1;

	for (Letter letter = 0; letter <= QUACKLE_ALPHABET_PARAMETERS->lastLetter(); ++letter)
		if (counts[(int)letter] > 0)
			ret *= nCr(QUACKLE_ALPHABET_PARAMETERS->count(letter), counts[(int)letter]);

	return ret;
}

double Bag::probabilityOfDrawingFromBag(const LetterString &letters, const Bag &bag)
{
	char bagCounts[QUACKLE_FIRST_LETTER + QUACKLE_MAXIMUM_ALPHABET_SIZE];
	String::counts(bag.tiles(), bagCounts);

	char counts[QUACKLE_FIRST_LETTER + QUACKLE_MAXIMUM_ALPHABET_SIZE];
	String::counts(String::clearBlankness(letters), counts);

	float ret = 1;

	for (Letter letter = 0; letter < QUACKLE_FIRST_LETTER + QUACKLE_MAXIMUM_ALPHABET_SIZE; ++letter)
		if (counts[(int)letter] > 0)
			ret *= nCr(bagCounts[(int)letter], counts[(int)letter]);

	return ret;
}

double Bag::probabilityOfDrawing(const LetterString &letters)
{
	return probabilityOfDrawingFromBag(letters, *this);
}


UVString Bag::toString() const
{
	UVString ret;

	LongLetterString sortedLetters = m_tiles;
	sort(sortedLetters.begin(), sortedLetters.end());

	const LongLetterString::const_iterator end(sortedLetters.end());
	for (LongLetterString::const_iterator it = sortedLetters.begin(); it != end; ++it)
		ret += QUACKLE_ALPHABET_PARAMETERS->userVisible(*it);

	return ret;
}

UVOStream &operator<<(UVOStream &o, const Bag &bag)
{
	o << "Bag (" << bag.size() << "): ";

	if (bag.empty())
		o << "[empty]";
	else
		o << bag.toString();

	return o;
}

