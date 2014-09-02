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

#ifndef QUACKLE_STRATEGYPARAMETERS_H
#define QUACKLE_STRATEGYPARAMETERS_H

#include <map>
#include "alphabetparameters.h"

namespace Quackle
{

class StrategyParameters
{
public:
	StrategyParameters();

	void initialize(const string &lexicon);
	bool isInitialized() const;

	// letters are raw letters include bottom marks
	double syn2(Letter letter1, Letter letter2) const;
	double tileWorth(Letter letter) const;
	double vcPlace(int start, int length, int consbits);
	double bogowin(int lead, int unseen, int blanks);
	double superleave(LetterString leave);
	
protected:
	bool loadSyn2(const string &filename);
	bool loadWorths(const string &filename);
	bool loadVcPlace(const string &filename);
	bool loadBogowin(const string &filename);
	bool loadSuperleaves(const string &filename);
	
	int mapLetter(Letter letter) const;

	double m_syn2[QUACKLE_FIRST_LETTER + QUACKLE_MAXIMUM_ALPHABET_SIZE][QUACKLE_FIRST_LETTER + QUACKLE_MAXIMUM_ALPHABET_SIZE];
	double m_tileWorths[QUACKLE_FIRST_LETTER + QUACKLE_MAXIMUM_ALPHABET_SIZE];
	double m_vcPlace[QUACKLE_MAXIMUM_BOARD_SIZE][QUACKLE_MAXIMUM_BOARD_SIZE][128];

	static const int m_bogowinArrayWidth = 601;
	static const int m_bogowinArrayHeight = 94;
	double m_bogowin[m_bogowinArrayWidth][m_bogowinArrayHeight];
        typedef map<LetterString, double> SuperLeavesMap;
	SuperLeavesMap m_superleaves;
	bool m_initialized;
};

inline bool StrategyParameters::isInitialized() const
{
	return m_initialized;
}

inline int StrategyParameters::mapLetter(Letter letter) const
{
	// no mapping needed
	return letter;
}

inline double StrategyParameters::syn2(Letter letter1, Letter letter2) const
{
	return m_syn2[mapLetter(letter1)][mapLetter(letter2)];
}

inline double StrategyParameters::tileWorth(Letter letter) const
{
	return m_tileWorths[mapLetter(letter)];
}

inline double StrategyParameters::vcPlace(int start, int length, int consbits)
{
	if ((consbits < 0) || (consbits >= 128) || 
		(start < 0) || (start >= QUACKLE_MAXIMUM_BOARD_SIZE) ||
		(length < 0) || (length >= QUACKLE_MAXIMUM_BOARD_SIZE))
		return 0;

	return m_vcPlace[start][length][consbits];
}

inline double StrategyParameters::bogowin(int lead, int unseen, int /* blanks */)
{
	if (lead < -300) return 0;
	if (lead > 300) return 1;

	if (unseen > 93) unseen = 93;

	if (unseen == 0)
	{
		if (lead < 0) return 0;
		else if (lead == 0) return 0.5;
		else return 1;
	}
	
	return m_bogowin[lead + 300][unseen];
}

inline double StrategyParameters::superleave(LetterString leave)
{
	if (leave.length() == 0)
		return 0.0;
	return m_superleaves[leave];
}

}

#endif
