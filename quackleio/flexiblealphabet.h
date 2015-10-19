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

#ifndef QUACKLE_FLEXIBLEALPHABET_H
#define QUACKLE_FLEXIBLEALPHABET_H

#include "alphabetparameters.h"

namespace QuackleIO
{

class FlexibleAlphabetParameters : public Quackle::AlphabetParameters
{
public:
	FlexibleAlphabetParameters();

	// Loads alphabet in our format.
	// Lines of file should have format:
	// <text (utf8)> <blank text (utf8)> <score (int)> <count (int)> <isVowel (0 or 1)>
	// OR
	// blank <score (int)> <count (int)>
	// OR
	// # some comment
	bool load(const QString &filename);
};

}

#endif

