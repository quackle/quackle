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

#ifndef QUACKLE_ENUMERATOR_H
#define QUACKLE_ENUMERATOR_H

#include <vector>

#include "bag.h"

using namespace std;

namespace Quackle
{
class Bag;

struct ProbableRack
{
	Rack rack;
	double probability;
	double possibility;
};
typedef vector<ProbableRack> ProbableRackList;

class Enumerator
{
public:
	Enumerator(Bag &B);

	// enumerates all rackSize racks and finds their probability
	void enumerate(ProbableRackList *racks, unsigned int rackSize);

	// assumes rack size is as defined in QUACKLE_PARAMETERS
	void enumerate(ProbableRackList *racks);
	void enumeratePossible(ProbableRackList *racks, const Bag &bag);

	// makes all of the probabilities sum to 1
	static void normalizeProbabilities(ProbableRackList *racks);

private:	
	void recurse(LetterString prefix, int i, Letter start, ProbableRackList *racks, unsigned int rackSize);
	char m_bagcounts[QUACKLE_FIRST_LETTER + QUACKLE_MAXIMUM_ALPHABET_SIZE];
	Bag m_bag;
	Bag m_possibleBag;
};


}

#endif
