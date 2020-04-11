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

#include "enumerator.h"
#include "bag.h"
#include "datamanager.h"
#include "gameparameters.h"

using namespace std;
using namespace Quackle;

Enumerator::Enumerator(Bag &B)
{
	m_bag = B;
}

void Enumerator::normalizeProbabilities(ProbableRackList *racks)
{
	double sum = 0;
	for (ProbableRackList::const_iterator it = racks->begin(); it != racks->end(); ++it)
		sum += (*it).probability;
	for (ProbableRackList::iterator it = racks->begin(); it != racks->end(); ++it)
		(*it).probability = (*it).probability / sum;

	sum = 0;
	for (ProbableRackList::const_iterator it = racks->begin(); it != racks->end(); ++it)
		sum += (*it).possibility;
	for (ProbableRackList::iterator it = racks->begin(); it != racks->end(); ++it)
		(*it).possibility = (*it).possibility / sum;
}

void Enumerator::enumerate(ProbableRackList *racks, unsigned int rackSize)
{
	racks->clear();

	m_bag.letterCounts(m_bagcounts);

	m_possibleBag = m_bag;
	
	recurse(LetterString(), 0, 0, racks, rackSize);

	normalizeProbabilities(racks);
}

void Enumerator::enumerate(ProbableRackList *racks)
{
	enumerate(racks, QUACKLE_PARAMETERS->rackSize());
}

void Enumerator::enumeratePossible(ProbableRackList *racks, const Bag &bag)
{
	racks->clear();

	m_bag.letterCounts(m_bagcounts);

	//UVcout << "enumeratePossible called with bag: " << bag << endl;

	m_possibleBag = m_bag;
	m_possibleBag.removeLetters(bag.tiles());

	//UVcout << "m_bag: " << m_bag << endl;
	//UVcout << "m_possibleBag: " << m_possibleBag << endl;

	recurse(LetterString(), 0, 0, racks, QUACKLE_PARAMETERS->rackSize());

	normalizeProbabilities(racks);

}

void Enumerator::recurse(LetterString prefix, int i, Letter start, ProbableRackList *racks, unsigned int rackSize)
{
	if (prefix.length() == rackSize)
	{
		ProbableRack probableRack;
		probableRack.rack = Rack(prefix);
		probableRack.probability = m_bag.probabilityOfDrawing(probableRack.rack.tiles());
		probableRack.possibility = m_possibleBag.probabilityOfDrawing(probableRack.rack.tiles());
		racks->push_back(probableRack);
		return;
	}

	for (Letter c = start; c <= QUACKLE_ALPHABET_PARAMETERS->lastLetter(); ++i, ++c)
	{
		if (m_bagcounts[i] > 0)
		{
			m_bagcounts[i]--;
			recurse(prefix + c, i, c, racks, rackSize);
			m_bagcounts[i]++;
		}
	}
}
