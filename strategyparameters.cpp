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

#include <iostream>
#include <fstream>

#include "alphabetparameters.h"
#include "boardparameters.h"
#include "datamanager.h"
#include "strategyparameters.h"

using namespace Quackle;
using namespace std;

StrategyParameters::StrategyParameters()
	: m_hasSyn2(false)
	, m_hasWorths(false)
	, m_hasVcPlace(false)
	, m_hasBogowin(false)
	, m_hasSuperleaves(false)
{
}

void StrategyParameters::initialize(const string &lexicon)
{
	m_hasSyn2 = loadSyn2(DataManager::self()->findDataFile("strategy", lexicon, "syn2"));
	m_hasWorths = loadWorths(DataManager::self()->findDataFile("strategy", lexicon, "worths"));
	m_hasVcPlace = loadVcPlace(DataManager::self()->findDataFile("strategy", lexicon, "vcplace"));
	m_hasBogowin = loadBogowin(DataManager::self()->findDataFile("strategy", lexicon, "bogowin"));
	m_hasSuperleaves = loadSuperleaves(DataManager::self()->findDataFile("strategy", lexicon, "superleaves")); 	
}

bool StrategyParameters::loadSyn2(const string &filename)
{
	for (int i = 0; i < QUACKLE_FIRST_LETTER + QUACKLE_MAXIMUM_ALPHABET_SIZE; ++i)
		for (int j = 0; j < QUACKLE_FIRST_LETTER + QUACKLE_MAXIMUM_ALPHABET_SIZE; ++j)
			m_syn2[i][j] = 0;

	UVIFStream file(filename.c_str());

	if (!file.is_open())
	{
		cerr << "Could not open " << filename << " to load syn2" << endl;
		return false;
	}

	while (!file.eof())
	{
		UVString letters;
		file >> letters;

		if (letters.empty())
			continue;

		LetterString letterString = QUACKLE_ALPHABET_PARAMETERS->encode(letters);
		if (letterString.length() != 2)
		{
			UVcerr << "letter string " << letters << " can not be encoded into two letters while reading syn2" << endl;
			break;
		}

		if (file.eof())
			break;

		double value;
		file >> value;

		m_syn2[(int)letterString[0]][(int)letterString[1]] = value;
		m_syn2[(int)letterString[1]][(int)letterString[0]] = value;
	}

	file.close();
	return true;
}

bool StrategyParameters::loadBogowin(const string &filename)
{
	for (int i = 0; i < m_bogowinArrayWidth; ++i)
		for (int j = 0; j < m_bogowinArrayHeight; ++j)
			m_bogowin[i][j] = 0;

	UVIFStream file(filename.c_str());

	if (!file.is_open())
	{
		cerr << "Could not open " << filename << " to load bogowin heuristic" << endl;
		return false;
	}

	while (!file.eof())
	{
		int lead, unseen;
		double wins;

		file >> lead;
		file >> unseen;
		file >> wins;
		
		m_bogowin[lead + 300][unseen] = wins;
	}
	
	file.close();
	return true;
}

bool StrategyParameters::loadWorths(const string &filename)
{
	for (int i = 0; i < QUACKLE_FIRST_LETTER + QUACKLE_MAXIMUM_ALPHABET_SIZE; ++i)
		m_tileWorths[i] = 0;

	UVIFStream file(filename.c_str());

	if (!file.is_open())
	{
		cerr << "Could not open " << filename << " to load worths" << endl;
		return false;
	}

	while (!file.eof())
	{
		UVString letters;
		file >> letters;

		if (letters.empty())
			continue;

		LetterString letterString = QUACKLE_ALPHABET_PARAMETERS->encode(letters);
		if (letterString.length() != 1)
		{
			UVcerr << "letter string " << letters << " can not be encoded into one letter while reading worths" << endl;
			break;
		}

		if (file.eof())
			break;

		double value;
		file >> value;

		m_tileWorths[(int)letterString[0]] = value;
	}

	file.close();
	return true;
}

bool StrategyParameters::loadVcPlace(const string &filename)
{
	for (int i = 0; i < QUACKLE_MAXIMUM_BOARD_SIZE; ++i)
		for (int j = 0; j < QUACKLE_MAXIMUM_BOARD_SIZE; ++j)
			for (int k = 0; k < 128; ++k)
				m_vcPlace[i][j][k] = 0;

	UVIFStream file(filename.c_str());

	if (!file.is_open())
	{
		cerr << "Could not open " << filename << " to load vcPlace heuristic" << endl;
		return false;
	}

	while (!file.eof())
	{
		unsigned int start;
		file >> start;

		if (file.eof())
			break;
	
		unsigned int length;
		file >> length;
		
		if (file.eof())
			break;

		unsigned int consbits;
		file >> consbits;
	
		if (file.eof())
			break;

		double value;
		file >> value;

		if ((start < QUACKLE_MAXIMUM_BOARD_SIZE) && 
			(length < QUACKLE_MAXIMUM_BOARD_SIZE) &&
			(consbits < 128))

		m_vcPlace[start][length][consbits] = value;
	}

	file.close();
	return true;	
}

bool StrategyParameters::loadSuperleaves(const string &filename)
{
	m_superleaves.clear();

	ifstream file(filename.c_str(), ios::in | ios::binary);

	if (!file.is_open())
	{
		cerr << "Could not open " << filename << " to load superleave heuristic" << endl;
		return false;
	}

	unsigned char leavesize;
	char leavebytes[16];
	unsigned char intvalueint;
	unsigned char intvaluefrac;
	unsigned int intvalue;

	while (!file.eof())
	{
		file.read((char*)(&leavesize), 1);
		file.read(leavebytes, leavesize);
		file.read((char*)(&intvaluefrac), 1);
		file.read((char*)(&intvalueint), 1);
		if (file.eof())
			break;

		intvalue = (unsigned int)(intvalueint) * 256 + (unsigned int)(intvaluefrac);
		LetterString leave = LetterString(leavebytes, leavesize);
	
		double value = (double(intvalue) / 256.0) - 128.0;
		m_superleaves.insert(m_superleaves.end(),
				     SuperLeavesMap::value_type(leave, value));
	}
	
	file.close();
	return true;	
}
