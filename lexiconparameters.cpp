/*
 *  Quackle -- Crossword game artificial intelligence and analysis tool
 *  Copyright (C) 2005-2006 Jason Katz-Brown and John O'Laughlin.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  
 *  02110-1301  USA
 */

#include <iostream>
#include <fstream>

#include "datamanager.h"
#include "lexiconparameters.h"
#include "uv.h"

using namespace Quackle;

LexiconParameters::LexiconParameters()
	: m_dawg(0), m_gaddag(0)
{
}

LexiconParameters::~LexiconParameters()
{
	unloadAll();
}

void LexiconParameters::unloadAll()
{
	unloadDawg();
	unloadGaddag();
}

void LexiconParameters::unloadDawg()
{
	delete[] m_dawg;
	m_dawg = 0;
}

void LexiconParameters::unloadGaddag()
{
	delete[] m_gaddag;
	m_gaddag = 0;
}

void LexiconParameters::loadDawg(const string &filename)
{
	unloadDawg();

	ifstream file(filename.c_str(), ios::in | ios::binary);
	if (!file.is_open())
	{
		UVcout << "couldn't open dawg " << filename.c_str() << endl;
		return;
	}

	m_dawg = new unsigned char[7000000];

	int i = 0;
	while (!file.eof())
	{
		file.read((char*)(m_dawg) + i, 7);
		i += 7;
	}
}

void LexiconParameters::loadGaddag(const string &filename)
{
	unloadGaddag();

	ifstream file(filename.c_str(), ios::in | ios::binary);
	if (!file.is_open())
	{
		UVcout << "couldn't open gaddag " << filename.c_str() << endl;
		UVcout << "Performance without gaddag won't be quite so awesome." << endl;
		return;
	}

	m_gaddag = new unsigned char[40000000];

	int i = 0;
	while (!file.eof())
	{
		file.read((char*)(m_gaddag) + i, 4);
		i += 4;
	}
}

string LexiconParameters::findDictionaryFile(const string &lexicon)
{
	return DataManager::self()->findDataFile("lexica", lexicon);
}

