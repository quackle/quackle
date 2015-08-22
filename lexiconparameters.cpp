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

#include <iostream>
#include <fstream>

#include "datamanager.h"
#include "lexiconparameters.h"
#include "uv.h"

using namespace Quackle;

class V0DawgInterpreter : public DawgInterpreter
{

	virtual void loadDawg(ifstream &file, unsigned char *dawg)
	{
		int i = 0;
		while (!file.eof())
		{
			file.read((char*)(dawg) + i, 7);
			i += 7;
		}
	}

	virtual void dawgAt(const unsigned char *dawg, int index, unsigned int &p, Letter &letter, bool &t, bool &lastchild, bool &british, int &playability) const
	{
		index *= 7;
		p = (dawg[index] << 16) + (dawg[index + 1] << 8) + (dawg[index + 2]);
		letter = dawg[index + 3];
		
		t = (letter & 32) != 0;
		lastchild = (letter & 64) != 0;
		british = !(letter & 128);
		letter = (letter & 31) + QUACKLE_FIRST_LETTER;

		playability = (dawg[index + 4] << 16) + (dawg[index + 5] << 8) + (dawg[index + 6]);
	}
	virtual int versionNumber() const { return 0; }
};

class V1DawgInterpreter : public DawgInterpreter
{

	virtual void loadDawg(ifstream &file, unsigned char *dawg)
	{
		int i = 0;
		while (!file.eof())
		{
			file.read((char*)(dawg) + i, 7);
			i += 7;
		}
	}

	virtual void dawgAt(const unsigned char *dawg, int index, unsigned int &p, Letter &letter, bool &t, bool &lastchild, bool &british, int &playability) const
	{
		index *= 7;
		p = (dawg[index] << 16) + (dawg[index + 1] << 8) + (dawg[index + 2]);
		letter = dawg[index + 3];
		
		t = (letter & 32) != 0;
		lastchild = (letter & 64) != 0;
		british = !(letter & 128);
		letter = (letter & 31) + QUACKLE_FIRST_LETTER;

		playability = (dawg[index + 4] << 16) + (dawg[index + 5] << 8) + (dawg[index + 6]);
	}
	virtual int versionNumber() const { return 1; }
};

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
	delete m_interpreter;
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

	char versionByte = file.get();
	file.unget();
	switch(versionByte)
	{
		case 0:
			m_interpreter = new V0DawgInterpreter();
			break;
		case 1:
			m_interpreter = new V1DawgInterpreter();
			break;
		default:
			UVcout << "couldn't open dawg " << filename.c_str() << endl;
			return;
	}

	m_dawg = new unsigned char[7000000];

	m_interpreter->loadDawg(file, m_dawg);
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

