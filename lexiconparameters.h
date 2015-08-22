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

#ifndef QUACKLE_LEXICONPARAMETERS_H
#define QUACKLE_LEXICONPARAMETERS_H

#include "alphabetparameters.h"
#include "gaddag.h"

namespace Quackle
{

class DawgInterpreter
{
public:
	virtual void loadDawg(ifstream &file, unsigned char *dawg) = 0;
	virtual void dawgAt(const unsigned char *dawg, int index, unsigned int &p, Letter &letter, bool &t, bool &lastchild, bool &british, int &playability) const = 0;
	virtual int versionNumber() const = 0;
	virtual ~DawgInterpreter() {};
};


class LexiconParameters
{
public:
	LexiconParameters();
	~LexiconParameters();

	void unloadAll();

	// true if we have a dawg or a gaddag
	bool hasSomething() const { return hasDawg() || hasGaddag(); };

	// loadDawg unloads the dawg if filename can't be opened
	void loadDawg(const string &filename);
	void unloadDawg();
	bool hasDawg() const { return m_dawg != NULL; };
	int dawgVersion() const { return m_interpreter->versionNumber(); };

	// loadGaddag unloads the gaddag if filename can't be opened
	void loadGaddag(const string &filename);
	void unloadGaddag();
	bool hasGaddag() const { return m_gaddag != NULL; };

	// finds a file in the lexica data directory
	static string findDictionaryFile(const string &lexicon);

	// a convenience field; this is unused by libquackle
	string lexiconName() const { return m_lexiconName; };
	void setLexiconName(const string &name) { m_lexiconName = name; };

	void dawgAt(int index, unsigned int &p, Letter &letter, bool &t, bool &lastchild, bool &british, int &playability) const
	{
		m_interpreter->dawgAt(m_dawg, index, p, letter, t, lastchild, british, playability);
	}
	const GaddagNode *gaddagRoot() const { return (GaddagNode *) &m_gaddag[0]; };

protected:
	unsigned char *m_dawg;
	unsigned char *m_gaddag;
	string m_lexiconName;
	DawgInterpreter *m_interpreter;
};

}
#endif
