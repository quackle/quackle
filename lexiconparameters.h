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

#ifndef QUACKLE_LEXICONPARAMETERS_H
#define QUACKLE_LEXICONPARAMETERS_H

#include "alphabetparameters.h"
#include "gaddag.h"

namespace Quackle
{

class LexiconParameters
{
public:
	LexiconParameters();
	~LexiconParameters();

	void unloadAll();

	// true if we have a dawg or a gaddag
	bool hasSomething() const;

	// loadDawg unloads the dawg if filename can't be opened
	void loadDawg(const string &filename);
	void unloadDawg();
	bool hasDawg() const;

	// loadGaddag unloads the gaddag if filename can't be opened
	void loadGaddag(const string &filename);
	void unloadGaddag();
	bool hasGaddag() const;

	// finds a file in the lexica data directory
	static string findDictionaryFile(const string &lexicon);

	// a convenience field; this is unused by libquackle
	string lexiconName() const;
	void setLexiconName(const string &name);

	unsigned char dawgAt(int index) const;
	const GaddagNode *gaddagRoot() const;

protected:
	unsigned char *m_dawg;
	unsigned char *m_gaddag;
	string m_lexiconName;
};

inline bool LexiconParameters::hasSomething() const
{
	return hasDawg() || hasGaddag();
}

inline bool LexiconParameters::hasDawg() const
{
	return m_dawg;
}

inline bool LexiconParameters::hasGaddag() const
{
	return m_gaddag;
}

inline unsigned char LexiconParameters::dawgAt(int index) const
{
	return m_dawg[index];
}

inline const GaddagNode *LexiconParameters::gaddagRoot() const
{
    return (GaddagNode *) &m_gaddag[0];
}

inline string LexiconParameters::lexiconName() const
{
	return m_lexiconName;
}

inline void LexiconParameters::setLexiconName(const string &name)
{
	m_lexiconName = name;
}

}
#endif
