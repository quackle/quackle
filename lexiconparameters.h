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

#include <vector>

#include "gaddag.h"
#include "v2gaddag.h"

namespace Quackle
{

class LexiconParameters;

class LexiconInterpreter
{
public:
	virtual void loadGaddag(ifstream &file, unsigned char* gaddag,
													V2Gaddag** v2gaddag) = 0;
	virtual int versionNumber() const = 0;
	virtual ~LexiconInterpreter() {};
};

class V0LexiconInterpreter;
class V1LexiconInterpreter;
class V2LexiconInterpreter;

class LexiconParameters
{
	friend class Quackle::V2LexiconInterpreter;

public:
	LexiconParameters();
	~LexiconParameters();

	void unloadAll();

	// true if we have a dawg or a gaddag
	bool hasSomething() const { return hasGaddag(); };

	void loadGaddags(const string &filename);
	void unloadGaddags();
	bool hasGaddag() const { return m_gaddags != NULL; };

	// finds a file in the lexica data directory
	static string findDictionaryFile(const string &lexicon);
	static bool hasUserDictionaryFile(const string &lexicon);

	// a convenience field; this is unused by libquackle
	string lexiconName() const { return m_lexiconName; };
	void setLexiconName(const string &name) { m_lexiconName = name; };

	const V2Gaddag *v2Gaddag() const { return m_v2gaddag; }
	const V2Gaddag *v2Gaddag_7to7() const { return m_v2gaddag_7to7; }
	const V2Gaddag *v2Gaddag_2to6() const { return m_v2gaddag_2to6; }
	
	string hashString(bool shortened) const;
	string copyrightString() const;
	const vector<string> &utf8Alphabet() const { return m_utf8Alphabet; };

protected:
	unsigned char *m_gaddags;
	
  unsigned char *m_gaddag;
	unsigned char *m_gaddag_7to7;
	unsigned char *m_gaddag_2to6;
	
  V2Gaddag* m_v2gaddag;
  V2Gaddag* m_v2gaddag_7to7;
	V2Gaddag* m_v2gaddag_2to6;
	
	//V2Gaddag* m_v2gaddag_2to7;
  //V2Gaddag* m_v2gaddag_8to8;
	
	string m_lexiconName;
	LexiconInterpreter *m_interpreter;
	char m_hash[16];
	vector<string> m_utf8Alphabet;

	LexiconInterpreter* createInterpreter(char version) const;
};

}
#endif
