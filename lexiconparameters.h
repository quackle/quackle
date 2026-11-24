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

#ifndef QUACKLE_LEXICONPARAMETERS_H
#define QUACKLE_LEXICONPARAMETERS_H

#include <vector>

#include "gaddag.h"

using std::vector;

namespace Quackle
{

class LexiconParameters;

class LexiconInterpreter
{
public:
	// the loaders return nullptr on success, or a diagnostic naming what is wrong with the file
	virtual const char *loadDawg(std::ifstream &file, LexiconParameters &lexparams) = 0;
	virtual const char *loadGaddag(std::ifstream &file, LexiconParameters &lexparams) = 0;
	virtual void dawgAt(const unsigned char *dawg, int index, unsigned int &p, Letter &letter, bool &t, bool &lastchild, bool &british,
		int &playability) const = 0;
	virtual int versionNumber() const = 0;
	virtual ~LexiconInterpreter() {};
};

class V0LexiconInterpreter;
class V1LexiconInterpreter;

class LexiconParameters
{
	friend class Quackle::V0LexiconInterpreter;
	friend class Quackle::V1LexiconInterpreter;

public:
	LexiconParameters();
	~LexiconParameters();

	void unloadAll();

	// true if we have a dawg or a gaddag
	bool hasSomething() const { return hasDawg() || hasGaddag(); };

	// loadDawg unloads the dawg if filename can't be opened or doesn't validate
	void loadDawg(const string &filename);
	void unloadDawg();
	bool hasDawg() const { return m_dawg != nullptr; };
	int dawgVersion() const { return m_interpreter->versionNumber(); };
	size_t dawgNodeCount() const { return m_dawgNodeCount; };

	// loadGaddag unloads the gaddag if filename can't be opened or doesn't validate
	void loadGaddag(const string &filename);
	void unloadGaddag();
	bool hasGaddag() const { return m_gaddag != nullptr; };
	size_t gaddagNodeCount() const { return m_gaddagNodeCount; };

	// finds a file in the lexica data directory
	static string findDictionaryFile(const string &lexicon);
	static bool hasUserDictionaryFile(const string &lexicon);

	// a convenience field; this is unused by libquackle
	string lexiconName() const { return m_lexiconName; };
	void setLexiconName(const string &name) { m_lexiconName = name; };

	void dawgAt(int index, unsigned int &p, Letter &letter, bool &t, bool &lastchild, bool &british, int &playability) const
	{
		m_interpreter->dawgAt(m_dawg, index, p, letter, t, lastchild, british, playability);
	}
	const GaddagNode *gaddagRoot() const { return (GaddagNode *)&m_gaddag[0]; };

	string hashString(bool shortened) const;
	string copyrightString() const;
	string logoFileName() const;
	const vector<string> &utf8Alphabet() const { return m_utf8Alphabet; };

protected:
	unsigned char *m_dawg;
	unsigned char *m_gaddag;
	size_t m_dawgNodeCount;
	size_t m_gaddagNodeCount;
	string m_lexiconName;
	LexiconInterpreter *m_interpreter;
	char m_hash[16];
	vector<string> m_utf8Alphabet;

	LexiconInterpreter *createInterpreter(char version) const;

private:
	string getLexiconCopyrightLine() const;

	// read the nodes from the stream's current position to the end of the file
	const char *readDawgNodes(std::ifstream &file);
	const char *readGaddagNodes(std::ifstream &file);

	const char *validateDawg() const;
	const char *validateGaddag() const;
	bool dawgIsAcyclic() const;
};

}
#endif
