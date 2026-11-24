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
#include <fstream>

#include "datamanager.h"
#include "lexiconparameters.h"
#include "uv.h"

using namespace Quackle;
using namespace std;

namespace
{

const size_t dawgNodeSize = 7;
const size_t gaddagNodeSize = 4;

// A dawg's child pointers are absolute, and the field is 24 bits wide.
const size_t maximumDawgNodeCount = 0xFFFFFF;

const unsigned char gaddagLastSiblingBit = 0x80;

}

class Quackle::V0LexiconInterpreter : public LexiconInterpreter
{

	virtual const char *loadDawg(ifstream &file, LexiconParameters &lexparams) { return lexparams.readDawgNodes(file); }

	virtual const char *loadGaddag(ifstream &file, LexiconParameters &lexparams) { return lexparams.readGaddagNodes(file); }

	virtual void dawgAt(const unsigned char *dawg, int index, unsigned int &p, Letter &letter, bool &t, bool &lastchild, bool &british,
		int &playability) const
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

class Quackle::V1LexiconInterpreter : public LexiconInterpreter
{

	virtual const char *loadDawg(ifstream &file, LexiconParameters &lexparams)
	{
		unsigned char bytes[3];
		file.get(); // skip past version byte
		file.read(lexparams.m_hash, sizeof(lexparams.m_hash));
		file.read((char *)bytes, 3);

		const int alphabetSize = file.get();
		if (!file.good())
			return "header is truncated";
		if (alphabetSize < QUACKLE_MINIMUM_ALPHABET_SIZE || alphabetSize > QUACKLE_MAXIMUM_ALPHABET_SIZE)
			return "header claims an impossible alphabet size";

		lexparams.m_utf8Alphabet.resize(alphabetSize);
		for (size_t i = 0; i < lexparams.m_utf8Alphabet.size(); i++)
		{
			file >> lexparams.m_utf8Alphabet[i];
			file.get(); // separator space
		}
		if (!file.good())
			return "alphabet in the header is truncated";

		return lexparams.readDawgNodes(file);
	}

	virtual const char *loadGaddag(ifstream &file, LexiconParameters &lexparams)
	{
		char hash[16];
		file.get(); // skip past version byte
		file.read(hash, sizeof(hash));
		if (!file.good())
			return "header is truncated";

		if (memcmp(hash, lexparams.m_hash, sizeof(hash)))
		{
			// If we're using a v0 DAWG, then ignore the hash
			for (size_t i = 0; i < sizeof(lexparams.m_hash); i++)
			{
				if (lexparams.m_hash[i] != 0)
					return "was built from a different lexicon than the dawg";
			}
		}

		return lexparams.readGaddagNodes(file);
	}

	virtual void dawgAt(const unsigned char *dawg, int index, unsigned int &p, Letter &letter, bool &t, bool &lastchild, bool &british,
		int &playability) const
	{
		index *= 7;
		p = (dawg[index] << 16) + (dawg[index + 1] << 8) + (dawg[index + 2]);
		letter = dawg[index + 3];

		lastchild = ((letter & 64) != 0);
		british = !(letter & 128);
		letter = (letter & 63) + QUACKLE_FIRST_LETTER;

		playability = (dawg[index + 4] << 16) + (dawg[index + 5] << 8) + (dawg[index + 6]);
		t = (playability != 0);
	}
	virtual int versionNumber() const { return 1; }
};

LexiconParameters::LexiconParameters()
	: m_dawg(nullptr), m_gaddag(nullptr), m_dawgNodeCount(0), m_gaddagNodeCount(0), m_interpreter(nullptr)
{
	memset(m_hash, 0, sizeof(m_hash));
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
	m_dawg = nullptr;
	m_dawgNodeCount = 0;
	delete m_interpreter;
	m_interpreter = nullptr;
	m_utf8Alphabet.clear();
	memset(m_hash, 0, sizeof(m_hash));
}

void LexiconParameters::unloadGaddag()
{
	delete[] m_gaddag;
	m_gaddag = nullptr;
	m_gaddagNodeCount = 0;
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

	const int versionByte = file.get();
	if (!file.good())
	{
		UVcerr << "refusing dawg " << filename.c_str() << ": the file is empty" << std::endl;
		return;
	}

	m_interpreter = createInterpreter((char)versionByte);
	if (m_interpreter == nullptr)
	{
		UVcerr << "refusing dawg " << filename.c_str() << ": unknown format version" << std::endl;
		return;
	}

	file.seekg(0, ios_base::beg);

	const char *trouble = m_interpreter->loadDawg(file, *this);
	if (trouble == nullptr)
		trouble = validateDawg();

	if (trouble != nullptr)
	{
		UVcerr << "refusing dawg " << filename.c_str() << ": " << trouble << std::endl;
		unloadDawg();
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

	if (!hasDawg())
	{
		UVcerr << "refusing gaddag " << filename.c_str() << ": no dawg is loaded to check it against" << std::endl;
		return;
	}

	const int versionByte = file.get();
	if (!file.good())
	{
		UVcerr << "refusing gaddag " << filename.c_str() << ": the file is empty" << std::endl;
		return;
	}
	if (versionByte < m_interpreter->versionNumber())
		return;
	file.seekg(0, ios_base::beg);

	// must create a local interpreter because dawg/gaddag versions might not match
	LexiconInterpreter *interpreter = createInterpreter((char)versionByte);
	if (interpreter == nullptr)
	{
		UVcerr << "refusing gaddag " << filename.c_str() << ": unknown format version" << std::endl;
		return;
	}

	const char *trouble = interpreter->loadGaddag(file, *this);
	delete interpreter;
	if (trouble == nullptr)
		trouble = validateGaddag();

	if (trouble != nullptr)
	{
		UVcerr << "refusing gaddag " << filename.c_str() << ": " << trouble << std::endl;
		unloadGaddag();
	}
}

static const char *readNodes(ifstream &file, size_t nodeSize, unsigned char *&nodes, size_t &nodeCount)
{
	if (!file.good())
		return "cannot be read";

	const streamoff header = file.tellg();
	file.seekg(0, ios_base::end);
	const streamoff size = file.tellg();
	if (header < 0 || size < header)
		return "cannot be measured";

	const streamoff body = size - header;
	if (body == 0)
		return "holds no nodes";
	if (body % (streamoff)nodeSize != 0)
		return "does not end on a node boundary";

	const streamoff count = body / (streamoff)nodeSize;
	// Index 0 is the root sentinel the traversals skip; index 1 is where they start.
	if (count < 2)
		return "holds no nodes to walk";

	const size_t bytes = (size_t)count * nodeSize;
	file.seekg(header, ios_base::beg);
	nodes = new unsigned char[bytes];
	file.read((char *)nodes, (streamsize)bytes);
	if (file.gcount() != (streamsize)bytes)
		return "is truncated";

	nodeCount = (size_t)count;
	return nullptr;
}

const char *LexiconParameters::readDawgNodes(ifstream &file)
{
	return readNodes(file, dawgNodeSize, m_dawg, m_dawgNodeCount);
}

const char *LexiconParameters::readGaddagNodes(ifstream &file)
{
	return readNodes(file, gaddagNodeSize, m_gaddag, m_gaddagNodeCount);
}

const char *LexiconParameters::validateDawg() const
{
	if (m_dawgNodeCount > maximumDawgNodeCount)
		return "has more nodes than a pointer field can reach";

	for (size_t i = 0; i < m_dawgNodeCount; ++i)
	{
		unsigned int p;
		Letter letter;
		bool t;
		bool lastchild;
		bool british;
		int playability;
		m_interpreter->dawgAt(m_dawg, (int)i, p, letter, t, lastchild, british, playability);

		if (p >= m_dawgNodeCount)
			return "has a node pointing past its last node";

		// The root sentinel's letter byte is the blank mark biased below the first
		// letter, so it decodes out of range; no traversal reads it.
		if (i > 0 && letter >= QUACKLE_FIRST_LETTER + QUACKLE_MAXIMUM_ALPHABET_SIZE)
			return "has a node whose letter is outside the alphabet";

		if (i + 1 == m_dawgNodeCount && !lastchild)
			return "ends in the middle of a sibling list";
	}

	// Shared suffixes point plenty of these pointers backwards, so acyclicity needs a walk.
	if (!dawgIsAcyclic())
		return "has a cycle";

	return nullptr;
}

bool LexiconParameters::dawgIsAcyclic() const
{
	vector<unsigned char> state(m_dawgNodeCount, 0); // 0 unvisited, 1 on the path, 2 finished
	vector<size_t> stack;
	stack.push_back(1 << 1);

	while (!stack.empty())
	{
		const size_t entry = stack.back();
		stack.pop_back();

		const size_t i = entry >> 1;
		if (entry & 1)
		{
			state[i] = 2;
			continue;
		}
		if (state[i] == 2)
			continue;
		if (state[i] == 1)
			return false;
		state[i] = 1;
		stack.push_back((i << 1) | 1);

		unsigned int p;
		Letter letter;
		bool t;
		bool lastchild;
		bool british;
		int playability;
		m_interpreter->dawgAt(m_dawg, (int)i, p, letter, t, lastchild, british, playability);

		if (p != 0)
			stack.push_back((size_t)p << 1);
		if (!lastchild && i + 1 < m_dawgNodeCount)
			stack.push_back((i + 1) << 1);
	}

	return true;
}

const char *LexiconParameters::validateGaddag() const
{
	// Children are reached by a positive offset and siblings by one node, so
	// bounding every offset also proves the graph acyclic.
	for (size_t i = 0; i < m_gaddagNodeCount; ++i)
	{
		const unsigned char *node = m_gaddag + i * gaddagNodeSize;
		const size_t child = (node[0] << 16) + (node[1] << 8) + node[2];
		if (child != 0 && i + child >= m_gaddagNodeCount)
			return "has a node pointing past its last node";

		const Letter letter = node[3] & 0x3F;
		if (letter != QUACKLE_GADDAG_SEPARATOR
			&& (letter < QUACKLE_FIRST_LETTER || letter >= QUACKLE_FIRST_LETTER + QUACKLE_MAXIMUM_ALPHABET_SIZE))
			return "has a node whose letter is outside the alphabet";
	}

	if ((m_gaddag[(m_gaddagNodeCount - 1) * gaddagNodeSize + 3] & gaddagLastSiblingBit) == 0)
		return "ends in the middle of a sibling list";

	return nullptr;
}

string LexiconParameters::findDictionaryFile(const string &lexicon)
{
	return QUACKLE_DATAMANAGER->findDataFile("lexica", lexicon);
}

bool LexiconParameters::hasUserDictionaryFile(const string &lexicon)
{
	return QUACKLE_DATAMANAGER->hasUserDataFile("lexica", lexicon);
}

string LexiconParameters::hashString(bool shortened) const
{
	const char hex[] = { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f' };
	string hashStr;
	for (size_t i = 0; i < sizeof(m_hash); i++)
	{
		hashStr.push_back(hex[(m_hash[i] & 0xF0) >> 4]);
		hashStr.push_back(hex[m_hash[i] & 0x0F]);
		if (shortened && i == 3)
			break;
	}
	return hashStr;
}

string LexiconParameters::getLexiconCopyrightLine() const
{
	string copyrightsFilename = QUACKLE_DATAMANAGER->makeDataFilename("lexica", "copyrights.txt", false);
	fstream copyrightsFile(copyrightsFilename, ios_base::in);
	while (copyrightsFile.good() && !copyrightsFile.eof())
	{
		string line;
		getline(copyrightsFile, line);
		if (line.size() < 9 || line.find_first_of(':') != 8)
			continue;
		if (hashString(true).compare(line.substr(0, 8)) != 0)
			continue;
		return line.substr(9);
	}
	return string();
}
string LexiconParameters::copyrightString() const
{
	string copyrightLine = getLexiconCopyrightLine();
	size_t colonPos = min(copyrightLine.size(), copyrightLine.find_last_of(':'));
	return copyrightLine.substr(0, colonPos);
}

string LexiconParameters::logoFileName() const
{
	string copyrightLine = getLexiconCopyrightLine();
	size_t colonPos = copyrightLine.find_last_of(':');
	if (colonPos == string::npos)
		return string();
	return QUACKLE_DATAMANAGER->makeDataFilename("lexica", copyrightLine.substr(colonPos + 1), false);
}

LexiconInterpreter *LexiconParameters::createInterpreter(char version) const
{
	switch (version)
	{
	case 0:
		return new V0LexiconInterpreter();
	case 1:
		return new V1LexiconInterpreter();
	default:
		return nullptr;
	}
}
