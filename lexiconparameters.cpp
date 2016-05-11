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

class Quackle::V2LexiconInterpreter : public LexiconInterpreter {
	virtual int versionNumber() const { return 2; }
	
	virtual void loadGaddag(ifstream &file, unsigned char* gaddag,
													V2Gaddag** v2gaddag) {
		UVcout << "V2 loadGaddag..." << endl;
		char hash[16];
		file.get(); // skip past version byte
		file.read(hash, sizeof(hash));  // skip past hash

		char lastLetter = file.get();   // skip past gaddag format parameters
		char bitsetSize = file.get();
		char indexSize = file.get();

		size_t i = 0;
		while (!file.eof()) {
			file.read((char*)(gaddag) + i, 1);
			i++;
		}
		UVcout << "read " << (i - 1) << " bytes into gaddag." << endl;
		*v2gaddag = new V2Gaddag(gaddag, lastLetter, bitsetSize, indexSize);
	}
};

LexiconParameters::LexiconParameters() : m_gaddags(NULL), m_interpreter(NULL) {
	memset(m_hash, 0, sizeof(m_hash));
}

LexiconParameters::~LexiconParameters() {
	unloadAll();
}

void LexiconParameters::unloadAll() {
	unloadGaddags();
}

void LexiconParameters::unloadGaddags() {
	delete[] m_gaddags;
	m_gaddags = NULL;
}

void LexiconParameters::loadGaddags(const string &filename) {
	unloadGaddags();

	vector<string> filenames;
	filenames.push_back(filename + "-7to7");
	filenames.push_back(filename + "-2to6");
	filenames.push_back(filename);

	vector<int> offsets;
	int totalGaddagsBytes = 0;
	char versionByte;
	for (const string& filename : filenames) {
		ifstream file(filename.c_str(), ios::in | ios::binary);
		if (!file.is_open()) {
			UVcout << "couldn't open gaddag " << filename << endl;
			return;
		}
		if (m_interpreter == NULL) {
			m_interpreter = createInterpreter(versionByte);
			versionByte = file.get();
			UVcout << "versionByte: " << (int)versionByte << endl;
		} else {
			char thisVersionByte = file.get();
			UVcout << "thisVersionByte: " << (int)thisVersionByte << endl;
			if (thisVersionByte != versionByte) {
				UVcout << "incompatible version byte "
							 << thisVersionByte << " != " << versionByte << endl;
				return;
			}
		}
		offsets.push_back(totalGaddagsBytes);
		file.seekg(0, ios_base::end);
    totalGaddagsBytes += file.tellg();
		file.seekg(0, ios_base::beg);
	}
	
	m_gaddags = new unsigned char[totalGaddagsBytes];
  UVcout << "totalGaddagsBytes: " << totalGaddagsBytes << endl;
	UVcout << "at m_gaddags: " << reinterpret_cast<unsigned long>(m_gaddags) << endl;


	m_gaddag_7to7 = m_gaddags + offsets[0];
  m_gaddag_2to6 = m_gaddags + offsets[1];
	m_gaddag = m_gaddags + offsets[2];
	
	for (unsigned int i = 0; i < filenames.size(); ++i) {
		ifstream file(filenames[i].c_str(), ios::in | ios::binary);
    if (i == 2) {		
			m_interpreter->loadGaddag(file, m_gaddag, &m_v2gaddag);
		} else if (i == 0) {
			m_interpreter->loadGaddag(file, m_gaddag_7to7, &m_v2gaddag_7to7);
		} else if (i == 1) {
			m_interpreter->loadGaddag(file, m_gaddag_2to6, &m_v2gaddag_2to6);
		}
	}
}

string LexiconParameters::findDictionaryFile(const string &lexicon) {
	return QUACKLE_DATAMANAGER->findDataFile("lexica", lexicon);
}

bool LexiconParameters::hasUserDictionaryFile(const string &lexicon) {
	return QUACKLE_DATAMANAGER->hasUserDataFile("lexica", lexicon);
}

string LexiconParameters::hashString(bool shortened) const {
	const char hex[] = {'0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
											'a', 'b', 'c', 'd', 'e', 'f' };
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

string LexiconParameters::copyrightString() const
{
	string copyrightsFilename =
		QUACKLE_DATAMANAGER->makeDataFilename("lexica", "copyrights.txt", false);
	fstream copyrightsFile(copyrightsFilename.c_str(), ios_base::in);
	while (copyrightsFile.good() && !copyrightsFile.eof()) {
		string line;
		getline(copyrightsFile, line);
		if (line.size() < 9 || line.find_first_of(':') != 8)
			continue;
		if (hashString(true).compare(line.substr(0,8)) != 0)
			continue;
		return line.substr(9, line.size());
	}
	return string();
}

LexiconInterpreter* LexiconParameters::createInterpreter(char version) const {
	UVcout << "createInterpreter..." << endl;
	switch(version)
	{
	case 2:
		return new V2LexiconInterpreter();
	default:
		UVcout << "Unknown LexiconInterpreter version: "
					 << static_cast<int>(version) << endl;
		return NULL;
				
	}
}
