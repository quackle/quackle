/*
 *  Quackle -- Crossword game artificial intelligence and analysis tool
 *  Copyright (C) 2005-2016 Jason Katz-Brown and John O'Laughlin.
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

#include "anagrammap.h"
#include "datamanager.h"
#include "primeset.h"

using namespace Quackle;

void AnagramMap::initialize(const string& lexicon) {
  loadAnagrams(DataManager::self()->
	       findDataFile("strategy", lexicon, "anagrammap"));
}

namespace {
  void readNTileAnagrams(ifstream* file, NTileAnagrams* anagrams) {
    file->read(reinterpret_cast<char*>(&(anagrams->numPlayed)),
	       sizeof(anagrams->numPlayed));
    for (int i = 0; i < 6; ++i) {
      int length = i + 1;
      int mask = 1 << length;
      if ((anagrams->numPlayed & mask) != 0) {
	file->read(reinterpret_cast<char*>(&(anagrams->bestLeaves[i])),
		   sizeof(anagrams->bestLeaves[i]));
	float floatLeave =  anagrams->bestLeaves[i] / 256.0;
	if ((floatLeave > 100.0 || floatLeave < -100)) {
	  UVcout << "weird leave: " << floatLeave << endl;
	}
      } else {
	anagrams->bestLeaves[i] = 0;
      }
    }
  }
  
  void readUsesTiles(ifstream* file, UsesTiles* usesTiles) {
    readNTileAnagrams(file, &(usesTiles->thruNone));
    file->read(reinterpret_cast<char*>(&(usesTiles->anahooks)),
	       sizeof(usesTiles->anahooks));
    const int numAnahooks = __builtin_popcount(usesTiles->anahooks);
    for (int i = 0; i < numAnahooks; ++i) {
      NTileAnagrams thruAnahook;
      readNTileAnagrams(file, &thruAnahook);
      usesTiles->thruOne.push_back(thruAnahook);
    }
  }
  
}  // namespace

void AnagramMap::loadAnagrams(const string& filename) {
  m_map.clear();
  UVcout << "loading anagrammap file: " << filename << endl;
  ifstream file(filename.c_str(), ios::in | ios::binary);

  if (!file.is_open()) {
    cerr << "Could not open " << filename << " to load primeset-keyed "
	 << "rack anagram info" << endl;
    return;
  }
  uint64_t blankPrime = QUACKLE_PRIMESET->lookUpTile(QUACKLE_BLANK_MARK);
  uint64_t product;
  UVcout << "Loading anagrammap..." << endl;
  while (!file.eof()) {
    file.read(reinterpret_cast<char*>(&product), sizeof(product));
    //UVcout << "product: " << product << endl;
    RackAnagrams anagrams;
    readUsesTiles(&file, &anagrams.usesWhatever);
    if (product % blankPrime == 0) {
      //UVcout << product << " has a blank! read usesNoBlanks!" << endl;
      readUsesTiles(&file, &anagrams.usesNoBlanks);
    }
    m_map[product] = anagrams;
    //return;
  }
  UVcout << "found " << m_map.size() << " key-value pairs"  << endl;
}

const RackAnagrams* AnagramMap::lookUp(const Rack& rack) {
  Product product = QUACKLE_PRIMESET->multiplyTiles(rack);
  const auto it = m_map.find(product);
  if (it == m_map.end()) return NULL;
  return &(it->second);
}
