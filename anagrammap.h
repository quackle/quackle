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

#ifndef QUACKLE_ANAGRAMMAP_H
#define QUACKLE_ANAGRAMMAP_H

#include <map>
//#include <unordered_map>

#include "primeset.h"
#include "rack.h"

namespace Quackle {
  struct NTileAnagrams {
    uint8_t numPlayed;
    // bestLeaves[0..5] map to {1, 2, 3, 4, 5, 6}
    int16_t bestLeaves[6]; // static_cast<int>(leave*256)
  };

  struct UsesTiles {
    NTileAnagrams thruNone;
    uint32_t anahooks;
    vector<NTileAnagrams> thruOne;
  };

  struct RackAnagrams {
    UsesTiles usesNoBlanks;
    UsesTiles mustUseBlank;
  };

  class AnagramMap {
  public:
    void initialize(const string& lexicon);
    const RackAnagrams* lookUp(const Rack& rack);
  private:
    void loadAnagrams(const string& filename);
    map<Product, RackAnagrams> m_map;
    //unordered_map<Product, RackAnagrams> m_map;
  };
}

#endif
