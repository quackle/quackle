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

#ifndef QUACKLE_PRIMESET_H
#define QUACKLE_PRIMESET_H

#include "alphabetparameters.h"
#include "rack.h"

namespace Quackle {
  typedef uint64_t Prime;
  typedef uint64_t Product;

  class PrimesetMaker {
  public:
    // Why not just use QUACKLE_ALPHABET_PARAMETERS?
    // Well, this way it's easier to work with Primesets outside of Quackle-land
    // for building lookup tables, etc.
    PrimesetMaker(const AlphabetParameters& alphabet);

    Product multiplyTiles(const Rack& tiles) const;
    Product multiplyTiles(const LetterString& tiles) const;
    Prime lookUpTile(Letter tile) const;

    Product multiplyLetters(const LetterString& letters) const;
    Prime lookUpLetter(Letter letter) const;

  protected:
    const AlphabetParameters& m_alphabet;
    Prime m_primes[QUACKLE_MAXIMUM_ALPHABET_SIZE + 1];
  };
}

#endif
