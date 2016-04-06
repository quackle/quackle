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

#include "alphabetparameters.h"
#include "primeset.h"
#include "rack.h"

using namespace Quackle;

namespace {
// Not fast, doesn't need to be 
bool isPrime(uint64_t n) {
	for (uint64_t d = 2; d * d <= n; ++d) {
		if (n % d == 0) return false;
	}
	return true;
}

uint64_t nextPrime(uint64_t n) {
	if (isPrime(n)) return n;
	return nextPrime(n + 1);
}

void fillPrimes(Quackle::Letter firstLetter, Quackle::Letter lastLetter,
								uint64_t* primes) {
	uint64_t n = 2;
	uint64_t prime;
	for (int i = firstLetter; i <= lastLetter; ++i) {
		prime = nextPrime(n);
		UVcout << "primes[" << static_cast<int>(i) << "]: " << prime << endl;
		primes[i] = prime;
		n = prime + 1;
	}
	prime = nextPrime(n);
	UVcout << "primes[" << static_cast<int>(QUACKLE_BLANK_MARK) << "]: "
				 << prime << endl;
	primes[QUACKLE_BLANK_MARK] = prime;
}
}  // namespace

PrimesetMaker::PrimesetMaker(const AlphabetParameters& alphabet)
	: m_alphabet(alphabet) {
  for (int i = 0; i <= QUACKLE_MAXIMUM_ALPHABET_SIZE; ++i) {
    m_primes[i] = 0;
  }
  fillPrimes(alphabet.firstLetter(), alphabet.lastLetter(), m_primes);
}

// Assumes no blanked letters are input.
// LA?IEST -> L * A * ? * I * E * S * T
Product PrimesetMaker::multiplyTiles(const LetterString& tiles) const {
	Product product = 1;
	for (unsigned int i = 0; i < tiles.size(); ++i) {
		product *= lookUpTile(tiles[i]);
	}
	return product;
}

Product PrimesetMaker::lookUpTile(Letter tile) const {
	return m_primes[tile];
}

Product PrimesetMaker::multiplyTiles(const Rack& tiles) const {
	return multiplyTiles(tiles.tiles());
}

// Blanks given as input are meant to be already on the board, designated,
// and thus fixed.
// LAzIEST -> L * A * Z * I * E * S * T
Product PrimesetMaker::multiplyLetters(const LetterString& letters) const {
	Product product = 1;
	for (unsigned int i = 0; i < letters.size(); ++i) {
		product *= lookUpLetter(letters[i]);
	}
	return product;
}

Product PrimesetMaker::lookUpLetter(Letter letter) const {
	Letter tile = m_alphabet.clearBlankness(letter);
	return m_primes[tile];
}

