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
#include <cassert>

#include "alphabetparameters.h"
#include "datamanager.h"

using namespace Quackle;
using namespace std;

LetterString String::left(const LetterString &letterString, int number)
{
	LetterString ret;

	int i = 0;
	LetterString::const_iterator myEnd(letterString.end());
	for (LetterString::const_iterator it = letterString.begin(); i < number && it != myEnd; ++it, ++i)
		ret += *it;

	return ret;
}

LetterString String::alphabetize(const LetterString &letterString)
{
	LetterString ret = letterString;
	sort(ret.begin(), ret.end());
	return ret;
}

LetterString String::clearBlankness(const LetterString &letterString)
{
	LetterString ret;

	LetterString::const_iterator myEnd(letterString.end());
	for (LetterString::const_iterator it = letterString.begin(); it != myEnd; ++it)
		ret += QUACKLE_ALPHABET_PARAMETERS->clearBlankness(*it);

	return ret;
}

LetterString String::setBlankness(const LetterString &letterString)
{
	LetterString ret;

	LetterString::const_iterator myEnd(letterString.end());
	for (LetterString::const_iterator it = letterString.begin(); it != myEnd; ++it)
		ret += QUACKLE_ALPHABET_PARAMETERS->setBlankness(*it);

	return ret;
}

LetterString String::usedTiles(const LetterString &letterString)
{
	LetterString used;

	const LetterString::const_iterator end(letterString.end());
	for (LetterString::const_iterator it = letterString.begin(); it != end; ++it)
	{
		if (*it == QUACKLE_BLANK_MARK || QUACKLE_ALPHABET_PARAMETERS->isBlankLetter(*it))
			used += QUACKLE_BLANK_MARK;
		else if (QUACKLE_ALPHABET_PARAMETERS->isPlainLetter(*it))
			used += *it;
	}

	return used;
}

void String::counts(const LetterString &letterString, char *countsArray)
{
	for (int j = 0; j < QUACKLE_FIRST_LETTER + QUACKLE_MAXIMUM_ALPHABET_SIZE; j++)
		countsArray[j] = 0;

	const LetterString::const_iterator end(letterString.end());
	for (LetterString::const_iterator it = letterString.begin(); it != end; ++it)
		countsArray[(int)*it]++;
}

void String::counts(const LongLetterString &letterString, char *countsArray)
{
	for (int j = 0; j < QUACKLE_FIRST_LETTER + QUACKLE_MAXIMUM_ALPHABET_SIZE; j++)
		countsArray[j] = 0;

	const LongLetterString::const_iterator end(letterString.end());
	for (LongLetterString::const_iterator it = letterString.begin(); it != end; ++it)
		countsArray[(int)*it]++;
}

////////////

AlphabetParameters::AlphabetParameters()
	: m_length(0)
{
	setAlphabet(emptyAlphabet());
}

void AlphabetParameters::setAlphabet(const Alphabet &alphabet)
{
	m_alphabet = alphabet;
	updateLength();

	const Alphabet::const_iterator alphabetEnd(m_alphabet.end());
	Alphabet::const_iterator alphabetIt;
	for (alphabetIt = m_alphabet.begin(); alphabetIt != alphabetEnd; ++alphabetIt) {
		assert(m_letterLookup.find(alphabetIt->text()) == m_letterLookup.end());
		m_letterLookup[alphabetIt->text()] = int(alphabetIt - m_alphabet.begin());
	}
	
}

void AlphabetParameters::setLetterParameter(Letter letter, const LetterParameter &letterParameter)
{
	assert(m_letterLookup.find(letterParameter.text()) == m_letterLookup.end());
	if (letter >= (int) m_alphabet.size())
	{
		m_alphabet.resize(letter + 1);
		updateLength();
	}

	m_alphabet[letter] = letterParameter;
	m_letterLookup[letterParameter.text()] = letter;
}

void AlphabetParameters::updateLength()
{
	m_length = int(m_alphabet.size() - QUACKLE_FIRST_LETTER);
}

Alphabet AlphabetParameters::emptyAlphabet()
{
	Alphabet ret(QUACKLE_FIRST_LETTER);
	ret[QUACKLE_NULL_MARK] = LetterParameter(QUACKLE_NULL_MARK, MARK_UV(" "), MARK_UV(""));
	ret[QUACKLE_BLANK_MARK] = LetterParameter(QUACKLE_BLANK_MARK, MARK_UV("?"), MARK_UV(""));
	ret[QUACKLE_PLAYED_THRU_MARK] = LetterParameter(QUACKLE_PLAYED_THRU_MARK, MARK_UV("."), MARK_UV(""));
	ret[QUACKLE_PLAYTHRU_START_MARK] = LetterParameter(QUACKLE_PLAYTHRU_START_MARK, MARK_UV("("), MARK_UV(""));
	ret[QUACKLE_PLAYTHRU_END_MARK] = LetterParameter(QUACKLE_PLAYTHRU_END_MARK, MARK_UV(")"), MARK_UV(""));
	return ret;
}

void AlphabetParameters::setCount(Letter letter, int count)
{
	m_alphabet[letter].setCount(count);
}

void AlphabetParameters::setScore(Letter letter, int score)
{
	m_alphabet[letter].setScore(score);
}

LetterString AlphabetParameters::clearBlankness(const LetterString &letterString) const
{
	LetterString ret;

	const LetterString::const_iterator end(letterString.end());
	for (LetterString::const_iterator it = letterString.begin(); it != end; ++it)
		ret += clearBlankness(*it);

	return ret;
}

UVString AlphabetParameters::userVisible(const LetterString &letterString) const
{
	UVString ret;

	const LetterString::const_iterator end(letterString.end());
	for (LetterString::const_iterator it = letterString.begin(); it != end; ++it)
		ret += userVisible(*it);

	return ret;
}

UVString AlphabetParameters::userVisible(Letter letter) const
{
	if (isBlankLetter(letter))
		return letterParameter(letter - QUACKLE_BLANK_OFFSET).blankText();
	else
		return letterParameter(letter).text();
}

LetterString AlphabetParameters::encode(const UVString &word, UVString *leftover) const
{
	LetterString ret;

	UVString leftoverQuery;
	const Alphabet::const_iterator alphabetEnd(m_alphabet.end());
	LetterLookupMap::const_iterator lookupEnd = m_letterLookup.end();
	
	const UVString::const_iterator end(word.end());
	for (UVString::const_iterator it = word.begin(); it != end; ++it)
	{
		UVString query(leftoverQuery);
		query += *it;

		bool blank = false;

		Alphabet::const_iterator alphabetIt;
		LetterLookupMap::const_iterator lookupIt = m_letterLookup.find(query);

		if (lookupIt == lookupEnd)
		{
			for (alphabetIt = m_alphabet.begin(); alphabetIt != alphabetEnd; ++alphabetIt)
				if (alphabetIt->blankText() == query)
					break;

			if (alphabetIt == alphabetEnd)
			{
				leftoverQuery = query;
				continue;
			}

			blank = true;
			ret += blank? setBlankness(alphabetIt->letter()) : alphabetIt->letter();
		} else {
			ret += blank? setBlankness(m_alphabet[lookupIt->second].letter()) : m_alphabet[lookupIt->second].letter();
		}
		leftoverQuery.clear();
	}

	//UVcout << "encoded " << word << " to " << ret << endl; 

	if (leftover)
		*leftover = leftoverQuery;

	return ret;
}

string AlphabetParameters::findAlphabetFile(const string &alphabet)
{
	return DataManager::self()->findDataFile("alphabets", alphabet + ".quackle_alphabet");
}

////////

EnglishAlphabetParameters::EnglishAlphabetParameters()
{
	m_alphabetName = "default";

	const int englishTileScores[26] =
	{
		1, // A
		3, // B
		3, // C
		2, // D
		1, // E
		4, // F
		2, // G
		4, // H
		1, // I
		8, // J
		5, // K
		1, // L
		3, // M
		1, // N
		1, // O
		3, // P
		10,// Q
		1, // R
		1, // S
		1, // T
		1, // U
		4, // V
		4, // W
		8, // X
		4, // Y
		10,// Z
	};

	const int englishTileCounts[27] =
	{
		9, // A
		2, // B
		2, // C
		4, // D
		12, // E
		2, // F
		3, // G
		2, // H
		9, // I
		1, // J
		1, // K
		4, // L
		2, // M
		6, // N
		8, // O
		2, // P
		1, // Q
		6, // R
		4, // S
		6, // T
		4, // U
		2, // V
		2, // W
		1, // X
		2, // Y
		1, // Z
		2, // ?
	};

	Letter letter = QUACKLE_FIRST_LETTER;
	for (UVChar charIndex = MARK_UV('A'); charIndex <= MARK_UV('Z'); ++charIndex, ++letter)
	{
		UVString letterString;
		UVString lowerLetterString;
		letterString += charIndex;
		lowerLetterString += (UVChar)towlower(charIndex);

		const bool isVowel = (charIndex == MARK_UV('A')) || (charIndex == MARK_UV('E')) || (charIndex == MARK_UV('I')) || (charIndex == MARK_UV('O')) || (charIndex == MARK_UV('U'));

		const int score = englishTileScores[letter - QUACKLE_FIRST_LETTER];
		const int count = englishTileCounts[letter - QUACKLE_FIRST_LETTER];
		
		setLetterParameter(letter, LetterParameter(letter, letterString, lowerLetterString, score, count, isVowel));
	}

	setCount(QUACKLE_BLANK_MARK, englishTileCounts[26]);
}

UVOStream &operator<<(UVOStream& o, const Quackle::LetterParameter &letterParameter)
{
	o << letterParameter.letter() << " [" << letterParameter.text() << ", " << letterParameter.blankText() << "]";
	return o;
}

UVOStream &operator<<(UVOStream& o, const Quackle::Alphabet &alphabet)
{
	o << "Alphabet (size " << alphabet.size() << "): ";
	Alphabet::const_iterator end(alphabet.end());
	for (Alphabet::const_iterator it = alphabet.begin(); it != end; ++it)
		o << (*it) << " ";
	return o;
}

