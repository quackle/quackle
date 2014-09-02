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

#ifndef QUACKLE_ALPHABETPARAMETERS_H
#define QUACKLE_ALPHABETPARAMETERS_H

#include <vector>
#include <map>

#include "fixedstring.h"
#include "uv.h"

using namespace std;

#define QUACKLE_MAXIMUM_ALPHABET_SIZE 55
#define QUACKLE_MINIMUM_ALPHABET_SIZE 1

// Quackle's alphabet scheme includes a way to represent blanks
// in two ways: either as the blank letter or as the letter plus
// the blank offset

// we officially define null but others are only euglenids+i...
#define QUACKLE_NULL_MARK_TEXT MARK_UV(" ")
#define QUACKLE_NULL_MARK 0

#define QUACKLE_BLANK_MARK 1 // let this be '?'
#define QUACKLE_PLAYED_THRU_MARK 2 // let this be '.'
#define QUACKLE_PLAYTHRU_START_MARK 3 // let this be '('
#define QUACKLE_PLAYTHRU_END_MARK 4 // let this be ')'
#define QUACKLE_FIRST_LETTER 5

#define QUACKLE_BLANK_OFFSET QUACKLE_MAXIMUM_ALPHABET_SIZE

namespace Quackle
{

// Letter & LetterString are internally encoded strings mapped
// from UVString (UV == user visible) to 1-n indices.  The are intended 
// to be language independent whereas UVStrings are language dependent.
#define LETTER_STRING_MAXIMUM_LENGTH FIXED_STRING_MAXIMUM_LENGTH
typedef unsigned char Letter;
typedef FixedLengthString LetterString;
typedef std::string LongLetterString;

typedef std::vector<LetterString> WordList;

namespace String
{

LetterString left(const LetterString &letterString, int number);
LetterString alphabetize(const LetterString &letterString);

LetterString clearBlankness(const LetterString &letterString);
LetterString setBlankness(const LetterString &letterString);

// turns string like .ANELINn into .ANELIN?
LetterString usedTiles(const LetterString &letterString);

// allocate a countsArray of size
// QUACKLE_FIRST_LETTER + QUACKLE_MAXIMUM_ALPHABET_SIZE
void counts(const LetterString &letterString, char *countsArray);
void counts(const LongLetterString &letterString, char *countsArray);

inline Letter back(const LetterString &letterString)
{
	return *(letterString.end() - 1);
}

inline void pop_back(LetterString &letterString)
{
	letterString.erase(letterString.end() - 1);
}

inline Letter front(const LetterString &letterString)
{
	return *letterString.begin();
}

inline void pop_front(LetterString &letterString)
{
	letterString.erase(letterString.begin());
}

inline LetterString allButFront(const LetterString &letterString)
{
	return letterString.substr(1, letterString.length() - 1);
}

}

class LetterParameter
{
public:
	LetterParameter();
	LetterParameter(Letter letter);
	LetterParameter(Letter letter, const UVString &text, const UVString &blankText, int score = 0, int count = 0, bool isVowel = false);

	UVString text() const;
	void setText(const UVString &text);

	UVString blankText() const;
	void setBlankText(const UVString &blankText);

	Letter letter() const;
	void setLetter(Letter letter);

	int score() const;
	void setScore(int score);

	// number of doodads in the bag
	int count() const;
	void setCount(int count);

	bool isVowel() const;
	void setVowel(bool isVowel);

private:
	Letter m_letter;
	UVString m_text;
	UVString m_blankText;
	int m_score;
	int m_count;
	bool m_isVowel;
};

inline LetterParameter::LetterParameter()
	: m_letter(QUACKLE_NULL_MARK), m_score(0), m_count(0), m_isVowel(false)
{
}

inline LetterParameter::LetterParameter(Letter letter)
	: m_letter(letter), m_score(0), m_count(0), m_isVowel(false)
{
}

inline LetterParameter::LetterParameter(Letter letter, const UVString &text, const UVString &blankText, int score, int count, bool isVowel)
	: m_letter(letter), m_text(text), m_blankText(blankText), m_score(score), m_count(count), m_isVowel(isVowel)
{
}

inline UVString LetterParameter::text() const
{
	return m_text;
}

inline void LetterParameter::setText(const UVString &text)
{
	m_text = text;
}

inline UVString LetterParameter::blankText() const
{
	return m_blankText;
}

inline void LetterParameter::setBlankText(const UVString &blankText)
{
	m_blankText = blankText;
}

inline Letter LetterParameter::letter() const
{
	return m_letter;
}

inline void LetterParameter::setLetter(Letter letter)
{
	m_letter = letter;
}

inline int LetterParameter::score() const
{
	return m_score;
}

inline void LetterParameter::setScore(int score)
{
	m_score = score;
}

inline int LetterParameter::count() const
{
	return m_count;
}

inline void LetterParameter::setCount(int count)
{
	m_count = count;
}

inline bool LetterParameter::isVowel() const
{
	return m_isVowel;
}

inline void LetterParameter::setVowel(bool isVowel)
{
	m_isVowel = isVowel;
}

typedef vector<LetterParameter> Alphabet;

class AlphabetParameters
{
public:
	AlphabetParameters();

	// Returns how many letters there are (excludes blanks and null letter).
	// Thus this return value is how many letters there are inclusively between
	// QUACKLE_FIRST_LETTER and lastLetter()
	int length() const;

	// first letter in alphabet -- same as the #define
	Letter firstLetter() const;

	// the last letter in the alphabet -- all letters after this are blanks
	Letter lastLetter() const;

	void setAlphabet(const Alphabet &alphabet);

	const LetterParameter& letterParameter(Letter letter) const;
	void setLetterParameter(Letter letter, const LetterParameter &letterParameter);

	// get an alphabet with blank and null values set; note
	// this empty alphabet specifies no blanks in the bag
	static Alphabet emptyAlphabet();

	// useful for setting number of blanks
	void setCount(Letter letter, int count);

	// useful for setting score of blanks
	void setScore(Letter letter, int score);

	// whether letter is between firstLetter + blank_offset
	// and lastLetter + blank_offset
	bool isBlankLetter(Letter letter) const;

	// returns letter if it's not a blank, or a plain
	// (nonblank) version of letter otherwise
	Letter clearBlankness(Letter letter) const;
	LetterString clearBlankness(const LetterString &letterString) const;

	// returns letter if it's a blank, or a blank
	// version of letter otherwise
	Letter setBlankness(Letter letter) const;

	// whether letter is between first and last letter
	bool isPlainLetter(Letter letter) const;

	// whether letter is a blank or plain letter or not
	bool isSomeLetter(Letter letter) const;

	int count(Letter letter) const;
	int score(Letter letter) const;
	bool isVowel(Letter letter) const;

	// alphabet-based conversion facilities:
	// LetterString -> UVString
	UVString userVisible(const LetterString &letterString) const;
	UVString userVisible(Letter letter) const;

	// UVString -> LetterString. Letters that could not be encoded are 
	// stored in leftover if it is non-null.
	LetterString encode(const UVString &word, UVString *leftover = 0) const;

	// a convenience field; this is unused by libquackle
	string alphabetName() const;
	void setAlphabetName(const string &name);

	// finds a file in the alphabets data directory
	static string findAlphabetFile(const string &alphabet);

protected:
	void updateLength();

	int m_length;
	Alphabet m_alphabet;
	typedef map<UVString, int> LetterLookupMap;
	LetterLookupMap m_letterLookup;

	string m_alphabetName;
};

inline int AlphabetParameters::length() const
{
	return m_length;
}

inline Letter AlphabetParameters::firstLetter() const
{
	return QUACKLE_FIRST_LETTER;
}

inline Letter AlphabetParameters::lastLetter() const
{
	return QUACKLE_FIRST_LETTER + m_length - 1;
}

inline const LetterParameter& AlphabetParameters::letterParameter(Letter letter) const
{
        assert(letter < m_alphabet.size());
	return m_alphabet[letter];
}

inline bool AlphabetParameters::isBlankLetter(Letter letter) const
{
	return letter > lastLetter();
}

inline Letter AlphabetParameters::clearBlankness(Letter letter) const
{
	if (isBlankLetter(letter))
		return letter - QUACKLE_BLANK_OFFSET;
	else
		return letter;
}

inline Letter AlphabetParameters::setBlankness(Letter letter) const
{
	if (!isBlankLetter(letter))
		return letter + QUACKLE_BLANK_OFFSET;
	else
		return letter;
}

inline bool AlphabetParameters::isPlainLetter(Letter letter) const
{
	return letter >= firstLetter() && letter <= lastLetter();
}

inline bool AlphabetParameters::isSomeLetter(Letter letter) const
{
	return isBlankLetter(letter) || isPlainLetter(letter);
}

inline int AlphabetParameters::count(Letter letter) const
{
	return letterParameter(letter).count();
}

inline int AlphabetParameters::score(Letter letter) const
{
	return letterParameter(letter).score();
}

inline bool AlphabetParameters::isVowel(Letter letter) const
{
	return letterParameter(letter).isVowel();
}

inline string AlphabetParameters::alphabetName() const
{
	return m_alphabetName;
}

inline void AlphabetParameters::setAlphabetName(const string &name)
{
	m_alphabetName = name;
}

class EnglishAlphabetParameters : public AlphabetParameters
{
public:
	EnglishAlphabetParameters();
};

}

#ifdef QUACKLE_USE_OWN_LETTERSTRING

inline Quackle::LetterString operator+(const Quackle::LetterString &letterString1, const Quackle::LetterString &letterString2)
{
	Quackle::LetterString ret(letterString1);
	return ret += letterString2;
}

inline Quackle::LetterString operator+(const Quackle::LetterString &letterString, Quackle::Letter letter)
{
	Quackle::LetterString ret;
	ret += letterString;
	return ret += letter;
}

inline Quackle::LetterString operator+(Quackle::Letter letter, const Quackle::LetterString &letterString)
{
	Quackle::LetterString ret(letter);
	return ret += letterString;
}

#endif // QUACKLE_USE_OWN_LETTERSTRING

UVOStream &operator<<(UVOStream& o, const Quackle::LetterParameter &letterParameter);
UVOStream &operator<<(UVOStream& o, const Quackle::Alphabet &alphabet);

#endif
