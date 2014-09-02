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

#ifndef QUACKER_UTIL_H
#define QUACKER_UTIL_H

#include <QString>

#include <alphabetparameters.h>
#include <datamanager.h>
#include <uv.h>

namespace Quackle
{
	class Move;
	class Rack;
}

namespace QuackleIO
{

class UtilSettings
{
public:
	UtilSettings();
	static UtilSettings *self();

	bool octothorpBritish;
	bool vowelFirst;

private:
	static UtilSettings *m_self;
};

class Util
{
public:
	static UVString qstringToString(const QString &qstring);
	static Quackle::LetterString encode(const QString &qstring);

	// encode as above but clear blankess of all letters
	static Quackle::LetterString nonBlankEncode(const QString &qstring);

	static QString uvStringToQString(const UVString &stdWString);
	static QString letterStringToQString(const Quackle::LetterString &letterString);
	static QString letterToQString(const Quackle::Letter &letter);

	// non-ui strings
	static string qstringToStdString(const QString &qstring);
	static QString stdStringToQString(const string &stdString);

	static QString moveToDetailedString(const Quackle::Move &move);
	static QString moveToSensitiveString(const Quackle::Move &move);

	// make alphagram
    static Quackle::LetterString alphagram(const Quackle::LetterString &word);
    static QString alphagram(const QString &word);

	// make pattern of letters user wants based on settings
	static Quackle::LetterString arrangeLettersForUser(const Quackle::LetterString &word);
	static Quackle::LetterString arrangeLettersForUser(const Quackle::Rack &rack);
	static QString arrangeLettersForUser(const QString &word);

	// make rack, converting letters to uppercase and changing "." into blank
	static Quackle::Rack makeRack(const QString &letters);

	// Some alphabets enclose some of their letters in pipes, like |TT|, if
	// ambiguity could arise without letters being explicitly delimited.
	// This method returns a string that has no pipes and each letter separated
	// with a space.
	static QString sanitizeUserVisibleLetterString(const QString &pipedString);

	// Returns a string of symbols, like an octothorp for a british word.
	static QString symbolsFor(const Quackle::LetterString &word);
};

}

#endif
