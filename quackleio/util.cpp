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

#include <QtCore>

#include <move.h>
#include <rack.h>

#include "dict.h"
#include "dictfactory.h"
#include "util.h"

using namespace QuackleIO;

UtilSettings *UtilSettings::m_self = 0;
UtilSettings *UtilSettings::self()
{
	return m_self;
}

UtilSettings::UtilSettings()
	: octothorpBritish(true), vowelFirst(false), scoreInvalidAsZero(false)
{
	m_self = this;
}

QString Util::moveToDetailedString(const Quackle::Move &move)
{
	QString prettyTiles = letterStringToQString(move.prettyTiles());

	QString ret;

	switch (move.action)
	{
	case Quackle::Move::Pass:
		ret = QObject::tr("Pass");
		break;
	
	case Quackle::Move::Exchange:
		ret = QObject::tr("Exch. %1").arg(prettyTiles);
		break;
	
	case Quackle::Move::BlindExchange:
		ret = QObject::tr("Exch. %1").arg(move.tiles().length());
		break;
	
	case Quackle::Move::UnusedTilesBonusError:
	case Quackle::Move::UnusedTilesBonus:
		ret = QObject::tr("2*(%1)").arg(letterStringToQString(Util::alphagram(move.usedTiles())));
		if (move.action == Quackle::Move::UnusedTilesBonusError)
			ret += " [Endgame Error]";
		break;
	
	case Quackle::Move::TimePenalty:
		ret = QObject::tr("%1 point time penalty").arg(move.effectiveScore());
		break;
	
	case Quackle::Move::Nonmove:
		ret = QObject::tr("None");
		break;
	
	case Quackle::Move::Place:
	case Quackle::Move::PlaceError:
		ret = uvStringToQString(move.positionString()) + " ";
		ret += prettyTiles;

		if (UtilSettings::self()->octothorpBritish)
			ret += symbolsFor(move.wordTiles());

		if (move.isChallengedPhoney())
			ret = QObject::tr("%1 [Challenged Off]").arg(ret);

		if (move.action == Quackle::Move::PlaceError)
			ret += " [Endgame Misdraw]";

		break;
	}

	if (move.scoreAddition() != 0)
		ret = QString("%1 [and %2%3]").arg(ret).arg(move.scoreAddition() > 0? QObject::tr("+") : QString()).arg(move.scoreAddition());

	return ret;
}

QString Util::moveToSensitiveString(const Quackle::Move &move)
{
	QString ret;

	if (move.action == Quackle::Move::Exchange)
		ret = QObject::tr("Exch. %1").arg(move.prettyTiles().length());
	else
		ret = moveToDetailedString(move);

	return ret;
}

UVString Util::qstringToString(const QString &qstring)
{
#if QUACKLE_USE_WCHAR_FOR_USER_VISIBLE
	return qstring.toStdWString();
#else
	return string(qstring.toUtf8().data());
#endif
}

QString Util::uvStringToQString(const UVString &uvString)
{
#if QUACKLE_USE_WCHAR_FOR_USER_VISIBLE
	return QString::fromStdWString(uvString);
#else
	return QString::fromUtf8(uvString.c_str());
#endif
}

Quackle::LetterString Util::encode(const QString &qstring)
{
	return QUACKLE_ALPHABET_PARAMETERS->encode(qstringToString(qstring));
}

Quackle::LetterString Util::nonBlankEncode(const QString &qstring)
{
	return QUACKLE_ALPHABET_PARAMETERS->clearBlankness(encode(qstring));
}

QString Util::letterStringToQString(const Quackle::LetterString &letterString)
{
	return uvStringToQString(QUACKLE_ALPHABET_PARAMETERS->userVisible(letterString));
}

QString Util::letterToQString(const Quackle::Letter &letter)
{
	return uvStringToQString(QUACKLE_ALPHABET_PARAMETERS->userVisible(letter));
}

string Util::qstringToStdString(const QString &qstring)
{
	return string(qstring.toLatin1());
}

QString Util::stdStringToQString(const string &stdString)
{
	return QString::fromLatin1(stdString.c_str());
}

Quackle::LetterString Util::alphagram(const Quackle::LetterString &word) 
{
	return Quackle::String::alphabetize(word);
}

QString Util::alphagram(const QString &word)
{
	return letterStringToQString(Quackle::String::alphabetize(encode(word)));
}

Quackle::LetterString Util::arrangeLettersForUser(const Quackle::LetterString &word)
{
	Quackle::LetterString alphabetized = Quackle::String::alphabetize(word);

	Quackle::LetterString vowels;
	Quackle::LetterString nonvowels;
	Quackle::LetterString blanks;
	for (Quackle::LetterString::iterator it = alphabetized.begin(); it != alphabetized.end(); ++it)
	{
		if ((*it) == QUACKLE_BLANK_MARK)
			blanks.push_back(*it);
		else if (UtilSettings::self()->vowelFirst && QUACKLE_ALPHABET_PARAMETERS->isVowel(*it))
			vowels.push_back(*it);
		else
			nonvowels.push_back(*it);
	}

	return vowels + nonvowels + blanks;
}

Quackle::LetterString Util::arrangeLettersForUser(const Quackle::Rack &rack)
{
	return arrangeLettersForUser(rack.tiles());
}

Quackle::Rack Util::makeRack(const QString &letters)
{
	return Quackle::Rack(QuackleIO::Util::encode(letters.toUpper().replace('.', "?")));
}

QString Util::arrangeLettersForUser(const QString &word)
{
	return letterStringToQString(arrangeLettersForUser(encode(word)));
}

QString Util::sanitizeUserVisibleLetterString(const QString &pipedString)
{
	QString pipedStringCopy = pipedString;
	pipedStringCopy.replace("|", " ");
	return pipedStringCopy.simplified();
}

QString Util::symbolsFor(const Quackle::LetterString &word)
{
	if (DictFactory::querier()->isBritish(word))
	{
		return "#";
	}

	return "";
}

