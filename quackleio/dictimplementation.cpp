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

#include <alphabetparameters.h>
#include <datamanager.h>
#include <generator.h>
#include <lexiconparameters.h>
#include <quackleio/util.h>

#include "dictimplementation.h"

QuackleIO::DictImplementation::DictImplementation()
{
}

QuackleIO::DictImplementation::~DictImplementation()
{
}

Dict::WordList QuackleIO::DictImplementation::query(const QString &query, int flags)
{
	QString modifiedQuery = query;
	modifiedQuery.replace(".", "?");

	int anagramFlags = Quackle::Generator::ClearBlanknesses;

	if (flags & Dict::Querier::NoRequireAllLetters)
		anagramFlags |= Quackle::Generator::NoRequireAllLetters;

	QRegularExpression wildcardRegexp("[\\*/]");
	if (wildcardRegexp.match(modifiedQuery).hasMatch())
	{
		if (!(flags & Dict::Querier::NoRequireAllLetters))
			anagramFlags |= Quackle::Generator::AddAnyLetters;

		modifiedQuery.replace(wildcardRegexp, QString());
	}

	vector<Quackle::LetterString> words(m_generator.anagramLetters(QuackleIO::Util::encode(modifiedQuery), anagramFlags));
	Dict::WordList ret;

	vector<Quackle::LetterString>::const_iterator end = words.end();
	for (vector<Quackle::LetterString>::const_iterator it = words.begin(); it != end; ++it)
	{
		Dict::Word dictWord;
		dictWord.word = QuackleIO::Util::letterStringToQString(*it);
		dictWord.wordLetterString = (*it);
		m_generator.storeWordInfo(&dictWord);

		if (flags & WithExtensions)
			m_generator.storeExtensions(&dictWord);

		ret.push_back(dictWord);
	}

	if (flags & NoRequireAllLetters)
	{
		ret.setSortBy(Dict::WordList::LengthLongestFirst);
	}
	else
	{
		ret.setSortBy(Dict::WordList::Alphabetical);
	}

	std::sort(ret.begin(), ret.end());

	return ret;
}

QString QuackleIO::DictImplementation::alphagram(const QString &letters) const
{
	return QuackleIO::Util::letterStringToQString(QuackleIO::Util::alphagram(QuackleIO::Util::encode(letters)));
}

bool QuackleIO::DictImplementation::isLoaded() const
{
	return QUACKLE_LEXICON_PARAMETERS->hasSomething();
}

bool QuackleIO::DictImplementation::isBritish(const Quackle::LetterString &word)
{
	Quackle::WordWithInfo wordWithInfo;
	wordWithInfo.wordLetterString = word;
	m_generator.storeWordInfo(&wordWithInfo);
	return wordWithInfo.british;
}

