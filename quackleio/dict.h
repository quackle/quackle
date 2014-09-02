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

#ifndef QUACKLEIO_DICT_H
#define QUACKLEIO_DICT_H

#include <vector>

#include <QStringList>

#include <alphabetparameters.h>
#include <generator.h>

namespace Dict
{

class Extension : public Quackle::ExtensionWithInfo
{
public:
	Extension() {}
	Extension(const Quackle::ExtensionWithInfo extensionWithInfo);
	QString word;
};

typedef QList<Extension> ExtensionList;

class Word : public Quackle::WordWithInfo
{
public:
	QString word;

	static ExtensionList extensionsByLength(int length, const ExtensionList &list);

	ExtensionList getFrontExtensionList() const;
	ExtensionList getBackExtensionList() const;

private:
	ExtensionList getExtensionList(bool front) const;
};

// Returns true if word1 is less playable than word2;
// otherwise returns false.
bool operator<(const Dict::Word &word1, const Dict::Word &word2);

class WordList : public QList<Word>
{
public:
	WordList();
	~WordList();

	enum SortType { Alphabetical, Playability, Length, LengthLongestFirst, Probability };

	void setSortBy(SortType sortType);
	static SortType sortType;
};

typedef QList<WordList> WordListList;

class Querier
{
public:
	virtual ~Querier() {};

	enum QueryFlags { None = 0x0000, WithExtensions = 0x0001, NoRequireAllLetters = 0x0002, CallUpdate = 0x0004 };

	virtual WordList query(const QString &query, int flags = None) = 0;
	virtual QString alphagram(const QString &letters) const = 0;
	virtual bool isBritish(const Quackle::LetterString &word) = 0;
	virtual bool isLoaded() const = 0;
};

}

#endif
