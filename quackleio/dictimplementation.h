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

#ifndef QUACKLE_DICTIMPLEMENTATION_H
#define QUACKLE_DICTIMPLEMENTATION_H

#include <generator.h>

#include "dict.h"

namespace QuackleIO
{

class DictImplementation : public Dict::Querier
{
public:
	DictImplementation();
	virtual ~DictImplementation();

	virtual Dict::WordList query(const QString &query, int flags = None);
	virtual QString alphagram(const QString &letters) const;
	virtual bool isBritish(const Quackle::LetterString &word);
	virtual bool isLoaded() const;

private:
	Quackle::Generator m_generator;
};

}

#endif

