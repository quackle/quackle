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

#ifndef QUACKLE_GCGIO_H
#define QUACKLE_GCGIO_H

#include "logania.h"

namespace QuackleIO
{

class GCGIO : public Logania
{
public:
	GCGIO();
	~GCGIO() {};

	virtual Quackle::Game *read(const QString &filename, int flags);
	virtual Quackle::Game *read(QTextStream &stream, int flags);
	virtual bool canRead(QTextStream &stream) const;
	virtual void write(const Quackle::Game &game, QTextStream &stream);
	virtual QString filter() const;

private:
	int readSignedInt(const QString &intString) const;
};

}

#endif

