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

#ifndef QUACKLE_QUEENIE_H
#define QUACKLE_QUEENIE_H

#include <QtCore/QStringList>

namespace QuackleIO
{

class Logania;

class Queenie
{
public:
	~Queenie();

	static Queenie *self();

	// deletes self and resets
	static void cleanUp();

	// returns 0 if no handler found for the file type
	Logania *loganiaForFile(const QString &filename);

	const QStringList &filters() const;

	Logania *defaultLogania();

protected:
	Queenie();
	Queenie(const Queenie&);
	Queenie& operator=(const Queenie &);

	QList<Logania *> m_loganias;
	QStringList m_filters;

private:
	static Queenie *m_self;
};

}

#endif
