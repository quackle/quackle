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

#include <iostream>

#include <QtCore>

#include "gcgio.h"
#include "queenie.h"

using namespace QuackleIO;

Queenie *Queenie::m_self = 0;

Queenie *Queenie::self()
{
	if (m_self == 0)
		m_self = new Queenie();

	return m_self;
}

void Queenie::cleanUp()
{
	delete m_self;
	m_self = 0;
}

Queenie::Queenie()
{
	m_loganias.push_back(new GCGIO);

	for (QList<Logania *>::const_iterator it = m_loganias.begin(); it != m_loganias.end(); ++it)
		m_filters.push_back((*it)->filter());
}

Queenie::~Queenie()
{
	while (!m_loganias.isEmpty())
		delete m_loganias.takeFirst();
}

Logania *Queenie::loganiaForFile(const QString &filename)
{
	for (QList<Logania *>::const_iterator it = m_loganias.begin(); it != m_loganias.end(); ++it)
	{
		QFile file(filename);
	
		if (!file.open(QIODevice::ReadOnly | QIODevice::Text))
			return 0;

		QTextStream in(&file);

		if ((*it)->canRead(in))
			return (*it);
	}

	return 0;
}

const QStringList &Queenie::filters() const
{
	return m_filters;
}

Logania *Queenie::defaultLogania()
{
	return m_loganias.front();
}

