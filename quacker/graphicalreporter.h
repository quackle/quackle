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

#ifndef QUACKER_GRAPHICAL_REPORTER_H
#define QUACKER_GRAPHICAL_REPORTER_H

#include <QFile>
#include <QTextStream>

namespace Quackle
{
	class ComputerPlayer;
	class GamePosition;
	class Game;
}

class GraphicalReporter
{
public:
	// If generateImages is true, output must be an existing directory.
	// If false, output is an HTML file.
	GraphicalReporter(const QString &output, bool generateImages);

	// makes header and report for all positions
	void reportGame(const Quackle::Game &game, Quackle::ComputerPlayer *computerPlayer);

	// makes header and puts into the index file
	void reportHeader(const Quackle::Game &game);

	// makes graphical displays of top n plays of position, dumps to images 
	// and puts links to them in the index file.
	void reportPosition(const Quackle::GamePosition &position, Quackle::ComputerPlayer *computerPlayer);

protected:
	QString makeFilename(const QString &filename) const;

	// opens the index file (m_output/index.html or m_output if it's a file)
	// if it is not already open
	void openIndex();

	QString m_output;
	QFile m_indexFile;
	QTextStream m_indexStream;

	bool m_generateImages;
};

#if (QT_VERSION >= QT_VERSION_CHECK(5, 14, 0)) && !defined(m_endl)
#	define m_endl Qt::endl
#elif !defined(m_endl)
#	define m_endl endl
#endif


#endif
