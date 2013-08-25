/*
 *  Quackle -- Crossword game artificial intelligence and analysis tool
 *  Copyright (C) 2005-2006 Jason Katz-Brown and John O'Laughlin.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  
 *  02110-1301  USA
 */

#include <QtCore>

#include "game.h"
#include "streamingreporter.h"
#include "util.h"

using namespace QuackleIO;

StreamingReporter::StreamingReporter()
{
}

void StreamingReporter::reportGame(const Quackle::Game &game, Quackle::ComputerPlayer *computerPlayer, QTextStream &stream)
{
	UVString header;
	Quackle::Reporter::reportHeader(game, &header);
	stream << Util::uvStringToQString(header);

	const Quackle::PositionList::const_iterator end(game.history().end());
	for (Quackle::PositionList::const_iterator it = game.history().begin(); it != end; ++it)
	{
		UVString subreport;
		Quackle::Reporter::reportPosition((*it), computerPlayer, &subreport);

		// endl flushes the stream, which we want
		stream << Util::uvStringToQString(subreport) << endl;
	}

	UVString stats;
	Quackle::Reporter::reportGameStatistics(game, &stats);
	stream << Util::uvStringToQString(stats);
}

