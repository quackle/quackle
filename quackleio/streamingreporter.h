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

#ifndef QUACKLE_STREAMINGREPORTER_H
#define QUACKLE_STREAMINGREPORTER_H

#include "reporter.h"

class QTextStream;

namespace Quackle
{
	class Game;
}

namespace QuackleIO
{

// Full-game reporter using Qt streaming.

class StreamingReporter
{
public:
	StreamingReporter();

	// duplicates functionality of Quackle::Reporter::reportGame but suitable
	// for streaming so everything doesn't appear in the file at once.
	static void reportGame(const Quackle::Game &game, Quackle::ComputerPlayer *computerPlayer, QTextStream &stream);
};

}

#endif
