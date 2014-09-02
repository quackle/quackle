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

#ifndef QUACKLE_REPORTER_H
#define QUACKLE_REPORTER_H

namespace Quackle
{

class GamePosition;
class ComputerPlayer;

class Reporter
{
public:
	Reporter() {};

	// Generates a formatted report of one position.
	// If computerPlayer is null, a report is made without showing the best plays.
	static void reportPosition(const GamePosition &position, ComputerPlayer *computerPlayer, UVString *report);

	// generates some formatted stats about the game
	static void reportGameStatistics(const Game &game, UVString *report);

	// header that credits Quackle
	static void reportHeader(const Game &game, UVString *report);

	// Report of all moves and endgame statistics.
	// computerPlayer can be null.
	static void reportGame(const Game &game, ComputerPlayer *computerPlayer, UVString *report);
};

}

#endif
