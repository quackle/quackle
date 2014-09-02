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

#ifndef QUACKLE_LOGANIA_H
#define QUACKLE_LOGANIA_H

#include <QString>

class QTextStream;

namespace Quackle
{
	class Game;
}

namespace QuackleIO
{

// Abstract interfaces for game log input/output

class Logania
{
public:
	virtual ~Logania() {};

	// if MaintainBoardPreparation is not set, boards will not be prepared
	// for analysis and you'll have to do this manually. If you set this flag
	// DataManager and a generator need to be set up.
	enum ReadFlags { BasicLoad = 0x0000, MaintainBoardPreparation = 0x0001 };

	// allocates a new Game, fills it up, and returns it.
	// Returned game is either 0, or a game with at least one position.
	// Boards of game are ready for analysis if there is a DataManager set
	// up; otherwise they won't have their crosses prepared
	virtual Quackle::Game *read(QTextStream &stream, int flags) = 0;

	virtual bool canRead(QTextStream &stream) const = 0;
	virtual void write(const Quackle::Game &game, QTextStream &stream) = 0;
	virtual QString filter() const = 0;
};

}

#endif
