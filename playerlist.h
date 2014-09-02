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

#ifndef QUACKLE_PLAYERLIST_H
#define QUACKLE_PLAYERLIST_H

#include <vector>

#include "alphabetparameters.h"
#include "player.h"

using namespace std;

namespace Quackle
{

class Player;

class PlayerList : public vector<Player>
{
public:
	PlayerList();

	const Player &playerForId(int id, bool &found) const;
	const Player &playerForName(const UVString &name, bool &found) const;
};

}

#endif
