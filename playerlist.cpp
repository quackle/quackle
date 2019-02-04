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

#include "playerlist.h"

using namespace Quackle;

PlayerList::PlayerList()
{
}

const Player &PlayerList::playerForId(int id, bool &found) const
{
	const PlayerList::const_iterator theend(end());
	for (PlayerList::const_iterator it = begin(); it != theend; ++it)
	{
		if ((*it).id() == id)
		{
			found = true;
			return (*it);
		}
	}
	
	found = false;
	return back();
}

const Player &PlayerList::playerForName(const UVString &name, bool &found) const
{
	const PlayerList::const_iterator theend(end());
	for (PlayerList::const_iterator it = begin(); it != theend; ++it)
		if ((*it).name() == name)
		{
			found = true;
			return (*it);
		}
	
	found = false;
	return back();
}

