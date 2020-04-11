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

#include "computerplayercollection.h"

#include "computerplayer.h"
#include "bogowinplayer.h"
#include "endgameplayer.h"
#include "resolvent.h"

using namespace Quackle;

ComputerPlayerCollection::ComputerPlayerCollection()
{
}

Player ComputerPlayerCollection::createPlayer(ComputerPlayer *computerPlayer)
{
	Player ret(computerPlayer->name(), Player::ComputerPlayerType, computerPlayer->id());
	ret.setComputerPlayer(computerPlayer);
	return ret;
}

void ComputerPlayerCollection::addPlayer(ComputerPlayer *computerPlayer)
{
	push_back(createPlayer(computerPlayer));
}

ComputerPlayerCollection ComputerPlayerCollection::fullCollection()
{
	ComputerPlayerCollection ret;
	ret.addPlayer(new EndgamePlayer());
	ret.addPlayer(new StaticPlayer());
	ret.addPlayer(new FiveMinutePlayer());
	ret.addPlayer(new TwentySecondPlayer());
	ret.addPlayer(new TorontoPlayer());
        //ret.addPlayer(new InferringPlayer());
	return ret;
}
