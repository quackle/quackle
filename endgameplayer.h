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

#ifndef QUACKLE_ENDGAMEPLAYER_H
#define QUACKLE_ENDGAMEPLAYER_H

#include "computerplayer.h"
#include "endgame.h"

namespace Quackle
{

class EndgamePlayer : public ComputerPlayer
{
public:
	EndgamePlayer();
	virtual ~EndgamePlayer();

	virtual Move move();
	virtual MoveList moves(int nmoves);
	virtual ComputerPlayer *clone() { return new EndgamePlayer; }

	virtual bool isUserVisible() const;

	virtual void setDispatch(ComputerDispatch *dispatch);

private:
	Endgame m_endgame;
};

inline bool EndgamePlayer::isUserVisible() const
{
	return true;
}

}

#endif
