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

#ifndef QUACKLE_RESOLVENT_H
#define QUACKLE_RESOLVENT_H

#include "alphabetparameters.h"
#include "computerplayer.h"

namespace Quackle
{

class Resolvent : public ComputerPlayer
{
public:
	Resolvent();
	virtual ~Resolvent();

	virtual Move move();
	virtual MoveList moves(int nmoves);
	virtual ComputerPlayer *clone() { return new Resolvent; }

	virtual bool isSlow() const;
	virtual bool isUserVisible() const;
};

inline bool Resolvent::isUserVisible() const
{
	return true;
}

class InferringPlayer : public Resolvent
{
public:
	InferringPlayer();
	virtual ~InferringPlayer();
	virtual ComputerPlayer *clone() { return new InferringPlayer; }
};

class TorontoPlayer : public Resolvent
{
public:
	TorontoPlayer();
	virtual ~TorontoPlayer();
	virtual ComputerPlayer *clone() { return new TorontoPlayer; }
};

class FiveMinutePlayer : public Resolvent
{
public:
	FiveMinutePlayer();
	virtual ~FiveMinutePlayer();
	virtual ComputerPlayer *clone() { return new FiveMinutePlayer; }
};

class TwentySecondPlayer : public Resolvent
{
public:
	TwentySecondPlayer();
	virtual ~TwentySecondPlayer();
	virtual ComputerPlayer *clone() { return new TwentySecondPlayer; }
};

}

#endif
