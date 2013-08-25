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

#ifndef QUACKLE_PREENDGAME_H
#define QUACKLE_PREENDGAME_H

#include "computerplayer.h"

namespace Quackle
{

class Preendgame : public ComputerPlayer
{
public:
	Preendgame();
	virtual ~Preendgame();

	virtual Move move();
	virtual MoveList moves(int nmoves);
	virtual ComputerPlayer *clone() { return new Preendgame; }

	virtual bool isUserVisible() const;
	virtual bool isSlow() const;

	static int maximumTilesInBagToEngage();

private:
	int calculateTimeLimit() const;
	int calculateInitialCandidates() const;

	// Warning: this function has many side affects!
	void getInitialMoves(MoveList *moves);

	double calculateFractionAllottedToInitialBogo() const;

	int m_initialCandidates;
	int m_nestednessDenominatorBase;

	bool m_debugPreendgame;
};

inline bool Preendgame::isUserVisible() const
{
	return false;
}

inline bool Preendgame::isSlow() const
{
	return true;
}

}

#endif
