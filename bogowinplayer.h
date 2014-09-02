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

#ifndef QUACKLE_BOGOWINPLAYER_H
#define QUACKLE_BOGOWINPLAYER_H

#include "computerplayer.h"
#include "endgame.h"

namespace Quackle
{

class SmartBogowin : public ComputerPlayer
{
public:
	SmartBogowin();
	virtual ~SmartBogowin();

	virtual Move move();
	virtual MoveList moves(int nmoves);
	virtual ComputerPlayer *clone() { return new SmartBogowin; }

	virtual bool isSlow() const;
	virtual bool isUserVisible() const;
	virtual double bogopoints(Move &move);

protected:
	int minIterations() const;
	int maxIterations() const;

	Endgame m_endgame;

	int m_additionalInitialCandidates;
	int m_minIterationsPerSecond;
	int m_maxIterationsPerSecond;
	int m_nestedMinIterationsPerSecond;
	int m_nestedMaxIterationsPerSecond;

    bool m_inferring;
};

inline bool SmartBogowin::isSlow() const
{
	return true;
}

inline bool SmartBogowin::isUserVisible() const
{
	return true;
}

inline int SmartBogowin::minIterations() const
{
	if (currentPosition().nestedness() > 0)
		return m_nestedMinIterationsPerSecond * m_parameters.secondsPerTurn;
		
	return m_minIterationsPerSecond * m_parameters.secondsPerTurn;
}

inline int SmartBogowin::maxIterations() const
{
	if (currentPosition().nestedness() > 0)
		return m_nestedMaxIterationsPerSecond * m_parameters.secondsPerTurn;
	
	return m_maxIterationsPerSecond * m_parameters.secondsPerTurn;
}

}
#endif
