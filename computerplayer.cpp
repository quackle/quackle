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

#include "computerplayer.h"
#include "endgameplayer.h"

using namespace Quackle;

ComputerPlayer::ComputerPlayer()
	: m_name(MARK_UV("Computer Player")), m_id(0), m_dispatch(0)
{
	m_parameters.secondsPerTurn = 10;
    m_parameters.inferring = false;
}

ComputerPlayer::~ComputerPlayer()
{
}

void ComputerPlayer::setDispatch(ComputerDispatch *dispatch)
{
	m_dispatch = dispatch;
	m_simulator.setDispatch(dispatch);
}

void ComputerPlayer::setPosition(const GamePosition &position)
{
	m_simulator.setPosition(position);
}

bool ComputerPlayer::shouldAbort()
{
	return m_dispatch && m_dispatch->shouldAbort();
}

void ComputerPlayer::signalFractionDone(double fractionDone)
{
	if (m_dispatch)
		m_dispatch->signalFractionDone(fractionDone);
}

void ComputerPlayer::considerMove(const Move &move)
{
	m_simulator.addConsideredMove(move);
}

void ComputerPlayer::setConsideredMoves(const MoveList &moves)
{
	m_simulator.setConsideredMoves(moves);
}

MoveList ComputerPlayer::moves(int /* nmoves */)
{
	MoveList ret;
	ret.push_back(move());
	return ret;
}

///////

StaticPlayer::StaticPlayer()
{
	m_name = MARK_UV("Static Player");
	m_id = 1;
}

StaticPlayer::~StaticPlayer()
{
}

Move StaticPlayer::move()
{
	return m_simulator.currentPosition().staticBestMove();
}

MoveList StaticPlayer::moves(int nmoves)
{
	m_simulator.currentPosition().kibitz(nmoves);
	return m_simulator.currentPosition().moves();
}

ScalingDispatch::ScalingDispatch(ComputerDispatch *shadow, double scale, double addition)
	: m_shadow(shadow), m_scale(scale), m_addition(addition)
{
}

bool ScalingDispatch::shouldAbort()
{
	return m_shadow->shouldAbort();
}

void ScalingDispatch::signalFractionDone(double fractionDone)
{
	m_shadow->signalFractionDone(fractionDone * m_scale + m_addition);
}
