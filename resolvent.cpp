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

#include <iostream>
#include <time.h>

#include "bogowinplayer.h"
#include "endgameplayer.h"
#include "preendgame.h"
#include "resolvent.h"

using namespace Quackle;

Resolvent::Resolvent()
{
    m_name = MARK_UV("Resolvent");
    m_id = 201;
}

Resolvent::~Resolvent()
{
}

Move Resolvent::move()
{
    return moves(1).back();
}

MoveList Resolvent::moves(int nmoves)
{
    // UVcout << "Resolvent generating move from position:" << endl;
    // UVcout << m_simulator.currentPosition() << endl;

    ComputerPlayer *delegatee;

    if (m_simulator.currentPosition().bag().empty())
    {
        // Case 1: Straight endgame.
        delegatee = new EndgamePlayer;
    }
    else if (currentPosition().bag().size() <= Preendgame::maximumTilesInBagToEngage())
    {
        // Case 2: Preendgame.
        delegatee = new Preendgame;
    }
    else
    {
        // Case 3: Beginning and middle of the game.
        delegatee = new SmartBogowin;
    }

    delegatee->setParameters(parameters());
    delegatee->setDispatch(currentPosition().nestedness() > 0? 0 : m_dispatch);
    delegatee->setPosition(m_simulator.currentPosition());
    delegatee->setConsideredMoves(m_simulator.consideredMoves());
    MoveList moves = delegatee->moves(nmoves);
    delete delegatee;

    return moves;
}

bool Resolvent::isSlow() const
{
    return true;
}

InferringPlayer::InferringPlayer()
{
    m_name = MARK_UV("Inferring Player");
    m_id = 2012;
    m_parameters.secondsPerTurn = 20;
    m_parameters.inferring = true;
}

InferringPlayer::~InferringPlayer()
{
}

TorontoPlayer::TorontoPlayer()
{
	m_name = MARK_UV("Championship Player");
	m_id = 2006;
	m_parameters.secondsPerTurn = 66;
}

TorontoPlayer::~TorontoPlayer()
{
}

FiveMinutePlayer::FiveMinutePlayer()
{
	m_name = MARK_UV("Five Minute Championship Player");
	m_id = 5208;
	m_parameters.secondsPerTurn = 60 * 5;
}

FiveMinutePlayer::~FiveMinutePlayer()
{
}

TwentySecondPlayer::TwentySecondPlayer()
{
	m_name = MARK_UV("Twenty Second Championship Player");
	m_id = 5209;
	m_parameters.secondsPerTurn = 20;
}

TwentySecondPlayer::~TwentySecondPlayer()
{
}
