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
#include <math.h>
#include <time.h>

#include "bogowinplayer.h"
#include "clock.h"
#include "enumerator.h"
#include "preendgame.h"
#include "resolvent.h"

using namespace Quackle;
using namespace std;

Preendgame::Preendgame()
	: m_initialCandidates(30), m_nestednessDenominatorBase(2)
{
	m_name = MARK_UV("Preendgame");
	m_id = 202;

	m_debugPreendgame = false;
}

Preendgame::~Preendgame()
{
}

int Preendgame::maximumTilesInBagToEngage()
{
	// TODO fix up 2-in-bag
	return 2;
}

int Preendgame::calculateInitialCandidates() const
{
	int ret = m_initialCandidates;
	if (currentPosition().nestedness() > 0)
		ret = static_cast<int>(ceil(ret / pow((double)m_nestednessDenominatorBase, (int) currentPosition().nestedness())));
	return ret;
}

const int timeLimitPerSecondsPerTurn = 6;

int Preendgame::calculateTimeLimit() const
{
	return m_parameters.secondsPerTurn * timeLimitPerSecondsPerTurn;
}

double Preendgame::calculateFractionAllottedToInitialBogo() const
{
	return 1.0 / static_cast<double>(timeLimitPerSecondsPerTurn);
}

void Preendgame::getInitialMoves(MoveList *moves)
{
	if (currentPosition().nestedness() > 0)
	{
		currentPosition().kibitz(calculateInitialCandidates());
		currentPosition().makeSureMoveListContainsMoves(m_simulator.consideredMoves());
		currentPosition().incrementNestedness();

		*moves = currentPosition().moves();
		currentPosition().removeAllMoves();
	}
	else
	{
		SmartBogowin bogo;

		bogo.setParameters(m_parameters);
		currentPosition().incrementNestedness();
		bogo.setPosition(currentPosition());
		bogo.setConsideredMoves(m_simulator.consideredMoves());

		ScalingDispatch *scalingDispatch = 0;

		if (m_dispatch)
			scalingDispatch = new ScalingDispatch(m_dispatch, calculateFractionAllottedToInitialBogo(), 0);

		bogo.setDispatch(scalingDispatch);
		*moves = bogo.moves(calculateInitialCandidates());

		delete scalingDispatch;
	}
}

Move Preendgame::move()
{
	return moves(1).back();
}

MoveList Preendgame::moves(int nmoves)
{
	MoveList ret;
	if (currentPosition().bag().empty() || currentPosition().bag().size() > maximumTilesInBagToEngage())
	{
		UVcout << "Preendgame used in improper location." << endl;
		return ret;
	}

	const double fractionAllottedToInitialBogo = calculateFractionAllottedToInitialBogo();
	const int timeLimit = calculateTimeLimit();
	Stopwatch stopwatch;

	// Get enumerated racks.
	Bag unseenBag = currentPosition().unseenBag();
	Enumerator enumerator(unseenBag);
	ProbableRackList racks;

	if (currentPosition().nestedness() % 2)
		enumerator.enumeratePossible(&racks, currentPosition().bag());
	else
		enumerator.enumerate(&racks);

	
	signalFractionDone(0);

	MoveList moves;
	getInitialMoves(&moves);
	if (m_debugPreendgame)
	{
		UVcout << currentPosition().nestednessIndentation() << "Preendgame's candidates from bogo:" << endl;
		UVcout << moves << endl;
	}

	for (MoveList::iterator moveIt = moves.begin(); moveIt != moves.end(); ++moveIt)
	{
		(*moveIt).win = 0;
		(*moveIt).possibleWin = 0;
		// (*moveIt).equity = 0;
	}

	GamePosition tempPosition;
	Resolvent resolvent;

	int j = 0;
	for (MoveList::iterator moveIt = moves.begin(); moveIt != moves.end(); ++moveIt, ++j)
	{
		(*moveIt).win = 1;
		(*moveIt).possibleWin = 1;

		int i = 0;
		for (ProbableRackList::iterator it = racks.begin(); it != racks.end(); ++it, ++i)
		{
			if (m_debugPreendgame)
			{
				UVcout << "\n" << currentPosition().nestednessIndentation() << "Turn " << currentPosition().turnNumber() << ", Rack " << i + 1 << " of " << racks.size() << ", Move " << j + 1 << " of " << moves.size() << ": " << *moveIt << "." << endl;
				UVcout << currentPosition().nestednessIndentation() << currentPosition().currentPlayer().name() << " on turn with " << currentPosition().currentPlayer().rack() << " vs. oppo " << currentPosition().nextPlayer()->name() << " with " << (*it).rack << " prob " << (*it).probability << " poss " << (*it).possibility << endl;
			}

			tempPosition = currentPosition();

			tempPosition.setOppRack((*it).rack);
			tempPosition.setMoveMade(*moveIt);
			tempPosition.incrementTurn(NULL);
			tempPosition.makeMove(*moveIt);
			//tempPosition.incrementNestedness();
			
			resolvent.setPosition(tempPosition);
			Move resolventMove = resolvent.move();

			if (m_debugPreendgame)
			{
				UVcout << currentPosition().nestednessIndentation() << "In response, resolvent makes move " << resolventMove << endl;
			}

			(*moveIt).win -= (*it).probability * resolventMove.win;
			(*moveIt).possibleWin -= (*it).possibility * resolventMove.win;
			(*moveIt).equity -= (*it).probability * resolventMove.equity;

			// These can get negative with rounding inaccuracy.
			if ((*moveIt).win < 0)
				(*moveIt).win = 0;
			if ((*moveIt).possibleWin < 0)
				(*moveIt).possibleWin = 0;

			// This optimization leads to incorrect results.
			//if (currentPosition().nestedness() > 0 && resolventMove.win == 0)
			//	break;
			
			signalFractionDone(fractionAllottedToInitialBogo + (1 - fractionAllottedToInitialBogo) * (max(static_cast<double>(j * racks.size() + i) / static_cast<double>(racks.size() * moves.size()), static_cast<double>(stopwatch.elapsed()) / static_cast<double>(timeLimit))));
		}

		if (stopwatch.exceeded(timeLimit))
			break;

		if (shouldAbort())
			goto sort_and_return;
	}

	if (m_debugPreendgame)
	{
		UVcout << currentPosition().nestednessIndentation() << "Turn " << currentPosition().turnNumber() << ": " << currentPosition().currentPlayer().name() << " on turn with " << currentPosition().currentPlayer().rack() << " has top 10 plays: " << endl;
	}

	sort_and_return:

	MoveList::sort(moves, MoveList::Win);

	int i = 1;
	for (MoveList::iterator moveIt = moves.begin(); moveIt != moves.end(); ++moveIt, ++i)
	{
		if (i > nmoves)
			break;

		(*moveIt).win = (*moveIt).possibleWin;
		ret.push_back(*moveIt);

		if (m_debugPreendgame)
		{
			UVcout << currentPosition().nestednessIndentation() << i << ". " << *moveIt << endl;
		}
	}

	return ret;
}
