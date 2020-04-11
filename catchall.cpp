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

#include "board.h"
#include "datamanager.h"
#include "game.h"
#include "strategyparameters.h"
#include "catchall.h"

using namespace Quackle;

double CatchallEvaluator::equity(const GamePosition &position, const Move &move) const
{
	//UVcout << "catchall being used on " << move.tiles() << endl;
	if (position.board().isEmpty())
	{
		double adjustment = 0;
	
		if (move.action == Move::Place)
		{
			int start = move.startcol;
			if (move.startrow < start)
				start = move.startrow;
		
			LetterString wordTiles = move.tiles();
		
			int length = wordTiles.length();
	
			int consbits = 0;
			for (signed int i = wordTiles.length() - 1; i >= 0; i--)
			{
				consbits <<= 1;
                if (QUACKLE_ALPHABET_PARAMETERS->isVowel(QUACKLE_ALPHABET_PARAMETERS->clearBlankness(wordTiles[i])))
					consbits |= 1;
			}

			adjustment = QUACKLE_STRATEGY_PARAMETERS->vcPlace(start, length, consbits);
		}
		else
			adjustment = 3.5;

		// UVcout << "placement adjustment for " << move << " is " << adjustment << endl;
		return ScorePlusLeaveEvaluator::equity(position, move) + adjustment;
	}
	
	else if (position.bag().size() > 0)
	{
		int leftInBagPlusSeven = position.bag().size() - move.usedTiles().length() + 7;
		double heuristicArray[13] =
		{
			0.0, -8.0, 0.0, -0.5, -2.0, -3.5, -2.0,
			2.0, 10.0, 7.0,  4.0, -1.0, -2.0
		};
		double timingHeuristic = 0.0;
		if (leftInBagPlusSeven < 13) timingHeuristic = heuristicArray[leftInBagPlusSeven];
		return ScorePlusLeaveEvaluator::equity(position, move) + timingHeuristic;
	}
	else
	{
		return endgameResult(position, move) + move.score;
	}
}

double CatchallEvaluator::endgameResult(const GamePosition &position, const Move &move) const
{
	Rack leave = position.currentPlayer().rack() - move;

	if (leave.empty())
	{
		double deadwood = 0;
		for (PlayerList::const_iterator it = position.players().begin(); 
		     it != position.players().end(); ++it)
		{
			if (!(*it == position.currentPlayer()))
			{
				deadwood += it->rack().score();
			}
		}

		return deadwood * 2;
	}

    return -8.00 - 2.61 * leave.score();
}

