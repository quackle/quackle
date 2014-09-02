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

#include "board.h"
#include "datamanager.h"
#include "game.h"
#include "evaluator.h"
#include "strategyparameters.h"
#include "catchall.h"

using namespace Quackle;

double Evaluator::equity(const GamePosition &position, const Move &move) const
{
	(void) position;
	return move.effectiveScore();
}

double Evaluator::playerConsideration(const GamePosition &position, const Move &move) const
{
	(void) position;
	(void) move;
	return 0;
}

double Evaluator::sharedConsideration(const GamePosition &position, const Move &move) const
{
	(void) position;
	(void) move;
	return 0;
}

double Evaluator::leaveValue(const LetterString &leave) const
{
	(void) leave;
	return 0;
}

////////////

double ScorePlusLeaveEvaluator::equity(const GamePosition &position, const Move &move) const
{
	return playerConsideration(position, move) + sharedConsideration(position, move) + move.effectiveScore();
}

double ScorePlusLeaveEvaluator::playerConsideration(const GamePosition &position, const Move &move) const
{
	return leaveValue((position.currentPlayer().rack() - move).tiles());
}

double ScorePlusLeaveEvaluator::sharedConsideration(const GamePosition &position, const Move &move) const
{
	(void) position;
	(void) move;
	return 0;
}

double ScorePlusLeaveEvaluator::leaveValue(const LetterString &leave) const
{
	if (!QUACKLE_STRATEGY_PARAMETERS->isInitialized())
		return 0;

	LetterString alphabetized = String::alphabetize(leave);
	
	if (QUACKLE_STRATEGY_PARAMETERS->superleave(alphabetized))
		return QUACKLE_STRATEGY_PARAMETERS->superleave(alphabetized);

	double value = 0;
	double synergy = 0;
	LetterString uniqleave;

	const LetterString::const_iterator leaveEnd(leave.end());

	if (!leave.empty())
	{
		for (LetterString::const_iterator leaveIt = leave.begin(); leaveIt != leaveEnd; ++leaveIt)
			value += QUACKLE_STRATEGY_PARAMETERS->tileWorth(*leaveIt);

		for (unsigned int i = 0; i < leave.length() - 1; ++i)
			if (leave[i] == leave[i + 1])
				value += QUACKLE_STRATEGY_PARAMETERS->syn2(leave[i], leave[i]);

		uniqleave += leave[0];
		for (unsigned int i = 1; i < leave.length(); ++i)
			if (uniqleave[uniqleave.length() - 1] != leave[i])
				uniqleave += leave[i];

		if (uniqleave.length() >= 2)
		{
			for (unsigned int i = 0; i < uniqleave.length() - 1; ++i)
				for (unsigned int j = i + 1; j < uniqleave.length(); ++j)
					synergy += QUACKLE_STRATEGY_PARAMETERS->syn2(uniqleave[i], uniqleave[j]);

			// TODO handle the Q
			
			bool holding_bad_tile = false;
			for (unsigned int i = 0; i < uniqleave.length(); ++i) {
				if (QUACKLE_STRATEGY_PARAMETERS->tileWorth(uniqleave[i]) < -5.5) {
					holding_bad_tile = true;
				}
			}
			
			if ((synergy > 3.0) && !holding_bad_tile) {
			    synergy += 1.5 * (synergy - 3.0);
			}

			value += synergy;
		}    
	}

	int vowels = 0;
	int cons = 0;

	for (LetterString::const_iterator leaveIt = leave.begin(); leaveIt != leaveEnd; ++leaveIt)
	{
		if (*leaveIt != QUACKLE_BLANK_MARK)
		{
			if (QUACKLE_ALPHABET_PARAMETERS->isVowel(*leaveIt))
				vowels++;
			else
				cons++;
		}
	} 

	const float vcvalues[8][8] =
	{
		{  0.0,   0.0,  -1.0,  -2.5,  -5.0,  -8.5, -13.5, -18.5},
		{ -1.0,   0.0,   0.5,   0.0,  -1.5,  -5.0, -10.0,   0.0},
		{ -3.5,  -1.0,   0.5,   1.5,  -1.5,  -3.0,   0.0,   0.0},
		{ -7.0,  -3.5,  -0.5,   2.5,   0.0,   0.0,   0.0,   0.0},
		{-10.0,  -6.5,  -3.0,   0.0,   0.0,   0.0,   0.0,   0.0},
		{-13.5, -11.5,  -8.0,   0.0,   0.0,   0.0,   0.0,   0.0},
		{-18.5, -16.5,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0},
		{-23.5,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0},
	};

#ifdef DEBUG_BOARD
	UVcout << QUACKLE_ALPHABET_PARAMETERS->userVisible(leave) << " has " << vowels << " vowels, " << cons << " cons.  value of " << vcvalues[vowels][cons] << endl;
#endif

	value += vcvalues[vowels][cons];

	if (value < -40)
		value = -40;

#ifdef DEBUG_BOARD
	UVcout << "leave " << QUACKLE_ALPHABET_PARAMETERS->userVisible(leave) << " worth " << value << " uniq " << QUACKLE_ALPHABET_PARAMETERS->userVisible(uniqleave) << " synergy " << synergy << endl;
#endif

	return value;
}

