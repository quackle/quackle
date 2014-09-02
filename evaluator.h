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

#ifndef QUACKLE_EVALUATOR_H
#define QUACKLE_EVALUATOR_H

#include "alphabetparameters.h"

namespace Quackle
{

class GamePosition;
class Move;

class Evaluator
{
public:
	virtual ~Evaluator() {};

	// Default implementations of these return zero for everything but
	// equity, which returns the score of the move.

	// Return equity of move based on static evaluation
	// suitable for equity field of move. Rack must be alphabetized.
	virtual double equity(const GamePosition &position, const Move &move) const;

	virtual double playerConsideration(const GamePosition &position, const Move &move) const;
	virtual double sharedConsideration(const GamePosition &position, const Move &move) const;

	virtual double leaveValue(const LetterString &leave) const;
};

class ScorePlusLeaveEvaluator : public Evaluator
{
public:
	virtual ~ScorePlusLeaveEvaluator() {};

	// Evaluator that always returns a score+leave equity
	virtual double equity(const GamePosition &position, const Move &move) const;

	virtual double playerConsideration(const GamePosition &position, const Move &move) const;
	virtual double sharedConsideration(const GamePosition &position, const Move &move) const;

	virtual double leaveValue(const LetterString &leave) const;
};

}

#endif
