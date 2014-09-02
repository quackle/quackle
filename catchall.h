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

#ifndef QUACKLE_CATCHALL_H
#define QUACKLE_CATCHALL_H

#include "evaluator.h"

namespace Quackle
{

class CatchallEvaluator : public ScorePlusLeaveEvaluator
{
public:
	// Evaluator that returns score+leave equity for non-bag-empty positions,
	// otherwise returns approximate endgame equity
	virtual double equity(const GamePosition &position, const Move &move) const;
	
	double endgameResult(const GamePosition &position, const Move &move) const;
};

}

#endif
