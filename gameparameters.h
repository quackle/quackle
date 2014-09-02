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

#ifndef QUACKLE_GAMEPARAMETERS_H
#define QUACKLE_GAMEPARAMETERS_H

namespace Quackle
{

class GameParameters
{
public:
	GameParameters();

	int minimumTilesForExchange() const;
	void setMinimumTilesForExchange(int minimumTilesForExchange);

	// negative number -> no limit
	int numberOfScorelessTurnsThatEndsGame() const;
	void setNumberOfScorelessTurnsThatEndsGame(int numberOfScorelessTurnsThatEndsGame);

	int bingoBonus() const;
	void setBingoBonus(int bingoBonus);

	int rackSize() const;
	void setRackSize(int rackSize);

	unsigned int overdrawPenalty() const;
	void setOverdrawPenalty(unsigned int overdrawPenalty);

protected:
	int m_minimumTilesForExchange;
	int m_numberOfScorelessTurnsThatEndsGame;
	int m_bingoBonus;
	int m_rackSize;
	unsigned int m_overdrawPenalty;
};

inline int GameParameters::minimumTilesForExchange() const
{
	return m_minimumTilesForExchange;
}

inline int GameParameters::numberOfScorelessTurnsThatEndsGame() const
{
	return m_numberOfScorelessTurnsThatEndsGame;
}

inline void GameParameters::setNumberOfScorelessTurnsThatEndsGame(int numberOfScorelessTurnsThatEndsGame)
{
	m_numberOfScorelessTurnsThatEndsGame = numberOfScorelessTurnsThatEndsGame;
}

inline int GameParameters::bingoBonus() const
{
	return m_bingoBonus;
}

inline int GameParameters::rackSize() const
{
	return m_rackSize;
}

inline unsigned int GameParameters::overdrawPenalty() const
{
	return m_overdrawPenalty;
}

inline void GameParameters::setMinimumTilesForExchange(int minimumTilesForExchange)
{
	m_minimumTilesForExchange = minimumTilesForExchange;
}

inline void GameParameters::setBingoBonus(int bingoBonus)
{
	m_bingoBonus = bingoBonus;
}

inline void GameParameters::setRackSize(int rackSize)
{
	m_rackSize = rackSize;
}

inline void GameParameters::setOverdrawPenalty(unsigned int overdrawPenalty)
{
	m_overdrawPenalty = overdrawPenalty;
}

class EnglishParameters : public GameParameters
{
public:
	EnglishParameters();
};

}

#endif
