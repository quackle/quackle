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

#include <algorithm>
#include <iostream>

#include "computerplayer.h"
#include "endgame.h"
#include "game.h"
#include "move.h"

// define this to get lame debugging messages
// #define DEBUG_ENDGAME

using namespace Quackle;
using namespace std;

Endgame::Endgame()
	: m_logfileIsOpen(false), m_hasHeader(false), m_dispatch(0)
{
	m_originalGame.addPosition();

	m_subnestedInitialPlayNumber = 1;
	m_subnestedDisappointPlayNumber = 1;

	m_nestedInitialPlayNumber = 14;
	m_nestedDisappointPlayNumber = 7;

	m_unnestedInitialPlayNumber = 320;
	m_unnestedDisappointPlayNumber = 160;
}

Endgame::~Endgame()
{
	closeLogfile();
}

void Endgame::setPosition(const GamePosition &position)
{
	writeLogFooter();

	m_originalGame.setCurrentPosition(position);

	m_endgameMoves.clear();
	MoveList::const_iterator end = m_originalGame.currentPosition().moves().end();
	for (MoveList::const_iterator it = m_originalGame.currentPosition().moves().begin(); it != end; ++it)
		m_endgameMoves.push_back(EndgameMove(*it));
}

void Endgame::setDispatch(ComputerDispatch *dispatch)
{
	m_dispatch = dispatch;
}

void Endgame::setLogfile(const string &logfile, bool append)
{
	if (m_logfile == logfile && isLogging())
		return;

	closeLogfile();
	m_logfile = logfile;

	if (m_logfile.empty())
	{
		closeLogfile();
		return;
	}

	const ios::openmode flags = append? (ios::out | ios::app) : ios::out;
	m_logfileStream.open(m_logfile.c_str(), flags);

	m_logfileIsOpen = m_logfileStream.is_open();
	if (!m_logfileIsOpen)
		cerr << "Could not open " << m_logfile << " to write simulation log" << endl;

	m_hasHeader = false;
}

void Endgame::logMessage(const UVString &message)
{
	if (isLogging())
		m_logfileStream << message << endl;
}

void Endgame::closeLogfile()
{
	if (isLogging())
	{
		if (m_hasHeader)
			writeLogFooter();

		m_logfileStream.close();
		m_logfileIsOpen = false;
	}
}

void Endgame::writeLogHeader()
{
	if (isLogging())
	{
		m_logfileStream << "<simulation>" << endl;
		m_xmlIndent = MARK_UV("\t");

		m_hasHeader = true;

		// TODO include position data
	}
}

void Endgame::writeLogFooter()
{
	if (isLogging())
	{
		m_xmlIndent = MARK_UV("");
		m_logfileStream << "</simulation>" << endl;

		m_hasHeader = false;
	}
}

void Endgame::setIncludedMoves(const MoveList &moves)
{
	m_endgameMoves.clear();
	for (MoveList::const_iterator it = moves.begin(); it != moves.end(); ++it)
	{
		// UVcout << "adding move: " << (*it) << endl;
		EndgameMoveList::iterator endgameMoveIt;
		for (endgameMoveIt = m_endgameMoves.begin(); endgameMoveIt != m_endgameMoves.end(); ++endgameMoveIt)
		{
			if ((*it) == (*endgameMoveIt).move)
				break;
		}

		// move wasn't found; add it
		if (endgameMoveIt == m_endgameMoves.end())
			m_endgameMoves.push_back(EndgameMove(*it));
	}
}

/*
void Endgame::pruneTo(double equityThreshold, int maxNumberOfMoves)
{
	MoveList equityMoves(moves());
	MoveList toSetIncluded;
	const double absoluteEquityThreshold = equityMoves[0].equity - equityThreshold;

	const MoveList::const_iterator end = equityMoves.end();
	int i = 0;
	for (MoveList::const_iterator it = equityMoves.begin(); i < maxNumberOfMoves && it != end; ++it, ++i)
		if ((*it).equity >= absoluteEquityThreshold)
			toSetIncluded.push_back(*it);

	setIncludedMoves(toSetIncluded);
}
*/

double Endgame::disappoint(EndgameMove &hope, double bestPessimistic)
{
#ifdef DEBUG_ENDGAME
	UVcout << "    disappoint() called" << endl;
#endif

	double newOptimistic = hope.optimistic;
	
	const int realStartPlayerId = m_originalGame.currentPosition().currentPlayer().id();
	
	double beforeSpread = m_originalGame.currentPosition().spread(realStartPlayerId);
	
	m_endgameGame = m_originalGame;
	m_endgameGame.setCandidate(hope.move);
	m_endgameGame.commitCandidate(true);
	
	const int startPlayerId = m_endgameGame.currentPosition().currentPlayer().id();
	const size_t numberOfPlayers = m_originalGame.currentPosition().players().size();

	int initialPlayNumber;
	unsigned int nestedness = m_originalGame.currentPosition().nestedness();
	if (nestedness > 1)
		initialPlayNumber = m_subnestedDisappointPlayNumber;
	else if (nestedness == 1)
		initialPlayNumber = m_nestedDisappointPlayNumber;
	else
		initialPlayNumber = m_unnestedDisappointPlayNumber;
		
	//int initialPlayNumber = m_originalGame.currentPosition().nestedness() > 0 ? m_nestedDisappointPlayNumber : m_unnestedDisappointPlayNumber;

	m_endgameGame.currentPosition().kibitz(initialPlayNumber);
	
	MoveList moves = m_endgameGame.currentPosition().moves();
	
	MoveList::const_iterator moveIt = moves.begin();

#ifdef DEBUG_ENDGAME		
	UVcout << "    disappoint's moves has " << moves.size() << " moves." << endl;
#endif	
	while((newOptimistic > bestPessimistic) && moveIt != moves.end())
	{
#ifdef DEBUG_ENDGAME
		UVcout << "    seeing if " << *moveIt << " wrecks us." << endl;
#endif
		m_endgameGame = m_originalGame;
		m_endgameGame.setCandidate(hope.move);
		m_endgameGame.commitCandidate(true);

		size_t levelNumber = 1;
		size_t playerNumber = 1;

		while (!m_endgameGame.currentPosition().gameOver())
		{
			for (playerNumber = 1; 
				 (playerNumber <= numberOfPlayers) && 
				 !m_endgameGame.currentPosition().gameOver();
				 playerNumber++)
			{
				const int playerId = m_endgameGame.currentPosition().currentPlayer().id();

				Move move = Move::createNonmove();

				if (playerId == startPlayerId && levelNumber == 1)
					move = (*moveIt);
				else
					move = m_endgameGame.currentPosition().staticBestMove();
				
#ifdef DEBUG_ENDGAME
				UVcout << "      level:" << levelNumber << ", player: " << playerId << ", move: " << move << ", score: " << move.score << ", equity: " << move.equity << endl;
#endif
				m_endgameGame.setCandidate(move);
				m_endgameGame.commitCandidate(true);
			}

			levelNumber++;
		}

		m_endgameGame.currentPosition().adjustScoresToFinishGame();

		double afterSpread = m_endgameGame.currentPosition().spread(realStartPlayerId);
		
		double spread = afterSpread - beforeSpread;

		if (spread < newOptimistic)
			newOptimistic = spread;

		moveIt++;
	}

	return newOptimistic;
}

Move Endgame::solve(int /* nestedness */)
{
#ifdef DEBUG_ENDGAME
	UVcout << "Endgame::solve() called with position:" << endl;
#endif
	
#ifdef DEBUG_ENDGAME
	UVcout << m_originalGame.currentPosition() << endl;
#endif

	int initialPlayNumber;
	unsigned int origNestedness = m_originalGame.currentPosition().nestedness();
	if (origNestedness > 1)
		initialPlayNumber = m_subnestedInitialPlayNumber;
	else if (origNestedness == 1)
		initialPlayNumber = m_nestedInitialPlayNumber;
	else
		initialPlayNumber = m_unnestedInitialPlayNumber;
	
	currentPosition().kibitz(initialPlayNumber);
	setIncludedMoves(currentPosition().moves());

	const int startPlayerId = m_originalGame.currentPosition().currentPlayer().id();
	const size_t numberOfPlayers = m_originalGame.currentPosition().players().size();

	double bestPessimistic = -1000;
	EndgameMove bestPessMove(Move::createNonmove());
	
	EndgameMoveList::iterator moveEnd = m_endgameMoves.end();
	for (EndgameMoveList::iterator moveIt = m_endgameMoves.begin(); moveIt != moveEnd; ++moveIt)
	{
		if (m_dispatch && m_dispatch->shouldAbort())
		{
			break;
		}

#ifdef DEBUG_ENDGAME
		UVcout << "naively playing out " << (*moveIt).move << ":" << endl;
#endif

		m_endgameGame = m_originalGame;
		
		double beforeSpread = m_endgameGame.currentPosition().spread(startPlayerId);
		
		size_t levelNumber = 1;
		size_t playerNumber = 1;

		while (!m_endgameGame.currentPosition().gameOver())
		{
			for (playerNumber = 1; 
				 (playerNumber <= numberOfPlayers) && 
				 !m_endgameGame.currentPosition().gameOver();
				 playerNumber++)
			{
				const int playerId = m_endgameGame.currentPosition().currentPlayer().id();

				Move move = Move::createNonmove();

				if (playerId == startPlayerId && levelNumber == 1)
					move = (*moveIt).move;
				else
					move = m_endgameGame.currentPosition().staticBestMove();
				
#ifdef DEBUG_ENDGAME
				UVcout << "    level:" << levelNumber << ", player: " << playerId << ", move: " << move << ", score: " << move.score << ", equity: " << move.equity << endl;
#endif
				m_endgameGame.setCandidate(move);
				m_endgameGame.commitCandidate(true);
			}

			levelNumber++;
		}

		if ((playerNumber == 2) && (levelNumber == 2))
			(*moveIt).outplay = true;
		else
			(*moveIt).outplay = false;
			
		m_endgameGame.currentPosition().adjustScoresToFinishGame();

		double afterSpread = m_endgameGame.currentPosition().spread(startPlayerId);
		
		double spread = afterSpread - beforeSpread;
		
#ifdef DEBUG_ENDGAME
		UVcout << "    spread: " << spread << endl;
#endif
	
		(*moveIt).optimistic = spread;
		(*moveIt).estimated = spread;

		if ((*moveIt).outplay)
			(*moveIt).pessimistic = spread;
		else
			(*moveIt).pessimistic = -1000;
	
		if ((*moveIt).pessimistic >= bestPessimistic)
		{
			bestPessimistic = (*moveIt).pessimistic;
			bestPessMove = (*moveIt);
		}
	}
	
	stable_sort(m_endgameMoves.begin(), m_endgameMoves.end(), EndgameMoveList::optimisticComparator);

	for (EndgameMoveList::iterator it = m_endgameMoves.begin(); it != m_endgameMoves.end(); ++it)
	{
#ifdef DEBUG_ENDGAME
		UVcout << (*it).move << ", optimistic: " << (*it).optimistic << ", pessimistic: " << (*it).pessimistic << endl;
#endif
		if ((*it).optimistic < bestPessimistic)
		{
			goto found_best_pessimistic_move;
		}
		
		if (!((*it).outplay))
		{
#ifdef DEBUG_ENDGAME
			UVcout << (*it) << "  original optimism: " << (*it).optimistic << endl;
#endif
			(*it).optimistic = disappoint((*it), bestPessimistic);
	
			if ((*it).optimistic > bestPessimistic)
				(*it).pessimistic = (*it).optimistic;

#ifdef DEBUG_ENDGAME
			UVcout << (*it) << "  disappointed optimism: " << (*it).optimistic << endl;
#endif
			if ((*it).pessimistic > bestPessimistic)
			{
				bestPessMove = (*it);
				bestPessimistic = (*it).pessimistic;
			}
		}
	}

	found_best_pessimistic_move:
	reallyPlayOut(bestPessMove.move, 0);
	return bestPessMove.move;
}

void Endgame::reallyPlayOut(Move &chosenMove, int nestedness)
{
	const int startPlayerId = m_originalGame.currentPosition().currentPlayer().id();
	const size_t numberOfPlayers = m_originalGame.currentPosition().players().size();

	Game playoutGame = m_originalGame;
		
	size_t levelNumber = 1;
	size_t playerNumber = 1;

	double beforeSpread = playoutGame.currentPosition().spread(startPlayerId);

	while (!playoutGame.currentPosition().gameOver())
	{
		for (playerNumber = 1; 
			 (playerNumber <= numberOfPlayers) && 
			 !playoutGame.currentPosition().gameOver();
			 playerNumber++)
		{
			const int playerId = playoutGame.currentPosition().currentPlayer().id();

			Move move = Move::createNonmove();

			if (playerId == startPlayerId && levelNumber == 1)
			{
				move = chosenMove;
			}
			else
			{
				Endgame quickieEndgame;
				quickieEndgame.setPosition(playoutGame.currentPosition());
				move = quickieEndgame.solve(nestedness);
			}
			
#ifdef DEBUG_ENDGAME
			UVcout << "    level:" << levelNumber << ", player: " << playerId << ", move: " << move << ", score: " << move.score << ", equity: " << move.equity << endl;
#endif
			
			playoutGame.setCandidate(move);
			playoutGame.commitCandidate(true);
		}

		levelNumber++;
	}

	playoutGame.currentPosition().adjustScoresToFinishGame();

	double afterSpread = playoutGame.currentPosition().spread(startPlayerId);
    double spread = afterSpread - beforeSpread;

#ifdef DEBUG_ENDGAME
	UVcout << "afterSpread: " << afterSpread << endl;
	UVcout << "spread: " << spread << endl;
#endif
	
	if (afterSpread > 0) chosenMove.win = 1.0;
	if (afterSpread < 0) chosenMove.win = 0.0;
	if (afterSpread == 0) chosenMove.win = 0.5;

	chosenMove.equity = spread;
}

bool EndgameMoveList::optimisticComparator(const EndgameMove &move1, const EndgameMove &move2)
{
	return move1.optimistic > move2.optimistic;
}

MoveList Endgame::moves(unsigned int nmoves)
{
	if (m_dispatch)
	{
		m_dispatch->signalFractionDone(0);
	}

	Move best = solve(0);
	double bestEquity = best.equity;

	MoveList playout;
	MoveList ret;

	size_t maxPlayedOut = nmoves;

	if (maxPlayedOut > m_endgameMoves.size())
		maxPlayedOut = m_endgameMoves.size();

	playout.push_back(best);

	size_t i = 0;
	const EndgameMoveList::const_iterator end = m_endgameMoves.end();
	for (EndgameMoveList::const_iterator it = m_endgameMoves.begin(); (it != end) && (i < maxPlayedOut); ++it)
	{
		if (m_dispatch && m_dispatch->shouldAbort())
		{
			break;
		}

		Move move((*it).move);
		if (!(move == best)) 
		{
			reallyPlayOut(move, 1);
			if (move.equity > bestEquity)
			{
				reallyPlayOut(move, 0);
				if (move.equity > bestEquity)
					bestEquity = move.equity;
			}
			playout.push_back(move);
		}
		i++;

		if (m_dispatch)
		{
			m_dispatch->signalFractionDone(static_cast<float>(i) / static_cast<float>(m_endgameMoves.size() < maxPlayedOut? m_endgameMoves.size() : maxPlayedOut));
		}
	}

	MoveList::sort(playout, MoveList::Equity);

	i = 0;
	for (MoveList::const_iterator it = playout.begin(); (it != playout.end()) && (i < nmoves); ++it)
	{
		ret.push_back(*it);
		i++;
	}

	return ret;
}

////////////

UVOStream& operator<<(UVOStream &o, const Quackle::EndgameMove &move)
{
	o << "Endgame move " << move.move << ":";
	o << endl;
    return o;
}

UVOStream& operator<<(UVOStream& o, const Quackle::EndgameMoveList& moves)
{
	const Quackle::EndgameMoveList::const_iterator end(moves.end());
	for (Quackle::EndgameMoveList::const_iterator it = moves.begin(); it != end; ++it)
		o << (*it) << endl;
    return o;
}

