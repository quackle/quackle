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

#include <iostream>
#include <math.h>

#include "computerplayer.h"
#include "datamanager.h"
#include "game.h"
#include "gameparameters.h"
#include "move.h"
#include "sim.h"
#include "strategyparameters.h"

// define this to get lame debugging messages
//#define DEBUG_SIM

using namespace Quackle;

Simulator::Simulator()
	: m_logfileIsOpen(false), m_hasHeader(false), m_dispatch(0), m_iterations(0), m_ignoreOppos(false)
{
	m_originalGame.addPosition();
}

Simulator::~Simulator()
{
	closeLogfile();
}

void Simulator::setPosition(const GamePosition &position)
{
	if (hasSimulationResults())
		writeLogFooter();

	m_originalGame.setCurrentPosition(position);

	m_consideredMoves.clear();
	m_simmedMoves.clear();
	MoveList::const_iterator end = m_originalGame.currentPosition().moves().end();
	for (MoveList::const_iterator it = m_originalGame.currentPosition().moves().begin(); it != end; ++it)
		m_simmedMoves.push_back(SimmedMove(*it));

	resetNumbers();
}

void Simulator::setLogfile(const string &logfile, bool append)
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

void Simulator::logMessage(const UVString &message)
{
	if (isLogging())
		m_logfileStream << message << endl;
}

void Simulator::closeLogfile()
{
	if (isLogging())
	{
		if (m_hasHeader)
			writeLogFooter();

		m_logfileStream.close();
		m_logfileIsOpen = false;
	}
}

void Simulator::writeLogHeader()
{
	if (isLogging())
	{
		m_logfileStream << "<simulation>" << endl;
		m_xmlIndent = MARK_UV("\t");

		m_hasHeader = true;

		// TODO include position data
	}
}

void Simulator::writeLogFooter()
{
	if (isLogging())
	{
		m_xmlIndent = MARK_UV("");
		m_logfileStream << "</simulation>" << endl;

		m_hasHeader = false;
	}
}

void Simulator::setDispatch(ComputerDispatch *dispatch)
{
	m_dispatch = dispatch;
}

void Simulator::setIncludedMoves(const MoveList &moves)
{
	for (SimmedMoveList::iterator simmedMoveIt = m_simmedMoves.begin(); simmedMoveIt != m_simmedMoves.end(); ++simmedMoveIt)
		(*simmedMoveIt).setIncludeInSimulation(false);

	MoveList::const_iterator end = moves.end();
	for (MoveList::const_iterator it = moves.begin(); it != end; ++it)
	{
		SimmedMoveList::iterator simmedMoveIt;
		for (simmedMoveIt = m_simmedMoves.begin(); simmedMoveIt != m_simmedMoves.end(); ++simmedMoveIt)
		{
			if ((*it) == (*simmedMoveIt).move)
			{
				(*simmedMoveIt).setIncludeInSimulation(true);
				break;
			}
		}

		// move wasn't found; add it
		if (simmedMoveIt == m_simmedMoves.end())
			m_simmedMoves.push_back(SimmedMove(*it));
	}
}

void Simulator::makeSureConsideredMovesAreIncluded()
{
	MoveList movesSuperset(moves(/* prune */ true, /* sort by win */ true));
	for (MoveList::const_iterator it = m_consideredMoves.begin(); it != m_consideredMoves.end(); ++it)
		if (!movesSuperset.contains(*it))
			movesSuperset.push_back(*it);
	setIncludedMoves(movesSuperset);
}

void Simulator::moveConsideredMovesToBeginning(MoveList &moves) const
{
	for (MoveList::const_iterator consideredIt = m_consideredMoves.begin(); consideredIt != m_consideredMoves.end(); ++consideredIt)
	{
		for (MoveList::iterator it = moves.begin(); it != moves.end(); ++it)
		{
			if (*consideredIt == *it)
			{
				moves.erase(it);
				moves.insert(moves.begin(), *consideredIt);
			}
		}
	}
}

void Simulator::addConsideredMove(const Move &move)
{
	m_consideredMoves.push_back(move);
}

bool Simulator::isConsideredMove(const Move &move) const
{
	const bool ret = m_consideredMoves.contains(move);
	return ret;
}

void Simulator::pruneTo(double equityThreshold, int maxNumberOfMoves)
{
	MoveList equityMoves(moves(/* prune unincluded */ true));
	MoveList toSetIncluded;
	const double absoluteEquityThreshold = equityMoves[0].equity - equityThreshold;

	const MoveList::const_iterator end = equityMoves.end();
	int i = 0;
	for (MoveList::const_iterator it = equityMoves.begin(); i < maxNumberOfMoves && it != end; ++it, ++i)
	{
		if ((*it).equity >= absoluteEquityThreshold)
			toSetIncluded.push_back(*it);
	}

	setIncludedMoves(toSetIncluded);
}

void Simulator::resetNumbers()
{
	SimmedMoveList::iterator end = m_simmedMoves.end();
	for (SimmedMoveList::iterator moveIt = m_simmedMoves.begin(); moveIt != end; ++moveIt)
		(*moveIt).clear();

	m_iterations = 0;
}

void Simulator::simulate(int plies, int iterations)
{
	for (int i = 0; i < iterations; ++i)
	{
		if (m_dispatch && m_dispatch->shouldAbort())
			break;
		simulate(plies);
	}
}

void Simulator::simulate(int plies)
{
#ifdef DEBUG_SIM
	UVcout << "let's simulate for " << plies << " plies" << endl;
#endif

	++m_iterations;

	randomizeOppoRacks();
	randomizeDrawingOrder();

	const int startPlayerId = m_originalGame.currentPosition().currentPlayer().id();
	const int numberOfPlayers = m_originalGame.currentPosition().players().size();

	if (plies < 0)
		plies = 1000;

	// specified plies doesn't include candidate play
	++plies;
	
	// level one's first move is the zeroth ply (the candidate)
	const int decimalTurns = (plies % numberOfPlayers);

	// also one-indexed
	const int levels = (int)((plies - decimalTurns) / numberOfPlayers);

	if (isLogging())
	{
		if (!m_hasHeader)
			writeLogHeader();

		m_logfileStream << m_xmlIndent << "<iteration index=\"" << m_iterations << "\">" << endl;
		m_xmlIndent += MARK_UV('\t');
	}

	SimmedMoveList::iterator moveEnd = m_simmedMoves.end();
	for (SimmedMoveList::iterator moveIt = m_simmedMoves.begin(); moveIt != moveEnd; ++moveIt)
	{
		if (!(*moveIt).includeInSimulation())
			continue;

#ifdef DEBUG_SIM
		UVcout << "simulating " << (*moveIt).move << ":" << endl;
#endif

		if (isLogging())
		{
			m_logfileStream << m_xmlIndent << "<playahead>" << endl;
			m_xmlIndent += MARK_UV('\t');
		}

		m_simulatedGame = m_originalGame;
		double residual = 0;

		(*moveIt).setNumberLevels(levels + 1);

		int levelNumber = 1;
		for (LevelList::iterator levelIt = (*moveIt).levels.begin(); levelNumber <= levels + 1 && levelIt != (*moveIt).levels.end() && !m_simulatedGame.currentPosition().gameOver(); ++levelIt, ++levelNumber)
		{
			const int decimal = levelNumber == levels + 1? decimalTurns : numberOfPlayers;
			if (decimal == 0)
				continue;

			(*levelIt).setNumberScores(decimal);

			int playerNumber = 1;
			for (PositionStatisticsList::iterator scoresIt = (*levelIt).statistics.begin(); scoresIt != (*levelIt).statistics.end() && !m_simulatedGame.currentPosition().gameOver(); ++scoresIt, ++playerNumber)
			{
				const int playerId = m_simulatedGame.currentPosition().currentPlayer().id();

				if (isLogging())
				{
					m_logfileStream << m_xmlIndent << "<ply index=\"" << (levelNumber - 1) * numberOfPlayers + playerNumber - 1 << "\">" << endl;
					m_xmlIndent += MARK_UV('\t');
				}

				Move move = Move::createNonmove();

				if (playerId == startPlayerId && levelNumber == 1)
					move = (*moveIt).move;
				else if (m_ignoreOppos && playerId != startPlayerId)
					move = Move::createPassMove();
				else
					move = m_simulatedGame.currentPosition().staticBestMove();

				int deadwoodScore = 0;
				if (m_simulatedGame.currentPosition().doesMoveEndGame(move))
				{
					LetterString deadwood;
					deadwoodScore = m_simulatedGame.currentPosition().deadwood(&deadwood);
					// account for deadwood in this move rather than a separate
					// UnusedTilesBonus move.
					move.score += deadwoodScore;
				}

				(*scoresIt).score.incorporateValue(move.score);
				(*scoresIt).bingos.incorporateValue(move.isBingo? 1.0 : 0.0);

				if (isLogging())
				{
					m_logfileStream << m_xmlIndent << m_simulatedGame.currentPosition().currentPlayer().rack().xml() << endl;
					m_logfileStream << m_xmlIndent << move.xml() << endl;
				}

				// record future-looking residuals
				bool isFinalTurnForPlayerOfSimulation = false;

				if (levelNumber == levels)
					isFinalTurnForPlayerOfSimulation = playerNumber > decimalTurns;
				else if (levelNumber == levels + 1)
					isFinalTurnForPlayerOfSimulation = playerNumber <= decimalTurns;

				const bool isVeryFinalTurnOfSimulation = (decimalTurns == 0 && levelNumber == levels && playerNumber == numberOfPlayers) || (levelNumber == levels + 1 && playerNumber == decimalTurns);

				if (isFinalTurnForPlayerOfSimulation && !(m_ignoreOppos && playerId != startPlayerId))
				{
					double residualAddend = m_simulatedGame.currentPosition().calculatePlayerConsideration(move);
					if (isLogging())
						m_logfileStream << m_xmlIndent << "<pc value=\"" << residualAddend << "\" />" << endl;

					if (isVeryFinalTurnOfSimulation)
					{
						// experimental -- do shared resource considerations
						// matter in a plied simulation?
	
						const double sharedResidual = m_simulatedGame.currentPosition().calculateSharedConsideration(move);
						residualAddend += sharedResidual;

						if (isLogging() && sharedResidual != 0)
							m_logfileStream << m_xmlIndent << "<sc value=\"" << sharedResidual << "\" />" << endl;
					}

					if (playerId == startPlayerId)
						residual += residualAddend;
					else
						residual -= residualAddend;
				}

				// commiting the move will account for deadwood again
				// so avoid double counting from above.
				move.score -= deadwoodScore; 
				m_simulatedGame.setCandidate(move);

				m_simulatedGame.commitCandidate(!isVeryFinalTurnOfSimulation);

				if (isLogging())
				{
					m_xmlIndent = m_xmlIndent.substr(0, m_xmlIndent.length() - 1);
					m_logfileStream << m_xmlIndent << "</ply>" << endl;
				}
			}
		}

		(*moveIt).residual.incorporateValue(residual);

		const int spread = m_simulatedGame.currentPosition().spread(startPlayerId);
		(*moveIt).gameSpread.incorporateValue(spread);

		if (m_simulatedGame.currentPosition().gameOver())
		{
			const float wins = spread > 0? 1 : spread == 0? 0.5F : 0;
			(*moveIt).wins.incorporateValue(wins);

			if (isLogging())
			{
				m_logfileStream << m_xmlIndent << "<gameover win=\"" << wins << "\" />" << endl;
			}
		}
		else
		{
			if (m_simulatedGame.currentPosition().currentPlayer().id() == startPlayerId)
				(*moveIt).wins.incorporateValue(QUACKLE_STRATEGY_PARAMETERS->bogowin((int)(spread + residual), m_simulatedGame.currentPosition().bag().size() + QUACKLE_PARAMETERS->rackSize(), 0));
			else
				(*moveIt).wins.incorporateValue(1.0 - QUACKLE_STRATEGY_PARAMETERS->bogowin((int)(-spread - residual), m_simulatedGame.currentPosition().bag().size() + QUACKLE_PARAMETERS->rackSize(), 0));
		}	
		

		if (isLogging())
		{
			m_xmlIndent = m_xmlIndent.substr(0, m_xmlIndent.length() - 1);
			m_logfileStream << m_xmlIndent << "</playahead>" << endl;
		}
	}

	if (isLogging())
	{
		m_xmlIndent = m_xmlIndent.substr(0, m_xmlIndent.length() - 1);
		m_logfileStream << m_xmlIndent << "</iteration>" << endl;
	}
}

void Simulator::randomizeOppoRacks()
{
#ifdef DEBUG_SIM
	UVcout << "RANDOMIZE OPPO RACKS " << endl;
#endif

	m_originalGame.currentPosition().ensureProperBag();

	Bag bag(m_originalGame.currentPosition().unseenBag());

	const PlayerList::const_iterator end = m_originalGame.currentPosition().players().end();
	for (PlayerList::const_iterator it = m_originalGame.currentPosition().players().begin(); it != end; ++it)
	{
		if (((*it) == m_originalGame.currentPosition().currentPlayer()))
			continue;

		// TODO -- some kind of inference engine can be inserted here
		Rack rack = m_partialOppoRack;

		// We must refill the partial rack from a bag that does not 
		// contain the partial rack.
		bag.removeLetters(rack.tiles());
		bag.refill(rack);

		m_originalGame.currentPosition().setPlayerRack((*it).id(), rack, /* adjust bag */ true);
	}

#ifdef DEBUG_SIM
	UVcout << "RANDOMIZE OPPO RACKS DONE" << endl;
#endif

	m_originalGame.currentPosition().ensureProperBag();
}

void Simulator::setPartialOppoRack(const Rack &rack)
{
	m_partialOppoRack = rack;
}

void Simulator::randomizeDrawingOrder()
{
	m_originalGame.currentPosition().setDrawingOrder(m_originalGame.currentPosition().bag().someShuffledTiles());
}

MoveList Simulator::moves(bool prune, bool byWin) const
{
	MoveList ret;

	const bool useCalculatedEquity = hasSimulationResults();

	const SimmedMoveList::const_iterator end = m_simmedMoves.end();
	for (SimmedMoveList::const_iterator it = m_simmedMoves.begin(); it != end; ++it)
	{
		if (prune && !(*it).includeInSimulation())
			continue;

		Move move((*it).move);

		if (useCalculatedEquity)
		{
			move.equity = (*it).calculateEquity();
			move.win = (*it).wins.averagedValue();
		}

		ret.push_back(move);
	}

	if (byWin && useCalculatedEquity)
		MoveList::sort(ret, MoveList::Win);
	else
		MoveList::sort(ret, MoveList::Equity);

	return ret;
}

const SimmedMove &Simulator::simmedMoveForMove(const Move &move) const
{
	const SimmedMoveList::const_iterator end = m_simmedMoves.end();
	for (SimmedMoveList::const_iterator it = m_simmedMoves.begin(); it != end; ++it)
		if ((*it).move == move)
			return *it;
	
	return m_simmedMoves.back();
}

int Simulator::numLevels() const
{
	if (m_simmedMoves.empty())
		return 0;
	return m_simmedMoves.front().levels.size();
}

int Simulator::numPlayersAtLevel(int levelIndex) const
{
	if (m_simmedMoves.empty())
		return 0;
	return m_simmedMoves.front().levels[levelIndex].statistics.size();
}

////////////

double AveragedValue::standardDeviation() const
{
	return m_incorporatedValues <= 1 ? 0 :
		sqrt(
				(m_incorporatedValues * m_squaredValueSum - m_valueSum * m_valueSum)
				/ (m_incorporatedValues * (m_incorporatedValues - 1))
			);
}

void AveragedValue::clear()
{
	m_valueSum = 0;
	m_squaredValueSum = 0;
	m_incorporatedValues = 0;
}

////////////

double SimmedMove::calculateEquity() const
{
	if (levels.empty())
	{
		return move.equity;
	}

	double equity = 0;

	for (LevelList::const_iterator levelIt = levels.begin(); levelIt != levels.end(); ++levelIt)
	{
		for (PositionStatisticsList::const_iterator scoresIt = (*levelIt).statistics.begin(); scoresIt != (*levelIt).statistics.end(); ++scoresIt)
		{
			if (scoresIt == (*levelIt).statistics.begin())
				equity += (*scoresIt).score.averagedValue();
			else
				equity -= (*scoresIt).score.averagedValue();
		}
	}

	equity += residual.averagedValue();

	return equity;
}

double SimmedMove::calculateWinPercentage() const
{
	return wins.hasValues()? wins.averagedValue() * 100 : move.win;
}

void SimmedMove::setNumberLevels(unsigned int number)
{
	while (levels.size() < number)
		levels.push_back(Level());
}

void SimmedMove::clear()
{
	levels.clear();
}

PositionStatistics SimmedMove::getPositionStatistics(int level, int playerIndex) const
{
	return levels[level].statistics[playerIndex];
}

AveragedValue PositionStatistics::getStatistic(StatisticType type) const
{
	switch (type)
	{
	case StatisticScore:
		return score;
	case StatisticBingos:
		return bingos;
	}

	return AveragedValue();
}

////////////

void Level::setNumberScores(unsigned int number)
{
	while (statistics.size() < number)
		statistics.push_back(PositionStatistics());
}

//////////

UVOStream& operator<<(UVOStream &o, const Quackle::AveragedValue &value)
{
	o << "[" << value.valueSum() << "/" << value.incorporatedValues() << "=" << value.averagedValue() << " sd " << value.standardDeviation() << "]";
    return o;
}

UVOStream& operator<<(UVOStream &o, const Quackle::PositionStatistics &value)
{
	o << "Stats: score " << value.score << ", bingos " << value.bingos << endl;
    return o;
}

UVOStream& operator<<(UVOStream &o, const Quackle::Level &level)
{
	for (Quackle::PositionStatisticsList::const_iterator it = level.statistics.begin(); it != level.statistics.end(); ++it)
		o << *it;
    return o;
}

UVOStream& operator<<(UVOStream &o, const Quackle::SimmedMove &move)
{
	o << "Simmed move " << move.move << ":";

	int levelNumber = 1;
	for (Quackle::LevelList::const_iterator it = move.levels.begin(); it != move.levels.end(); ++it, ++levelNumber)
		o << endl << "level " << levelNumber << ": " << (*it);
	
	o << endl;
	o << "Being simmed: " << move.includeInSimulation() << endl;
	o << "Residual: " << move.residual << endl;
	o << "Spread: " << move.gameSpread << endl;
	o << "Wins: " << move.wins << endl;
    return o;
}

UVOStream& operator<<(UVOStream& o, const Quackle::SimmedMoveList& moves)
{
	const Quackle::SimmedMoveList::const_iterator end(moves.end());
	for (Quackle::SimmedMoveList::const_iterator it = moves.begin(); it != end; ++it)
		o << (*it) << endl;
    return o;
}

