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

#ifndef QUACKLE_SIM_H
#define QUACKLE_SIM_H

#include <vector>

#include "alphabetparameters.h"
#include "game.h"

namespace Quackle
{

class ComputerDispatch;

struct AveragedValue
{
    // new zeroed value
    AveragedValue()
        : m_valueSum(0), m_squaredValueSum(0), m_incorporatedValues(0)
    {
    }

    void incorporateValue(double newValue);

    // zero everything
    void clear();

    long double valueSum() const;
    long double squaredValueSum() const;
    long int incorporatedValues() const;

    // whether or not incorporatedValues is greater than zero
    bool hasValues() const;

    // valueSum() / incorporatedValues() or zero
    // if there have been no incorporated values
    double averagedValue() const;

    // sqrt((incorporatedValues() * squaredValueSum() - valueSum()^2) / (incorporatedValues() * (incorporatedValues() - 1)))
    double standardDeviation() const;

private:
    long double m_valueSum;
    long double m_squaredValueSum;
    long int m_incorporatedValues;
};

inline void AveragedValue::incorporateValue(double newValue)
{
    m_valueSum += newValue;
    m_squaredValueSum += newValue * newValue;
    ++m_incorporatedValues;
}

inline long double AveragedValue::valueSum() const
{
    return m_valueSum;
}

inline long double AveragedValue::squaredValueSum() const
{
    return m_squaredValueSum;
}

inline double AveragedValue::averagedValue() const
{
    return m_incorporatedValues == 0? 0 : (m_valueSum / m_incorporatedValues);
}

inline long int AveragedValue::incorporatedValues() const
{
    return m_incorporatedValues;
}

inline bool AveragedValue::hasValues() const
{
    return m_incorporatedValues > 0;
}

struct PositionStatistics 
{
    enum StatisticType { StatisticScore, StatisticBingos };
    AveragedValue getStatistic(StatisticType type) const;

    AveragedValue score;
    AveragedValue bingos;
};

typedef vector<PositionStatistics> PositionStatisticsList;

struct Level
{
    // expand the scores list to be at least number long
    void setNumberScores(unsigned int number);

    PositionStatisticsList statistics;
};

typedef vector<Level> LevelList;

struct SimmedMove
{
    SimmedMove(const Move &_move) : move(_move), m_includeInSimulation(true) { }

    // + our scores - their scores + residual, except if we have no levels,
    // in which case returns move.equity
    double calculateEquity() const;

    // average wins value * 100, except if we have no win percentage data,
    // in which case returns move.win
    double calculateWinPercentage() const;

    // expand the levels list to be at least number long
    void setNumberLevels(unsigned int number);

    // clear all level values
    void clear();

    bool includeInSimulation() const;
    void setIncludeInSimulation(bool includeInSimulation);

    Move move;
    LevelList levels;
    AveragedValue residual;
    AveragedValue gameSpread;
    AveragedValue wins;

    PositionStatistics getPositionStatistics(int level, int playerIndex) const;

private:
    bool m_includeInSimulation;
};

inline bool SimmedMove::includeInSimulation() const
{
    return m_includeInSimulation;
}

inline void SimmedMove::setIncludeInSimulation(bool includeInSimulation)
{
    m_includeInSimulation = includeInSimulation;
}

typedef vector<SimmedMove> SimmedMoveList;

class Simulator
{
public:
    // constructs a new simulator
    Simulator();
    ~Simulator();

    // Simulate moves on this position. Also initializes the
    // move list, rack, resets numbers, and closes logfile.
    void setPosition(const GamePosition &position);

    // get access to the position that starts each playahead of the
    // simulation; use to rechange rack or scores etcetera
    GamePosition &currentPosition();
    const GamePosition &currentPosition() const;

    const History &history() const;

    // If logfile is an empty string, logging is disabled.
    // If logfile is the same logfile as currently set, nothing
    // happens. If it is different, old logfile is closed if it
    // was open. If append is false, this destroys file contents
    // already in logfile.
    void setLogfile(const string &logfile, bool append = true);
    string logfile() const;

    // Will honor dispatch->shouldAbort() but won't signal
    // any doneness for now.
    void setDispatch(ComputerDispatch *dispatch);
    ComputerDispatch *dispatch() const;

    // append message to logfile if one is open
    void logMessage(const UVString &message);

    bool isLogging() const;
    void closeLogfile();

    // Set moves to include in simulation. Moves that
    // are in the simmed list now that are not in this given
    // list still live in the simmed list but are not iterated thru
    // during simulation. Moves that were not in the simmed list are added
    // to front of simmed move list and level values zeroed.
    void setIncludedMoves(const MoveList &moves);

    void makeSureConsideredMovesAreIncluded();
    void moveConsideredMovesToBeginning(MoveList &moves) const;

    // moves that are immune from pruning
    void setConsideredMoves(const MoveList &moves);
    const MoveList &consideredMoves() const;
    void addConsideredMove(const Move &move);
    bool isConsideredMove(const Move &move) const;

    // include only currently included moves that are within
    // equityThreshold points below the best play and cap at
    // maxNumberOfMoves
    void pruneTo(double equityThreshold, int maxNumberOfMoves);

    // if ignore is true, oppos will always pass in simulation
    void setIgnoreOppos(bool ignore);
    bool ignoreOppos() const;

    // set values for all levels of all moves to zero
    void resetNumbers();

    // Run a chunk of the simulation.
    // If plies is negative, simulation runs to end of game.
    // Iterations is how many iterations to run before returning;
    // more iterations can be computed and incorporated by recalling 
    // simulate().
    void simulate(int plies, int iterations);

    // simulate one iteration
    void simulate(int plies);

    // Set oppo's rack to some partially-known tiles.
    // Set this to an empty rack if no tiles are known, so
    // that all tiles are chosen randomly each iteration.
    void setPartialOppoRack(const Rack &rack);
    const Rack &partialOppoRack() const;

    // Set oppo's racks to something random, including
    // tiles specified by setPartialOppoRack above.
    // Possibly inference-aided randomness.
    void randomizeOppoRacks();

    // set drawing order for the first refill
    void randomizeDrawingOrder();

    // returns maximal number of iterations over all moves since
    // resetting numbers
    int iterations() const;

    // returns true if any iterations have been run
    bool hasSimulationResults() const;

    // full simulation information
    const SimmedMoveList &simmedMoves() const;

    // Return the moves sorted by simulated equity.
    // If prune is true, does not include plays that aren't included
    // in simulation anymore.
    MoveList moves(bool prune = false, bool byWin = false) const;

    const SimmedMove &simmedMoveForMove(const Move &move) const;

    int numLevels() const;
    int numPlayersAtLevel(int levelIndex) const;

protected:
    void writeLogHeader();
    void writeLogFooter();

    UVOFStream m_logfileStream;
    string m_logfile;
    bool m_logfileIsOpen;
    bool m_hasHeader;
    UVString m_xmlIndent;

    Rack m_partialOppoRack;

    Game m_originalGame;
    Game m_simulatedGame;
    ComputerDispatch *m_dispatch;

    SimmedMoveList m_simmedMoves;

    // moves that are immune from pruning
    MoveList m_consideredMoves;

    int m_iterations;
    bool m_ignoreOppos;
};

inline GamePosition &Simulator::currentPosition()
{
    return m_originalGame.currentPosition();
}

inline const GamePosition &Simulator::currentPosition() const
{
    return m_originalGame.currentPosition();
}

inline const History &Simulator::history() const
{
    return m_originalGame.history();
}

inline ComputerDispatch *Simulator::dispatch() const
{
	return m_dispatch;
}

inline string Simulator::logfile() const
{
	return m_logfile;
}

inline bool Simulator::isLogging() const
{
	return m_logfileIsOpen;
}

inline const Rack &Simulator::partialOppoRack() const
{
	return m_partialOppoRack;
}

inline void Simulator::setConsideredMoves(const MoveList &moves)
{
	m_consideredMoves = moves;
}

inline const MoveList &Simulator::consideredMoves() const
{
	return m_consideredMoves;
}

inline void Simulator::setIgnoreOppos(bool ignore)
{
	m_ignoreOppos = ignore;
}

inline bool Simulator::ignoreOppos() const
{
	return m_ignoreOppos;
}

inline int Simulator::iterations() const
{
	return m_iterations;
}

inline bool Simulator::hasSimulationResults() const
{
	return m_iterations > 0;
}

inline const SimmedMoveList &Simulator::simmedMoves() const
{
	return m_simmedMoves;
}

}

UVOStream& operator<<(UVOStream& o, const Quackle::AveragedValue &value);
UVOStream& operator<<(UVOStream& o, const Quackle::Level &level);
UVOStream& operator<<(UVOStream& o, const Quackle::SimmedMove &move);
UVOStream& operator<<(UVOStream& o, const Quackle::SimmedMoveList &move);

#endif
