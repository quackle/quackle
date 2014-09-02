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

#ifndef QUACKLE_ENDGAME_H
#define QUACKLE_ENDGAME_H

#include <fstream>
#include <math.h>
#include <vector>

#include "alphabetparameters.h"
#include "game.h"

namespace Quackle
{

struct EndgameMove
{
	EndgameMove(const Move &_move) : move(_move), optimistic(0), pessimistic(0), estimated(0), outplay(false) { }
	Move move;
	double optimistic;
	double pessimistic;
	double estimated;
	bool outplay;
};

class EndgameMoveList : public vector<EndgameMove>
{
public:
	static bool optimisticComparator(const EndgameMove &move1, const EndgameMove &move2);
};

class Endgame
{
public:
	// constructs a new endgame solver
	Endgame();
	~Endgame();

	// Find the best move in this position. Also initializes the
	// move list, rack, resets numbers, and closes logfile.
	void setPosition(const GamePosition &position);

	// get access to the position that starts each playahead of the
	// endgame; use to rechange rack or scores etcetera
	GamePosition &currentPosition();
	const GamePosition &currentPosition() const;

	void setDispatch(ComputerDispatch *dispatch);

	// If logfile is an empty string, logging is disabled.
	// If logfile is the same logfile as currently set, nothing
	// happens. If it is different, old logfile is closed if it
	// was open. If append is false, this destroys file contents
	// already in logfile.
	void setLogfile(const string &logfile, bool append = true);
	string logfile() const;

	// append message to logfile if one is open
	void logMessage(const UVString &message);

	bool isLogging() const;
	void closeLogfile();

	// Set moves to include in solution.
	void setIncludedMoves(const MoveList &moves);

	// include only currently included moves that are within
	// equityThreshold points below the best play and cap at
	// maxNumberOfMoves
	// void pruneTo(double equityThreshold, int maxNumberOfMoves);

	// return a list of moves, sorted by estimated equity
	MoveList moves(unsigned int nmoves);
	
	// return the move list
	const EndgameMoveList &endgameMoves() const;

	// Return the best move
	Move solve(int nestedness);
	void reallyPlayOut(Move &move, int nestedness);

	double disappoint(EndgameMove &hope, double bestPessimistic);
	
protected:
	void writeLogHeader();
	void writeLogFooter();

	UVOFStream m_logfileStream;
	string m_logfile;
	bool m_logfileIsOpen;
	bool m_hasHeader;
	UVString m_xmlIndent;

	Game m_originalGame;
	Game m_endgameGame;
	ComputerDispatch *m_dispatch;

	EndgameMoveList m_endgameMoves;
	int m_nestedDisappointPlayNumber;
	int m_subnestedDisappointPlayNumber;
	int m_unnestedDisappointPlayNumber;
	int m_nestedInitialPlayNumber;
	int m_subnestedInitialPlayNumber;
	int m_unnestedInitialPlayNumber;
};

inline GamePosition &Endgame::currentPosition()
{
	return m_originalGame.currentPosition();
}

inline const GamePosition &Endgame::currentPosition() const
{
	return m_originalGame.currentPosition();
}

inline string Endgame::logfile() const
{
	return m_logfile;
}

inline bool Endgame::isLogging() const
{
	return m_logfileIsOpen;
}

inline const EndgameMoveList &Endgame::endgameMoves() const
{
	return m_endgameMoves;
}

}

UVOStream& operator<<(UVOStream& o, const Quackle::EndgameMove &move);
UVOStream& operator<<(UVOStream& o, const Quackle::EndgameMoveList &move);

#endif
