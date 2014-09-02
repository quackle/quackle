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

#ifndef QUACKLE_COMPUTERPLAYER_H
#define QUACKLE_COMPUTERPLAYER_H

#include "sim.h"

#define QUACKLE_NULL_COMPUTER_PLAYER_ID 0
#define QUACKLE_STATIC_COMPUTER_PLAYER_ID 1

namespace Quackle
{

// Settings that all players should follow.
struct ComputerParameters
{
	// upper bound on time to contemplate current moves
	int secondsPerTurn;

    // when simming, use likely rack leaves for opponent based on their previous play
    bool inferring;
};

class ComputerDispatch
{
public:
    ComputerDispatch() {}
    virtual ~ComputerDispatch() {}

    // To be called by the computer player.
    // Returns true if the computation should be aborted.
    virtual bool shouldAbort() = 0;

    // To be called by the computer player.
    // Returns true if the computation should be aborted.
    // Default implementation does nothing.
    virtual void signalFractionDone(double fractionDone) = 0;
};

// Shadows another ComputerDispatch but scales its fractions
class ScalingDispatch : public ComputerDispatch
{
public:
    ScalingDispatch(ComputerDispatch *shadow, double scale, double addition);
    virtual ~ScalingDispatch() {}

    virtual bool shouldAbort();
    virtual void signalFractionDone(double fractionDone);

private:
    ComputerDispatch *m_shadow;
    double m_scale;
    double m_addition;
};

class ComputerPlayer
{
public:
    // constructs a new computer player
    ComputerPlayer();
    virtual ~ComputerPlayer();

    void setParameters(const ComputerParameters &parameters);
    const ComputerParameters &parameters() const;

    // prepare to generate move for current player
    // on this position
    virtual void setPosition(const GamePosition &position);

    // get access to the position that we're playing from
    GamePosition &currentPosition();
    const GamePosition &currentPosition() const;

    // returns true if we have a dispatch and it says to abort
    bool shouldAbort();

    // if we have a dispatch, signals fractionDone on the dispatch
    void signalFractionDone(double fractionDone);

    // Ensures this move will be in the moves() output.
	// Implementations of computer player must themselves make sure 
	// that this move is in the moves() results; this function by default
	// adds the move to the simulator's considered moves list so it
	// won't be pruned away.
	void considerMove(const Move &move);
	void setConsideredMoves(const MoveList &moves);

	const UVString &name() const;
	int id() const;

	// the best move
	virtual Move move() = 0;

	// make a new one of yourself
	virtual ComputerPlayer *clone() = 0;

	// top n moves; default implementation returns
	// a one-item list with move()
	virtual MoveList moves(int nmoves);

	// Whether or not this player takes more than a few seconds to make a play.
	// False by default.
	virtual bool isSlow() const;

	// Whether this player should be an option to play against.
	// False by default.
	virtual bool isUserVisible() const;

	// dispatch is used to tell computer player to abort
	// and for computer player to give status updates
	virtual ComputerDispatch *dispatch() const;

	// sets dispatch for this player and its simulator
	virtual void setDispatch(ComputerDispatch *dispatch);

protected:
	// a max function for convenience
	static double max(double v1, double v2);
	static int max(int v1, int v2);

	Simulator m_simulator;
	UVString m_name;
	int m_id;
	ComputerParameters m_parameters;
	ComputerDispatch *m_dispatch;
};

inline GamePosition &ComputerPlayer::currentPosition()
{
	return m_simulator.currentPosition();
}

inline const GamePosition &ComputerPlayer::currentPosition() const
{
	return m_simulator.currentPosition();
}

inline void ComputerPlayer::setParameters(const ComputerParameters &parameters)
{
	m_parameters = parameters;
}

inline const ComputerParameters &ComputerPlayer::parameters() const
{
	return m_parameters;
}

inline const UVString &ComputerPlayer::name() const
{
	return m_name;
}

inline int ComputerPlayer::id() const
{
	return m_id;
}

inline bool ComputerPlayer::isSlow() const
{
	return false;
}

inline bool ComputerPlayer::isUserVisible() const
{
	return false;
}

inline ComputerDispatch *ComputerPlayer::dispatch() const
{
	return m_dispatch;
}

inline double ComputerPlayer::max(double v1, double v2)
{
	return v1 > v2? v1 : v2;
}

inline int ComputerPlayer::max(int v1, int v2)
{
	return v1 > v2? v1 : v2;
}

// Static player has ID 1!
class StaticPlayer : public ComputerPlayer
{
public:
	StaticPlayer();
	virtual ~StaticPlayer();

	virtual ComputerPlayer *clone() { return new StaticPlayer; }

	virtual Move move();
	virtual MoveList moves(int nmoves);
};

}

#endif
