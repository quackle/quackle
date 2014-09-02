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

#ifndef QUACKLE_PLAYER_H
#define QUACKLE_PLAYER_H

#include "rack.h"
#include "uv.h"

namespace Quackle
{

class ComputerPlayer;

class Player
{
public:
	enum PlayerType { ComputerPlayerType = 0, HumanPlayerType = 1};

	// creates a Computer player whose id is -1 and zero score
	Player();

	// creates a player with zero score
	Player(const UVString &name, int playerType = HumanPlayerType, int id = -1);

	void setName(const UVString &newName) { m_name = newName; }
	const UVString &name() const { return m_name; }

	void setAbbreviatedName(const UVString &newAbbreviatedName) { m_abbreviatedName = newAbbreviatedName; }
	const UVString &abbreviatedName() const { return m_abbreviatedName; }

	void setType(int playerType) { m_playerType = playerType; }
	int type() const { return m_playerType; }

	void setComputerPlayer(ComputerPlayer *computerPlayer) { m_computerPlayer = computerPlayer; }
	ComputerPlayer *computerPlayer() const { return m_computerPlayer; }

	// ID of real players must be greater than zero
	int id() const { return m_id; }

	// This never needs to be called by users!
	void setId(int id) { m_id = id; }

	void setRack(const Rack &newRack) { m_rack = newRack; }
	void setRack(const LetterString &rack) { m_rack = Rack(rack); }
	const Rack &rack() const { return m_rack; }

	void setScore(int score) { m_score = score; }
	int score() const { return m_score; }

	// addition = negative -> subtract from score
	void addToScore(int addition);

	// what was drawn to get TO this rack
	const Rack &drawnLetters() const { return m_drawnLetters; }
	void setDrawnLetters(const Rack &drawnLetters) { m_drawnLetters = drawnLetters; }

	// not used by libquackle -- whether the racks of this player are gibberish
	// or correct correspondents to real life
	bool racksAreKnown() const { return m_racksAreKnown; }
	void setRacksAreKnown(bool racksAreKnown) { m_racksAreKnown = racksAreKnown; }

	// returns true if rack, score are equal
	bool positionallyEqual(const Player &otherPlayer) const;

	UVString storeInformationToString() const;

	// these do no error checking!!! careful!
	void loadInformationFromString(const UVString &info);
	static Player makePlayerFromString(const UVString &info);

private:
	UVString m_name;
	UVString m_abbreviatedName;
	int m_id;
	int m_playerType;
	ComputerPlayer *m_computerPlayer;

	Rack m_rack;
	int m_score;

	Rack m_drawnLetters;
	bool m_racksAreKnown;
};

// comparison based on player ID
inline bool operator<(const Quackle::Player &player1, const Quackle::Player &player2)
{
	return player1.id() < player2.id();
}

}

inline bool operator==(const Quackle::Player &player1, const Quackle::Player &player2)
{
	return player1.id() == player2.id();
}

UVOStream &operator<<(UVOStream &o, const Quackle::Player &player);

#endif
