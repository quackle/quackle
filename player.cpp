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

#include <iostream>
#include <sstream>

#include "computerplayer.h"
#include "datamanager.h"
#include "player.h"

using namespace Quackle;

Player::Player()
	: m_name(MARK_UV("No Name")), m_abbreviatedName(MARK_UV("NoName")), m_id(-1), m_playerType(ComputerPlayerType), m_computerPlayer(0), m_score(0), m_racksAreKnown(true)
{
}

Player::Player(const UVString &name, int playerType, int id)
	: m_name(name), m_abbreviatedName(name), m_id(id), m_playerType(playerType), m_computerPlayer(0), m_score(0), m_racksAreKnown(true)
{
}

void Player::addToScore(int addition)
{
	m_score += addition;
}

bool Player::positionallyEqual(const Player &otherPlayer) const
{
	return rack().equals(otherPlayer.rack()) && score() == otherPlayer.score();
}

UVString Player::storeInformationToString() const
{
	UVOStringStream ss;
	ss << m_id << ';' << m_playerType << ';' << (m_playerType == ComputerPlayerType? m_computerPlayer->id() : (m_racksAreKnown? 0 : 1)) << ';' << m_name;
	return ss.str();
}

void Player::loadInformationFromString(const UVString &info)
{
	int i = 0;
	UVString whatIsLeft(info);
	for (UVString::size_type semicolonIndex; i <= 2; ++i)
	{
		semicolonIndex = whatIsLeft.find(MARK_UV(';'));

		if (semicolonIndex == string::npos)
			break;

		UVString itemString = whatIsLeft.substr(0, semicolonIndex);
		whatIsLeft = whatIsLeft.substr(semicolonIndex + 1);

		UVStringStream ss(itemString);
		int itemInt;
		ss >> itemInt;

		switch (i)
		{
		case 0:
			m_id = itemInt;
			break;

		case 1:
			m_playerType = itemInt;
			break;

		case 2:
			switch (m_playerType)
			{
			case ComputerPlayerType:
				if (QUACKLE_DATAMANAGER_EXISTS)
				{
					bool computerPlayerExists = false;
					const Player &computerPlayer = QUACKLE_COMPUTER_PLAYERS.playerForId(itemInt, computerPlayerExists);

					if (computerPlayerExists)
						m_computerPlayer = computerPlayer.computerPlayer();
				}

				break;

			case HumanPlayerType:
				m_racksAreKnown = !itemInt;
				break;
			}

			break;

		default:
			break;
		}
	}

	m_name = whatIsLeft;
}

Player Player::makePlayerFromString(const UVString &info)
{
	Player ret;
	ret.loadInformationFromString(info);
	return ret;
}

UVOStream &operator<<(UVOStream &o, const Quackle::Player &player)
{
	o << (player.type() == Quackle::Player::ComputerPlayerType? MARK_UV("Computer") : MARK_UV("Human")) << " Player " << player.name() << " (id " << player.id() << ")" << " with score " << player.score() << " holding " << player.rack() << " after drawing [" << player.drawnLetters() << "]";
	return o;
}

