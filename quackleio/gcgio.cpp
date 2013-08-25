/*
 *  Quackle -- Crossword game artificial intelligence and analysis tool
 *  Copyright (C) 2005-2006 Jason Katz-Brown and John O'Laughlin.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  
 *  02110-1301  USA
 */

#include <QtCore>

#include <datamanager.h>

#include "game.h"
#include "gcgio.h"
#include "util.h"

using namespace QuackleIO;

GCGIO::GCGIO()
{
}

Quackle::Game *GCGIO::read(QTextStream &stream, int flags)
{
	Quackle::Game *ret = new Quackle::Game;
	Quackle::PlayerList players;
	
	Quackle::Rack incompleteRack;
	bool hasIncompleteRack = false;;

	const bool canMaintainCrosses = flags & Logania::MaintainBoardPreparation;

	bool gameStarted = false;

	QString line;
	while (!stream.atEnd())
	{
		line = stream.readLine();
		QStringList strings = line.split(QRegExp("\\s+"), QString::SkipEmptyParts);

		if (line.startsWith("#"))
		{
			if (line.startsWith("#player"))
			{
				QString firstChunk = strings.front();
				int id = firstChunk.right(1).toInt();

				strings.pop_front();

				if (strings.isEmpty())
				{
					UVcerr << "GCG error reading " << Util::qstringToString(line) << ": no player abbreviation in #player" << endl;
					return ret;
				}

				QString abbreviation = strings.front();
				strings.pop_front();

				if (strings.isEmpty())
				{
					UVcerr << "GCG error reading " << Util::qstringToString(line) << ": no player name in #player" << endl;
					return ret;
				}

				QString name = strings.join(" ");

				Quackle::Player newPlayer(Util::qstringToString(name), Quackle::Player::HumanPlayerType);
				newPlayer.setId(id);
				newPlayer.setAbbreviatedName(Util::qstringToString(abbreviation));
				players.push_back(newPlayer);
			}
			else if (line.startsWith("#title"))
				ret->setTitle(Util::qstringToString(line.right(line.length() - 7)));
			else if (line.startsWith("#description"))
				ret->setDescription(Util::qstringToString(line.right(line.length() - 13)));
			else if (line.startsWith("#note") && ret->hasPositions())
				ret->currentPosition().setExplanatoryNote(Util::qstringToString(line.right(line.length() - 6)));
			else if (line.startsWith("#rack") && ret->hasPositions())
			{
				QString firstChunk = strings.front();
				int id = firstChunk.right(1).toInt();

				strings.pop_front();

				if (strings.isEmpty())
				{
					UVcerr << "GCG error reading " << Util::qstringToString(line) << ": no rack in #rack" << endl;
					return ret;
				}

				const QString rackString = strings.front();
				const Quackle::Rack rack(Util::encode(rackString));

				ret->currentPosition().setPlayerRack(/* zero index */ id - 1, rack);
			}
			else if (line.startsWith("#incomplete"))
			{
				strings.pop_front();

				const QString rackString = strings.isEmpty()? QString() : strings.front();
				incompleteRack = Util::encode(rackString);
				hasIncompleteRack = true;
			}
		}
		else if (line.startsWith(">"))
		{
			if (!gameStarted)
			{
				ret->setPlayers(players);
				gameStarted = true;
			}

			strings.pop_front();

			if (strings.isEmpty())
			{
				UVcerr << "GCG error reading " << Util::qstringToString(line) << ": incomplete move" << endl;
				return ret;
			}

			const QString rackString = strings.front();
			strings.pop_front();

			// end of game unused tiles bonus
			if (rackString.startsWith("(") && rackString.endsWith(")"))
			{
				// end the game
				if (ret->hasPositions())
					ret->commitCandidate(canMaintainCrosses);
				else
					ret->addPosition();
				continue;
			}

			if (strings.isEmpty())
			{
				UVcerr << "GCG error reading " << Util::qstringToString(line) << ": incomplete move" << endl;
				return ret;
			}

			const Quackle::Rack rack(Util::encode(rackString));

			Quackle::Move move = Quackle::Move::createNonmove();

			const QString firstMoveBite = strings.front();
			if (firstMoveBite.startsWith("--"))
			{
				Quackle::Move lastMoveMade = ret->currentPosition().moveMade();
				lastMoveMade.setIsChallengedPhoney(true);
				ret->currentPosition().setMoveMade(lastMoveMade);
			}
			else if (firstMoveBite.startsWith("-"))
			{
				const QString exchangedLetters = firstMoveBite.right(firstMoveBite.length() - 1);
				if (exchangedLetters.isEmpty())
					move = Quackle::Move::createPassMove();
				else
					move = Quackle::Move::createExchangeMove(Util::encode(exchangedLetters));
			}
			else if (firstMoveBite.startsWith("(time)"))
			{
				strings.pop_front();

				if (strings.isEmpty())
				{
					UVcerr << "GCG error reading " << Util::qstringToString(line) << ": incomplete move" << endl;
					return ret;
				}
			}
			else if (firstMoveBite.startsWith("(challenge)"))
			{
				strings.pop_front();

				if (strings.isEmpty())
				{
					UVcerr << "GCG error reading " << Util::qstringToString(line) << ": incomplete move" << endl;
					return ret;
				}

				Quackle::Move move = ret->currentPosition().moveMade();
				move.setScoreAddition(readSignedInt(strings.front()));
				ret->currentPosition().setMoveMade(move);
			}
			else
			{
				const QString positionString = firstMoveBite;

				strings.pop_front();

				if (strings.isEmpty())
				{
					UVcerr << "GCG error reading " << Util::qstringToString(line) << ": incomplete move" << endl;
					return ret;
				}

				const QString placeTiles = strings.front(); 
				strings.pop_front();

				// if score is negative, it is rescored later to the proper score
				int score = -1;

				if (!strings.isEmpty())
					score = readSignedInt(strings.front());;

				move = Quackle::Move::createPlaceMove(Util::qstringToString(positionString), Util::encode(placeTiles));
				move.score = score;
			}

			if (move.isAMove())
			{
				if (ret->hasPositions())
					ret->commitCandidate(canMaintainCrosses);
				else
					ret->addPosition();

				ret->currentPosition().setCurrentPlayerRack(rack);
				ret->currentPosition().ensureMovePrettiness(move);
				ret->currentPosition().ensureMoveTilesDoNotIncludePlayThru(move);

				int correctScore = ret->currentPosition().calculateScore(move);
				if (move.score < 0)
				{
					move.score = correctScore;
				}
				else
				{
					if (correctScore != move.score)
					{
						move.setScoreAddition(move.score - correctScore);
						move.score = correctScore;
					}
				}

				ret->currentPosition().setMoveMade(move);
			}
		}
	}

	if (!gameStarted || !ret->currentPosition().gameOver())
	{
		if (ret->hasPositions())
			ret->commitCandidate(canMaintainCrosses);
		else
			ret->addPosition();

		if (hasIncompleteRack)
		{
			ret->currentPosition().setCurrentPlayerRack(incompleteRack);
		}
	}

	return ret;
}

int GCGIO::readSignedInt(const QString &intString) const
{
	QString bonus = intString;
	int sign = 1;

	if (bonus.startsWith("+"))
	{
		bonus = bonus.right(bonus.length() - 1);
	}
	else if (bonus.startsWith("-"))
	{
		sign = -1;
		bonus = bonus.right(bonus.length() - 1);
	}

	return sign * bonus.toInt();
}

bool GCGIO::canRead(QTextStream &stream) const
{
	QString firstChunk;
	stream >> firstChunk;
	if (firstChunk.startsWith("#"))
		return true;
	
	return false;
}

void GCGIO::write(const Quackle::Game &game, QTextStream &stream)
{
	Quackle::PlayerList players = game.players();
	for (Quackle::PlayerList::iterator it = players.begin(); it != players.end(); ++it)
	{
		stream << "#player" << (*it).id() + 1 << " " << Util::uvStringToQString((*it).abbreviatedName()) << " " << Util::uvStringToQString((*it).name()) << endl;
	}

	if (!game.title().empty())
		stream << "#title " << Util::uvStringToQString(game.title()) << endl;

	if (!game.description().empty())
		stream << "#description " << Util::uvStringToQString(game.description()) << endl;

	const Quackle::PositionList::const_iterator end(game.history().end());
	for (Quackle::PositionList::const_iterator it = game.history().begin(); it != end; ++it)
	{
		Quackle::Move move = (*it).committedMove();
		move.setPrettyTiles((*it).board().prettyTilesOfMove(move, /* don't mark playthru */ false));

		if (move.isAMove())
		{
			int outputScore = move.score;
			int outputScoreAddition = move.scoreAddition();

			// special case != 5 score additions;
			// GCG has no way to specify them, so we roll the score addition
			// into the regular score silently
			if (outputScoreAddition != 5)
			{
				outputScore = move.score + move.scoreAddition();
				outputScoreAddition = 0;
			}

			stream << ">" << Util::uvStringToQString((*it).currentPlayer().abbreviatedName()) << ": " << Util::letterStringToQString((*it).currentPlayer().rack().alphaTiles()) << " " << Util::uvStringToQString(move.toString()) << " +" << outputScore << " " << outputScore + (*it).currentPlayer().score() << endl;

			if (move.isChallengedPhoney())
			{
				stream << ">" << Util::uvStringToQString((*it).currentPlayer().abbreviatedName()) << ": " << Util::letterStringToQString((*it).currentPlayer().rack().alphaTiles()) << " --  -" << outputScore << " " << move.effectiveScore() + (*it).currentPlayer().score() << endl;
			}

			if (outputScoreAddition != 0)
			{
				QString nextRack = "UNKNOWN";
				bool foundOne = false;
				for (Quackle::PositionList::const_iterator secondIterator = it; secondIterator != end; ++secondIterator)
				{
					if ((*secondIterator).currentPlayer().id() == (*it).currentPlayer().id())
					{
						if (foundOne)
						{
							nextRack = Util::letterStringToQString((*secondIterator).currentPlayer().rack().alphaTiles());
							break;
						}
						else
						{
							foundOne = true;
							continue;
						}
					}
				}

				stream << ">" << Util::uvStringToQString((*it).currentPlayer().abbreviatedName()) << ": " << nextRack << " (challenge) " << ((outputScoreAddition > 0)? "+" : "") << outputScoreAddition << " " << (outputScoreAddition + outputScore + (*it).currentPlayer().score()) << endl;
			}
		}

		if (!(*it).explanatoryNote().empty())
			stream << "#note " << Util::uvStringToQString((*it).explanatoryNote()) << endl;
	}

	const Quackle::GamePosition &lastPosition = game.history().lastPosition();

	if (!lastPosition.gameOver())
	{
		stream << "#rack" << lastPosition.currentPlayer().id() + 1 << " " << Util::letterStringToQString(lastPosition.currentPlayer().rack().alphaTiles()) << endl;
	}
}

QString GCGIO::filter() const
{
	return QString("*.gcg");
}

