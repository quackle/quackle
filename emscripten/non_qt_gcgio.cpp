/**
 * This file is a rewrite of the gcgio.cpp in Quackle, without using Qt
 */
#include <iostream>
#include <fstream>
#include <string>
#include <streambuf>

#include <deque>
#include "datamanager.h"

#include "game.h"
#include "non_qt_gcgio.h"

using namespace EmQuackle;

// God C++ is hideous. These functions are cobbled together from various SO
// answers.
template<typename T>
deque<T>
split(const T & str, const T & delimiters) {
    deque<T> v;
    typename T::size_type start = 0;
    auto pos = str.find_first_of(delimiters, start);
    while(pos != T::npos) {
        if(pos != start) // ignore empty tokens
            v.emplace_back(str, start, pos - start);
        start = pos + 1;
        pos = str.find_first_of(delimiters, start);
    }
    if(start < str.length()) // ignore trailing delimiter
        v.emplace_back(str, start, str.length() - start); // add what's left of the string
    return v;
}

static bool startsWith(const std::string& s, const std::string& prefix) {
    return s.size() >= prefix.size() && s.compare(0, prefix.size(), prefix) == 0;
}

static bool endsWith(const std::string& s, const std::string& suffix) {
	return s.size() >= suffix.size() && s.compare(
		s.size() - suffix.size(), string::npos, suffix) == 0;
}

std::string tail(std::string const& source, size_t const length) {
  if (length >= source.size()) { return source; }
  return source.substr(source.size() - length);
}

Quackle::LetterString encodeLS(const string &str)
{
	return QUACKLE_ALPHABET_PARAMETERS->encode(str);
}

GCGIO::GCGIO()
{
}

Quackle::Game *GCGIO::readFromString(const char *contents)
{
    Quackle::Game *ret = new Quackle::Game;
	Quackle::PlayerList players;

	Quackle::Rack incompleteRack;
	bool hasIncompleteRack = false;

	const bool canMaintainCrosses = true;

	bool gameStarted = false;

    std::stringstream ss(contents);
    std::string line;
    while (std::getline(ss, line))
    {
		std::cout << line << endl;
		std::deque<string> strings = split<string>(line, "\t ");

		if (startsWith(line, "#"))
		{
			if (startsWith(line, "#player"))
			{
				string firstChunk = strings.front();
				int id = firstChunk.back() - '0';

				string abbreviation = strings[1];
				string name = "";
				for (unsigned int i = 2; i < strings.size(); i++)
				{
					name.append(strings[i] + " ");
				}

				Quackle::Player newPlayer(name, Quackle::Player::HumanPlayerType);
				newPlayer.setId(id);
				newPlayer.setAbbreviatedName(abbreviation);
				players.push_back(newPlayer);
			}
			else if (startsWith(line, "#title"))
				ret->setTitle(tail(line, line.length() - 7));
			else if (startsWith(line, "#description"))
				ret->setDescription(tail(line, line.length() - 13));
			else if (startsWith(line, "#note") && ret->hasPositions())
				ret->currentPosition().setExplanatoryNote(tail(line, line.length() - 6));
			else if (startsWith(line, "#rack") && ret->hasPositions())
			{
				string firstChunk = strings.front();
				int id = firstChunk.back() - '0';

				const string rackString = strings[1];
				const Quackle::Rack rack(encodeLS(rackString));

				ret->currentPosition().setPlayerRack(/* zero index */ id - 1, rack);
			}
			else if (startsWith(line, "#incomplete"))
			{
				const string rackString = strings.size() == 1 ? "" : strings[1];
				incompleteRack = encodeLS(rackString);
				hasIncompleteRack = true;
			}
			// else if (line.startsWith("#character-encoding"))
			// {
			// 	QString encoding{line.right(line.length() - 20).trimmed()};
			// 	stream.setCodec(QTextCodec::codecForName(encoding.toLatin1()));
			// }
		}

		else if (startsWith(line, ">"))
		{
			if (!gameStarted)
			{
				ret->setPlayers(players);
				gameStarted = true;
			}
			UVString currentPlayer = strings.front().substr(
				1, strings.front().size() - 2);
			strings.pop_front();

			const string rackString = strings.front();
			strings.pop_front();

			// end of game unused tiles bonus
			if (startsWith(rackString, "(") && endsWith(rackString, ")"))
			{
				// end the game
				if (ret->hasPositions() && !ret->currentPosition().gameOver())
					ret->commitCandidate(canMaintainCrosses);
				else
					ret->addPosition();

				const string rack = rackString.substr(
					1, rackString.size() - 2);
				ret->currentPosition().setTileBonus(currentPlayer,
													encodeLS(rack),
													atoi(strings.front().c_str()));
				continue;
			}

			if (strings.empty())
			{
				UVcerr << "GCG error reading " << line << ": incomplete move" << endl;
				return ret;
			}

			const Quackle::Rack rack(encodeLS(rackString));

			Quackle::Move move = Quackle::Move::createNonmove();

			const string firstMoveBite = strings.front();
			if (startsWith(firstMoveBite, "--"))
			{
				Quackle::Move lastMoveMade = ret->currentPosition().moveMade();
				lastMoveMade.setIsChallengedPhoney(true);
				ret->currentPosition().setMoveMade(lastMoveMade);
			}
			else if (startsWith(firstMoveBite, "-"))
			{
				// XXX: TEST EXCHANGES!!!
				const string exchangedLetters = tail(
					firstMoveBite, firstMoveBite.length() - 1);
				bool isLetterCount = false;
				// this accounts for stuff like exchange 7 (-7)
				// not in the spec though!
				char *end;
				int letterCount = strtol(exchangedLetters.c_str(), &end, 10);
				if (*end == '\0') {
					// success
					isLetterCount = true;
				}

				if (exchangedLetters.empty() || (isLetterCount && letterCount == 0))
					move = Quackle::Move::createPassMove();
				else if (isLetterCount)
				{
					Quackle::LetterString encodedLetters;

					for (int i = 0; i < letterCount; ++i)
						encodedLetters.push_back(QUACKLE_BLANK_MARK);
					move = Quackle::Move::createExchangeMove(encodedLetters, true);
				}
				else
					move = Quackle::Move::createExchangeMove(encodeLS(exchangedLetters), false);
			}
			else if (startsWith(firstMoveBite, "(time)"))
			{
				strings.pop_front();

				if (strings.empty())
				{
					UVcerr << "GCG error reading " << line << ": incomplete move" << endl;
					return ret;
				}
			}
			else if (startsWith(firstMoveBite, "(challenge)"))
			{
				strings.pop_front();

				if (strings.empty())
				{
					UVcerr << "GCG error reading " << line << ": incomplete move" << endl;
					return ret;
				}

				Quackle::Move move = ret->currentPosition().moveMade();
				move.setScoreAddition(readSignedInt(strings.front()));
				ret->currentPosition().setMoveMade(move);
			}
			else
			{
				const string positionString = firstMoveBite;

				strings.pop_front();

				if (strings.empty())
				{
					UVcerr << "GCG error reading " << line << ": incomplete move" << endl;
					return ret;
				}

				const string placeTiles = strings.front();
				strings.pop_front();

				// if score is negative, it is rescored later to the proper score
				int score = -1;

				if (!strings.empty())
					score = readSignedInt(strings.front());

				move = Quackle::Move::createPlaceMove(positionString, encodeLS(placeTiles));
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

Quackle::Game *GCGIO::readFile(const std::string &filename)
{
    Quackle::Game *game;

	std::ifstream t(filename);
	std::stringstream buffer;
	buffer << t.rdbuf();

	game = readFromString(buffer.str().c_str());

	return game;

}

int GCGIO::readSignedInt(const string &intString) const
{
	return atoi(intString.c_str());
	// string bonus = intString;
	// int sign = 1;

	// if (startsWith(bonus, "+"))
	// {
	// 	bonus = bonus.right(bonus.length() - 1);
	// }
	// else if (startsWith(bonus, "-"))
	// {
	// 	sign = -1;
	// 	bonus = bonus.right(bonus.length() - 1);
	// }

	// return sign * bonus.toInt();
}
