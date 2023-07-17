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

#include <iomanip>
#include <iostream>
#include <algorithm>

#include "alphabetparameters.h"
#include "computerplayer.h"
#include "datamanager.h"
#include "game.h"

#include "reporter.h"

using namespace Quackle;
using namespace std;

void Reporter::reportPosition(const GamePosition &position, ComputerPlayer *computerPlayer, UVString *report)
{
	UVOStringStream s;

	UVOStringStream titleStream;

	if (!position.gameOver())
		titleStream << position.currentPlayer().name() << MARK_UV(": Turn ") << position.turnNumber() << MARK_UV('\n');

	const Quackle::PlayerList players(position.endgameAdjustedScores());

	for (PlayerList::const_iterator it = players.begin(); it != players.end(); ++it)
	{
		s.width(3);
		s << right << ((*it) == position.currentPlayer()? MARK_UV("->") : MARK_UV(" "));
		s << MARK_UV(' ');
		s.width(24);
		s << left << (*it).name() << MARK_UV(' ');
		s.width(9);
		s << (*it).rack().toString() << MARK_UV(' ');
		s.width(4);
		s << (*it).score();
		s << MARK_UV('\n');
	}

	if (computerPlayer && !position.gameOver())
	{
		computerPlayer->setPosition(position);

		if (position.committedMove().isAMove())
			computerPlayer->considerMove(position.committedMove());

		const unsigned int movesToShow = 10;
		MoveList moves = computerPlayer->moves(movesToShow);

		int ourMoveIndex = 0;
		int i = 1;
		for (Quackle::MoveList::const_iterator it = moves.begin(); it != moves.end(); ++it, ++i)
		{
			if ((*it) == position.committedMove())
			{
				ourMoveIndex = i;
				break;
			}
		}

		bool isUrp = false;

		if (position.committedMove().isAMove())
		{
			// our move not in list
			if (ourMoveIndex == 0)
			{
				if (moves.size() == movesToShow)
					moves.pop_back();

				isUrp = true;
				ourMoveIndex = movesToShow;
				moves.push_back(position.committedMove());
			}
		}

		int highestScore = 0;
		double highestEquity = 0;
		size_t widestPositionString = 0;
		unsigned int widestMove = 0;
		bool hasWinPercentages = false;
		const Quackle::MoveList::const_iterator end(moves.end());
		for (Quackle::MoveList::const_iterator it = moves.begin(); it != end; ++it)
		{
			if ((*it).prettyTiles().length() > widestMove)
				widestMove = (*it).prettyTiles().length();
			if ((*it).positionString().length() > widestPositionString)
				widestPositionString = (*it).positionString().length();
			if ((*it).win > 0)
				hasWinPercentages = true;
			if ((*it).equity > highestEquity)
				highestEquity = (*it).equity;
			if ((*it).score > highestScore)
				highestScore = (*it).score;
		}

		s << MARK_UV("--");

		UVOStringStream headerStream;
		headerStream << computerPlayer->name();

		headerStream << "'s choices (your play: ";
		if (isUrp)
			headerStream << "urp";
		else
			headerStream << ourMoveIndex;
		headerStream << ")";

		s.width(43);
		s << setfill(MARK_UV('-'));
		s << left << headerStream.str() << MARK_UV('\n');
		s << setfill(MARK_UV(' '));

		i = 1;
		for (Quackle::MoveList::const_iterator it = moves.begin(); it != end; ++it, ++i)
		{
			// column 2, the valuation.
			s.width(5);
			if ((*it).equity > (highestEquity - .01) && (*it).equity < (highestEquity + .01))
			{
				s << MARK_UV("best");
			}
			else
			{
				s << right << showpoint;
				s.precision(3);
				s << (highestEquity - (*it).equity);
			}

			s << (i == ourMoveIndex? MARK_UV("*") : MARK_UV(" "));

			// column 3, the position string.
			s << left;
			s.width(widestPositionString);
			switch ((*it).action)
			{
			case Move::Place:
			case Move::PlaceError:
				s << (*it).positionString();
				break;

			case Move::Exchange:
			case Move::BlindExchange:
				s << MARK_UV("xch");
				break;

			case Move::Pass:
				s << MARK_UV("pas");
				break;
			case Move::UnusedTilesBonus:
			case Move::UnusedTilesBonusError:
			case Move::TimePenalty:
			case Move::Nonmove:
				break;
			}
			s << MARK_UV(" ");

			// column 4, the word
			s.width(widestMove);
			s << left << QUACKLE_ALPHABET_PARAMETERS->userVisible((*it).prettyTiles()) << MARK_UV(" ");

			// column 5, the score
			s.width(highestScore >= 100? 3 : (highestScore >= 10? 2 : 1));
			s << left << (*it).score << MARK_UV(" ");

			// column 6, the win percentage
			if (hasWinPercentages)
			{
				s.precision(4);
				s.width(5);
				s << showpoint << ((*it).win * 100.0) << MARK_UV("% ");
			}

			// column 7, the leave

			s << (position.currentPlayer().rack() - (*it)).toString() << MARK_UV('\n');
		}
	}

	if (position.gameOver())
	{
		s << MARK_UV("\n Game over.\n\n");
	}

	int j = 0;
	UVString wrappedTiles;
	LongLetterString unseenTiles = position.unseenBag().tiles();
	std::sort(unseenTiles.begin(), unseenTiles.end());
	for (Quackle::LongLetterString::const_iterator it = unseenTiles.begin(); it != unseenTiles.end(); ++it, ++j)
	{
		if (j >= 44)
		{
			wrappedTiles += MARK_UV('\n');
			j = 0;
		}

		wrappedTiles += QUACKLE_ALPHABET_PARAMETERS->userVisible(*it);
	}

	s << MARK_UV("--");
	s.width(43);
	s << setfill(MARK_UV('-'));
	s << MARK_UV("Tracking") << MARK_UV('\n');
	s << wrappedTiles << MARK_UV("  ") << unseenTiles.size() << MARK_UV('\n');

	UVString reportString = s.str();
	UVString boardString = position.board().toString();
	
	*report = titleStream.str();

	// Ensure that the board ends with a newline.
	boardString += MARK_UV("\n");

	// Put board and report side by side.
	UVString::const_iterator boardIt = boardString.begin();
	UVString::const_iterator reportIt = reportString.begin();
	UVString::const_iterator reportLineBeginning = reportString.begin();
	UVString::const_iterator boardLineBeginning = boardString.begin();
	const UVString::const_iterator reportEnd = reportString.end();
	const UVString::const_iterator boardEnd = boardString.end();
	while (true)
	{
		if (boardIt == boardEnd && reportIt == reportEnd)
			break;

		if (boardIt != boardEnd && (*boardIt) != MARK_UV('\n'))
		{
			++boardIt;
			continue;
		}

		if (reportIt != reportEnd && (*reportIt) != MARK_UV('\n'))
		{
			++reportIt;
			continue;
		}

		if (boardIt != boardEnd)
		{
			report->append(boardLineBeginning, boardIt);
		}
		if (reportIt != reportEnd)
		{
			report->append(MARK_UV(" "));
			report->append(reportLineBeginning, reportIt);
		}

		boardLineBeginning = boardIt + 1;
		reportLineBeginning = reportIt + 1;

		if (boardIt != boardEnd)
			++boardIt;
		if (reportIt != reportEnd)
			++reportIt;

		report->append(MARK_UV("\n"));
	}
}

void Reporter::reportHeader(const Game & /* game */, UVString *report)
{
	UVOStringStream s;
	s << MARK_UV("Game Report\nGenerated by Quackle crossword game AI and analysis software\nhttp://quackle.org\n\n");
	*report = s.str();
}

void Reporter::reportGameStatistics(const Game &game, UVString *report)
{
        (void) game;
	UVOStringStream s;
	*report = s.str();
}

void Reporter::reportGame(const Game &game, ComputerPlayer *computerPlayer, UVString *report)
{
	UVOStringStream s;

	UVString header;
	reportHeader(game, &header);
	s << header;

	const PositionList::const_iterator end(game.history().end());
	for (PositionList::const_iterator it = game.history().begin(); it != end; ++it)
	{
		UVString subreport;
		reportPosition(*it, computerPlayer, &subreport);

		s << subreport << MARK_UV('\n');
	}

	UVString stats;
	reportGameStatistics(game, &stats);
	s << stats;

	*report = s.str();
}
