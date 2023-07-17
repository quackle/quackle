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
#include <cassert>

#include "datamanager.h"
#include "move.h"

using namespace Quackle;
using namespace std;

bool operator==(const Move &move1, const Move &move2)
{
	bool ret = false;

	if (move1.action == move2.action && move1.scoreAddition() == move2.scoreAddition())
	{
		switch (move1.action)
		{
			case Quackle::Move::Place:
			case Quackle::Move::PlaceError:
				ret = (move1.horizontal == move2.horizontal && move1.startrow == move2.startrow && move1.startcol == move2.startcol && move1.tiles() == move2.tiles() && move1.isChallengedPhoney() == move2.isChallengedPhoney());
				break;

			case Quackle::Move::UnusedTilesBonus:
			case Quackle::Move::UnusedTilesBonusError:
			case Quackle::Move::Exchange:
				ret = (Quackle::String::alphabetize(move1.tiles()) == Quackle::String::alphabetize(move2.tiles()));
				break;

			case Quackle::Move::BlindExchange:
				ret = (move1.tiles().length() == move2.tiles().length());
				break;

			case Quackle::Move::Pass:
			case Quackle::Move::Nonmove:
			case Quackle::Move::TimePenalty:
				ret = true;
				break;
		}
	}

	return ret;
}

bool Quackle::operator<(const Move &move1, const Move &move2)
{
	if (move1.action != move2.action)
		return move1.action < move2.action;
	if (move1.tiles() != move2.tiles())
		return move1.tiles() < move2.tiles();
	if (move1.horizontal != move2.horizontal)
		return move1.horizontal < move2.horizontal;
	if (move1.startrow != move2.startrow)
		return move1.startrow < move2.startrow;
	if (move1.startcol != move2.startcol)
		return move1.startcol < move2.startcol;
	if (move1.scoreAddition() != move2.scoreAddition())
		return move1.scoreAddition() < move2.scoreAddition();
	if (move1.isChallengedPhoney() != move2.isChallengedPhoney())
		return move1.isChallengedPhoney() < move2.isChallengedPhoney();

	return false;
}

LetterString Move::usedTiles() const
{
    return (m_isChallengedPhoney || action == BlindExchange) ? LetterString() : String::usedTiles(m_tiles);
}

LetterString Move::wordTiles() const
{
    LetterString word;

	const LetterString::const_iterator end(m_prettyTiles.end());
	for (LetterString::const_iterator it = m_prettyTiles.begin(); it != end; ++it)
        if (*it != QUACKLE_PLAYTHRU_START_MARK && *it != QUACKLE_PLAYTHRU_END_MARK)
            word += QUACKLE_ALPHABET_PARAMETERS->clearBlankness(*it);

	return word;
}

LetterString Move::wordTilesWithNoPlayThru() const
{
    LetterString word;
	LetterString used = usedTiles();

	const LetterString::const_iterator end(used.end());
	for (LetterString::const_iterator it = used.begin(); it != end; ++it)
        if (*it != QUACKLE_PLAYED_THRU_MARK)
            word += QUACKLE_ALPHABET_PARAMETERS->clearBlankness(*it);

	return word;
}

UVString Move::xml() const
{
	UVString actionString;
	UVString rest;
	bool includeRest = false;
	bool includeTiles = false;
	bool includeScore = false;

	switch (action)
	{
	case Pass:
		actionString = MARK_UV("pass");
		break;

	case Exchange:
	case BlindExchange:
		actionString = MARK_UV("exchange");
		includeTiles = true;
		break;

	case Nonmove:
		actionString = MARK_UV("nonmove");
		break;

	case TimePenalty:
		actionString = MARK_UV("timepenalty");
		includeScore = true;
		break;

	case UnusedTilesBonusError:
	case UnusedTilesBonus:
		actionString = MARK_UV("unusedtilesbonus");
		includeTiles = true;
		includeScore = true;
		break;

	case Place:
	case PlaceError:
	{
		actionString = MARK_UV("place");

		UVOStringStream restStream;

		// adding row and column numbers I suppose is bloat
		//restStream << "position=\"" << positionString() << "\" startrow=\"" << startrow << "\" startcolumn=\"" << startcol << "\"";
		restStream << "position=\"" << positionString() << "\"";

		rest = restStream.str();

		includeTiles = true;
		includeScore = true;
		includeRest = true;
		break;
	}
	}

	UVOStringStream xmlStream;
	xmlStream << "<move action=\"" << actionString << "\"";

	if (includeTiles)
		xmlStream << " tiles=\"" << QUACKLE_ALPHABET_PARAMETERS->userVisible(tiles()) << "\"";
	if (includeRest)
		xmlStream << " " << rest;
	if (includeScore)
		xmlStream << " score=\"" << score << "\"";

	xmlStream << " />";
	return xmlStream.str();
}

UVString Move::toString() const
{
	UVOStringStream ss;

	if (action == Quackle::Move::Pass)
		ss << "- ";
	else if (action == Quackle::Move::Exchange)
		ss << "-" << QUACKLE_ALPHABET_PARAMETERS->userVisible(m_tiles);
	else if (action == Quackle::Move::BlindExchange)
		ss << "-" << m_tiles.length();
	else if (action == Quackle::Move::Nonmove)
		ss << "nonmove";
	else if (action == Quackle::Move::TimePenalty)
		ss << "timepenalty " << score;
	else if (action == Quackle::Move::UnusedTilesBonus || action == Quackle::Move::UnusedTilesBonusError)
		ss << "(" << QUACKLE_ALPHABET_PARAMETERS->userVisible(m_tiles) << ")";
    else if (action == Quackle::Move::Place || action == Quackle::Move::PlaceError)
	{
		ss << positionString();
		ss << " " << QUACKLE_ALPHABET_PARAMETERS->userVisible(m_tiles);
    }

	return ss.str();
}

UVString Move::debugString() const
{
	UVOStringStream ss;

    ss << toString() << " (score = " << score << ", equity = " << equity << ", win% = " << win;

	if (m_scoreAddition != 0)
		ss << ", scoreAddition = " << m_scoreAddition;

	ss << ")";

	return ss.str();
}

UVString Move::positionString() const
{
	UVOStringStream ss;

	if (horizontal)
	{
		ss << startrow + 1;
		ss << (char)('A' + startcol);
	}
	else
	{
		ss << (char)('A' + startcol);
		ss << startrow + 1;
	}

	return ss.str();
}

Move Move::createPlaceMove(UVString placeString, LetterString word)
{
	int row = 0, column = 0;
	bool horizontal = true;

	UVString rowString, colString;
	if (iswdigit(placeString[0]))
	{
		rowString = placeString.substr(0, iswdigit(placeString[1])? 2 : 1);
		colString = placeString.substr(iswdigit(placeString[1])? 2 : 1);
	}
	else
	{
		horizontal = false;
		colString = placeString.substr(0, 1);
		rowString = placeString.substr(1);
	}

	UVStringStream ss(rowString);
	ss >> row;
	--row; // zero index it

	column = towupper(colString[0]) - MARK_UV('A');

	if (row < 0 || row > 9999)
		row = 0;
	if (column < 0 || column > 9999)
		column = 0;

	return createPlaceMove(row, column, horizontal, word);
}

Move Move::createPlaceMove(int zeroIndexedRow, int zeroIndexedColumn, bool horizontal, LetterString word)
{
	Move move;

	move.horizontal = horizontal;
	move.startrow = zeroIndexedRow;
	move.startcol = zeroIndexedColumn;
	move.setTiles(word);
	move.action = Move::Place;

	return move;
}

Move Move::createChallengedPhoney(UVString placeString, LetterString word)
{
	Move move = createPlaceMove(placeString, word);
	move.setIsChallengedPhoney(true);
	return move;
}

Move Move::createChallengedPhoney(int zeroIndexedRow, int zeroIndexedColumn, bool horizontal, LetterString word)
{
	Move move = createPlaceMove(zeroIndexedRow, zeroIndexedColumn, horizontal, word);
	move.setIsChallengedPhoney(true);
	return move;
}

Move Move::createExchangeMove(LetterString tilesToExchange, bool isBlind)
{
	Move move;

	move.action = isBlind ? Move::BlindExchange : Move::Exchange;
	move.setTiles(tilesToExchange);
	move.score = 0;

	return move;
}

Move Move::createUnusedTilesBonus(LetterString unusedTiles, int bonus)
{
	Move move;

	move.action = Move::UnusedTilesBonus;
	move.setTiles(unusedTiles);
	move.score = bonus;

	return move;
}

Move Move::createTimePenalty(int penalty)
{
	Move move;

	move.action = Move::TimePenalty;
	move.score = -penalty;

	return move;
}

Move Move::createPassMove()
{
	Move move;

	move.action = Move::Pass;
	move.score = 0;
	move.equity = -999;

	return move;
}

Move Move::createNonmove()
{
	Move move;

	move.action = Move::Nonmove;
	move.score = -9999;
	move.equity = -9999;

	return move;
}

UVOStream& operator<<(UVOStream& o, const Quackle::Move& m)
{
	o << m.debugString();
    return o;
}

//////////

bool MoveList::contains(const Move &move) const
{
	const MoveList::const_iterator ourEnd(end());
	for (MoveList::const_iterator it = begin(); it != ourEnd; ++it)
		if (*it == move)
			return true;

	return false;
}

void MoveList::sort(MoveList &list, SortType type)
{
	sortNonReverse(list, type);
	reverse(list.begin(), list.end());
}

void MoveList::sortNonReverse(MoveList &list, SortType type)
{
	switch (type)
	{
	case Win:
		stable_sort(list.begin(), list.end(), winComparator);
		break;
	case Equity:
		stable_sort(list.begin(), list.end(), equityComparator);
		break;
	case Score:
		stable_sort(list.begin(), list.end(), scoreComparator);
		break;
	case Alphabetical:
		stable_sort(list.begin(), list.end(), alphabeticalComparator);
		break;
	}
}

bool MoveList::winComparator(const Move &move1, const Move &move2)
{
	if (move1.win == move2.win) {
		if (move1.equity == move2.equity) {
			return wordPosComparator(move1, move2);
		}
		return move1.equity < move2.equity;
	}
	return move1.win < move2.win;
}

bool MoveList::equityComparator(const Move &move1, const Move &move2)
{
	if (move1.equity == move2.equity) {
		return wordPosComparator(move1, move2);
	}
	return move1.equity < move2.equity;
}

// This is used to enforce an arbitrary total order when the other compare
// is == and we want determinism.
bool MoveList::wordPosComparator(const Move &move1, const Move &move2)
{
	if (move1.startrow != move2.startrow) {
		return move1.startrow < move2.startrow;
	}

	if (move1.startcol != move2.startcol) {
		return move1.startcol < move2.startcol;
	}

	if (move1.horizontal != move2.horizontal) {
		return move1.horizontal < move2.horizontal;
	}

	if (move1.effectiveScore() != move2.effectiveScore()) {
		return move1.effectiveScore() < move2.effectiveScore();
	}

	assert(move1.tiles() != move2.tiles());
	return move1.tiles() < move2.tiles();
}

bool MoveList::scoreComparator(const Move &move1, const Move &move2)
{
	if (move1.effectiveScore() == move2.effectiveScore()) {
		return wordPosComparator(move1, move2);
	}
	return move1.effectiveScore() < move2.effectiveScore();
}

bool MoveList::alphabeticalComparator(const Move &move1, const Move &move2)
{
	if (move1.tiles() == move2.tiles()) {
		return wordPosComparator(move1, move2);
	}
	return move1.tiles() < move2.tiles();
}

UVOStream& operator<<(UVOStream& o, const Quackle::MoveList& moves)
{
	Quackle::MoveList::const_iterator end(moves.end());
	for (Quackle::MoveList::const_iterator it = moves.begin(); it != end; ++it)
		o << (*it) << endl;
    return o;
}

