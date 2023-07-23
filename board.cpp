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

#include <sstream>
#include <iostream>

#include "board.h"
#include "boardparameters.h"
#include "datamanager.h"
#include "gameparameters.h"
#include "generator.h"

using namespace Quackle;

//#define DEBUG_BOARD

inline int letterMultiplier(int row, int column)
{
	return QUACKLE_BOARD_PARAMETERS->letterMultiplier(row, column);
}

inline int wordMultiplier(int row, int column)
{
	return QUACKLE_BOARD_PARAMETERS->wordMultiplier(row, column);
}

Board::Board()
    : m_width(QUACKLE_BOARD_PARAMETERS->width()), 
      m_height(QUACKLE_BOARD_PARAMETERS->height()), 
      m_empty(true)
{
}

Board::Board(int width, int height)
    : m_width(width), m_height(height), m_empty(true)
{
}

Bag Board::tilesOnBoard() const
{
	Bag ret;
	ret.clear();

	for (int row = 0; row < m_height; row++)
	{
		for (int col = 0; col < m_width; col++)
		{
			if (m_letters[row][col] != QUACKLE_NULL_MARK)
			{
				LetterString letters;
				letters += m_isBlank[row][col]? QUACKLE_BLANK_MARK : m_letters[row][col];
				ret.toss(letters);
			}
		}
	}

	return ret;
}

Bag Board::tilesNotOnBoard() const
{
	Bag ret;

	for (int row = 0; row < m_height; row++)
		for (int col = 0; col < m_width; col++)
			if (m_letters[row][col] != QUACKLE_NULL_MARK)
				ret.removeLetter(m_isBlank[row][col]? QUACKLE_BLANK_MARK : m_letters[row][col]);

	return ret;
}

bool Board::isConnected(const Move &move) const
{
	bool ret = false;

	if (m_empty)
		return true;

	if (move.action == Move::Place)
	{
		int i = 0;
		for (const auto& it : move.tiles())
		{
			const int row = move.horizontal? move.startrow : i + move.startrow;
			const int column = move.horizontal? i + move.startcol : move.startcol;

			if (isNonempty(row, column) || (row > 0 && isNonempty(row - 1, column)) || (column > 0 && isNonempty(row, column - 1)) || (row < m_height - 1 && isNonempty(row + 1, column)) || (column < m_width - 1 && isNonempty(row, column + 1)))
				return true;
			i++;
		}
	}

	return ret;
}

bool Board::isUnacceptableOpeningMove(const Move &move) const
{
	if (!m_empty)
		return false;

	if (move.action == Move::Place)
	{
		int i = 0;
		for (const auto& it : move.tiles())
		{
			const int row = move.horizontal? move.startrow : i + move.startrow;
			const int column = move.horizontal? i + move.startcol : move.startcol;

			if (row == QUACKLE_BOARD_PARAMETERS->startRow() && column == QUACKLE_BOARD_PARAMETERS->startColumn())
				return false;
			i++;
		}
	}

	return true;
}

void Board::updateBritishness()
{
	Generator generator;

	LetterString word;
	for (int row = 0; row < m_height; row++)
	{
		for (int col = 0; col < m_width; col++)
		{
			bool isBritish = false;

			if (m_letters[row][col] != QUACKLE_NULL_MARK)
			{
				word.clear();
				word += QUACKLE_ALPHABET_PARAMETERS->clearBlankness(m_letters[row][col]);

				for (int j = row - 1; j >= 0; --j)
				{
					if (m_letters[j][col] == QUACKLE_NULL_MARK)
						break;
					else
						word = QUACKLE_ALPHABET_PARAMETERS->clearBlankness(m_letters[j][col]) + word;
				}

				for (int j = row + 1; j < m_height; ++j)
				{
					if (m_letters[j][col] == QUACKLE_NULL_MARK)
						break;
					else
						word += QUACKLE_ALPHABET_PARAMETERS->clearBlankness(m_letters[j][col]);
				}

				if (word.length() > 1)
				{
					WordWithInfo wordWithInfo;
					wordWithInfo.wordLetterString = word;
					generator.storeWordInfo(&wordWithInfo);

					if (wordWithInfo.british)
						isBritish = true;
				}

				word.clear();
				word += QUACKLE_ALPHABET_PARAMETERS->clearBlankness(m_letters[row][col]);

				for (int j = col - 1; j >= 0; --j)
				{
					if (m_letters[row][j] == QUACKLE_NULL_MARK)
						break;
					else
						word = QUACKLE_ALPHABET_PARAMETERS->clearBlankness(m_letters[row][j]) + word;
				}

				for (int j = col + 1; j < m_width; ++j)
				{
					if (m_letters[row][j] == QUACKLE_NULL_MARK)
						break;
					else
						word += QUACKLE_ALPHABET_PARAMETERS->clearBlankness(m_letters[row][j]);
				}

				if (word.length() > 1)
				{
					WordWithInfo wordWithInfo;
					wordWithInfo.wordLetterString = word;
					generator.storeWordInfo(&wordWithInfo);

					if (wordWithInfo.british)
						isBritish = true;
				}
			}

			m_isBritish[row][col] = isBritish;
		}
	}
}

MoveList Board::allWordsFormedBy(const Move &move) const
{
	MoveList ret;

	if (move.tiles().length() > 1)
		ret.push_back(move);

	if (move.action == Move::Place)
	{
		if (m_empty)
		{
			ret.push_back(move);
		}
		else
		{
			LetterString word;

			if (move.horizontal)
			{
				int i = 0;
				for (const auto& it : move.tiles())
				{
					if (m_letters[move.startrow][i + move.startcol] == QUACKLE_NULL_MARK)
					{
						word.clear();
						word += it;

						int startRow = 0;
						for (int j = move.startrow - 1; j >= 0; --j)
						{
							if (m_letters[j][i + move.startcol] == QUACKLE_NULL_MARK)
							{
								startRow = j + 1;
								break;
							}
							else
							{
								word = m_letters[j][i + move.startcol] + word;
							}
						}

						for (int j = move.startrow + 1; j < m_height; ++j)
						{
							if (m_letters[j][i + move.startcol] == QUACKLE_NULL_MARK)
								j = m_height;
							else
								word += m_letters[j][i + move.startcol];
						}

						if (word.length() > 1)
						{
							ret.push_back(Move::createPlaceMove(startRow, (i + move.startcol), /* vertical */ false, word));
						}
					}
					i++;
				}
			}
			else
			{
				int i = 0;
				for (const auto& it : move.tiles())
				{
					if (m_letters[i + move.startrow][move.startcol] == QUACKLE_NULL_MARK)
					{
						word.clear();
						word += it;

						int startColumn = 0;
						for (int j = move.startcol - 1; j >= 0; --j)
						{
							if (m_letters[i + move.startrow][j] == QUACKLE_NULL_MARK)
							{
								startColumn = j + 1;
								break;
							}
							else
							{
								word = m_letters[i + move.startrow][j] + word;
							}
						}

						for (int j = move.startcol + 1; j < m_width; ++j)
						{
							if (m_letters[i + move.startrow][j] == QUACKLE_NULL_MARK)
								j = m_width;
							else
								word += m_letters[i + move.startrow][j];
						}

						if (word.length() > 1)
						{
							ret.push_back(Move::createPlaceMove((i + move.startrow), startColumn, /* horizontal */ true, word));
						}
					}
					i++;
				}
			}
		}
	}

	for (auto& it : ret)
	{
		it.setTiles(sanitizedTilesOfMove(it));
		it.setPrettyTiles(prettyTilesOfMove(it));
		it.score = score(it);
	}

	return ret;
}

int Board::score(const Move &move, bool *isBingo) const
{
	if (isBingo != 0)
		*isBingo = false;

	if (move.action == Move::Place)
	{
		int total;
		int laid = 0;
		int mainscore = 0;
		int hookscore = 0;
		int wordmult = 1;

		if (move.horizontal)
		{
			int i = 0;
			const LetterString::const_iterator end(move.tiles().end());
			for (LetterString::const_iterator it = move.tiles().begin(); it != end; ++it, ++i)
			{
				if (m_letters[move.startrow][i + move.startcol] == QUACKLE_NULL_MARK)
				{
					if (QUACKLE_ALPHABET_PARAMETERS->isPlainLetter(*it))
						mainscore += QUACKLE_ALPHABET_PARAMETERS->score(*it) * letterMultiplier(move.startrow, i + move.startcol);

					++laid;

					wordmult *= wordMultiplier(move.startrow, i + move.startcol);

					int thishook = 0;
					int hooked = 0;

					for (int j = move.startrow - 1; j >= 0; --j)
					{
						if (m_letters[j][i + move.startcol] == QUACKLE_NULL_MARK)
							j = -1;
						else
						{
							++hooked;

							if (!m_isBlank[j][i + move.startcol])
								thishook += QUACKLE_ALPHABET_PARAMETERS->score(m_letters[j][i + move.startcol]);
						}
					}

					for (int j = move.startrow + 1; j < m_height; ++j)
					{
						if (m_letters[j][i + move.startcol] == QUACKLE_NULL_MARK)
							j = m_height;
						else
						{
							++hooked;

							if (!m_isBlank[j][i + move.startcol])
								thishook += QUACKLE_ALPHABET_PARAMETERS->score(m_letters[j][i + move.startcol]);
						}
					}

					if (hooked > 0)
					{
						if (QUACKLE_ALPHABET_PARAMETERS->isPlainLetter(*it))
							thishook += QUACKLE_ALPHABET_PARAMETERS->score(*it) * letterMultiplier(move.startrow, i + move.startcol);

						thishook *= wordMultiplier(move.startrow, i + move.startcol);
						hookscore += thishook;
					} 
				}
				else if (!m_isBlank[move.startrow][i + move.startcol])
					mainscore += QUACKLE_ALPHABET_PARAMETERS->score(m_letters[move.startrow][i + move.startcol]);
			}
		}
		else
		{
			int i = 0;
			const LetterString::const_iterator end(move.tiles().end());
			for (LetterString::const_iterator it = move.tiles().begin(); it != end; ++it, ++i)
			{
				if (m_letters[i + move.startrow][move.startcol] == QUACKLE_NULL_MARK)
				{
					if (QUACKLE_ALPHABET_PARAMETERS->isPlainLetter(*it))
						mainscore += QUACKLE_ALPHABET_PARAMETERS->score(*it) * letterMultiplier(i + move.startrow, move.startcol);

					++laid;

					wordmult *= wordMultiplier(i + move.startrow, move.startcol);

					int thishook = 0;
					int hooked = 0;

					for (int j = move.startcol - 1; j >= 0; --j)
					{
						if (m_letters[i + move.startrow][j] == QUACKLE_NULL_MARK)
							j = -1;
						else
						{
							++hooked;

							if (!m_isBlank[i + move.startrow][j])
								thishook += QUACKLE_ALPHABET_PARAMETERS->score(m_letters[i + move.startrow][j]);
						}
					}

					for (int j = move.startcol + 1; j < m_width; ++j)
					{
						if (m_letters[i + move.startrow][j] == QUACKLE_NULL_MARK)
							j = m_width;
						else
						{
							++hooked;

							if (!m_isBlank[i + move.startrow][j])
								thishook += QUACKLE_ALPHABET_PARAMETERS->score(m_letters[i + move.startrow][j]);
						}
					}

					if (hooked > 0)
					{
						if (QUACKLE_ALPHABET_PARAMETERS->isPlainLetter(*it))
							thishook += QUACKLE_ALPHABET_PARAMETERS->score(*it) * letterMultiplier(i + move.startrow, move.startcol);

						thishook *= wordMultiplier(i + move.startrow, move.startcol);
						hookscore += thishook;
					}
				}
				else if (!m_isBlank[i + move.startrow][move.startcol])
					mainscore += QUACKLE_ALPHABET_PARAMETERS->score(m_letters[i + move.startrow][move.startcol]);
			}
		}

		total = hookscore;

		if (move.tiles().length() > 1)
			total += mainscore * wordmult;

		if (laid == QUACKLE_PARAMETERS->rackSize())
		{
			if (isBingo != 0)
				*isBingo = true;
			total += QUACKLE_PARAMETERS->bingoBonus();
		}

#ifdef DEBUG_BOARD
		UVcout << "scoring " << move << " as " << total << "; mainscore: " << mainscore << " wordmult: " << wordmult << " hookscore: " << hookscore << " laid: " << laid << endl;
#endif

		return total;
	}

	// other plays have score of zero
	return 0;
}

LetterString Board::prettyTilesOfMove(const Move &move, bool markPlayThruTiles) const
{
	LetterString ret;
	const int startTileCol = move.startcol;
	const int startTileRow = move.startrow;

	const LetterString::const_iterator end(move.tiles().end());
	int i = 0;
	bool insidePlayThru = false;
	for (LetterString::const_iterator it = move.tiles().begin(); it != end; ++it, ++i)
	{
		if (move.isAlreadyOnBoard(*it))
		{
			int currentTileCol = startTileCol;
			int currentTileRow = startTileRow;

			if (move.horizontal)
				currentTileCol += i;
			else
				currentTileRow += i;

			if (markPlayThruTiles && !insidePlayThru) {
				ret += QUACKLE_PLAYTHRU_START_MARK;
				insidePlayThru = true;
			}

			ret += m_letters[currentTileRow][currentTileCol];
		}
		else 
		{
		        if (insidePlayThru) {
			    assert(markPlayThruTiles);
			    ret += QUACKLE_PLAYTHRU_END_MARK;
			    insidePlayThru = false;
			}
			ret += *it;
		}
	}

	if (insidePlayThru) {
	    assert(markPlayThruTiles);
	    ret += QUACKLE_PLAYTHRU_END_MARK;
	}

	return ret;
}

LetterString Board::sanitizedTilesOfMove(const Move &move) const
{
	if (move.action != Move::Place)
		return move.tiles();

	LetterString ret;
	const int startTileCol = move.startcol;
	const int startTileRow = move.startrow;

	const LetterString::const_iterator end(move.tiles().end());
	int i = 0;
	for (LetterString::const_iterator it = move.tiles().begin(); it != end; ++it, ++i)
	{
			int currentTileCol = startTileCol;
			int currentTileRow = startTileRow;

			if (move.horizontal)
				currentTileCol += i;
			else
				currentTileRow += i;

			if (m_letters[currentTileRow][currentTileCol] == QUACKLE_NULL_MARK)
				ret += *it;
			else
				ret += QUACKLE_PLAYED_THRU_MARK;
	}

	return ret;
}

void Board::makeMove(const Move &move)
{
	if (move.action == Move::Place)
	{
		m_empty = false;
		int col = move.startcol;
		int row = move.startrow;

		const LetterString::const_iterator end(move.tiles().end());
		for (LetterString::const_iterator it = move.tiles().begin(); it != end; ++it)
		{
			if (m_letters[row][col] == QUACKLE_NULL_MARK)
			{
				m_letters[row][col] = *it;
				m_isBlank[row][col] = QUACKLE_ALPHABET_PARAMETERS->isBlankLetter(*it);
			}

			if (move.horizontal)
				col++;
			else
				row++;
		}
	}
}

UVString Board::toString() const
{
	UVOStringStream ss;

	ss << "   ";

	for (int col = 0; col < m_width; ++col)
		ss << (UVChar)(MARK_UV('A') + col) << " ";

	ss << MARK_UV('\n') << "   ";

	for (int col = 0; col < m_width; ++col)
		ss << "--";

	ss << MARK_UV('\n');

	for (int row = 0; row < m_height; row++)
	{
		if ((row + 1) < 10)
			ss << ' ' << row + 1 << '|';
		else
			ss << row + 1 << '|';

		for (int col = 0; col < m_width; col++)
		{
			if (m_letters[row][col] != QUACKLE_NULL_MARK)
			{
				ss << QUACKLE_ALPHABET_PARAMETERS->userVisible(m_letters[row][col]);
			}
			else
			{
				if (letterMultiplier(row, col) == 2)
					ss << "'";
				else if (letterMultiplier(row, col) == 3)
					ss << '"';
				else if (letterMultiplier(row, col) == 4)
					ss << '^';
				else if (wordMultiplier(row, col) == 2)
					ss << '-';
				else if (wordMultiplier(row, col) == 3)
					ss << '=';
				else if (wordMultiplier(row, col) == 4)
					ss << '~';
				else
					ss << ' ';
			}

			if (col < width() - 1)
				ss << ' ';

		}

		ss << '|' << MARK_UV('\n');
	}

	ss << "   ";

	for (int col = 0; col < m_width; ++col)
		ss << "--";

	return ss.str();
}

UVString Board::htmlBoard(const int tileSize) const
{
	UVOStringStream ss;

	const int tdWidth = tileSize;
	const int tdHeight = tdWidth;

	const UVString nothingBgcolor = "gainsboro";
	const UVString markBgcolor = "tan";

	const UVString centerAlign = "valign=\"middle\" align=\"center\"";

	ss << "<table bgcolor=dimgrey>\n";
	ss << "<tr>\n";
	ss << "<td bgcolor=\"" << markBgcolor << "\" " << centerAlign << ">" << "&nbsp;" << "</td>";
	for (int col = 0; col < m_width; ++col)
		ss << "<td width=" << tdWidth << " bgcolor=\"" << markBgcolor << "\" " << centerAlign << ">" << (UVChar)(MARK_UV('A') + col) << "</td>";
	ss << "</tr>\n";

	for (int row = 0; row < m_height; row++)
	{
		ss << "<tr>\n";
		ss << "<td height=" << tdHeight << " bgcolor=\"" << markBgcolor << "\" " << centerAlign << ">" << row + 1 << "</td>\n";

		for (int col = 0; col < m_width; col++)
		{
			UVString bgcolor = nothingBgcolor;
			if (letterMultiplier(row, col) == 2)
				bgcolor = "cornflowerblue";
			else if (letterMultiplier(row, col) == 3)
				bgcolor = "slateblue";
			else if (letterMultiplier(row, col) == 4)
				bgcolor = "blueviolet";
			else if (wordMultiplier(row, col) == 2)
				bgcolor = "palevioletred";
			else if (wordMultiplier(row, col) == 3)
				bgcolor = "firebrick";
			else if (wordMultiplier(row, col) == 4)
				bgcolor = "goldenrod";

			ss << "<td height=" << tdHeight << " width=" << tdWidth << " bgcolor=\"" << bgcolor << "\" " << centerAlign << ">";
			if (m_letters[row][col] != QUACKLE_NULL_MARK)
			{
				const int fontSize = static_cast<int>(tileSize * 5/9);
				if (QUACKLE_ALPHABET_PARAMETERS->isBlankLetter(m_letters[row][col]))
				{
					const int blankFontSize = static_cast<int>(fontSize * 0.8);
					ss << "<table style=\"border: 1pt; border-style: dashed\"><tr><td width=" << tdWidth * 0.8 << " height=" << tdHeight * 0.8 << " bgcolor=\"" << bgcolor << "\" " << centerAlign << ">";
					ss << "<span style=\"font-size: " << blankFontSize << "px\">";
					ss << QUACKLE_ALPHABET_PARAMETERS->userVisible(QUACKLE_ALPHABET_PARAMETERS->clearBlankness(m_letters[row][col]));
					ss << "</span>";
					ss << "</td></tr></table>";
				}
				else
				{
					const int idealValueFontSize = static_cast<int>(tileSize * 2/9);
					const int minimumValueFontSize = 7;
					const int valueFontSize = minimumValueFontSize > idealValueFontSize? minimumValueFontSize : idealValueFontSize;
					ss << "<span style=\"font-size: " << fontSize << "px\">";
					ss << QUACKLE_ALPHABET_PARAMETERS->userVisible(m_letters[row][col]);
					ss << "</span>";
					ss << "<span style=\"font-size: " << valueFontSize << "px\">";
					ss << QUACKLE_ALPHABET_PARAMETERS->score(m_letters[row][col]);
					ss << "</span>";
				}
			}
			else
			{
				ss << "&nbsp;";
			}
			ss << "</td>\n";
		}

		ss << "</tr>\n";
	}

	ss << "</table>\n";

	return ss.str();
}

// TODO this obviously needs some generalization and it should
// show quad bonuses if there are any on the board.
UVString Board::htmlKey() const
{
	return
	"<table border=1>\n"
	"<tr><td width=30 height=30>Color</td><td>Bonus</td></tr>\n"
	"<tr><td width=30 height=30 bgcolor=\"cornflowerblue\">&nbsp;</td><td>Double Letter Score</td></tr>\n"
	"<tr><td width=30 height=30 bgcolor=\"slateblue\">&nbsp;</td><td>Triple Letter Score</td></tr>\n"
	"<tr><td width=30 height=30 bgcolor=\"palevioletred\">&nbsp;</td><td>Double Word Score</td></tr>\n"
	"<tr><td width=30 height=30 bgcolor=\"firebrick\">&nbsp;</td><td>Triple Word Score</td></tr>\n"
	"</table>\n";
}

UVOStream &operator<<(UVOStream &o, const Board &board)
{
	o << board.toString();
	return o;
}

void Board::prepareEmptyBoard()
{
	m_empty = true;

	for (int i = 0; i < m_height; ++i)
	{
		for (int j = 0; j < m_width; ++j)
		{
			m_letters[i][j] = QUACKLE_NULL_MARK;
			m_isBlank[i][j] = false;
			m_vcross[i][j].set();
			m_hcross[i][j].set();
		}
	}
}

Board::TileInformation Board::tileInformation(int row, int col) const
{
	TileInformation ret;

	if (m_letters[row][col] != QUACKLE_NULL_MARK)
	{
		ret.tileType = LetterTile;
		ret.isBlank = m_isBlank[row][col];
		ret.letter = QUACKLE_ALPHABET_PARAMETERS->clearBlankness(m_letters[row][col]);
		ret.isBritish = m_isBritish[row][col];
	}
	else
	{
		if (letterMultiplier(row, col) > 1)
		{
			ret.tileType = BonusSquareTile;
			ret.bonusSquareType = LetterBonus;
			ret.bonusMultiplier = letterMultiplier(row, col);
		}
		else if (wordMultiplier(row, col) > 1)
		{
			ret.tileType = BonusSquareTile;
			ret.bonusSquareType = WordBonus;
			ret.bonusMultiplier = wordMultiplier(row, col);
		}
	}

	if (row == QUACKLE_BOARD_PARAMETERS->startRow() && col == QUACKLE_BOARD_PARAMETERS->startColumn())
		ret.isStartLocation = true;

    ret.isOnRack = false;

	return ret;
}
