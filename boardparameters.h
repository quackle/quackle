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

#ifndef QUACKLE_BOARDPARAMETERS_H
#define QUACKLE_BOARDPARAMETERS_H

#include "board.h"

namespace Quackle
{

class BoardParameters
{
public:
	BoardParameters();

	// Does not serialize name...caller still has to get/set name manually
	void Serialize(ostream &stream);
	static BoardParameters *Deserialize(istream &stream);
	
	int width() const;
	void setWidth(int width);

	int height() const;
	void setHeight(int width);

	// start row and column are zero-indexed!
	int startRow() const;
	void setStartRow(int startRow);

	int startColumn() const;
	void setStartColumn(int startRow);

	enum LetterMultiplier { sls=1, dls=2, tls=3, qls=4, lsCount = qls };
	int letterMultiplier(int row, int column) const;
	void setLetterMultiplier(int row, int column, LetterMultiplier multiplier);

	enum WordMultiplier { sws=1, dws=2, tws=3, qws=4, wsCount = qws };
	int wordMultiplier(int row, int column) const;
	void setWordMultiplier(int row, int column, WordMultiplier multiplier);

	// unused by libquackle
	UVString name() const;
	void setName(const UVString &name);

protected:
	int m_width;
	int m_height;

	int m_startRow;
	int m_startColumn;

	int m_letterMultipliers[QUACKLE_MAXIMUM_BOARD_SIZE][QUACKLE_MAXIMUM_BOARD_SIZE];
	int m_wordMultipliers[QUACKLE_MAXIMUM_BOARD_SIZE][QUACKLE_MAXIMUM_BOARD_SIZE];

	UVString m_name;
};

inline int BoardParameters::width() const
{
	return m_width;
}

inline void BoardParameters::setWidth(int width)
{
	m_width = width;
}

inline int BoardParameters::height() const
{
	return m_height;
}

inline void BoardParameters::setHeight(int height)
{
	m_height = height;
}

inline int BoardParameters::startRow() const
{
	return m_startRow;
}

inline void BoardParameters::setStartRow(int startRow)
{
	m_startRow = startRow;
}

inline int BoardParameters::startColumn() const
{
	return m_startColumn;
}

inline void BoardParameters::setStartColumn(int startColumn)
{
	m_startColumn = startColumn;
}

inline int BoardParameters::letterMultiplier(int row, int column) const
{
	return m_letterMultipliers[row][column];
}

inline void BoardParameters::setLetterMultiplier(int row, int column, BoardParameters::LetterMultiplier multiplier)
{
	m_letterMultipliers[row][column] = (int) multiplier;
}

inline int BoardParameters::wordMultiplier(int row, int column) const
{
	return m_wordMultipliers[row][column];
}

inline void BoardParameters::setWordMultiplier(int row, int column, BoardParameters::WordMultiplier multiplier)
{
	m_wordMultipliers[row][column] = (int) multiplier;
}

inline UVString BoardParameters::name() const
{
	return m_name;
}

inline void BoardParameters::setName(const UVString &name)
{
	m_name = name;
}

// Name: A Random Board I Dislike
class EnglishBoard : public BoardParameters
{
public:
	EnglishBoard();
};

}
#endif
