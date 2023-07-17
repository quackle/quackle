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

#include "boardparameters.h"

using namespace Quackle;
using namespace std;

BoardParameters::BoardParameters()
	: m_width(15), m_height(15), m_startRow(7), m_startColumn(7)
{
	m_name = MARK_UV("Empty Board");

	for (int i = 0; i < QUACKLE_MAXIMUM_BOARD_SIZE; ++i)
	{
		for (int j = 0; j < QUACKLE_MAXIMUM_BOARD_SIZE; ++j)
		{
			m_letterMultipliers[i][j] = 1;
			m_wordMultipliers[i][j] = 1;
		}
	}
}

void BoardParameters::Serialize(ostream &stream)
{
	stream << "Quackle\n" << 1; // board version number, in case the format changes
	stream << " " << m_width << " " << m_height;
	stream << " " << m_startRow << " " << m_startColumn;
	for (int i = 0; i < m_width; i++)
		for (int j = 0; j < m_height; j++)
			stream << " " << m_letterMultipliers[i][j] << " " << m_wordMultipliers[i][j];
}

BoardParameters *BoardParameters::Deserialize(istream &stream)
{
	BoardParameters *param = new BoardParameters();
	string gameId;
	int version;
	
	stream >> gameId;
	if (gameId != "Quackle") {
        delete param;
		return 0;
    }

	stream.ignore();
	stream >> version;
		
	if (version > 1) {
        delete param;
		return 0;
    }
		
	stream >> param->m_width >> param->m_height;
	stream >> param->m_startRow >> param->m_startColumn;
	if (!stream.eof() && !stream.fail())
	{
		for (int i = 0; i < param->m_width; i++)
			for (int j = 0; j < param->m_height; j++)
				stream >> param->m_letterMultipliers[i][j] >> param->m_wordMultipliers[i][j];
	}
	
	return param;
}

//////////

EnglishBoard::EnglishBoard()
{
	m_height = 13;
	m_width = 17;

	m_startRow = 6;
	m_startColumn = 8;

	m_name = MARK_UV("A Random Board I Dislike");

    const int letterm[13][17] =
	{
      // A  B  C  D  E  F  G  H  I  J  K  L  M  N  O  P  Q
        {1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1},
        {1, 1, 1, 1, 1, 3, 1, 1, 1, 3, 1, 1, 1, 1, 1, 1, 1},
        {1, 1, 1, 1, 1, 1, 2, 1, 2, 1, 1, 1, 1, 1, 1, 2, 1},
        {2, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 2, 1, 3},
        {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
        {1, 3, 1, 1, 1, 3, 1, 1, 1, 3, 1, 1, 1, 3, 1, 1, 1},
        {1, 1, 2, 1, 1, 1, 2, 1, 2, 1, 1, 1, 2, 1, 1, 1, 1},
        {1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1},
        {1, 1, 2, 1, 1, 1, 2, 1, 2, 1, 1, 1, 2, 1, 1, 1, 1},
        {1, 3, 1, 1, 1, 3, 1, 1, 1, 3, 1, 1, 1, 3, 1, 1, 3},
        {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1},
        {2, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 2, 1, 1},
        {1, 1, 1, 1, 1, 1, 2, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1},
    };

    const int wordm[13][17] =
	{
      // A  B  C  D  E  F  G  H  I  J  K  L  M  N  O  P  Q
        {3, 1, 1, 1, 1, 1, 1, 3, 1, 1, 1, 1, 1, 1, 3, 1, 4},
        {1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1},
        {1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1},
        {1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1},
        {1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1},
        {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
        {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 4},
        {3, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 3, 1, 1},
        {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
        {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
        {1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1},
        {1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1},
        {1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 4},
    }; 

	for (int i = 0; i < 13; ++i)
	{
		for (int j = 0; j < 17; ++j)
		{
			m_letterMultipliers[i][j] = letterm[i][j];
			m_wordMultipliers[i][j] = wordm[i][j];
		}
	}
}

