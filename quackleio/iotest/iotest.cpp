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

#include <QtCore>

#include "datamanager.h"
#include "game.h"
#include "gcgio.h"
#include "trademarkedboards.h"

void testGCGIO();

int main()
{
	Quackle::DataManager dataManager;
	dataManager.setBoardParameters(new ScrabbleBoard());

	testGCGIO();
	return 0;
}

void testGCGIO()
{
	QuackleIO::GCGIO io;
	QFile file("capp.gcg");

	if (!file.open(QIODevice::ReadOnly | QIODevice::Text))
	{
		cerr << "Could not open gcg" << endl;
		return;
	}

	QTextStream in(&file);

	Quackle::Game *game = io.read(in, QuackleIO::Logania::BasicLoad);
	UVcout << game->history() << endl;

	UVcout << "Final scores: " << endl;
	Quackle::PlayerList players = game->currentPosition().endgameAdjustedScores();
	const Quackle::PlayerList::const_iterator end(players.end());
	for (Quackle::PlayerList::const_iterator it = players.begin(); it != end; ++it)
		UVcout << *it << endl;

	file.close();

	QFile outFile("my-capp.gcg");

	if (!outFile.open(QIODevice::WriteOnly | QIODevice::Text))
	{
		cerr << "Could not open gcg output file" << endl;
		return;
	}

	QTextStream out(&outFile);
	io.write(*game, out);

	outFile.close();

	delete game;
}

