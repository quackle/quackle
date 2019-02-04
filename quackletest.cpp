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
#include <string>

#include "boardparameters.h"
#include "computerplayer.h"
#include "computerplayercollection.h"
#include "datamanager.h"
#include "generator.h"
#include "lexiconparameters.h"
#include "reporter.h"
#include "strategyparameters.h"
#include "bogowinplayer.h"
#include "game.h"
#include "sim.h"

void testAdvanceToEnd(Quackle::Game &game);
void testAnagrammer();
void testBag(Quackle::Game &game);
void testBasic(Quackle::Game &game);
void testEndgame(Quackle::Game &game);
void testSimulation(Quackle::Game &game);
void testValidator(Quackle::Game &game);
void testGame();
void testGameReport(const Quackle::Game &game);

int main()
{
	Quackle::DataManager dataManager;

	dataManager.setAppDataDirectory("data");
	dataManager.lexiconParameters()->loadDawg(Quackle::LexiconParameters::findDictionaryFile("twl06.dawg"));
	dataManager.lexiconParameters()->loadGaddag(Quackle::LexiconParameters::findDictionaryFile("twl06.gaddag"));
	dataManager.strategyParameters()->initialize("twl06");
	dataManager.setBoardParameters(new Quackle::EnglishBoard());

	const bool seedRandoms = false;
	if (seedRandoms)
		dataManager.seedRandomNumbers('E' + 'm' + 'i' + 'l' + 'y' + 'Y' + 'K' + 'o');

	const int gameCnt = 1000;
	//const int gameCnt = 1;
	for (int game = 0; game < gameCnt; ++game) {
		testGame();
	}
	
	return 0;
}

void testGame()
{
	Quackle::Game game;

	Quackle::PlayerList players;

	Quackle::Player bogowinA(MARK_UV("BogowinA"), Quackle::Player::ComputerPlayerType, 110);
	bogowinA.setComputerPlayer(new Quackle::SmartBogowin());
	players.push_back(bogowinA);
	
	Quackle::Player bogowinB(MARK_UV("BogowinB"), Quackle::Player::ComputerPlayerType, 110);
	bogowinB.setComputerPlayer(new Quackle::SmartBogowin());
	players.push_back(bogowinB);

	game.setPlayers(players);
	game.associateKnownComputerPlayers();

	game.addPosition();

	//testValidator(game);

	const bool setupRetroPosition = false;

	if (setupRetroPosition)
	{
		game.commitMove(Quackle::Move::createPlaceMove(MARK_UV("8c"), QUACKLE_ALPHABET_PARAMETERS->encode(MARK_UV("AMNION"))));
		game.currentPosition().setCurrentPlayerRack(Quackle::Rack(QUACKLE_ALPHABET_PARAMETERS->encode(MARK_UV("L"))));
		UVcout << "current rack: " << game.currentPosition().currentPlayer().rack() << endl;
		game.currentPosition().kibitz(10);
		UVcout << "moves: " << endl << game.currentPosition().moves() << endl;
	}

	const int playahead = 50;

	for (int i = 0; i < playahead; ++i)
	{
		if (game.currentPosition().gameOver())
		{
			UVcout << "GAME OVER" << endl;
			break;
		}

		const Quackle::Player player(game.currentPosition().currentPlayer());
		Quackle::Move compMove(game.haveComputerPlay());
		UVcout << "with " << player.rack() << ", " << player.name() << " commits to " << compMove << endl;
	}

	UVcout << game.currentPosition() << endl;

	testGameReport(game);

	// insert test...() calls here
	// testSimulation(game);
	
	//UVcout << "History:" << endl << game.history() << endl;
}

void testGameReport(const Quackle::Game &game)
{
	UVString report;
	Quackle::StaticPlayer player;
	Quackle::Reporter::reportGame(game, &player, &report);
	UVcout << report;
}

void testBasic(Quackle::Game &game)
{
}

void spitOutWords(const vector<Quackle::LetterString> &words)
{
	vector<Quackle::LetterString>::const_iterator end = words.end();
	for (vector<Quackle::LetterString>::const_iterator it = words.begin(); it != end; ++it)
		UVcout << QUACKLE_ALPHABET_PARAMETERS->userVisible(*it) << endl;
}

void testAnagrammer()
{
	Quackle::Generator generator;
	UVcout << "anagrams of EFIILNT: " << endl;
	spitOutWords(generator.anagramLetters(QUACKLE_ALPHABET_PARAMETERS->encode(MARK_UV("EFIILNT"))));
	UVcout << "anagrams of AEPRSu: " << endl;
	spitOutWords(generator.anagramLetters(QUACKLE_ALPHABET_PARAMETERS->encode(MARK_UV("AEPRSu"))));
	UVcout << "anagrams of EFIILNT?: " << endl;
	spitOutWords(generator.anagramLetters(QUACKLE_ALPHABET_PARAMETERS->encode(MARK_UV("EFIILNT?"))));
	UVcout << "build of EFIILNT: " << endl;
	spitOutWords(generator.anagramLetters(QUACKLE_ALPHABET_PARAMETERS->encode(MARK_UV("EFIILNT")), Quackle::Generator::NoRequireAllLetters));
	UVcout << "free search of NUTPICK: " << endl;
	spitOutWords(generator.anagramLetters(QUACKLE_ALPHABET_PARAMETERS->encode(MARK_UV("NUTPICK")), Quackle::Generator::AddAnyLetters));
}

void testAdvanceToEnd(Quackle::Game &game)
{
	UVcout << "now advancing to end..." << endl;

	game.advanceToNoncomputerPlayer();

	UVcout << game.currentPosition();

	Quackle::PlayerList winners(game.currentPosition().leadingPlayers());
	for (Quackle::PlayerList::const_iterator it = winners.begin(); it != winners.end(); ++it)
		UVcout << *it << " wins!!" << endl;
}

void testBag(Quackle::Game &game)
{
	UVcout << "BAG TEST" << endl;
	UVcout << "current player rack: " << game.currentPosition().currentPlayer().rack() << endl;
	game.currentPosition().setCurrentPlayerRack(Quackle::Rack(QUACKLE_ALPHABET_PARAMETERS->encode(MARK_UV("AAAAAAA"))));
	UVcout << "current player rack set to: " << game.currentPosition().currentPlayer().rack() << endl;
	UVcout << "current bag now " << game.currentPosition().bag() << endl;
	game.commitMove(Quackle::Move::createPlaceMove(MARK_UV("8d"), QUACKLE_ALPHABET_PARAMETERS->encode(MARK_UV("PEAFOwL"))));
	UVcout << "current bag after PEAFOwL " << game.currentPosition().bag() << endl;
}

void testEndgame(Quackle::Game &game)
{
	// dataManager.setEvaluator(new Quackle::CatchallEvaluator());

	const int playahead = 20;
	for (int i = 0; i < playahead; ++i)
	{
		if (game.currentPosition().gameOver())
			break;
		Quackle::Move compMove(game.haveComputerPlay());
	}
}

void testSimulation(Quackle::Game &game)
{
	const int kibitzLength = 3;
	game.currentPosition().kibitz(kibitzLength);

	//UVcout << game.currentPosition() << endl;
	UVcout << game.currentPosition().moves() << endl;

	Quackle::Simulator simulator;
	simulator.setLogfile("quackletest.simulation", false);
	simulator.setPosition(game.currentPosition());

	simulator.simulate(20, 5);
	UVcout << simulator.simmedMoves() << endl;
	UVcout << "after " << simulator.iterations() << " iterations pruning to those within five points" << endl;

	simulator.pruneTo(5, kibitzLength);

	int iterationStep = 1;
	int iterationsToRun = 2;

	const bool longSim = true;
	if (longSim)
	{
		iterationStep = 10;
		iterationsToRun = 50;
	}

	const int plies = 2;

	simulator.setIgnoreOppos(false);

	for (int iterations = 0; iterations < iterationsToRun; iterations += iterationStep)
	{
		simulator.simulate(plies, iterationStep);
		UVcout << "sim results after " << iterations + iterationStep << " iterations: " << endl;

		const Quackle::SimmedMoveList &moves = simulator.simmedMoves();

		for (Quackle::SimmedMoveList::const_iterator it = moves.begin(); it != moves.end(); ++it)
		{
			UVcout << *it << endl;
		}

		UVcout << endl;
	}
}

void testValidator(Quackle::Game &game)
{
	game.commitMove(Quackle::Move::createPlaceMove(MARK_UV("8d"), QUACKLE_ALPHABET_PARAMETERS->encode(MARK_UV("MANIA"))));
	game.commitMove(Quackle::Move::createPlaceMove(MARK_UV("7c"), QUACKLE_ALPHABET_PARAMETERS->encode(MARK_UV("RANI"))));
	game.commitMove(Quackle::Move::createPlaceMove(MARK_UV("f6"), QUACKLE_ALPHABET_PARAMETERS->encode(MARK_UV("P..ION"))));
	game.commitMove(Quackle::Move::createPlaceMove(MARK_UV("10b"), QUACKLE_ALPHABET_PARAMETERS->encode(MARK_UV("STAT.R"))));
	game.currentPosition().setCurrentPlayerRack(Quackle::Rack(QUACKLE_ALPHABET_PARAMETERS->encode(MARK_UV("AIAIAIAI"))));

	UVcout << game.currentPosition().board().allWordsFormedBy(Quackle::Move::createPlaceMove(MARK_UV("9c"), QUACKLE_ALPHABET_PARAMETERS->encode(MARK_UV("AI"))));
}

