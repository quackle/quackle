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

#ifndef QUACKLE_DATAMANAGER_H
#define QUACKLE_DATAMANAGER_H

#include <string>

#include "playerlist.h"

using namespace std;

#define QUACKLE_DATAMANAGER Quackle::DataManager::self()
#define QUACKLE_DATAMANAGER_EXISTS Quackle::DataManager::exists()
#define QUACKLE_EVALUATOR Quackle::DataManager::self()->evaluator()
#define QUACKLE_PARAMETERS Quackle::DataManager::self()->parameters()
#define QUACKLE_ALPHABET_PARAMETERS Quackle::DataManager::self()->alphabetParameters()
#define QUACKLE_BOARD_PARAMETERS Quackle::DataManager::self()->boardParameters()
#define QUACKLE_LEXICON_PARAMETERS Quackle::DataManager::self()->lexiconParameters()
#define QUACKLE_STRATEGY_PARAMETERS Quackle::DataManager::self()->strategyParameters()
#define QUACKLE_COMPUTER_PLAYERS Quackle::DataManager::self()->computerPlayers()

namespace Quackle
{

// General singleton type that will be around whenever
// you use libquackle.
// It provides access to lexica (todo), random numbers,
// and all parameters for a game

class AlphabetParameters;
class BoardParameters;
class Evaluator;
class GameParameters;
class LexiconParameters;
class PlayerList;
class StrategyParameters;

class DataManager
{
public:
	// sets up singleton, seeds random number generator,
	// and creates default parameter instances
	DataManager();

	~DataManager();

	static DataManager *self();
	static bool exists();

	// Are we in shape to run a game?
	// Makes sure there's a lexicon at least.
	bool isGood() const;

	Evaluator *evaluator();
	void setEvaluator(Evaluator *evaluator);

	// game and board parameter objects are owned and deleted by
	// this data manager
	GameParameters *parameters();
	void setParameters(GameParameters *parameters);

	AlphabetParameters *alphabetParameters();
	void setAlphabetParameters(AlphabetParameters *alphabetParameters);

	BoardParameters *boardParameters();
	void setBoardParameters(BoardParameters *boardParameters);

	LexiconParameters *lexiconParameters();
	void setLexiconParameters(LexiconParameters *lexiconParameters);

	StrategyParameters *strategyParameters();
	void setStrategyParameters(StrategyParameters *strategyParameters);

	// When the data manager dies or setComputerPlayers is called, it deletes
	// all of the computer players pointed to by the players in this list. The
	// players' names are the names of the computer players, and the players'
	// ids are the ids of the computer players. The last player on the list
	// should be considered the best player.
	const PlayerList &computerPlayers() const;
	void setComputerPlayers(const PlayerList &playerList);
	void cleanupComputerPlayers();

	// Find a file at datadir/subdir/lexicon/file.
	// If this doesn't exist, tries backupLexicon instead of lexicon.
	// Returns empty string if the file is not found.
	string findDataFile(const string &subDirectory, const string &lexicon, const string &file);

	// Find a file at datadir/subdir/file.
	// Returns empty string if the file is not found.
	string findDataFile(const string &subDirectory, const string &file);

	// Returns true if the data file is in user-land.
	bool hasUserDataFile(const string &subDirectory, const string &file);

	// returns similarly-named file
	string makeDataFilename(const string &subDirectory, const string &lexicon, const string &file, bool user);
	string makeDataFilename(const string &subDirectory, const string &file, bool user);

	void setBackupLexicon(string backupLexicon) { m_backupLexicon = backupLexicon; }
	string backupLexicon() { return m_backupLexicon; }

	void setAppDataDirectory(string directory) { m_appDataDirectory = directory; }
	string appDataDirectory() { return m_appDataDirectory; }

	void setUserDataDirectory(string directory) { m_userDataDirectory = directory; }
	string userDataDirectory() { return m_userDataDirectory; }

	void seedRandomNumbers(unsigned int seed);
	int randomNumber();

private:
	static DataManager *m_self;

	bool fileExists(const string &filename);

	string m_appDataDirectory;

	string m_userDataDirectory;

	// lexicon that has all data files
	string m_backupLexicon;

	Evaluator *m_evaluator;
	GameParameters *m_parameters;
	AlphabetParameters *m_alphabetParameters;
	BoardParameters *m_boardParameters;
	LexiconParameters *m_lexiconParameters;
	StrategyParameters *m_strategyParameters;

	PlayerList m_computerPlayers;
};

inline DataManager *DataManager::self()
{
	return m_self;
}

inline bool DataManager::exists()
{
	return m_self != 0;
}

inline Evaluator *DataManager::evaluator()
{
	return m_evaluator;
}

inline GameParameters *DataManager::parameters()
{
	return m_parameters;
}

inline AlphabetParameters *DataManager::alphabetParameters()
{
	return m_alphabetParameters;
}

inline BoardParameters *DataManager::boardParameters()
{
	return m_boardParameters;
}

inline LexiconParameters *DataManager::lexiconParameters()
{
	return m_lexiconParameters;
}

inline StrategyParameters *DataManager::strategyParameters()
{
	return m_strategyParameters;
}

inline const PlayerList &DataManager::computerPlayers() const
{
	return m_computerPlayers;
}

}

#endif
