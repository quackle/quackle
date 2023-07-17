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

#include <time.h>
#include <sys/stat.h>
#include <cstdlib>

#include "catchall.h"
#include "computerplayer.h"
#include "datamanager.h"
#include "alphabetparameters.h"
#include "boardparameters.h"
#include "evaluator.h"
#include "catchall.h"
#include "gameparameters.h"
#include "lexiconparameters.h"
#include "strategyparameters.h"

#define QUACKLDEBUG

using namespace Quackle;
using namespace std;

DataManager *DataManager::m_self = 0;

DataManager::DataManager()
	: m_evaluator(0), m_parameters(0), m_alphabetParameters(0), m_boardParameters(0), m_lexiconParameters(0), m_strategyParameters(0)
{
	m_self = this;
	setAppDataDirectory(".");
	setUserDataDirectory(".");

	seed_seq session_seed = {(unsigned) random_device{}(), (unsigned) time(nullptr)};
	seedRandomNumbers(session_seed);

	m_alphabetParameters = new EnglishAlphabetParameters;
	m_evaluator = new CatchallEvaluator;
	m_parameters = new EnglishParameters;
	m_boardParameters = new EnglishBoard;
	m_lexiconParameters = new LexiconParameters;
	m_strategyParameters = new StrategyParameters;
}

DataManager::~DataManager()
{
	delete m_evaluator;
	delete m_parameters;
	delete m_alphabetParameters;
	delete m_boardParameters;
	delete m_lexiconParameters;
	delete m_strategyParameters;

	cleanupComputerPlayers();
}

bool DataManager::isGood() const
{
	return m_lexiconParameters->hasSomething();
}

void DataManager::setEvaluator(Evaluator *evaluator)
{
	delete m_evaluator;
	m_evaluator = evaluator;
}

void DataManager::setParameters(GameParameters *parameters)
{
	delete m_parameters;
	m_parameters = parameters;
}

void DataManager::setAlphabetParameters(AlphabetParameters *alphabetParameters)
{
	delete m_alphabetParameters;
	m_alphabetParameters = alphabetParameters;
}

void DataManager::setBoardParameters(BoardParameters *boardParameters)
{
	delete m_boardParameters;
	m_boardParameters = boardParameters;
}

void DataManager::setLexiconParameters(LexiconParameters *lexiconParameters)
{
	delete m_lexiconParameters;
	m_lexiconParameters = lexiconParameters;
}

void DataManager::setStrategyParameters(StrategyParameters *strategyParameters)
{
	delete m_strategyParameters;
	m_strategyParameters = strategyParameters;
}

void DataManager::setComputerPlayers(const PlayerList &playerList)
{
	cleanupComputerPlayers();
	m_computerPlayers = playerList;
}

void DataManager::cleanupComputerPlayers()
{
	const PlayerList::iterator end(m_computerPlayers.end());
	for (PlayerList::iterator it = m_computerPlayers.begin(); it != end; ++it)
		delete (*it).computerPlayer();
	
	m_computerPlayers.clear();
}

bool DataManager::fileExists(const string &filename)
{
	// fixme: convert to wchar
	struct stat buf;
	int i = stat(filename.c_str(), &buf);
	if (i == 0)
		return true;
	else
		return false;
}

string DataManager::findDataFile(const string &subDirectory, const string &lexicon, const string &file)
{
	string fname = makeDataFilename(subDirectory, lexicon, file, true);
	if (!fileExists(fname))
		fname = makeDataFilename(subDirectory, lexicon, file, false);
	if (!fileExists(fname) && lexicon.substr(0,3) == "csw")
		fname = makeDataFilename(subDirectory, "csw", file, false);
	if (!fileExists(fname))
		fname = makeDataFilename(subDirectory, m_backupLexicon, file, false);
	if (!fileExists(fname))
		fname = makeDataFilename(subDirectory, "default", file, false);
	if (!fileExists(fname))
		fname = string();
	
	return fname;
}

string DataManager::findDataFile(const string &subDirectory, const string &file)
{
	string fname = makeDataFilename(subDirectory, file, true);
	if (!fileExists(fname))
		fname = makeDataFilename(subDirectory, file, false);
	if (!fileExists(fname))
		fname = string();

	return fname;
}

bool DataManager::hasUserDataFile(const string &subDirectory, const string &file)
{
	string fname = makeDataFilename(subDirectory, file,	true);
	return fileExists(fname);
}

string DataManager::makeDataFilename(const string &subDirectory, const string &lexicon, const string &file, bool user)
{
	return (user ? m_userDataDirectory : m_appDataDirectory) + "/" + subDirectory + "/" + lexicon + "/" + file;
}

string DataManager::makeDataFilename(const string &subDirectory, const string &file, bool user)
{
	return (user ? m_userDataDirectory : m_appDataDirectory) + "/" + subDirectory + "/" + file;
}

void DataManager::seedRandomNumbers(unsigned int seed)
{
	lock_guard<mutex> lock(m_RngMutex);
	m_mersenneTwisterRng.seed(seed);
}

void DataManager::seedRandomNumbers(seed_seq& seed)
{
	lock_guard<mutex> lock(m_RngMutex);
	m_mersenneTwisterRng.seed(seed);
}

int DataManager::randomInteger(int low, int high)
{
	lock_guard<mutex> lock(m_RngMutex);
	return uniform_int_distribution<>(low, high)(m_mersenneTwisterRng);
}
