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

#include <iostream>
#include <limits>
#include <algorithm>

#include <bogowinplayer.h>
#include <computerplayercollection.h>
#include <resolvent.h>
#include <datamanager.h>
#include <endgameplayer.h>
#include <game.h>
#include <gameparameters.h>
#include <lexiconparameters.h>
#include <strategyparameters.h>
#include <enumerator.h>
#include <reporter.h>

#include <quackleio/dictimplementation.h>
#include <quackleio/flexiblealphabet.h>
#include <quackleio/froggetopt.h>
#include <quackleio/gcgio.h>
#include <quackleio/util.h>

#include "trademarkedboards.h"
#include "testharness.h"

using namespace Quackle;

static ComputerPlayer*
checkPlayerName(const QString& computer)
{
	bool playerFound = false;
	const Quackle::Player &player = QUACKLE_COMPUTER_PLAYERS.playerForName(QuackleIO::Util::qstringToString(computer), playerFound);
	if (playerFound)
	{
		return player.computerPlayer();
	}
	else
	{
		UVcout << "Computer " << QuackleIO::Util::qstringToString(computer) << " not found!" << endl;
		exit(1);
	}
}

TestHarness::TestHarness()
	: m_computerPlayerToTest(0), m_computerPlayer2ToTest(0), m_quiet(false)
{
	m_gamesDir = "games";
	m_dataManager.setComputerPlayers(Quackle::ComputerPlayerCollection::fullCollection());
}

TestHarness::~TestHarness()
{
}

const char *usage =
"Optional arguments:\n"
"--computer=; sets the computer player (default 'Speedy Player').\n"
"--mode=\n"
"       'positions' (default) runs computer player all positions.\n"
"       'report' asks computer player for report on all positions.\n"
"       'htmlreport' asks for html report on all positions.\n"
"       'selfplay' runs 1000 selfplay games.\n"
"       'playability' output info for computing playability values.\n"
"       'enumerate' lists all racks.\n"
"       'staticleaves' output static leave values of racks in 'racks' file.\n"
"       'randomracks' spit out random racks (forever?).\n"
"       'leavecalc' spit out roughish values of leaves in 'leaves' file.\n"
"       'anagram' anagrams letters supplied in --letters.\n"
"--position=game.gcg; this option can be repeated to specify positions\n"
"                     to test.\n"
"--lexicon=; sets the lexicon (default 'twl06').\n"
"--alphabet=; sets the alphabet (default 'english').\n"
"--seed=integer; set the random seed for reproducability.\n"
"--report; generate reports for selfplay games (default false).\n"
"--letters; letters to anagram.\n"
"--build; when mode is anagram, do not require that all letters be used.\n"
"--quiet; print nothing during selfplay games (default false).\n"
"--repetitions=integer; the number of games for selfplay (default 1000).\n";

void TestHarness::executeFromArguments()
{
	GetOpt opts;

	QString mode;
	QString computer;
	QString computer2;
	QString seedString;
	QString repString;
	bool build;
	QString letters;
	bool help;
	bool report;
	unsigned int seed = numeric_limits<unsigned int>::max();
	unsigned int reps = 1000;

	opts.addOption('c', "computer", &computer);
	opts.addOption('d', "computer2", &computer2);
	opts.addOption('a', "alphabet", &m_alphabet);
	opts.addOption('l', "lexicon", &m_lexicon);
	opts.addOption('m', "mode", &mode);
	opts.addOption('s', "seed", &seedString);
	opts.addOption('r', "repetitions", &repString);
	opts.addOption('t', "letters", &letters);
	opts.addRepeatableOption("position", &m_positions);

	opts.addSwitch("report", &report);
	opts.addSwitch("build", &build);
	opts.addSwitch("quiet", &m_quiet);
	opts.addSwitch("help", &help);

	if (!opts.parse())
		return;

	if (help)
	{
		UVcout << usage << endl;
		return;
	}

	if (mode.isNull())
		mode = "positions";
	if (computer.isNull())
		computer = "Speedy Player";
	if (computer2.isNull())
		computer2 = computer;
	if (m_lexicon.isNull())
		m_lexicon = "twl06";
	if (m_alphabet.isNull())
		m_alphabet = "english";
	if (!seedString.isNull())
	        seed = seedString.toUInt();
	if (!repString.isNull())
	        reps = repString.toUInt();


	m_computerPlayerToTest = checkPlayerName(computer);
	m_computerPlayer2ToTest = checkPlayerName(computer2);

	startUp();

	if (mode == "positions")
		testPositions();
	else if (mode == "report")
		testReport(false);
	else if (mode == "htmlreport")
		testReport(true);
	else if (mode == "enumerate")
		enumerateAll();
	else if (mode == "staticleaves")
		staticLeaves(QString("racks"));
	else if (mode == "randomracks")
		randomRacks();
	else if (mode == "leavecalc")
		leaveCalc(QString("leaves"));
	else if (mode == "anagram")
		anagram(letters, build);
	else if (mode == "selfplay")
		selfPlayGames(seed, reps, report, false);
	else if (mode == "playability")
		selfPlayGames(seed, reps, report, true);
	else if (mode == "worddump")
		wordDump();
	else if (mode == "bingos")
		bingos();
}

void TestHarness::startUp()
{
	UVcout << "Starting up.";

	m_dataManager.setBackupLexicon("twl06");
	m_dataManager.setAppDataDirectory("../data");

	QString alphabetFile = QuackleIO::Util::stdStringToQString(Quackle::AlphabetParameters::findAlphabetFile(QuackleIO::Util::qstringToStdString(m_alphabet)));
	QuackleIO::FlexibleAlphabetParameters *flexure = new QuackleIO::FlexibleAlphabetParameters;
	if (flexure->load(alphabetFile))
	{
		m_dataManager.setAlphabetParameters(flexure);
	}
	else
	{
		UVcerr << "Couldn't load alphabet " << QuackleIO::Util::qstringToString(m_alphabet) << endl;
		delete flexure;
	}

	m_dataManager.setBoardParameters(new ScrabbleBoard());

	m_dataManager.lexiconParameters()->loadDawg(Quackle::LexiconParameters::findDictionaryFile(QuackleIO::Util::qstringToStdString(m_lexicon + ".dawg")));
	UVcout << ".";

   	m_dataManager.lexiconParameters()->loadGaddag(Quackle::LexiconParameters::findDictionaryFile(QuackleIO::Util::qstringToStdString(m_lexicon + ".gaddag")));
	UVcout << ".";

	m_dataManager.strategyParameters()->initialize(QuackleIO::Util::qstringToStdString(m_lexicon));

	UVcout << endl;

	m_gamesDir = QString("games_PLAYERNAME_%1").arg(QDateTime::currentDateTime().toString("dd.MM_hh.mm.ss"));
}

void TestHarness::testFromFile(const QString &file)
{
	UVcout << "Testing game from " << QuackleIO::Util::qstringToString(file) << endl;
	Quackle::Game *game = createNewGame(file);
	if (game)
	{
		testPosition(game->currentPosition(), computerPlayerToTest());
	}

	delete game;
}

double TestHarness::leaveSim(const Rack &R, int iterations)
{
	double sum = 0.0;

	UVcout << R << endl;
	Bag B;
	B.removeLetters(R.tiles());

	int tilesToLeave = 14 + rand() % (93 - 14);

	for (int i = 0; i < iterations; i++)
	{
		Quackle::Game game;

		Quackle::PlayerList players;

		Quackle::Player compyA(m_computerPlayerToTest->name() + MARK_UV(" A"), Quackle::Player::ComputerPlayerType, 0);
		compyA.setAbbreviatedName(MARK_UV("A"));
		compyA.setComputerPlayer(m_computerPlayerToTest);
		players.push_back(compyA);

		Quackle::Player compyB(m_computerPlayer2ToTest->name() + MARK_UV(" B"), Quackle::Player::ComputerPlayerType, 1);
		compyB.setAbbreviatedName(MARK_UV("B"));
		compyB.setComputerPlayer(m_computerPlayer2ToTest);
		players.push_back(compyB);

		game.setPlayers(players);
		game.associateKnownComputerPlayers();

		game.addPosition();

		game.currentPosition().setCurrentPlayerRack(Rack(""), true);
		game.currentPosition().setOppRack(Rack(""), true);

		Bag startBag = B;
		game.currentPosition().setBag(startBag);

		game.addPosition();

		UVcout << "NEW GAME" << endl;

		while (game.currentPosition().bag().size() > tilesToLeave)
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

		game.currentPosition().setCurrentPlayerRack(R, true);
		Quackle::Move compMove(game.haveComputerPlay());
		sum += compMove.equity;
	}

	return sum / iterations;
}

void TestHarness::leaveCalc(const QString &filename)
{
	QuackleIO::GCGIO io;
	QFile file(filename);

	if (!file.open(QIODevice::ReadOnly | QIODevice::Text))
	{
		UVcout << "Could not open file " << QuackleIO::Util::qstringToString(filename) << endl;
		return;
	}

	QTextStream in(&file);

	const int iterations = 10;

	while (!in.atEnd())
	{
		QString line = in.readLine();
		Rack rack(QuackleIO::Util::encode(line));
		double value = leaveSim(rack, iterations);
		UVcout << "leavecalc: " << rack << " " << value << endl;
	}

	file.close();

}

void TestHarness::randomRacks()
{
	for (;;)
	{
		Game game;

		Quackle::PlayerList players;

		Player compyA(m_computerPlayerToTest->name() + MARK_UV(" A"), Quackle::Player::ComputerPlayerType, 0);
		compyA.setAbbreviatedName(MARK_UV("A"));
		compyA.setComputerPlayer(m_computerPlayerToTest);
		players.push_back(compyA);

		game.setPlayers(players);

		game.addPosition();

		UVcout << game.currentPosition().currentPlayer().rack() << endl;
	}
}

void TestHarness::staticLeaves(const QString &filename)
{
	QuackleIO::GCGIO io;
	QFile file(filename);

	if (!file.open(QIODevice::ReadOnly | QIODevice::Text))
	{
		UVcout << "Could not open file " << QuackleIO::Util::qstringToString(filename) << endl;
		return;
	}

	QTextStream in(&file);

	Quackle::Game game;

	Quackle::PlayerList players;

	Quackle::Player compyA(m_computerPlayerToTest->name() + MARK_UV(" A"), Quackle::Player::ComputerPlayerType, 0);
	compyA.setAbbreviatedName(MARK_UV("A"));
	compyA.setComputerPlayer(m_computerPlayerToTest);
	players.push_back(compyA);

	Quackle::Player compyB(m_computerPlayer2ToTest->name() + MARK_UV(" B"), Quackle::Player::ComputerPlayerType, 1);
	compyB.setAbbreviatedName(MARK_UV("B"));
	compyB.setComputerPlayer(m_computerPlayer2ToTest);
	players.push_back(compyB);

	game.setPlayers(players);
	game.associateKnownComputerPlayers();

	game.addPosition();

	while (!in.atEnd())
	{
		QString line = in.readLine();
		Rack rack(QuackleIO::Util::encode(line));
		game.currentPosition().setCurrentPlayerRack(rack);
		Move move = game.currentPosition().staticBestMove();
		//Move scoredExch = game.currentPosition().scoreMove(exchZero);
		//UVcout << rack << " " << scoredExch << endl;
		UVcout << rack << " " << move << endl;
	}

	file.close();
}

void TestHarness::enumerateAll()
{
	Quackle::Bag B;
	Enumerator E(B);
	ProbableRackList racks;
	E.enumerate(&racks);
	for (ProbableRackList::iterator it = racks.begin(); it != racks.end(); ++it)
		UVcout << (*it).rack << " " << (*it).probability << endl;
}

struct PowerRack
{
    PowerRack(ProbableRack &r) : rack(r.rack),
				 stemProbability(r.probability),
				 usableTiles(0),
				 power(0) {}

    Rack   rack;
    double stemProbability; /* MSP */
    int    usableTiles; /* UT */
    double power; /* MMPR */
};

typedef vector<PowerRack> PowerRackList;

static bool powerSort(const PowerRack& r1,
		      const PowerRack& r2)
{
    return r1.power > r2.power;
}

// Looks for a single letter that acts to pluralize words based on the assumption
// that it will make a very common last letter in the lexicon.
static Letter findPluarlizer()
{
    // generate all the words in the lexicon
    Generator gen;
    int flags = Generator::AnagramRearrange | Generator::AddAnyLetters
	| Generator::ClearBlanknesses;
    WordList all = gen.anagramLetters("", flags);

    // count the frequency of occurrence of the final letter
    const int allLetters = QUACKLE_FIRST_LETTER + QUACKLE_MAXIMUM_ALPHABET_SIZE;
    int endCounts[allLetters];
    for (int j = 0; j < allLetters; j++) {
	endCounts[j] = 0;
    }
    for (WordList::iterator it = all.begin(); it != all.end(); ++it) {
	LetterString word = *it;
	Letter last = word[word.length() - 1];
 	endCounts[last]++;
    }

    // find the most common and 2nd most common final letters
    int maxEndCnt = 0;
    int maxEndLetter = QUACKLE_NULL_MARK;
    int max2EndCnt = 0;
    int max2EndLetter = QUACKLE_NULL_MARK;
    for (int j = 0; j < allLetters; j++) {
	if (endCounts[j] > maxEndCnt) {
	    max2EndCnt = maxEndCnt;
	    max2EndLetter = maxEndLetter;
	    maxEndCnt = endCounts[j];
	    maxEndLetter = j;
	} else if (endCounts[j] > max2EndCnt) {
	    max2EndCnt = endCounts[j];
	    max2EndLetter = j;
	}
    }

//     UVcout << QUACKLE_ALPHABET_PARAMETERS->userVisible(maxEndLetter)
// 	   << " " << endCounts[maxEndLetter] << endl;
//     UVcout << QUACKLE_ALPHABET_PARAMETERS->userVisible(max2EndLetter)
// 	   << " " << endCounts[max2EndLetter] << endl;

    // We found a pluralizer if it is much more common, otherwise this
    // language doesn't seem to have one.
    if (endCounts[maxEndLetter] > 2 * endCounts[max2EndLetter]) {
	UVcout << QUACKLE_ALPHABET_PARAMETERS->userVisible(maxEndLetter)
	       << " is the pluralizer" << endl;
	return maxEndLetter;
    }
    return QUACKLE_NULL_MARK;
}

void TestHarness::bingos()
{
    Letter pluralizer = findPluarlizer();

    // enumerate all racks not considering blanks
    Quackle::Bag B;
    int blankCnt = 0;
    while (B.removeLetter(QUACKLE_BLANK_MARK)) {
	++blankCnt;
    }
    Enumerator E(B);
    ProbableRackList enumRacks;
    E.enumerate(&enumRacks, 6);

    // convert probable racks to power racks for extra fields
    PowerRackList racks;
    for (ProbableRackList::iterator it = enumRacks.begin();
	 it != enumRacks.end(); ++it) {
	racks.push_back(PowerRack(*it));
    }

    // get counts of all non-blank letters
    Letter start = QUACKLE_ALPHABET_PARAMETERS->firstLetter();
    Letter end = QUACKLE_ALPHABET_PARAMETERS->lastLetter();
    char bagCounts[QUACKLE_FIRST_LETTER + QUACKLE_MAXIMUM_ALPHABET_SIZE];
    B.letterCounts(bagCounts);

    Generator gen;
    int cnt = 0;
    for (PowerRackList::iterator it = racks.begin(); it != racks.end(); ++it) {
	LetterString letters(it->rack.tiles());

	char rackCounts[QUACKLE_FIRST_LETTER + QUACKLE_MAXIMUM_ALPHABET_SIZE];
	String::counts(letters, rackCounts);

	if (++cnt % 1000 == 0) {
	    UVcout << "stem: " << QUACKLE_ALPHABET_PARAMETERS->userVisible(letters)
		   << " (" << cnt << " / " << racks.size() << ")" << endl;
	}

	// Count how many tiles combine with rack to make a bingo
	int usableTiles = 0;
	for (Letter c = start; c <= end; ++c) {
	    if (bagCounts[c] <= rackCounts[c]) {
		continue; // none left in bag to draw
	    }
	    int flags = Generator::AnagramRearrange | Generator::SingleMatch;
	    WordList anagrams = gen.anagramLetters(letters + c, flags);
	    if (anagrams.size() > 0) {
		usableTiles += bagCounts[c] - rackCounts[c]; // count all in the bag
	    }
	}

	// if we can make anything we can also do so with blanks
	if (usableTiles > 0) {
	    usableTiles += blankCnt;
	}
	it->usableTiles = usableTiles;
	if (rackCounts[pluralizer]) {
	    // Baron: "However, due to the frequent retention of the S in
	    // actual play, its attributed frequency was subsequently increased
	    // artificially 50%".  I generalized 'S' to a pluralizer for
	    // multi-language support.
	    it->stemProbability *= 1.5;
	}
	it->power = it->stemProbability * usableTiles;
    }

    sort(racks.begin(), racks.end(), powerSort);

    // Normalize max MSP to 1 (or 1.5 if it contains pluralizer) and MMPR
    double scale = 1 / racks.begin()->stemProbability;
    if (racks.begin()->rack.contains(LetterString(1, pluralizer))) {
	scale *= 1.5;
    }
    for (PowerRackList::iterator it = racks.begin(); it != racks.end(); ++it) {
	it->stemProbability *= scale;
	it->power *= scale;
    }

    // dump results
    for (PowerRackList::iterator it = racks.begin(); it != racks.end(); ++it) {
	UVcout << it->rack << " " << it->stemProbability << " "
	       << it->usableTiles << " " << it->power << endl;
    }
}

void TestHarness::testPosition(const Quackle::GamePosition &position, Quackle::ComputerPlayer *player)
{
	player->setPosition(position);

	ProbableRackList racks;
	Quackle::Bag unseenBag = position.unseenBag();

	if (unseenBag.size() <= QUACKLE_PARAMETERS->rackSize() + 3)
	{
		Enumerator enumerator(unseenBag);
		enumerator.enumerate(&racks);
		UVcout << racks.size() << " enumerations: " << endl;
		for (ProbableRackList::iterator it = racks.begin(); it != racks.end(); ++it)
			UVcout << (*it).rack << " " << (*it).probability << endl;
	}

	const int movesToShow = 10;
	UVcout << "Testing " << computerPlayerToTest()->name() << " on:" << endl;
	UVcout << position << endl;
	UVcout << "Generating moves..." << endl;

	MoveList moves = computerPlayerToTest()->moves(movesToShow);

	for (Quackle::MoveList::const_iterator it = moves.begin(); it != moves.end(); ++it)
	{
		UVcout << *it << endl;
	}
}

void TestHarness::anagram(const QString &letters, bool build)
{
	QuackleIO::DictImplementation dict;
	Dict::WordList list = dict.query(letters, build? Dict::Querier::NoRequireAllLetters : Dict::Querier::None);

	for (Dict::WordList::Iterator it = list.begin(); it != list.end(); ++it)
	{
		UVcout << QuackleIO::Util::qstringToString((*it).word);
		if ((*it).british)
			UVcout << "#";
		UVcout << endl;
	}
}

Quackle::Game *TestHarness::createNewGame(const QString &filename)
{
	QuackleIO::GCGIO io;
	QFile file(filename);

	if (!file.open(QIODevice::ReadOnly | QIODevice::Text))
	{
		UVcout << "Could not open gcg " << QuackleIO::Util::qstringToString(filename) << endl;
		return 0;
	}

	QTextStream in(&file);
	Quackle::Game *game = io.read(in, QuackleIO::Logania::MaintainBoardPreparation);
	file.close();

	return game;
}

void TestHarness::testPositions()
{
	UVcout << "Testing " << m_positions.size() << " positions with " << m_computerPlayerToTest->name() << "." << endl;
	for (QStringList::iterator it = m_positions.begin(); it != m_positions.end(); ++it)
		testFromFile(*it);
}

void TestHarness::testReport(bool html)
{
	UVcout << "Reporting on " << m_positions.size() << " positions with " << m_computerPlayerToTest->name() << "." << endl;
	for (QStringList::iterator it = m_positions.begin(); it != m_positions.end(); ++it)
	{
		Quackle::Game *game = createNewGame(*it);
		if (game)
		{
			if (html)
			{
				UVcout << game->currentPosition().board().htmlBoard(45) << endl;
			}
			else
			{
				UVString report;
				Quackle::Reporter::reportGame(*game, m_computerPlayerToTest, &report);
				UVcout << report << endl;
			}
		}

		delete game;
	}
}

void TestHarness::selfPlayGames(unsigned int seed, unsigned int reps, bool reports, bool playability)
{
	if (seed != numeric_limits<unsigned int>::max()) {
		UVcout << "using seed " << seed << endl;
		m_dataManager.seedRandomNumbers(seed);
	}

	for (unsigned int i = 0; i < reps; i++)
		selfPlayGame(i, reports, playability);
}

void TestHarness::selfPlayGame(unsigned int gameNumber, bool reports, bool playability)
{
	Quackle::Game game;

	Quackle::PlayerList players;

	Quackle::Player compyA(m_computerPlayerToTest->name() + MARK_UV(" A"), Quackle::Player::ComputerPlayerType, 0);
	compyA.setAbbreviatedName(MARK_UV("A"));
	compyA.setComputerPlayer(m_computerPlayerToTest);
	players.push_back(compyA);

	Quackle::Player compyB(m_computerPlayer2ToTest->name() + MARK_UV(" B"), Quackle::Player::ComputerPlayerType, 1);
	compyB.setAbbreviatedName(MARK_UV("B"));
	compyB.setComputerPlayer(m_computerPlayer2ToTest);
	players.push_back(compyB);

	game.setPlayers(players);
	game.associateKnownComputerPlayers();

	game.addPosition();

	UVcout << "NEW GAME (#" << gameNumber << ")" << endl;

	QTime time;
	time.start();

	const int playahead = 50;
	int i;
	for (i = 0; i < playahead; ++i)
	{
		if (game.currentPosition().gameOver())
		{
			if (!m_quiet) {
				UVcout << "GAME OVER ";
				GamePosition &pos = game.currentPosition();
				const PlayerList players = pos.endgameAdjustedScores();
				for (PlayerList::const_iterator it = players.begin();
					it != players.end(); ++it) {
					UVcout << it->name() << " : " << it->score() << " ";
				}
				UVcout << endl;
			}
			break;
		}

		const Quackle::Player player(game.currentPosition().currentPlayer());

   		if (!m_quiet) {
            if (playability) {
                game.currentPosition().kibitz(100);
                Quackle::MoveList moves = game.currentPosition().moves();
                float bestEquity = moves.front().equity - 0.0001f;
                Quackle::MoveList tops;
                for (MoveList::iterator it = moves.begin(); it != moves.end(); ++it) {
                    if ((*it).equity >= bestEquity) {
                        tops.push_back(*it);
                    }
                }
                int numTops = tops.size();
                for (MoveList::iterator it = tops.begin(); it != tops.end(); ++it) {
                    MoveList words = game.currentPosition().allWordsFormedBy(*it);
                    for (MoveList::iterator it2 = words.begin(); it2 != words.end(); ++it2) {
                        Rack word = (*it2).prettyTiles();
                        UVcout << word << " " << numTops << endl;
                    }
                }
                int toPlay = rand() % numTops;
                //UVcout << "playing move #" << toPlay << endl;
                game.commitMove(tops[toPlay]);
            } else {
                Quackle::Move compMove(game.haveComputerPlay());
                UVcout << "with " << player.rack() << ", " << player.name()
                       << " commits to " << compMove << endl;
            }
        }
	}

	int secondsElapsed = static_cast<int>(time.elapsed() / 1000);
	if (!m_quiet) {
		UVcout << "Game " << gameNumber << " played in " << secondsElapsed
	           << " seconds with " << i << " moves" << endl;
	}

	if (!reports) {
	    return;
	}

	Quackle::StaticPlayer playah;
	UVString report;
	Quackle::Reporter::reportGame(game, &playah, &report);
	if (!m_quiet) { UVcout << report << endl; }

	QString gamesDir = m_gamesDir;
	gamesDir.replace("PLAYERNAME", QuackleIO::Util::uvStringToQString(m_computerPlayerToTest->name()));
	gamesDir.replace(" ", "_");
	QDir::current().mkdir(gamesDir);

	QString joinedCompyName = QuackleIO::Util::uvStringToQString(m_computerPlayer2ToTest->name());
	joinedCompyName.replace(" ", "_");
	QFile outFile(QString("%1/%2-game-%3.gcg").arg(gamesDir).arg(joinedCompyName).arg(gameNumber));

	if (!outFile.open(QIODevice::WriteOnly | QIODevice::Text))
	{
		UVcout << "Could not open gcg output file" << endl;
		return;
	}

	QuackleIO::GCGIO io;
	QTextStream out(&outFile);
	io.write(game, out);

	QFile outFileReport(QString("%1/%2-game-%3.report").arg(gamesDir).arg(joinedCompyName).arg(gameNumber));

	if (!outFileReport.open(QIODevice::WriteOnly | QIODevice::Text))
	{
		UVcout << "Could not open report output file" << endl;
		return;
	}
	QTextStream outReport(&outFileReport);
	outReport << QuackleIO::Util::uvStringToQString(report);
	outReport << "Game played in " << secondsElapsed << " seconds." << endl;

	outFile.close();
	outFileReport.close();
}

static void dumpGaddag(const GaddagNode *node, const LetterString &prefix)
{
    for (const GaddagNode* child = node->firstChild(); child; child = child->nextSibling()) {
	Letter childLetter = child->letter();
	LetterString newPrefix(prefix);
	newPrefix += childLetter;

	if (child->isTerminal()) {
	    UVcout << "wordDump: " << QUACKLE_ALPHABET_PARAMETERS->userVisible(newPrefix) << endl;
	}
	if (child->firstChild()) {
	    dumpGaddag(child, newPrefix);
	}
    }
}

void TestHarness::wordDump()
{
    if (QUACKLE_LEXICON_PARAMETERS->hasGaddag()) {
	dumpGaddag(QUACKLE_LEXICON_PARAMETERS->gaddagRoot(),
		      LetterString());
    } else {
	UVcout << "wordDump: no gaddag" << endl;
    }
}
