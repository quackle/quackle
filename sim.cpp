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
#include <math.h>

#include "computerplayer.h"
#include "datamanager.h"
#include "game.h"
#include "gameparameters.h"
#include "move.h"
#include "sim.h"
#include "strategyparameters.h"

#if defined(__APPLE__)
#	include <pthread.h>
#elif defined(_WIN32)
#	include <windows.h>
#elif defined(__linux__)
#	include <sys/resource.h>
#endif

// define this to get lame debugging messages
// #define DEBUG_SIM

using namespace Quackle;
using namespace std;

std::atomic_long SimmedMove::objectIdCounter { 0 };

Simulator::Simulator()
	: m_logfileIsOpen(false)
	, m_hasHeader(false)
	, m_dispatch(0)
	, m_iterations(0)
	, m_ignoreOppos(false)
	, m_threadCount(0)
	, m_threadQoS(ThreadQoS::Balanced)
{
	m_originalGame.addPosition();
	setRecommendedThreadCount();
}

Simulator::~Simulator()
{
	shutdownThreads();
	closeLogfile();
}

void Simulator::setPosition(const GamePosition &position)
{
	if (hasSimulationResults())
		writeLogFooter();

	m_originalGame.setCurrentPosition(position);

	m_consideredMoves.clear();
	m_simmedMoves.clear();
	for (const auto &it : m_originalGame.currentPosition().moves())
		m_simmedMoves.push_back(SimmedMove(it));

	resetNumbers();
}

void Simulator::setLogfile(const string &logfile, bool append)
{
	if (m_logfile == logfile && isLogging())
		return;

	closeLogfile();
	m_logfile = logfile;

	if (m_logfile.empty())
	{
		closeLogfile();
		return;
	}

	const ios::openmode flags = append ? (ios::out | ios::app) : ios::out;
	m_logfileStream.open(m_logfile.c_str(), flags);

	m_logfileIsOpen = m_logfileStream.is_open();
	if (!m_logfileIsOpen)
		cerr << "Could not open " << m_logfile << " to write simulation log" << endl;

	m_hasHeader = false;
}

void Simulator::logMessage(const UVString &message)
{
	if (isLogging())
		m_logfileStream << message << endl;
}

void Simulator::closeLogfile()
{
	if (isLogging())
	{
		if (m_hasHeader)
			writeLogFooter();

		m_logfileStream.close();
		m_logfileIsOpen = false;
	}
}

void Simulator::writeLogHeader()
{
	if (isLogging())
	{
		m_logfileStream << "<simulation>" << endl;
		m_xmlIndent = MARK_UV("\t");

		m_hasHeader = true;

		// TODO include position data
	}
}

void Simulator::writeLogFooter()
{
	if (isLogging())
	{
		m_xmlIndent = MARK_UV("");
		m_logfileStream << "</simulation>" << endl;

		m_hasHeader = false;
	}
}

void Simulator::setDispatch(ComputerDispatch *dispatch)
{
	m_dispatch = dispatch;
}

void Simulator::setIncludedMoves(const MoveList &moves)
{
	for (auto &simmedMoveIt : m_simmedMoves)
		simmedMoveIt.setIncludeInSimulation(false);

	for (auto &it : moves)
	{
		bool found = false;
		for (auto &simmedMoveIt : m_simmedMoves)
		{
			if (it == simmedMoveIt.move)
			{
				simmedMoveIt.setIncludeInSimulation(true);
				found = true;
				break;
			}
		}

		if (!found)
			m_simmedMoves.push_back(SimmedMove(it));
	}
}

void Simulator::makeSureConsideredMovesAreIncluded()
{
	MoveList movesSuperset(moves(/* prune */ true, /* sort by win */ true));
	for (const auto &it : m_consideredMoves)
		if (!movesSuperset.contains(it))
			movesSuperset.push_back(it);
	setIncludedMoves(movesSuperset);
}

void Simulator::moveConsideredMovesToBeginning(MoveList &moves) const
{
	for (const auto &consideredIt : m_consideredMoves)
	{
		for (auto it = moves.begin(); it != moves.end(); it++)
		{
			if (consideredIt == *it)
			{
				it = moves.erase(it);
				moves.insert(moves.begin(), consideredIt);
			}
		}
	}
}

void Simulator::addConsideredMove(const Move &move)
{
	m_consideredMoves.push_back(move);
}

bool Simulator::isConsideredMove(const Move &move) const
{
	const bool ret = m_consideredMoves.contains(move);
	return ret;
}

void Simulator::pruneTo(double equityThreshold, int maxNumberOfMoves)
{
	MoveList equityMoves(moves(/* prune unincluded */ true));
	MoveList toSetIncluded;
	const double absoluteEquityThreshold = equityMoves[0].equity - equityThreshold;

	const MoveList::const_iterator end = equityMoves.end();
	int i = 0;
	for (MoveList::const_iterator it = equityMoves.begin(); i < maxNumberOfMoves && it != end; ++it, ++i)
	{
		if ((*it).equity >= absoluteEquityThreshold)
			toSetIncluded.push_back(*it);
	}

	setIncludedMoves(toSetIncluded);
}

void Simulator::resetNumbers()
{
	for (auto &moveIt : m_simmedMoves)
		moveIt.clear();

	m_iterations = 0;
}

namespace
{

void applyThreadQoS(ThreadQoS qos)
{
#if defined(__APPLE__)

	qos_class_t qosClass = QOS_CLASS_BACKGROUND;
	switch (qos)
	{
	// BACKGROUND alone confines the thread to the efficiency cores on Apple silicon.
	case ThreadQoS::EnergyEfficient:
		qosClass = QOS_CLASS_BACKGROUND;
		break;
	case ThreadQoS::Balanced:
		qosClass = QOS_CLASS_UTILITY;
		break;
	case ThreadQoS::Performance:
		qosClass = QOS_CLASS_USER_INITIATED;
		break;
	}

	pthread_set_qos_class_self_np(qosClass, 0);

#elif defined(_WIN32)

	int priority = THREAD_PRIORITY_NORMAL;
	switch (qos)
	{
	case ThreadQoS::EnergyEfficient:
		priority = THREAD_PRIORITY_LOWEST;
		break;
	case ThreadQoS::Balanced:
		priority = THREAD_PRIORITY_NORMAL;
		break;
	case ThreadQoS::Performance:
		priority = THREAD_PRIORITY_ABOVE_NORMAL;
		break;
	}

	SetThreadPriority(GetCurrentThread(), priority);

// EcoQoS; needs Windows 10 1809 or later, and an SDK that knows about it.
#	ifdef THREAD_POWER_THROTTLING_CURRENT_VERSION
	THREAD_POWER_THROTTLING_STATE throttling = {};
	throttling.Version = THREAD_POWER_THROTTLING_CURRENT_VERSION;
	throttling.ControlMask = qos == ThreadQoS::EnergyEfficient ? THREAD_POWER_THROTTLING_EXECUTION_SPEED : 0;
	throttling.StateMask = throttling.ControlMask;
	SetThreadInformation(GetCurrentThread(), ThreadPowerThrottling, &throttling, sizeof(throttling));
#	endif

#elif defined(__linux__)

	// Linux nice is per-thread; PRIO_PROCESS with who == 0 is the calling thread.
	// Unprivileged, we can only lower priority, so Performance asks for nothing.
	if (qos == ThreadQoS::EnergyEfficient)
		setpriority(PRIO_PROCESS, 0, 10);

#else

	(void)qos;

#endif
}

}

void Simulator::simThreadFunc(SimmedMoveMessageQueue &incoming, SimmedMoveMessageQueue &outgoing, ThreadQoS qos)
{
	applyThreadQoS(qos);

	while (true)
	{
		auto result = incoming.pop_or_terminate();
		if (result.second)
			break;
		try
		{
			Simulator::simulateOnePosition(result.first, incoming.constants());
		}
		catch (...)
		{
			// reply even on failure so simulate() receives the reply
			// count it expects; it rethrows this on the main thread
			result.first.error = std::current_exception();
		}
		outgoing.push(result.first);
	}
}

size_t Simulator::recommendedThreadCount(ThreadQoS qos)
{
	// Constructing a Simulator before the data manager is a misuse, but it
	// should not be a crash; one thread still sims, just serially.
	if (!QUACKLE_DATAMANAGER_EXISTS)
		return 1;

	const CPUTopology &topology = QUACKLE_CPU_TOPOLOGY;

	switch (qos)
	{
	case ThreadQoS::EnergyEfficient:
		return (size_t)topology.efficiencyThreadPoolSize();
	case ThreadQoS::Balanced:
		return (size_t)topology.balancedThreadPoolSize();
	case ThreadQoS::Performance:
		break;
	}

	return (size_t)topology.performanceThreadPoolSize();
}

void Simulator::setRecommendedThreadCount()
{
	setThreadCount(recommendedThreadCount(m_threadQoS), m_threadQoS);
}

void Simulator::setThreadCount(size_t count, ThreadQoS qos)
{
	// A thread's QoS is applied at startup and never revisited, so a change
	// only takes hold on threads started after it.
	if (qos != m_threadQoS)
	{
		m_threadQoS = qos;
		shutdownThreads();
	}

	m_threadCount = count;

	if (count == 0)
		shutdownThreads();
	else if (!m_threadPool.empty())
		reconcileThreads();
}

// Every ComputerPlayer owns a Simulator, and Resolvent and Preendgame build
// one per move, nested; only the innermost ever sims. Starting the pool on
// demand keeps the rest from costing anything.
void Simulator::ensureThreads()
{
	if (m_threadPool.size() != m_threadCount)
		reconcileThreads();
}

void Simulator::reconcileThreads()
{
	while (m_threadCount > m_threadPool.size())
	{
		m_threadPool.emplace_back(Simulator::simThreadFunc, std::ref(m_sendQueue), std::ref(m_receiveQueue), m_threadQoS);
	}

	while (m_threadCount < m_threadPool.size())
	{
		m_sendQueue.send_terminate_one(m_threadPool.back().get_id());
		m_threadPool.back().join();
		m_threadPool.pop_back();
	}
}

void Simulator::shutdownThreads()
{
	if (m_threadPool.empty())
		return;

	m_sendQueue.send_terminate_all();
	for (auto &t : m_threadPool)
		t.join();
	m_threadPool.clear();
	// clear the flag so threads created by a later call don't
	// immediately terminate themselves
	m_sendQueue.reset_terminate_all();
}

void Simulator::simulate(int plies)
{
	simulate(plies, 1);
}

void Simulator::simulate(int plies, int iterations)
{
#ifdef DEBUG_SIM
	UVcout << "let's simulate for " << plies << " plies" << endl;
#endif

	if (plies < 0)
		plies = 1000;

	// specified plies doesn't include candidate play
	++plies;

	// A pure function of the position, plies and flags, none of which can
	// change while we block, so the workers can read it for the whole batch.
	SimmedMoveConstants constants;
	constants.game = m_originalGame;
	constants.partialOppoRack = m_partialOppoRack;
	constants.startPlayerId = m_originalGame.currentPosition().currentPlayer().id();
	constants.playerCount = int(m_originalGame.currentPosition().players().size());
	// level one's first move is the zeroth ply (the candidate)
	constants.decimalTurns = (plies % constants.playerCount);
	// also one-indexed
	constants.levelCount = (int)((plies - constants.decimalTurns) / constants.playerCount);
	constants.ignoreOppos = m_ignoreOppos;
	constants.isLogging = isLogging();

	m_sendQueue.setConstants(constants);

	if (isLogging() && !m_hasHeader)
		writeLogHeader();

	ensureThreads();

	const UVString indent = playaheadIndent();

	int remaining = iterations;
	while (remaining > 0)
	{
		// Only between chunks: once a chunk is pushed it must be drained,
		// or the next batch pops its replies and miscounts them as its own.
		if (m_dispatch && m_dispatch->shouldAbort())
			break;

		const int chunk = iterationsPerChunk(threadCount(), includedMoveCount(), remaining);
		const int firstIteration = m_iterations + 1;

		int perIteration = 0;
		for (int i = 0; i < chunk; ++i)
			perIteration = pushIteration(constants, indent, firstIteration + i);

		int incorporated = 0;
		const std::exception_ptr firstError = collectChunk(firstIteration, chunk, perIteration, &incorporated);

		m_iterations += incorporated;
		remaining -= chunk;

		// Rethrow a worker's exception only now, with the queues fully
		// drained and the log consistent, so the simulator remains usable.
		if (firstError)
			std::rethrow_exception(firstError);
	}
}

int Simulator::includedMoveCount() const
{
	int count = 0;
	for (const auto &it : m_simmedMoves)
		if (it.includeInSimulation())
			++count;
	return count;
}

int Simulator::iterationsPerChunk(size_t threadCount, int includedMoves, int remaining)
{
	if (includedMoves <= 0 || remaining <= 0)
		return 1;

	// Keep roughly this many playaheads queued per thread, so no thread runs
	// dry waiting on the collect and slow playaheads overlap with fast ones.
	// It also bounds work in flight at about threadCount * slack messages
	// however many iterations were asked for.
	const int slack = 3;
	int wanted = ((int)threadCount * slack + includedMoves - 1) / includedMoves;

	if (wanted < 1)
		wanted = 1;
	if (wanted > remaining)
		wanted = remaining;

	return wanted;
}

// <playahead> sits inside <iteration> inside the log root, and a worker's
// plies inside that. The depth is the same for every iteration, so a pushed
// message can carry it without waiting for its <iteration> to be opened.
UVString Simulator::playaheadIndent() const
{
	return m_xmlIndent + MARK_UV("\t\t");
}

int Simulator::pushIteration(const SimmedMoveConstants &constants, const UVString &indent, int iteration)
{
	int messageCount = 0;

	for (size_t moveIndex = 0; moveIndex < m_simmedMoves.size(); ++moveIndex)
	{
		SimmedMove &moveIt = m_simmedMoves[moveIndex];
		if (!moveIt.includeInSimulation())
			continue;

#ifdef DEBUG_SIM
		UVcout << "simulating " << moveIt.move << ":" << endl;
#endif

		moveIt.levels.setNumberLevels(constants.levelCount + 1);

		SimmedMoveMessage message;
		message.id = moveIt.id();
		message.moveIndex = (int)moveIndex;
		message.iteration = iteration;
		message.move = moveIt.move;
		// a message carries only its own playahead's samples; incorporateMessage() merges them
		message.levels.setNumberLevels(constants.levelCount + 1);
		message.xmlIndent = indent;

		m_sendQueue.push(message);
		messageCount++;
	}

	return messageCount;
}

std::exception_ptr Simulator::collectChunk(int firstIteration, int iterationCount, int messagesPerIteration, int *incorporated)
{
	*incorporated = 0;

	// Collect the whole chunk before incorporating any of it, so that results
	// and the log don't depend on the order in which threads finish.
	const size_t moveCount = m_simmedMoves.size();
	vector<SimmedMoveMessage> messages;
	vector<const SimmedMoveMessage *> byIterationAndMove(iterationCount * moveCount, nullptr);

	int outstanding = iterationCount * messagesPerIteration;
	messages.reserve(outstanding);
	while (outstanding-- > 0)
		messages.push_back(m_receiveQueue.pop());

	for (const auto &message : messages)
		byIterationAndMove[(message.iteration - firstIteration) * moveCount + message.moveIndex] = &message;

	std::exception_ptr firstError;
	for (int i = 0; i < iterationCount; ++i)
	{
		const SimmedMoveMessage *const *slot = &byIterationAndMove[i * moveCount];

		// An iteration that lost a playahead is dropped whole, along with
		// the rest of the chunk. Incorporating its survivors would leave the
		// candidates at unequal sample counts, and calculateEquity() would
		// then be comparing averages over unlike populations.
		for (size_t moveIndex = 0; moveIndex < moveCount; ++moveIndex)
			if (slot[moveIndex] && slot[moveIndex]->error)
				firstError = slot[moveIndex]->error;

		if (firstError)
			break;

		if (isLogging())
		{
			m_logfileStream << m_xmlIndent << "<iteration index=\"" << firstIteration + i << "\">" << endl;
			m_xmlIndent += MARK_UV('\t');
		}

		for (size_t moveIndex = 0; moveIndex < moveCount; ++moveIndex)
			if (slot[moveIndex])
				incorporateMessage(*slot[moveIndex]);

		if (isLogging())
		{
			m_xmlIndent = m_xmlIndent.substr(0, m_xmlIndent.length() - 1);
			m_logfileStream << m_xmlIndent << "</iteration>" << endl;
		}

		++*incorporated;
	}

	return firstError;
}

void Simulator::simulateOnePosition(SimmedMoveMessage &message, const SimmedMoveConstants &constants)
{
	Game game = constants.game;

	// Randomize here rather than once per iteration so that each
	// playahead sees an independent draw of oppo racks and tiles.
	randomizeOppoRacks(game.currentPosition(), constants.partialOppoRack);
	randomizeDrawingOrder(game.currentPosition());

	double residual = 0;

	int levelNumber = 1;
	for (LevelList::iterator levelIt = message.levels.begin();
		levelNumber <= constants.levelCount + 1 && levelIt != message.levels.end() && !game.currentPosition().gameOver();
		++levelIt, ++levelNumber)
	{
		const int decimal = levelNumber == constants.levelCount + 1 ? constants.decimalTurns : constants.playerCount;
		if (decimal == 0)
			continue;

		(*levelIt).setNumberScores(decimal);

		int playerNumber = 0;
		for (auto &scoresIt : (*levelIt).statistics)
		{
			if (game.currentPosition().gameOver())
				break;
			++playerNumber;
			const int playerId = game.currentPosition().currentPlayer().id();

			if (constants.isLogging)
			{
				message.logStream << message.xmlIndent << "<ply index=\"" << (levelNumber - 1) * constants.playerCount + playerNumber - 1
								  << "\">" << endl;
				message.xmlIndent += MARK_UV('\t');
			}

			Move move = Move::createNonmove();

			if (playerId == constants.startPlayerId && levelNumber == 1)
				move = message.move;
			else if (constants.ignoreOppos && playerId != constants.startPlayerId)
				move = Move::createPassMove();
			else
				move = game.currentPosition().staticBestMove();

			int deadwoodScore = 0;
			if (game.currentPosition().doesMoveEndGame(move))
			{
				LetterString deadwood;
				deadwoodScore = game.currentPosition().deadwood(&deadwood);
				// account for deadwood in this move rather than a separate
				// UnusedTilesBonus move.
				move.score += deadwoodScore;
			}

			scoresIt.score.incorporateValue(move.score);
			scoresIt.bingos.incorporateValue(move.isBingo ? 1.0 : 0.0);

			if (constants.isLogging)
			{
				message.logStream << message.xmlIndent << game.currentPosition().currentPlayer().rack().xml() << endl;
				message.logStream << message.xmlIndent << move.xml() << endl;
			}

			// record future-looking residuals
			bool isFinalTurnForPlayerOfSimulation = false;

			if (levelNumber == constants.levelCount)
				isFinalTurnForPlayerOfSimulation = playerNumber > constants.decimalTurns;
			else if (levelNumber == constants.levelCount + 1)
				isFinalTurnForPlayerOfSimulation = playerNumber <= constants.decimalTurns;

			const bool isVeryFinalTurnOfSimulation
				= (constants.decimalTurns == 0 && levelNumber == constants.levelCount && playerNumber == constants.playerCount)
				|| (levelNumber == constants.levelCount + 1 && playerNumber == constants.decimalTurns);

			if (isFinalTurnForPlayerOfSimulation && !(constants.ignoreOppos && playerId != constants.startPlayerId))
			{
				double residualAddend = game.currentPosition().calculatePlayerConsideration(move);
				if (constants.isLogging)
					message.logStream << message.xmlIndent << "<pc value=\"" << residualAddend << "\" />" << endl;

				if (isVeryFinalTurnOfSimulation)
				{
					// experimental -- do shared resource considerations
					// matter in a plied simulation?

					const double sharedResidual = game.currentPosition().calculateSharedConsideration(move);
					residualAddend += sharedResidual;

					if (constants.isLogging && sharedResidual != 0)
						message.logStream << message.xmlIndent << "<sc value=\"" << sharedResidual << "\" />" << endl;
				}

				if (playerId == constants.startPlayerId)
					residual += residualAddend;
				else
					residual -= residualAddend;
			}

			// committing the move will account for deadwood again
			// so avoid double counting from above.
			move.score -= deadwoodScore;
			game.setCandidate(move);

			game.commitCandidate(!isVeryFinalTurnOfSimulation);

			if (constants.isLogging)
			{
				message.xmlIndent = message.xmlIndent.substr(0, message.xmlIndent.length() - 1);
				message.logStream << message.xmlIndent << "</ply>" << endl;
			}
		}
	}

	message.residual = residual;
	int spread = game.currentPosition().spread(constants.startPlayerId);
	message.gameSpread = spread;

	if (game.currentPosition().gameOver())
	{
		message.bogowin = false;
		message.wins = spread > 0 ? 1 : spread == 0 ? 0.5 : 0;
	}
	else
	{
		message.bogowin = true;
		if (game.currentPosition().currentPlayer().id() == constants.startPlayerId)
			message.wins = QUACKLE_STRATEGY_PARAMETERS->bogowin(
				(int)(spread + residual), game.currentPosition().bag().size() + QUACKLE_PARAMETERS->rackSize(), 0);
		else
			message.wins = 1.0
				- QUACKLE_STRATEGY_PARAMETERS->bogowin(
					(int)(-spread - residual), game.currentPosition().bag().size() + QUACKLE_PARAMETERS->rackSize(), 0);
	}
}

void Simulator::incorporateMessage(const SimmedMoveMessage &message)
{
	// moveIndex is authoritative, but this is public API: a caller outside
	// simulate() can hand us a message it built itself, so fall back on id.
	SimmedMove *move = nullptr;
	if (message.moveIndex >= 0 && (size_t)message.moveIndex < m_simmedMoves.size() && m_simmedMoves[message.moveIndex].id() == message.id)
	{
		move = &m_simmedMoves[message.moveIndex];
	}
	else
	{
		for (auto &moveIt : m_simmedMoves)
		{
			if (moveIt.id() == message.id)
			{
				move = &moveIt;
				break;
			}
		}
	}

	if (!move)
		return;

	if (isLogging())
	{
		m_logfileStream << m_xmlIndent << "<playahead>" << endl;
		m_xmlIndent += MARK_UV('\t');
		m_logfileStream << message.logStream.str();
	}

	move->levels.incorporate(message.levels);
	move->residual.incorporateValue(message.residual);
	move->gameSpread.incorporateValue(message.gameSpread);
	move->wins.incorporateValue(message.wins);

	if (isLogging())
	{
		if (!message.bogowin)
			m_logfileStream << m_xmlIndent << "<gameover win=\"" << message.wins << "\" />" << endl;
		m_xmlIndent = m_xmlIndent.substr(0, m_xmlIndent.length() - 1);
		m_logfileStream << m_xmlIndent << "</playahead>" << endl;
	}
}

void Simulator::randomizeOppoRacks()
{
	randomizeOppoRacks(m_originalGame.currentPosition(), m_partialOppoRack);
}

void Simulator::randomizeOppoRacks(GamePosition &position, const Rack &partialOppoRack)
{
#ifdef DEBUG_SIM
	UVcout << "RANDOMIZE OPPO RACKS " << endl;
#endif

	position.ensureProperBag();

	Bag bag(position.unseenBag());

	for (const auto &it : position.players())
	{
		if (it == position.currentPlayer())
			continue;

		// TODO -- some kind of inference engine can be inserted here
		Rack rack = partialOppoRack;

		// We must refill the partial rack from a bag that does not
		// contain the partial rack.
		bag.removeLetters(rack.tiles());
		bag.refill(rack);

		position.setPlayerRack(it.id(), rack, /* adjust bag */ true);
	}

#ifdef DEBUG_SIM
	UVcout << "RANDOMIZE OPPO RACKS DONE" << endl;
#endif

	position.ensureProperBag();
}

void Simulator::setPartialOppoRack(const Rack &rack)
{
	m_partialOppoRack = rack;
}

void Simulator::randomizeDrawingOrder()
{
	randomizeDrawingOrder(m_originalGame.currentPosition());
}

void Simulator::randomizeDrawingOrder(GamePosition &position)
{
	position.setDrawingOrder(position.bag().someShuffledTiles());
}

MoveList Simulator::moves(bool prune, bool byWin) const
{
	MoveList ret;

	const bool useCalculatedEquity = hasSimulationResults();

	for (const auto &it : m_simmedMoves)
	{
		if (prune && !it.includeInSimulation())
			continue;

		Move move(it.move);

		if (useCalculatedEquity)
		{
			move.equity = it.calculateEquity();
			move.win = it.wins.averagedValue();
		}

		ret.push_back(move);
	}

	if (byWin && useCalculatedEquity)
		MoveList::sort(ret, MoveList::Win);
	else
		MoveList::sort(ret, MoveList::Equity);

	return ret;
}

const SimmedMove &Simulator::simmedMoveForMove(const Move &move) const
{
	for (const auto &it : m_simmedMoves)
		if (it.move == move)
			return it;

	return m_simmedMoves.back();
}

int Simulator::numLevels() const
{
	if (m_simmedMoves.empty())
		return 0;
	return int(m_simmedMoves.front().levels.size());
}

int Simulator::numPlayersAtLevel(int levelIndex) const
{
	if (m_simmedMoves.empty())
		return 0;
	return int(m_simmedMoves.front().levels[levelIndex].statistics.size());
}

////////////

double AveragedValue::standardDeviation() const
{
	return m_incorporatedValues <= 1
		? 0
		: sqrt((m_incorporatedValues * m_squaredValueSum - m_valueSum * m_valueSum) / (m_incorporatedValues * (m_incorporatedValues - 1)));
}

void AveragedValue::clear()
{
	m_valueSum = 0;
	m_squaredValueSum = 0;
	m_incorporatedValues = 0;
}

////////////

void SimmedMoveMessageQueue::push(SimmedMoveMessage &msg)
{
	std::lock_guard<std::mutex> lk(m_queueMutex);
	m_queue.push(std::move(msg));
	m_condition.notify_one();
}

std::pair<SimmedMoveMessage, bool> SimmedMoveMessageQueue::pop_or_terminate()
{
	std::unique_lock<std::mutex> lk(m_queueMutex);
	while (m_queue.empty() && !m_terminateAll && m_terminateOne != std::this_thread::get_id())
		m_condition.wait(lk);
	std::pair<SimmedMoveMessage, bool> result;
	result.second = m_terminateAll || m_terminateOne == std::this_thread::get_id();
	if (result.second)
		m_terminateOne = std::thread::id();
	else
	{
		result.first = std::move(m_queue.front());
		m_queue.pop();
	}
	return result;
}

SimmedMoveMessage SimmedMoveMessageQueue::pop()
{
	std::unique_lock<std::mutex> lk(m_queueMutex);
	while (m_queue.empty())
		m_condition.wait(lk);
	SimmedMoveMessage result = std::move(m_queue.front());
	m_queue.pop();
	return result;
}

void SimmedMoveMessageQueue::send_terminate_all()
{
	std::lock_guard<std::mutex> lk(m_queueMutex);
	m_terminateAll = true;
	m_condition.notify_all();
}

void SimmedMoveMessageQueue::reset_terminate_all()
{
	std::lock_guard<std::mutex> lk(m_queueMutex);
	m_terminateAll = false;
}

void SimmedMoveMessageQueue::send_terminate_one(const std::thread::id &id)
{
	std::lock_guard<std::mutex> lk(m_queueMutex);
	m_terminateOne = id;
	m_condition.notify_all();
}

////////////

double SimmedMove::calculateEquity() const
{
	if (levels.empty())
	{
		return move.equity;
	}

	double equity = 0;

	for (const auto &levelIt : levels)
	{
		for (PositionStatisticsList::const_iterator scoresIt = levelIt.statistics.begin(); scoresIt != levelIt.statistics.end(); scoresIt++)
		{
			if (scoresIt == levelIt.statistics.begin())
				equity += (*scoresIt).score.averagedValue();
			else
				equity -= (*scoresIt).score.averagedValue();
		}
	}

	equity += residual.averagedValue();

	return equity;
}

double SimmedMove::calculateWinPercentage() const
{
	return wins.hasValues() ? wins.averagedValue() * 100 : move.win;
}

void LevelList::setNumberLevels(unsigned int number)
{
	while (size() < number)
		push_back(Level());
}

void LevelList::incorporate(const LevelList &other)
{
	setNumberLevels((unsigned int)other.size());

	for (size_t i = 0; i < other.size(); ++i)
		(*this)[i].incorporate(other[i]);
}

void SimmedMove::clear()
{
	levels.clear();
	residual.clear();
	gameSpread.clear();
	wins.clear();
}

PositionStatistics SimmedMove::getPositionStatistics(int level, int playerIndex) const
{
	return levels[level].statistics[playerIndex];
}

void PositionStatistics::incorporate(const PositionStatistics &other)
{
	score.incorporate(other.score);
	bingos.incorporate(other.bingos);
}

AveragedValue PositionStatistics::getStatistic(StatisticType type) const
{
	switch (type)
	{
	case StatisticScore:
		return score;
	case StatisticBingos:
		return bingos;
	}

	return AveragedValue();
}

////////////

void Level::setNumberScores(unsigned int number)
{
	while (statistics.size() < number)
		statistics.push_back(PositionStatistics());
}

void Level::incorporate(const Level &other)
{
	setNumberScores((unsigned int)other.statistics.size());

	for (size_t i = 0; i < other.statistics.size(); ++i)
		statistics[i].incorporate(other.statistics[i]);
}

//////////

UVOStream &operator<<(UVOStream &o, const Quackle::AveragedValue &value)
{
	o << "[" << value.valueSum() << "/" << value.incorporatedValues() << "=" << value.averagedValue() << " sd " << value.standardDeviation()
	  << "]";
	return o;
}

UVOStream &operator<<(UVOStream &o, const Quackle::PositionStatistics &value)
{
	o << "Stats: score " << value.score << ", bingos " << value.bingos << endl;
	return o;
}

UVOStream &operator<<(UVOStream &o, const Quackle::Level &level)
{
	for (const auto &it : level.statistics)
		o << it;
	return o;
}

UVOStream &operator<<(UVOStream &o, const Quackle::SimmedMove &move)
{
	o << "Simmed move " << move.move << ":";

	int levelNumber = 0;
	for (const auto &it : move.levels)
		o << endl << "level " << ++levelNumber << ": " << it;

	o << endl;
	o << "Being simmed: " << move.includeInSimulation() << endl;
	o << "Residual: " << move.residual << endl;
	o << "Spread: " << move.gameSpread << endl;
	o << "Wins: " << move.wins << endl;
	return o;
}

UVOStream &operator<<(UVOStream &o, const Quackle::SimmedMoveList &moves)
{
	for (const auto &it : moves)
		o << it << endl;
	return o;
}
