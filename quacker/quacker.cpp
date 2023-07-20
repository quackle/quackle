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
#include <iostream>

#include <QtWidgets>

#include <game.h>
#include <boardparameters.h>
#include <computerplayer.h>
#include <gameparameters.h>

#include <quackleio/froggetopt.h>
#include <quackleio/util.h>
#include <quackleio/logania.h>
#include <quackleio/queenie.h>
#include <quackleio/streamingreporter.h>

#include "brb.h"
#include "configdialog.h"
#include "customqsettings.h"
#include "dashboard.h"
#include "geometry.h"
#include "graphicalreporter.h"
#include "history.h"
#include "lister.h"
#include "letterbox.h"
#include "lexiconparameters.h"
#include "movebox.h"
#include "noteeditor.h"
#include "newgame.h"
#include "oppothreadprogressbar.h"
#include "quacker.h"
#include "quackersettings.h"
#include "settings.h"
#include "simviewer.h"
#include "widgetfactory.h"
#include "view.h"

using namespace std;

const int kExtraPlaysToKibitz = 15;

TopLevel::TopLevel(QWidget *parent)
	: QMainWindow(parent), m_listerDialog(0), m_letterbox(0), m_simViewer(0), m_plies(2), m_logania(0), m_modified(false)
{
	QCoreApplication::setOrganizationName("Quackle.org");
	QCoreApplication::setOrganizationDomain("quackle.org");
	QCoreApplication::setApplicationName("Quackle");

	qRegisterMetaType<OppoThread*>("OppoThread*");

	m_quackerSettings = new QuackerSettings;

	m_settings = new Settings;
	m_settings->preInitialize();
	m_settings->createGUI();
	connect(m_settings, SIGNAL(refreshViews()), this, SLOT(updateAllViews()));
	
	m_game = new Quackle::Game;
	m_simulator = new Quackle::Simulator;

	createMenu();
	createWidgets();

	loadSettings();

	setCaption(tr("Welcome"));
        statusMessage(tr("Please wait for Quackle to load its data structures..."));
        
	QTimer::singleShot(0, this, SLOT(finishInitialization()));
}

TopLevel::~TopLevel()
{
	stopEverything();
	for (const auto& it : m_otherOppoThreads)
		it->wait();
	for (const auto& it : m_oppoThreads)
		it->wait();
	kibitzThreadFinished();
	computerPlayerDone();
	QuackleIO::Queenie::cleanUp();
	delete m_game;
	delete m_simulator;
	delete m_quackerSettings;
}

void TopLevel::closeEvent(QCloseEvent *closeEvent)
{
	pause(true);

	if (m_letterbox && m_letterbox->isVisible())
	{
		if (!m_letterbox->tryToClose())
		{
			closeEvent->ignore();
			return;
		}

		delete m_letterbox;
	}

	if (m_modified)
	{
		switch (askToSave())
		{
		case QMessageBox::Save:
			qApp->processEvents();
			save();

			// fall through

		case QMessageBox::Discard:
			closeEvent->accept();
			break;

		case QMessageBox::Cancel:
		default:
			closeEvent->ignore();
		}
	}
	else
		closeEvent->accept();

	saveSettings();
}

void TopLevel::finishInitialization()
{
	statusMessage(tr("Initializing..."));
	m_settings->initialize();
	m_settings->load();

	m_timer = new QTimer(this);
	connect(m_timer, SIGNAL(timeout()), this, SLOT(timeout()));
	m_simulationTimer = new QTimer(this);
	connect(m_simulationTimer, SIGNAL(timeout()), this, SLOT(incrementSimulation()));

	// Birthday
	m_birthdayTimer = new QTimer(this);
	connect(m_birthdayTimer, SIGNAL(timeout()), this, SLOT(birthdayBash()));

	QTimer::singleShot(0, this, SLOT(introduceToUser()));
}

void TopLevel::introduceToUser()
{
	CustomQSettings settings;
	QString statusText = QString::fromUtf8(QUACKLE_LEXICON_PARAMETERS->copyrightString().c_str());
	if (statusText.isEmpty())
		statusText = tr("Enjoy your quackling. Choose \"New game...\" from the Game menu to begin.");
	statusMessage(statusText);
	parseCommandLineOptions();

	if (!CustomQSettings().contains("quackle/hasBeenRun"))
		firstTimeRun();
}

void TopLevel::parseCommandLineOptions()
{
	GetOpt opts;
	QString filename;
	opts.addOptionalArgument("filename", &filename);
	if (!opts.parse())
		return;

	if (!filename.isNull())
		openFile(filename);
}

bool TopLevel::isPlayerOnTurnComputer() const
{
	return m_game->currentPosition().playerOnTurn().type() == Quackle::Player::ComputerPlayerType;
}

bool TopLevel::isCommitAllowed() const
{
	return !m_game->currentPosition().gameOver() && !(shouldOutcraftyCurrentPlayer() && isPlayerOnTurnComputer());
}

void TopLevel::commit()
{
	if (!m_game->hasPositions())
		return;
	
	if (!isCommitAllowed())
	{
		statusMessage(tr("%1 is currently on turn so you cannot commit. Please wait.").arg(QuackleIO::Util::uvStringToQString(m_game->currentPosition().playerOnTurn().name())));
		return;
	}

	if (m_game->candidate().isAMove())
	{
		stopEverything();

		m_game->commitCandidate();
		setModified(true);

		advanceGame();
	}
	else
	{
		statusMessage(tr("No move specified."));
	}
}

void TopLevel::pass()
{
	Quackle::Move pass(Quackle::Move::createPassMove());
	setCandidateMove(&pass);
}

void TopLevel::overdraw()
{
	bool ok = false;
	QString letters = QInputDialog::getText(this, tr("Handle overdraw - Quackle"), tr("Please input the letters that sit on the table faceup."), QLineEdit::Normal, QString(), &ok);

	if (!ok)
		return;

	if (letters.isEmpty())
		return;

	Quackle::LetterString throwbackLetterString;
	int validityFlags = m_game->currentPosition().handleOverdraw(QuackleIO::Util::nonBlankEncode(letters), &throwbackLetterString);

	if (validityFlags & Quackle::GamePosition::InvalidOverdrawNumber)
	{
		bool tryAgain = QMessageBox::question(this, tr("Verify Overdraw - Quackle"), dialogText(tr("Overdraw %1 does not contain at least %2 tiles, the minimum number of tiles you should turn over. Try again?").arg(letters).arg(QUACKLE_PARAMETERS->overdrawPenalty() + 1)), QMessageBox::Yes, QMessageBox::No) == QMessageBox::Yes;
		if (tryAgain)
			overdraw();  // try again
		return;
	}

	if (validityFlags & Quackle::GamePosition::OverdrawnTilesNotUnseen)
	{
		bool tryAgain = QMessageBox::question(this, tr("Verify Overdraw - Quackle"), dialogText(tr("Tiles in overdraw %1 are not in the unseen pool. Try again?").arg(letters)), QMessageBox::Yes, QMessageBox::No) == QMessageBox::Yes;
		if (tryAgain)
			overdraw();
		return;
	}

	QMessageBox::information(this, tr("Overdraw answer - Quackle"), dialogText(tr("Please put %1 back in the bag.")).arg(QuackleIO::Util::letterStringToQString(throwbackLetterString)));
}

void TopLevel::statusMessage(const QString &message)
{
	statusBar()->showMessage(message);
}

bool TopLevel::askToCarryOn(const QString &text)
{
	return QMessageBox::question(this, tr("Verify Play - Quackle"), dialogText(text), QMessageBox::Yes, QMessageBox::No) == QMessageBox::Yes;
}

void TopLevel::setCandidateMove(const Quackle::Move *move, bool *carryOnPtr)
{
	if (carryOnPtr != nullptr)
		*carryOnPtr = true;
	if (!m_game->hasPositions() || (move->action == Quackle::Move::Place && move->tiles().empty()))
		return;

	Quackle::Move prettiedMove(*move);
	m_game->currentPosition().ensureMoveTilesDoNotIncludePlayThru(prettiedMove);
	m_game->currentPosition().ensureMovePrettiness(prettiedMove);

	if (m_game->currentPosition().moves().contains(prettiedMove))
	{
		m_game->currentPosition().scoreMove(prettiedMove);
		m_game->setCandidate(prettiedMove);
	}
	else
	{
		bool playHasIllegalWords = false;
		int validityFlags = m_game->currentPosition().validateMove(prettiedMove);
		bool carryOn = true;

		while (carryOn && validityFlags != Quackle::GamePosition::ValidMove)
		{
			if (validityFlags & Quackle::GamePosition::TooLateExchange)
			{
				carryOn = askToCarryOn(tr("Bag must contain at least %1 tiles for an exchange.").arg(Quackle::DataManager::self()->parameters()->minimumTilesForExchange()));
				validityFlags ^= Quackle::GamePosition::TooLateExchange;
				continue;
			}

			if (validityFlags & Quackle::GamePosition::InvalidTiles)
			{
				if (!m_game->currentPosition().currentPlayer().racksAreKnown())
				{
					bool validifiable = validifyMove(prettiedMove);
					if (!validifiable)
					{
						carryOn = askToCarryOn(tr("%1 would need an impossible rack to make play %2; make play anyway?").arg(QuackleIO::Util::uvStringToQString(m_game->currentPosition().currentPlayer().name())).arg(QuackleIO::Util::moveToDetailedString(prettiedMove)));
					}
					else
					{
						carryOn = true;
					}
				}
				else
				{
					QMessageBox mb(QMessageBox::Question, tr("Verify Play"),
									tr("%1's rack does not include all tiles in %2; make play anyway?").arg(QuackleIO::Util::uvStringToQString(m_game->currentPosition().currentPlayer().name())).arg(QuackleIO::Util::moveToDetailedString(prettiedMove)));
					QPushButton* mb_yes = mb.addButton(QMessageBox::Yes);
					mb.addButton(QMessageBox::No);
					QPushButton* mb_unknownRacks = mb.addButton(tr("Assume unknown racks for this game"), QMessageBox::ApplyRole);
					mb.exec();
					carryOn = (mb.clickedButton() == mb_yes || mb.clickedButton() == mb_unknownRacks);
					if (mb.clickedButton() == mb_unknownRacks)
						m_game->currentPosition().currentPlayer().setRacksAreKnown(false);
				}

				validityFlags ^= Quackle::GamePosition::InvalidTiles;
				continue;
			}

			if (validityFlags & Quackle::GamePosition::InvalidPlace)
			{
				carryOn = askToCarryOn(tr("%1 does not connect to other plays on board; make play anyway?").arg(QuackleIO::Util::moveToDetailedString(prettiedMove)));
				validityFlags ^= Quackle::GamePosition::InvalidPlace;
				continue;
			}

			if (validityFlags & Quackle::GamePosition::InvalidOpeningPlace)
			{
				carryOn = askToCarryOn(tr("Opening play %1 does not cover the star; make play anyway?").arg(QuackleIO::Util::moveToDetailedString(prettiedMove)));
				validityFlags ^= Quackle::GamePosition::InvalidOpeningPlace;
				continue;
			}

			if (validityFlags & Quackle::GamePosition::UnacceptableWord)
			{
				carryOn = askToCarryOn(tr("%1 forms an unacceptable word; make play anyway?").arg(QuackleIO::Util::moveToDetailedString(prettiedMove)));

				if (carryOn)
				{
					if (QMessageBox::question(this, tr("Challenge Decision - Quackle"), dialogText(tr("If committed, should %1 be challenged off the board?")).arg(QuackleIO::Util::moveToDetailedString(prettiedMove)), QMessageBox::Yes, QMessageBox::No) == QMessageBox::Yes)
					{
						prettiedMove.setIsChallengedPhoney(true);
					}
					else
						playHasIllegalWords = true;
				}

				validityFlags ^= Quackle::GamePosition::UnacceptableWord;
				continue;
			}

			if (validityFlags & Quackle::GamePosition::InvalidAction)
			{
				statusMessage(tr("Urps."));
				carryOn = false;
				continue;
			}
		}

		if (!carryOn)
		{
			if (carryOnPtr != nullptr)
				*carryOnPtr = false;
			return;
		}

		if (playHasIllegalWords && QuackleIO::UtilSettings::self()->scoreInvalidAsZero)
			prettiedMove.score = 0;
		else
			m_game->currentPosition().scoreMove(prettiedMove);
		m_game->currentPosition().addAndSetMoveMade(prettiedMove);
		switchToTab(ChoicesTabIndex);
		ensureUpToDateSimulatorMoveList();
	}

	if (!m_game->currentPosition().currentPlayer().racksAreKnown() &&
		!m_game->currentPosition().currentPlayer().rack().contains(prettiedMove.usedTiles()) &&
		prettiedMove.action != Quackle::Move::BlindExchange)
	{
		m_game->currentPosition().setCurrentPlayerRack(Quackle::Rack(prettiedMove.usedTiles()));
	}

	updatePositionViews();

	// this duplicates the job of updateMoveViews as it also does
	// a check if we have simulation results -- but we don't want to send out
	// a moves changed signal if we don't have results because
	// we just sent out a position changed signal
	if (m_simulator->hasSimulationResults())
		updateMoveViews();
}

bool TopLevel::validifyMove(Quackle::Move &move)
{
	Quackle::LetterString tiles = move.usedTiles();
	Quackle::Rack rack = m_game->currentPosition().currentPlayer().rack();
	if (rack.contains(tiles))
		return true;

	if (move.action == Quackle::Move::Exchange)
	{
		if (tiles.size() > rack.size())
			return false;

		Quackle::LetterString newTiles;
		for (Quackle::LetterString::const_iterator it = tiles.begin(); it != tiles.end(); ++it)
		{
			Quackle::Letter theLetterLetter = *it;
			Quackle::LetterString theLetter;
			theLetter.push_back(theLetterLetter);

			if (!rack.contains(theLetter))
			{
				theLetterLetter = Quackle::String::front(rack.tiles());

				theLetter.clear();
				theLetter.push_back(theLetterLetter);
			}
			
			newTiles.push_back(theLetterLetter);
			rack.unload(theLetter);
		}

		move.setTiles(newTiles);
	}

	if (move.action == Quackle::Move::Place)
	{
		if (!m_game->currentPosition().canSetCurrentPlayerRackWithoutBagExpansion(Quackle::Rack(tiles)))
			return false;
	}

	return true;
}

void TopLevel::removeCandidateMoves(const Quackle::MoveList *moves)
{
	if (!m_game->hasPositions())
		return;

	const Quackle::MoveList::const_iterator end(moves->end());
	for (Quackle::MoveList::const_iterator it = moves->begin(); it != end; ++it)
		m_game->currentPosition().removeMove(*it);

	ensureUpToDateSimulatorMoveList();
	updateMoveViews();
}

void TopLevel::setRack(const Quackle::Rack &rack)
{
	if (!m_game->hasPositions())
		return;

	if (rack.empty())
		return;
	
	Quackle::Rack rackToSet = rack;

	if (!m_game->currentPosition().canSetPlayerRackWithoutBagExpansion(m_game->currentPosition().currentPlayer().id(), rack))
	{
		if (!askToCarryOn(tr("The rack %1 contains letters that are not available without modifying the tile distribution; do you wish to expand the bag to allow this?").arg(QuackleIO::Util::letterStringToQString(rack.tiles()))))
		{
			rackToSet = m_game->currentPosition().currentPlayer().rack();
		}
	}

	m_game->currentPosition().setPlayerRack(m_game->currentPosition().currentPlayer().id(), rackToSet);
	m_simulator->currentPosition().setCurrentPlayerRack(rackToSet);
	updatePositionViews();

	statusMessage(tr("%1's rack set to %2.").arg(QuackleIO::Util::uvStringToQString(m_game->currentPosition().currentPlayer().name())).arg(QuackleIO::Util::letterStringToQString(rackToSet.tiles())));
	setModified(true);
}

void TopLevel::setNote(const UVString &note)
{
	m_game->currentPosition().setExplanatoryNote(note);
	setModified(true);
}

void TopLevel::goToHistoryLocation(const Quackle::HistoryLocation *location)
{
	if (!m_game->hasPositions())
		return;

	// FIXME this shouldn't be necessary once OppoThread::abort() works.
	if (shouldOutcraftyCurrentPlayer() && isPlayerOnTurnComputer())
	{
		statusMessage(tr("Please wait for %1 to play before looking at the history.").arg(QuackleIO::Util::uvStringToQString(m_game->currentPosition().playerOnTurn().name())));
		return;
	}

	stopEverything();
	stopOutcraftyingCurrentPlayer();

	m_game->setCurrentPosition(*location);
	showToHuman();
}

void TopLevel::stopEverything()
{
	// stop simulation if it's going
	simulate(false);

	for (const auto& it : m_otherOppoThreads)
		it->abort();
	for (const auto& it : m_oppoThreads)
		it->abort();
}

OppoThreadProgressBar *TopLevel::createProgressBarForThread(OppoThread *thread)
{
	OppoThreadProgressBar *bar = new OppoThreadProgressBar(thread);
	m_progressIndicators[thread] = bar;
	statusBar()->addPermanentWidget(bar);
	return bar;
}

void TopLevel::removeProgressIndicators()
{
	QMap<OppoThread *, OppoThreadProgressBar *>::const_iterator it = m_progressIndicators.constBegin();
	while (it != m_progressIndicators.constEnd()) {
		statusBar()->removeWidget(it.value());
		delete it.value();
		++it;
	}

	m_progressIndicators.clear();
}

OppoThreadProgressBar *TopLevel::progressBarOfThread(OppoThread *thread)
{
	if (!m_progressIndicators.contains(thread))
		return 0;

	return m_progressIndicators[thread];
}

void TopLevel::removeProgressIndicator(OppoThread *thread)
{
	statusBar()->removeWidget(progressBarOfThread(thread));
	delete progressBarOfThread(thread);
	m_progressIndicators.remove(thread);
}

void TopLevel::updateAllViews()
{
	if (!m_game->hasPositions())
		return;

	updateHistoryViews();
	updatePositionViews();
	updateSimViews();

	m_game->currentPosition().ensureBoardIsPreparedForAnalysis();
}

void TopLevel::updatePositionViews()
{
	emit positionChanged(&m_game->currentPosition());

	m_simulateAction->setEnabled(!m_game->currentPosition().moves().empty());
	m_simulateDetailsAction->setEnabled(!m_game->currentPosition().moves().empty());
	m_simulateClearAction->setEnabled(!m_game->currentPosition().moves().empty());
	m_kibitzAction->setEnabled(!m_game->currentPosition().gameOver());
	m_kibitzFiftyAction->setEnabled(!m_game->currentPosition().gameOver());
	m_kibitzAllAction->setEnabled(!m_game->currentPosition().gameOver());
	m_kibitzAsActions->setEnabled(!m_game->currentPosition().gameOver());
	m_passAction->setEnabled(!m_game->currentPosition().gameOver());
	m_overdrawAction->setEnabled(!m_game->currentPosition().gameOver());
	m_reportAsActions->setEnabled(m_game->hasPositions());
	m_htmlReportAction->setEnabled(m_game->hasPositions());
	m_firstPositionAction->setEnabled(m_game->hasPositions());

#ifdef ENABLE_GRAPHICAL_REPORT
	m_graphicalReportAction->setEnabled(m_game->hasPositions());
#endif

	const bool hasAMove = m_game->currentPosition().moveMade().isAMove();

	const bool commitAllowed = isCommitAllowed();
	m_commitAction->setEnabled(commitAllowed && hasAMove);
	m_commitTopChoiceAction->setEnabled(commitAllowed);

	bool hasNextPosition;
	m_game->history().nextPosition(&hasNextPosition);
	m_nextPositionAction->setEnabled(hasNextPosition);

	bool hasPreviousPosition;
	m_game->history().previousPosition(&hasPreviousPosition);
	m_previousPositionAction->setEnabled(hasPreviousPosition);

	const QString assistiveText = tr("Click once or twice on the board, type, then press the Enter key.");

	if (commitAllowed && hasAMove)
		statusMessage(tr("Press a Commit button to play %1.").arg(QuackleIO::Util::moveToDetailedString(m_game->currentPosition().moveMade())));
	else if (isPlayerOnTurnComputer())
		statusMessage(tr("%1 to play.").arg(QuackleIO::Util::uvStringToQString(m_game->currentPosition().playerOnTurn().name())));
	else if (m_game->currentPosition().currentPlayer().drawnLetters().empty())
		statusMessage(tr("%1 to play. %2").arg(QuackleIO::Util::uvStringToQString(m_game->currentPosition().playerOnTurn().name())).arg(assistiveText));
	else
		statusMessage(tr("%1 to play after drawing %2. %3").arg(QuackleIO::Util::uvStringToQString(m_game->currentPosition().playerOnTurn().name())).arg(QuackleIO::Util::letterStringToQString(m_game->currentPosition().playerOnTurn().drawnLetters().tiles())).arg(assistiveText));

	updateListerDialogWithRack();
}

void TopLevel::updateMoveViews()
{
	if (m_simulator->hasSimulationResults())
	{
		const Quackle::MoveList& moveList = m_simulator->moves(/* prune */ true, /* sort by win */ true);
		emit movesChanged(&moveList);
	}
	else
	{
		const Quackle::MoveList moveList = m_game->currentPosition().moves();
		emit movesChanged(&moveList);
	}

	m_simulateAction->setEnabled(!m_game->currentPosition().moves().empty());
	m_simulateDetailsAction->setEnabled(!m_game->currentPosition().moves().empty());
	m_simulateClearAction->setEnabled(!m_game->currentPosition().moves().empty());
}

void TopLevel::updateHistoryViews()
{
	emit historyChanged(m_game->history());
}

void TopLevel::initializeGame(const Quackle::PlayerList *players)
{
	stopEverything();

	m_game->reset();
	m_filename = QString();
	m_logania = 0;
	setModified(false);

	if (players->empty())
		return;

	Quackle::PlayerList newPlayers(*players);

	// shuffle so same person doesn't go first twice in a row,
	// if there are multiple players in the game
	if (newPlayers.size() > 1)
	{
		UVString prevFirst = m_firstPlayerName;
		while (m_firstPlayerName == prevFirst || m_firstPlayerName.empty())
		{
			QUACKLE_DATAMANAGER->shuffle(newPlayers);
			m_firstPlayerName = newPlayers.front().name();
			if (all_of(newPlayers.begin(),
					   newPlayers.end(),
					   [&](const Quackle::Player& p) { return p.name() == prevFirst; }
					   ))
				break; // all player names are identical...break an infinite loop
		}
	}

	m_game->setPlayers(newPlayers);
	m_game->associateKnownComputerPlayers();

	m_game->addPosition();
	
	advanceGame();

	setCaption(gameTitle());

	const bool debugEndgame = false;
	if (debugEndgame)
	{
		for (int i = 0; i < 20; ++i)
			m_game->haveComputerPlay();
		advanceGame();
	}
}

void TopLevel::open()
{
	pause(true);

	if (m_modified)
	{
		switch (askToSave())
		{
		case QMessageBox::Save:
			save();

		case QMessageBox::Discard:
			break;

		case QMessageBox::Cancel:
		default:
			return;
		}
	}
	
	// QString getOpenFileName ( QWidget * parent = 0, const QString & caption = QString(), const QString & dir = QString(), const QString & filter = QString(), QString * selectedFilter = 0, Options options = 0 )
	QString defaultFilter = defaultGameFileFilter();
	QString filename = QFileDialog::getOpenFileName(this, tr("Choose game file to open"), getInitialDirectory(), gameFileFilters(), &defaultFilter);

	if (!filename.isEmpty())
		openFile(filename);
}

void TopLevel::openFile(const QString &filename)
{
	m_filename = filename;
	setInitialDirectory(filename);
	loadFile(m_filename);
	saveSettings();

	setCaption(gameTitle());
}

QString TopLevel::getInitialDirectory() const
{
	return m_initialDirectory.isEmpty()? QDir::homePath() : m_initialDirectory;
}

void TopLevel::setInitialDirectory(const QString &filename)
{
	QFileInfo file(filename);
	m_initialDirectory = file.path();
}

QString TopLevel::gameFileFilters() const
{
	return tr("Game files (%1);;All files (*)").arg(QuackleIO::Queenie::self()->filters().join(" "));
}

QString TopLevel::defaultGameFileFilter() const
{
	return QuackleIO::Queenie::self()->filters().join(" ");
}

void TopLevel::newGame()
{
	if (!setupCheck())
		return;
	
	pause(true);

	if (m_modified)
	{
		switch (askToSave())
		{
		case QMessageBox::Save:
			save();

		case QMessageBox::Discard:
			break;

		case QMessageBox::Cancel:
		default:
			return;
		}
	}
	
	NewGameDialog newGameDialog(this);
	switch (newGameDialog.exec())
	{
	case QDialog::Accepted:
	{
		const Quackle::PlayerList &players = newGameDialog.players();
		initializeGame(&players);
		break;
	}

	case QDialog::Rejected:
		break;
	}

	// Birthday
	startBirthday();
}

void TopLevel::setCaption(const QString &text)
{
	if (!text.isNull())
		m_ourCaption = text;

	if (m_filename.isEmpty())
		setWindowTitle(QString("%1 - Quackle").arg(m_ourCaption));
	else
	{
		QString filename = QDir::fromNativeSeparators(m_filename);
		filename = filename.mid(filename.lastIndexOf('/') + 1);
		setWindowTitle(QString("%1[*] - %2 - Quackle").arg(filename, m_ourCaption));
	}
}

void TopLevel::setModified(bool modified)
{
	m_modified = modified;
	// conditional check avoids Qt console outputs regarding missing "[*]"
	if (!modified || !m_filename.isEmpty())
		setWindowModified(m_modified);
}

bool TopLevel::setupCheck()
{
	bool okay = QUACKLE_DATAMANAGER->isGood();

	if (!okay)
	{
		QMessageBox::warning(this, tr("Not Ready - Quackle"), dialogText(tr("Quackle cannot load its lexicon files.")));
	}

	return okay;
}

void TopLevel::plugIntoBaseMatrix(BaseView *view)
{
	connect(view, SIGNAL(statusMessage(const QString &)), this, SLOT(statusMessage(const QString &)));
}

void TopLevel::plugIntoMatrix(View *view)
{
	plugIntoBaseMatrix(view);

	connect(view, SIGNAL(setCandidateMove(const Quackle::Move *, bool *)), this, SLOT(setCandidateMove(const Quackle::Move *, bool *)));
	connect(view, SIGNAL(removeCandidateMoves(const Quackle::MoveList *)), this, SLOT(removeCandidateMoves(const Quackle::MoveList *)));
	connect(view, SIGNAL(commit()), this, SLOT(commit()));
	connect(view, SIGNAL(setRack(const Quackle::Rack &)), this, SLOT(setRack(const Quackle::Rack &)));
	connect(view, SIGNAL(setNote(const UVString &)), this, SLOT(setNote(const UVString &)));
}

void TopLevel::plugIntoPositionMatrix(View *view)
{
	connect(this, SIGNAL(positionChanged(const Quackle::GamePosition *)), view, SLOT(positionChanged(const Quackle::GamePosition *)));
}

void TopLevel::plugIntoMoveMatrix(View *view)
{
	connect(this, SIGNAL(movesChanged(const Quackle::MoveList *)), view, SLOT(movesChanged(const Quackle::MoveList *)));
}

void TopLevel::plugIntoHistoryMatrix(HistoryView *view)
{
	plugIntoBaseMatrix(view);

	connect(view, SIGNAL(goToHistoryLocation(const Quackle::HistoryLocation *)), this, SLOT(goToHistoryLocation(const Quackle::HistoryLocation *)));

	connect(this, SIGNAL(historyChanged(const Quackle::History &)), view, SLOT(historyChanged(const Quackle::History &)));
}

QMessageBox::StandardButton TopLevel::askToSave()
{
	return QMessageBox::warning(this, tr("Unsaved Moves - Quackle"), dialogText(tr("There are unsaved moves in the current game. Save them?")), QMessageBox::Save | QMessageBox::Discard | QMessageBox::Cancel, QMessageBox::Save);
}

void TopLevel::generateList()
{
	if (!setupCheck())
		return;
	
	pause(true);

	if (m_listerDialog)
	{
		m_listerDialog->show();
		m_listerDialog->raise();
	}

	updateListerDialogWithRack();
}

void TopLevel::updateListerDialogWithRack()
{
	if (m_listerDialog && m_game->hasPositions())
		m_listerDialog->setQuery(QuackleIO::Util::letterStringToQString(m_game->currentPosition().currentPlayer().rack().tiles()));
}

void TopLevel::letterbox()
{
	if (!m_letterbox)
	{
		m_letterbox = new Letterbox(this, m_preferencesAction, m_listerDialog);
	}

	m_letterbox->show();
	m_letterbox->raise();
}

void TopLevel::kibitz()
{
	if (!m_game->hasPositions())
		return;

	const bool confuseUser = false;

	if (confuseUser)
	{
		const size_t currentlyKibitzed = m_game->currentPosition().moves().size();
		kibitz(currentlyKibitzed < kExtraPlaysToKibitz? kExtraPlaysToKibitz : (int)currentlyKibitzed + kExtraPlaysToKibitz);
	}
	else
	{
		kibitz(kExtraPlaysToKibitz);
	}
}

void TopLevel::kibitz(int numberOfPlays, Quackle::ComputerPlayer *computerPlayer)
{
	if (!m_game->hasPositions())
		return;

	if (computerPlayer)
	{
		OppoThread *thread = new OppoThread;
		m_otherOppoThreads.push_back(thread);
		connect(thread, SIGNAL(finished()), this, SLOT(kibitzThreadFinished()));
		connect(thread, SIGNAL(fractionDone(double, OppoThread *)), this, SLOT(playerFractionDone(double, OppoThread *)));
		thread->setPosition(m_game->currentPosition());

		thread->setPlayer(computerPlayer->clone());
		thread->findBestMoves(numberOfPlays);
		statusMessage(tr("Asked %1 for her choices. Please allow her time to think.").arg(QuackleIO::Util::uvStringToQString(computerPlayer->name())));
	}
	else
	{
		m_game->currentPosition().kibitz(numberOfPlays);
		kibitzFinished();
	}
}

void TopLevel::kibitzThreadFinished()
{
	if (m_otherOppoThreads.begin() == m_otherOppoThreads.end())
		return;
	QString name;
	QString rack;
	for (QList<OppoThread *>::iterator it = m_otherOppoThreads.begin(); it != m_otherOppoThreads.end(); )
	{
		if ((*it)->isFinished())
		{
			removeProgressIndicator(*it);

			m_game->currentPosition().setMoves((*it)->moves());
			name = QuackleIO::Util::uvStringToQString((*it)->player()->name());
			rack = QuackleIO::Util::letterStringToQString((*it)->position().currentPlayer().rack().tiles());

			// player is a clone
			delete (*it)->player();

			delete (*it);
			it = m_otherOppoThreads.erase(it);
		}
		else
		{
			++it;
		}
	}

	kibitzFinished();

	statusMessage(tr("Showing %1's choices from %2.").arg(name).arg(rack));
}

void TopLevel::kibitzFinished()
{
	updatePositionViews();
	ensureUpToDateSimulatorMoveList();

	switchToTab(ChoicesTabIndex);
}

void TopLevel::kibitzFifty()
{
	kibitz(50);
}

void TopLevel::kibitzAll()
{
	kibitz(INT_MAX);
}

void TopLevel::kibitzAs(Quackle::ComputerPlayer *computerPlayer)
{
	kibitz(kExtraPlaysToKibitz, computerPlayer);
}

void TopLevel::firstPosition()
{
	bool exists;
	const Quackle::GamePosition &firstPozzy = m_game->history().firstPosition(&exists);
	if (exists)
	{
		const Quackle::HistoryLocation &location = firstPozzy.location();
		goToHistoryLocation(&location);
	}
}

void TopLevel::nextPosition()
{
	bool exists;
	const Quackle::GamePosition &nextPozzy = m_game->history().nextPosition(&exists);
	if (exists)
	{
		const Quackle::HistoryLocation &location = nextPozzy.location();
		goToHistoryLocation(&location);
	}
}

void TopLevel::previousPosition()
{
	bool exists;
	const Quackle::GamePosition &previousPozzy = m_game->history().previousPosition(&exists);
	if (exists)
	{
		const Quackle::HistoryLocation &location = previousPozzy.location();
		goToHistoryLocation(&location);
	}
}

void TopLevel::commitTopChoice()
{
	if (!m_game->hasPositions())
		return;

	m_game->currentPosition().kibitz(1);
	m_game->setCandidate(m_game->currentPosition().moves().back());
	commit();
}

void TopLevel::ensureUpToDateSimulatorMoveList()
{
	m_simulator->setIncludedMoves(m_game->currentPosition().moves());
}

void TopLevel::simulate(bool startSimulation)
{
	m_simulateAction->setChecked(startSimulation);

	// it's not so useful to have sim control show/hide
	// like this
	//m_simulatorWidget->setVisible(startSimulation);

	if (startSimulation)
	{
		logfileChanged();
		incrementSimulation();
	}
	else
		m_simulationTimer->stop();
}

void TopLevel::simulateToggled(bool startSimulation)
{
	if (!m_game->hasPositions())
		return;

	simulate(startSimulation);

	if (startSimulation)
	{
		switchToTab(ChoicesTabIndex);
		statusMessage(tr("Starting simulation. To stop the simulation, uncheck the \"Simulate\" menu entry in the Move menu."));

		// Make sure the simulator has the correct partial oppo rack.
		partialOppoRackChanged();
	}
}

void TopLevel::clearSimulationResults()
{
	if (!m_game->hasPositions())
		return;

	m_simulator->resetNumbers();

	updateMoveViews();
	updateSimViews();
}

void TopLevel::timeout()
{
	UVcout << "toplevel::timeout" << endl;
}

void TopLevel::pliesSet(int plyIndex)
{
	QString plyString = m_pliesCombo->currentText();
	if (plyString == tr("Many"))
		m_plies = -1;
	else
		m_plies = plyString.toInt();
}

void TopLevel::ignoreOpposChanged()
{
	m_simulator->setIgnoreOppos(m_ignoreOpposCheck->isChecked());
}

void TopLevel::updatePliesCombo()
{
	int index;

	if (m_plies == -1)
		index = m_pliesToOffer;
	else
		index = m_plies - 1;

	m_pliesCombo->setCurrentIndex(index);
}

void TopLevel::logfileEnabled(bool on)
{
	setLogfileEnabled(on);
}

void TopLevel::setLogfileEnabled(bool /* enabled */)
{
	// not needed with QGroupBox
	//m_logfileChooser->setEnabled(enabled);
	//m_logfileEdit->setEnabled(enabled);

	logfileChanged();
}

bool TopLevel::isLogfileEnabled() const
{
	return m_logfileEnable->isChecked();
}

QString TopLevel::userSpecifiedLogfile() const
{
	return m_logfileEdit->text();
}

QString TopLevel::logfile() const
{
	return isLogfileEnabled()? userSpecifiedLogfile() : QString("");
}

void TopLevel::logfileChanged()
{
	m_simulator->setLogfile(QuackleIO::Util::qstringToStdString(logfile()), /* append */ true);
}

void TopLevel::chooseLogfile()
{
	QString filename;

	QFileDialog *fileDialog = new QFileDialog(this, tr("Choose log file"));

	fileDialog->setDirectory(userSpecifiedLogfile().isEmpty()? QDir::currentPath() : QFileInfo(userSpecifiedLogfile()).absolutePath());
	fileDialog->setFileMode(QFileDialog::AnyFile);
	fileDialog->setOption(QFileDialog::DontConfirmOverwrite);

	if (fileDialog->exec())
	{
		QStringList files(fileDialog->selectedFiles());
		if (!files.empty())
			filename = files.back();
	}

	if (filename.isEmpty())
		return;

	m_logfileEdit->setText(filename);
	logfileChanged();
}

void TopLevel::partialOppoRackEnabled(bool on)
{
	setPartialOppoRackEnabled(on);
}

void TopLevel::setPartialOppoRackEnabled(bool /* enabled */)
{
	partialOppoRackChanged();
}

bool TopLevel::isPartialOppoRackEnabled() const
{
	return m_partialOppoRackEnable->isChecked();
}

QString TopLevel::userSpecifiedPartialOppoRack() const
{
	return m_partialOppoRackEdit->text();
}

QString TopLevel::partialOppoRack() const
{
	return isPartialOppoRackEnabled()? userSpecifiedPartialOppoRack() : QString("");
}

void TopLevel::partialOppoRackChanged()
{
	Quackle::Rack rack = QuackleIO::Util::makeRack(partialOppoRack());
	if (!m_game->hasPositions())
		return;

	Quackle::LetterString oppoTiles = m_game->currentPosition().currentPlayer().rack().tiles();
	Quackle::LetterString doubleTiles = rack.tiles();
	for (Quackle::LetterString::const_iterator it = oppoTiles.begin(); it != oppoTiles.end(); ++it)
	{
		doubleTiles.push_back(*it);
	}
	Quackle::Rack doubleRack(doubleTiles);

	if (!m_game->currentPosition().canSetPlayerRackWithoutBagExpansion(m_game->currentPosition().currentPlayer().id(), doubleRack))
	{
		QMessageBox::warning(this, tr("Wrackful Rack - Quackle"), dialogText(tr("The rack %1 contains letters that are not possible for opponent to hold.").arg(QuackleIO::Util::letterStringToQString(rack.tiles()))));
		m_partialOppoRackEdit->selectAll();
		m_partialOppoRackEdit->setFocus();
		return;
	}

	m_simulator->setPartialOppoRack(rack);
}

void TopLevel::showSimulationDetails()
{
	if (!m_simViewer)
		m_simViewer = new SimViewer(this);

	m_simViewer->setSimulator(*m_simulator);
	m_simViewer->show();

	updateSimViews();
}

void TopLevel::incrementSimulation()
{
	if (m_simulateAction->isChecked())
	{
		m_simulator->simulate(m_plies);
		m_simulationTimer->start(0);

		if (m_simulator->iterations() % 10 == 0)
			updateMoveViews();

		updateSimViews();
	}
}

void TopLevel::updateSimViews()
{
	m_simulatorWidget->setTitle(m_simulator->hasSimulationResults()? tr("Simulation: %2 iterations").arg(m_simulator->iterations()) : tr("Simulation"));

	if (m_simViewer && m_simViewer->isVisible())
		m_simViewer->setSimulator(*m_simulator);
}

void TopLevel::loadFile(const QString &filename)
{
	if (filename.isEmpty())
		return;

	QString strippedFilename(filename.right(filename.length() - filename.lastIndexOf("/") - 1));
	statusMessage(tr("Loading %1...").arg(strippedFilename));
	qApp->processEvents();

	QFile file(filename);
	if (!file.exists())
	{
		QMessageBox::critical(this, tr("Error Loading Game File - Quackle"), dialogText(tr("Filename %1 does not exist.")).arg(filename));
		return;
	}

	if (!file.open(QIODevice::ReadOnly | QIODevice::Text))
	{
		QMessageBox::critical(this, tr("Error Loading Game File - Quackle"), dialogText(tr("%1 cannot be opened.")).arg(filename));
		return;
	}

	QuackleIO::Logania *logania = QuackleIO::Queenie::self()->loganiaForFile(filename);
	if (logania == 0)
	{
		QMessageBox::critical(this, tr("Error Loading Game File - Quackle"), dialogText(tr("Sorry, %1 is in a format Quackle cannot read.")).arg(filename));
		file.close();
		return;
	}

	QTextStream stream(&file);
	m_game = logania->read(stream, QuackleIO::Logania::MaintainBoardPreparation);

	file.close();

	m_logania = logania;

	statusMessage(tr("Loaded game from `%1'.").arg(filename));
	setModified(false);

	startBirthday();

	showToHuman();
}

void TopLevel::save()
{
	if (m_filename.isEmpty())
	{
		saveAs();
		return;
	}

	writeFile(m_filename);
}

void TopLevel::saveAs()
{
	QString defaultFilter = defaultGameFileFilter();
	QString filename = QFileDialog::getSaveFileName(this, tr("Choose file to which to save game"), getInitialDirectory(), gameFileFilters(), &defaultFilter);

	if (!filename.isEmpty())
	{
		m_filename = filename;
		setInitialDirectory(filename);
		writeFile(m_filename);
		saveSettings();
	}
}

void TopLevel::reportAs(Quackle::ComputerPlayer *player)
{
	QString filename = QFileDialog::getSaveFileName(this, tr("Choose file to which to write report"), getInitialDirectory());

	if (!filename.isEmpty())
	{
		setInitialDirectory(filename);
		QFile file(filename);
	
		if (!file.open(QIODevice::WriteOnly | QIODevice::Text))
		{
			QMessageBox::critical(this, tr("Error Writing File - Quacker"), dialogText(tr("Could not open %1 for writing.")).arg(filename));        
			return;    
		}    

		Quackle::ComputerPlayer *clone = player->clone();

		QTextStream stream(&file);
		SET_QTEXTSTREAM_TO_UTF8(stream);
		QuackleIO::StreamingReporter::reportGame(*m_game, clone, stream);
		delete clone;
	}
}

void TopLevel::htmlReport()
{
	Quackle::ComputerPlayer *player = new Quackle::StaticPlayer();
	QString filename = QFileDialog::getSaveFileName(this, tr("Choose HTML file to which to write report - Quackle"), getInitialDirectory(), "HTML Files (*.html)");

	if (!filename.isEmpty())
	{
		setInitialDirectory(filename);

		GraphicalReporter reporter(filename, /* don't generate images */ false);
		reporter.reportGame(*m_game, player);
	}

	delete player;
}

void TopLevel::graphicalReport()
{
	Quackle::ComputerPlayer *player = new Quackle::StaticPlayer();
	QString directory = QFileDialog::getExistingDirectory(this, tr("Choose directory to which to write report and graphics"), getInitialDirectory());

	if (!directory.isEmpty())
	{
		setInitialDirectory(directory);

		GraphicalReporter reporter(directory, /* generate images */ true);
		reporter.reportGame(*m_game, player);
	}

	delete player;
}

void TopLevel::writeFile(const QString &filename)
{
	if (!m_game->hasPositions())
		return;
	
	QFile file(filename);
	 
	if (!file.open(QIODevice::WriteOnly | QIODevice::Text))    
	{        
		QMessageBox::critical(this, tr("Error Writing File - Quacker"), dialogText(tr("Could not open %1 for writing.")).arg(filename));        
		return;    
	}    

	QTextStream stream(&file);

	QuackleIO::Logania *logania = m_logania? m_logania : QuackleIO::Queenie::self()->defaultLogania();

	if (logania == 0)
	{
		file.close();
		return;
	}

	logania->write(*m_game, stream);

	file.close();

	setModified(false);
	statusMessage(tr("Saved game to `%1'.").arg(filename));
}

void TopLevel::pause(bool paused)
{
	timerControl(paused);

	if (m_pauseAction->isChecked() != paused)
		m_pauseAction->setChecked(paused);

	if (!paused)
	{
		// set focus to widget that should have focus
		m_brb->grabFocus();
	}

/*
	if (paused)
		statusMessage(tr("Paused."));
	else
		statusMessage(tr("Resuming..."));
*/
}

void TopLevel::advanceGame()
{
	if (isPlayerOnTurnComputer())
		startOppoThread();

	showToHuman();
}

void TopLevel::startOppoThread()
{
	OppoThread *thread = new OppoThread;
	m_oppoThreads.push_back(thread);
	connect(thread, SIGNAL(finished()), this, SLOT(computerPlayerDone()));
	connect(thread, SIGNAL(fractionDone(double, OppoThread *)), this, SLOT(playerFractionDone(double, OppoThread *)));
	thread->setPosition(m_game->currentPosition());

	thread->setPlayer(m_game->currentPosition().playerOnTurn().computerPlayer()->clone());
	thread->findBestMoves(1);
}

void TopLevel::startOutcraftyingCurrentPlayer()
{
	if (shouldOutcraftyCurrentPlayer())
	{
		m_game->currentPosition().setCurrentPlayer(m_game->currentPosition().humanPlayer().id());
	}
}

bool TopLevel::shouldOutcraftyCurrentPlayer() const
{
	return !m_game->currentPosition().gameOver() && m_game->hasPositions() && m_game->currentPosition().location() == m_game->history().lastLocation();
}

void TopLevel::stopOutcraftyingCurrentPlayer()
{
	// Magic
	m_game->currentPosition().setCurrentPlayer(m_game->currentPosition().playerOnTurn().id());
}

void TopLevel::computerPlayerDone()
{
	if (m_oppoThreads.begin() == m_oppoThreads.end())
		return;

	Quackle::MoveList moves;

	for (QList<OppoThread *>::iterator it = m_oppoThreads.begin(); it != m_oppoThreads.end(); )
	{
		if ((*it)->isFinished())
		{
			removeProgressIndicator(*it);

			if ((*it)->position().playerOnTurn().positionallyEqual(m_game->currentPosition().playerOnTurn()))
			{
				moves = (*it)->moves();
			}
			else
			{
				UVcout << "Player on turn in position is not positionally equal to current player" << endl;
				UVcout << (*it)->position().playerOnTurn() << endl;
				UVcout << m_game->currentPosition().playerOnTurn() << endl;
			}

			// player is a clone
			delete (*it)->player();

			delete (*it);
			it = m_oppoThreads.erase(it);
		}
		else
		{
			++it;
			UVcout << "computerPlayerDone but thread is not finished" << endl;
		}
	}

	if (moves.empty())
	{
		UVcout << "compy moves are empty" << endl;
		return;
	}

	stopOutcraftyingCurrentPlayer();
	m_game->commitMove(moves.front());
	setModified(true);
	showToHuman();

	if (!m_game->currentPosition().gameOver() && isPlayerOnTurnComputer())
	{
		QTimer::singleShot(0, this, SLOT(advanceGame()));
		return;
	}
}

void TopLevel::playerFractionDone(double fraction, OppoThread *thread)
{
	OppoThreadProgressBar *bar = progressBarOfThread(thread);
	if (bar == 0)
		bar = createProgressBarForThread(thread);
	bar->setValue(static_cast<int>(fraction * 100));
}

void TopLevel::showToHuman()
{
	if (!m_game->currentPosition().gameOver())
	{
		startOutcraftyingCurrentPlayer();
	}

	Quackle::Rack rack(m_game->currentPosition().currentPlayer().rack());

	// TODO
	rack.setTiles(QuackleIO::Util::arrangeLettersForUser(rack));

	// make sure that the internal order of rack is how user likes it
	m_game->currentPosition().setCurrentPlayerRack(rack);

	m_simulator->setPosition(m_game->currentPosition());

	updateHistoryViews();
	updatePositionViews();

	ensureUpToDateSimulatorMoveList();
	updateSimViews();

	if (m_game->currentPosition().gameOver())
	{
		pause(true);

		statusMessage(tr("Game over. Double-click an entry in the History table to perform postmortem."));
	}
	else
	{
		pause(false);
	}

	m_showAsciiAction->setEnabled(true);
	m_saveAction->setEnabled(true);
	m_saveAsAction->setEnabled(true);

	switchToTab(HistoryTabIndex);
	m_brb->grabFocus();

	// TODO We should probably tell the user if the tile situation
	// has become nonideal instead of just having this function
	// spit things to stdout.

	m_game->currentPosition().ensureProperBag();
}

void TopLevel::timerControl(bool paused)
{
	if (paused)
		m_timer->stop();
	else
	{
		// start timer
		// eg m_timer->start(timerLength(), /* single shot */ true);
		// except we'll want a non-single-shot job
	}
}

QString TopLevel::gameTitle()
{
	QString ret;
	const Quackle::PlayerList &players(m_game->players());

	if (players.size() == 0)
		ret = tr("No Game");
	else if (players.size() == 1)
		ret = tr("%1's solo game").arg(QuackleIO::Util::uvStringToQString(players.front().name()));
	else if (players.size() == 2)
		ret = tr("%1 versus %2").arg(QuackleIO::Util::uvStringToQString(players.front().name())).arg(QuackleIO::Util::uvStringToQString(players.at(1).name()));
	else if (players.size() > 2)
		ret = tr("Game between %1 and friends").arg(QuackleIO::Util::uvStringToQString(players.front().name()));

	return ret;
}

void TopLevel::createMenu()
{
	const bool enableCommitAction = true;
	const bool putCommitActionOnToolbar = false;
	const bool enableLetterbox = false;

	//// Game menu
	
	m_newAction = new QAction(tr("&New game..."), this);
	m_newAction->setShortcut(tr("Ctrl+N"));
	connect(m_newAction, SIGNAL(triggered()), this, SLOT(newGame()));

	m_openAction = new QAction(tr("&Open..."), this);
	m_openAction->setShortcut(tr("Ctrl+O"));
	connect(m_openAction, SIGNAL(triggered()), this, SLOT(open()));

	m_saveAction = new QAction(tr("&Save"), this);
	m_saveAction->setEnabled(false);
	m_saveAction->setShortcut(tr("Ctrl+S"));
	connect(m_saveAction, SIGNAL(triggered()), this, SLOT(save()));

	m_saveAsAction = new QAction(tr("Save &as..."), this);
	m_saveAsAction->setEnabled(false);
	connect(m_saveAsAction, SIGNAL(triggered()), this, SLOT(saveAs()));

	QAction *printAction = new QAction(tr("&Print..."), this);
	connect(printAction, SIGNAL(triggered()), this, SLOT(print()));

	m_showAsciiAction = new QAction(tr("Show plaintext &board"), this);
	m_showAsciiAction->setEnabled(false);
	connect(m_showAsciiAction, SIGNAL(triggered()), this, SLOT(showAscii()));

	m_pauseAction = new QAction(tr("Pa&use"), this);
	m_pauseAction->setShortcut(tr("Ctrl+P"));
	m_pauseAction->setCheckable(true);
	connect(m_pauseAction, SIGNAL(toggled(bool)), this, SLOT(pause(bool)));

	QAction *quitAction = new QAction(tr("&Quit"), this);
	quitAction->setShortcut(tr("Ctrl+Q"));
	connect(quitAction, SIGNAL(triggered()), this, SLOT(close()));

	//// Move

	m_kibitzAction = new QAction(tr("&Generate choices"), this);
	m_kibitzAction->setShortcut(tr("Ctrl+G"));
	m_kibitzAction->setEnabled(false);
	connect(m_kibitzAction, SIGNAL(triggered()), this, SLOT(kibitz()));

	m_commitAction = new QAction(tr("Comm&it"), this);
	m_commitAction->setShortcut(tr("Ctrl+I"));
	m_commitAction->setEnabled(false);
	connect(m_commitAction, SIGNAL(triggered()), this, SLOT(commit()));

	m_passAction = new QAction(tr("&Pass"), this);
	m_passAction->setEnabled(false);
	connect(m_passAction, SIGNAL(triggered()), this, SLOT(pass()));

	m_overdrawAction = new QAction(tr("&Handle opponent overdraw..."), this);
	m_overdrawAction->setEnabled(false);
	connect(m_overdrawAction, SIGNAL(triggered()), this, SLOT(overdraw()));

	m_commitTopChoiceAction = new QAction(tr("&Commit top statically-evaluated choice"), this);
	m_commitTopChoiceAction->setShortcut(tr("Ctrl+T"));
	m_commitTopChoiceAction->setEnabled(false);
	connect(m_commitTopChoiceAction, SIGNAL(triggered()), this, SLOT(commitTopChoice()));

	m_kibitzFiftyAction = new QAction(tr("Generate 50 choices"), this);
	m_kibitzFiftyAction->setEnabled(false);
	connect(m_kibitzFiftyAction, SIGNAL(triggered()), this, SLOT(kibitzFifty()));

	m_kibitzAllAction = new QAction(tr("Generate &all choices"), this);
	m_kibitzAllAction->setEnabled(false);
	connect(m_kibitzAllAction, SIGNAL(triggered()), this, SLOT(kibitzAll()));

	//// Kibitzers and reports
	m_kibitzAsActions = new QActionGroup(this);
	m_reportAsActions = new QActionGroup(this);
	for (Quackle::PlayerList::const_reverse_iterator it = QUACKLE_COMPUTER_PLAYERS.rbegin(); it != QUACKLE_COMPUTER_PLAYERS.rend(); ++it)
	{
		if (!(*it).computerPlayer()->isUserVisible())
			continue;
		KibitzerListener *listener = new KibitzerListener((*it).computerPlayer(), this);

		QAction *kibitzAction = new QAction(tr("Ask %1 for choices").arg(QuackleIO::Util::uvStringToQString((*it).name())), this);
		kibitzAction->setIconText(tr("Ask %1").arg(QuackleIO::Util::uvStringToQString((*it).name())));
		connect(kibitzAction, SIGNAL(triggered()), listener, SLOT(kibitzTriggered()));

		QAction *reportAction = new QAction(tr("Ask %1 for a full-game report").arg(QuackleIO::Util::uvStringToQString((*it).name())), this);
		connect(reportAction, SIGNAL(triggered()), listener, SLOT(reportTriggered()));

		connect(listener, SIGNAL(kibitzAs(Quackle::ComputerPlayer *)), this, SLOT(kibitzAs(Quackle::ComputerPlayer *)));
		connect(listener, SIGNAL(reportAs(Quackle::ComputerPlayer *)), this, SLOT(reportAs(Quackle::ComputerPlayer *)));

		m_kibitzAsActions->addAction(kibitzAction);
		m_reportAsActions->addAction(reportAction);
	}
	m_kibitzAsActions->setEnabled(false);
	m_reportAsActions->setEnabled(false);

	// Go
	m_firstPositionAction = new QAction(tr("&First position"), this);
	m_firstPositionAction->setEnabled(false);
	m_firstPositionAction->setShortcut(tr("Ctrl+Home"));
	connect(m_firstPositionAction, SIGNAL(triggered()), this, SLOT(firstPosition()));

	m_previousPositionAction = new QAction(tr("&Back to previous position"), this);
	m_previousPositionAction->setIconText(tr("Back"));
	m_previousPositionAction->setEnabled(false);
	m_previousPositionAction->setShortcut(tr("Ctrl+B"));
	connect(m_previousPositionAction, SIGNAL(triggered()), this, SLOT(previousPosition()));

	m_nextPositionAction = new QAction(tr("&Forward to next position"), this);
	m_nextPositionAction->setIconText(tr("Forward"));
	m_nextPositionAction->setEnabled(false);
	m_nextPositionAction->setShortcut(tr("Ctrl+F"));
	connect(m_nextPositionAction, SIGNAL(triggered()), this, SLOT(nextPosition()));

	//// Simulation

	m_simulateAction = new QAction(tr("Start/Stop si&mulation"), this);
	m_simulateAction->setIconText(tr("Simulate"));
	m_simulateAction->setCheckable(true);
	m_simulateAction->setEnabled(false);
	connect(m_simulateAction, SIGNAL(toggled(bool)), this, SLOT(simulateToggled(bool)));

	m_simulateDetailsAction = new QAction(tr("Show simulation &details"), this);
	m_simulateDetailsAction->setEnabled(false);
	connect(m_simulateDetailsAction, SIGNAL(triggered()), this, SLOT(showSimulationDetails()));

	m_simulateClearAction = new QAction(tr("Clear simulation results"), this);
	m_simulateClearAction->setEnabled(false);
	connect(m_simulateClearAction, SIGNAL(triggered()), this, SLOT(clearSimulationResults()));

	//// Study

	QAction *letterboxAction = new QAction(tr("Letter&box"), this);
	letterboxAction->setShortcut(tr("Ctrl+B"));
	connect(letterboxAction, SIGNAL(triggered()), this, SLOT(letterbox()));

	m_generateAction = new QAction(tr("Generate word &list"), this);
	m_generateAction->setShortcut(tr("Ctrl+L"));
	connect(m_generateAction, SIGNAL(triggered()), this, SLOT(generateList()));

	//// Settings

	m_preferencesAction = new QAction(tr("&Configure Quackle..."), this);
	connect(m_preferencesAction, SIGNAL(triggered()), this, SLOT(showConfigDialog()));

	//// Help

	QAction *hintsAction = new QAction(tr("&Helpful Hints"), this);
	connect(hintsAction, SIGNAL(triggered()), this, SLOT(hints()));

	QAction *aboutAction = new QAction(tr("&About Quackle"), this);
	connect(aboutAction, SIGNAL(triggered()), this, SLOT(about()));

	QAction *aboutQtAction = new QAction(tr("About &Qt"), this);
	connect(aboutQtAction, SIGNAL(triggered()), qApp, SLOT(aboutQt()));

	//// Menus

	QMenu *game = menuBar()->addMenu(tr("&Game"));

	game->addAction(m_newAction);
	game->addAction(m_openAction);
	game->addSeparator();
	game->addAction(m_saveAction);
	game->addAction(m_saveAsAction);
	game->addSeparator();

	if (!enableLetterbox)
	{
		game->addAction(m_generateAction);
	}

	game->addAction(m_showAsciiAction);
	game->addSeparator();
	game->addAction(quitAction);

	QMenu *move = menuBar()->addMenu(tr("Mo&ve"));

	move->addAction(m_kibitzAction);
	move->addAction(m_kibitzFiftyAction);
	move->addAction(m_kibitzAllAction);

	move->addSeparator();

	for (const auto& it : m_kibitzAsActions->actions())
		move->addAction(it);

	move->addSeparator();

	move->addAction(m_passAction);
	move->addAction(m_overdrawAction);
	move->addSeparator();

	if (enableCommitAction)
		move->addAction(m_commitAction);
	move->addAction(m_commitTopChoiceAction);

	QMenu *go = menuBar()->addMenu(tr("&Go"));
	go->addAction(m_firstPositionAction);
	go->addAction(m_nextPositionAction);
	go->addAction(m_previousPositionAction);

	QMenu *reports = menuBar()->addMenu(tr("Re&ports"));

	for (const auto& it : m_reportAsActions->actions())
		reports->addAction(it);

	reports->addSeparator();

	m_htmlReportAction = new QAction(tr("Quick full-game HTML report"), this);
	m_htmlReportAction->setEnabled(false);
	connect(m_htmlReportAction, SIGNAL(triggered()), this, SLOT(htmlReport()));
	reports->addAction(m_htmlReportAction);

#ifdef ENABLE_GRAPHICAL_REPORT
	m_graphicalReportAction = new QAction(tr("Slow full-game HTML report with images"), this);
	m_graphicalReportAction->setEnabled(false);
	connect(m_graphicalReportAction, SIGNAL(triggered()), this, SLOT(graphicalReport()));
	reports->addAction(m_graphicalReportAction);
#endif

	QMenu *simulation = menuBar()->addMenu(tr("Si&mulation"));
	simulation->addAction(m_simulateAction);
	simulation->addAction(m_simulateDetailsAction);
	simulation->addAction(m_simulateClearAction);

	if (enableLetterbox)
	{
		QMenu *study = menuBar()->addMenu(tr("Stud&y"));
		study->addAction(m_generateAction);
		study->addAction(letterboxAction);
	}

	QMenu *settings = menuBar()->addMenu(tr("&Settings"));
	settings->addAction(m_preferencesAction);

	menuBar()->addSeparator();

	QMenu *help = menuBar()->addMenu(tr("&Help"));
	help->addAction(hintsAction);
	help->addSeparator();
	help->addAction(aboutAction);
	help->addAction(aboutQtAction);

	// file toolbar
	QToolBar *fileBar = addToolBar(tr("File"));
	fileBar->setObjectName("file-toolbar");
	fileBar->addAction(m_newAction);
	fileBar->addAction(m_generateAction);

	// move toolbar
	QToolBar *moveBar = addToolBar(tr("Move"));
	moveBar->setObjectName("move-toolbar");

	if (enableCommitAction && putCommitActionOnToolbar)
	{
		moveBar->addAction(m_commitAction);
		moveBar->addSeparator();
	}

	moveBar->addAction(m_kibitzAction);
	moveBar->addAction(m_nextPositionAction);
	moveBar->addAction(m_kibitzAsActions->actions().front());
	moveBar->addAction(m_simulateAction);

	// study toolbar
	if (enableLetterbox)
	{
		QToolBar *studyBar = addToolBar(tr("Study"));
		studyBar->setObjectName("study-toolbar");
		studyBar->addAction(letterboxAction);
	}
}

void TopLevel::createWidgets()
{
	m_splitter = new QSplitter(Qt::Horizontal, this);

	setCentralWidget(m_splitter);

	QWidget *leftSide = new QWidget;
	m_leftSideLayout = new QVBoxLayout(leftSide);
	Geometry::setupFramedLayout(m_leftSideLayout);

	m_dashboard = new Dashboard;
	plugIntoHistoryMatrix(m_dashboard);

	m_choicesWidget = new QWidget;
	QVBoxLayout *choicesLayout = new QVBoxLayout(m_choicesWidget);
	Geometry::setupFramedLayout(choicesLayout);

	m_frameWidget = new QFrame;
	m_frameWidget->setFrameShape(QFrame::HLine);
	m_frameWidget->setFrameShadow(QFrame::Sunken);

	m_simulatorWidget = new QGroupBox(tr("Simulation"));
	m_simulatorWidget->setFlat(true);
	QVBoxLayout *simulatorLayout = new QVBoxLayout(m_simulatorWidget);
	Geometry::setupInnerLayout(simulatorLayout);

	QHBoxLayout *plyLayout = new QHBoxLayout;
	Geometry::setupInnerLayout(plyLayout);

	m_pliesCombo = new QComboBox;
	QStringList plyOptions;

	for (int i = 1; i <= m_pliesToOffer; ++i)
		plyOptions.push_back(QString::number(i));

	plyOptions.push_back(tr("Many"));

	m_pliesCombo->addItems(plyOptions);
	connect(m_pliesCombo, SIGNAL(activated(int)), this, SLOT(pliesSet(int)));

	QLabel *plyLabel = new QLabel(tr("p&lies"));
	plyLabel->setBuddy(m_pliesCombo);

	m_ignoreOpposCheck = new QCheckBox(tr("oppos pass"));
	connect(m_ignoreOpposCheck, SIGNAL(stateChanged(int)), this, SLOT(ignoreOpposChanged()));

	m_showDetailsButton = new QPushButton(tr("&Details"));
	connect(m_showDetailsButton, SIGNAL(clicked()), this, SLOT(showSimulationDetails()));

	plyLayout->addWidget(m_pliesCombo);
	plyLayout->addWidget(plyLabel);
	plyLayout->addWidget(m_ignoreOpposCheck);
	plyLayout->addStretch();
	plyLayout->addWidget(m_showDetailsButton);
	simulatorLayout->addLayout(plyLayout);

	m_partialOppoRackEnable = new QGroupBox(tr("Specify partial oppo rack"));
	m_partialOppoRackEnable->setCheckable(true);
	m_partialOppoRackEnable->setFlat(true);
	connect(m_partialOppoRackEnable, SIGNAL(toggled(bool)), this, SLOT(partialOppoRackEnabled(bool)));

	QHBoxLayout *partialOppoRackLayout = new QHBoxLayout(m_partialOppoRackEnable);
	Geometry::setupInnerLayout(partialOppoRackLayout);

	m_partialOppoRackEdit = new QLineEdit;
	connect(m_partialOppoRackEdit, SIGNAL(textEdited(const QString &)), this, SLOT(partialOppoRackChanged()));
	
	partialOppoRackLayout->addWidget(m_partialOppoRackEdit);
	simulatorLayout->addWidget(m_partialOppoRackEnable);

	m_logfileEnable = new QGroupBox(tr("Log sim to file"));
	m_logfileEnable->setCheckable(true);
	m_logfileEnable->setFlat(true);
	connect(m_logfileEnable, SIGNAL(toggled(bool)), this, SLOT(logfileEnabled(bool)));

	QHBoxLayout *logfileLayout = new QHBoxLayout(m_logfileEnable);
	Geometry::setupInnerLayout(logfileLayout);

	m_logfileEdit = new QLineEdit;
	connect(m_logfileEdit, SIGNAL(editingFinished()), this, SLOT(logfileChanged()));

	m_logfileChooser = new QPushButton(tr("Browse..."));
	connect(m_logfileChooser, SIGNAL(clicked()), this, SLOT(chooseLogfile()));

	logfileLayout->addWidget(m_logfileEdit);
	logfileLayout->addWidget(m_logfileChooser);
	simulatorLayout->addWidget(m_logfileEnable);

	m_noteEditor = new NoteEditor;
	plugIntoMatrix(m_noteEditor);
	plugIntoPositionMatrix(m_noteEditor);

	m_moveBox = new MoveBox;
	plugIntoMatrix(m_moveBox);
	plugIntoPositionMatrix(m_moveBox);
	plugIntoMoveMatrix(m_moveBox);

	choicesLayout->addWidget(m_moveBox, 3);
	choicesLayout->addWidget(m_noteEditor, 1);
	choicesLayout->addWidget(m_frameWidget, 1);
	choicesLayout->addWidget(m_simulatorWidget, 1);

	m_history = new History;
	plugIntoHistoryMatrix(m_history);

	m_tabWidget = new QTabWidget;
	m_tabWidget->addTab(m_history, tr("Histor&y"));
	m_tabWidget->addTab(m_choicesWidget, tr("&Choices"));
	m_tabWidget->addTab(m_settings, tr("Se&ttings"));

	GraphicalFactory factory;
	m_brb = new BRB(&factory);
	plugIntoMatrix(m_brb);
	plugIntoPositionMatrix(m_brb);

	m_leftSideLayout->addWidget(m_dashboard);
	m_leftSideLayout->addWidget(m_tabWidget);

	m_splitter->addWidget(leftSide);
	m_splitter->addWidget(m_brb);

	m_splitter->setStretchFactor(1, 4);

	m_listerDialog = new ListerDialog(this, "quackle", tr("Quackle"),  ListerDialog::NothingToReturn);
}

void TopLevel::switchToTab(TabIndex index)
{
	m_tabWidget->setCurrentIndex(index);
}

QString TopLevel::playerString() const
{
	QString ret;

	if (!m_game->hasPositions())
		return tr("No game");

	Quackle::PlayerList players = m_game->currentPosition().endgameAdjustedScores();
	if (players.empty())
		return tr("No game");

	bool begin = true;
	int i = 0;
	const size_t maximumIndex = players.size() - 1;
	const Quackle::PlayerList::const_iterator end(players.end());
	for (Quackle::PlayerList::const_iterator it = players.begin(); it != end; ++it, ++i)
	{
		if (!begin)
			ret += tr(", ");

		if (!begin && i == maximumIndex && i >= 2)
			ret += tr("and ");

		ret += tr("%1 (score %2)").arg(QuackleIO::Util::uvStringToQString((*it).name())).arg((*it).score());
		begin = false;
	}

	return ret;
}

void TopLevel::showAscii()
{
	if (!m_game->hasPositions())
		return;

	QString text;

	// This is now usurped into Reporter's output.
	//text += tr("Players: %1").arg(playerString()) + "\n\n";

	UVString report;
	Quackle::Reporter::reportPosition(m_game->currentPosition(), 0, &report);
	text += QuackleIO::Util::uvStringToQString(report);

	int result = QMessageBox::question(this, tr("Plaintext board - Quackle"), QString("<pre>%1</pre>").arg(text), tr("&Write to file"), tr("Copy to clip&board"), tr("&OK"), 1);

	switch (result)
	{
	case 0:
	{
		QString filename = QFileDialog::getSaveFileName(this, tr("Choose file to print plaintext board to - Quackle"), getInitialDirectory());

		if (!filename.isEmpty())
		{
			setInitialDirectory(filename);
			writeAsciiToFile(text, filename);
		}
		break;
	}

	case 1:
		copyToClipboard(text);
		break;

	case 2:
		break;
	}
}

void TopLevel::copyToClipboard(const QString &text)
{
	QApplication::clipboard()->setText(text, QClipboard::Clipboard);
	QApplication::clipboard()->setText(text, QClipboard::Selection);

	statusMessage(tr("Copied to clipboard."));
}

void TopLevel::writeAsciiToFile(const QString &text, const QString &filename)
{
	QFile file(filename);
	 
	if (!file.open(QIODevice::WriteOnly | QIODevice::Text))    
	{        
		QMessageBox::critical(this, tr("Error Writing File - Quackle"), dialogText(tr("Could not open %1 for writing.")).arg(file.fileName()));        
		return;    
	}

	QTextStream stream(&file);
	SET_QTEXTSTREAM_TO_UTF8(stream);
	stream << text << "\n";

	file.close();

	statusMessage(tr("`%1' written.").arg(filename));
}

void TopLevel::print()
{
	pause(true);

	QString filename = QFileDialog::getSaveFileName(this, tr("Choose file to print to - Quackle"), m_filename + ".html");

	if (filename.isEmpty())
		return;

	QFile file(filename);
	 
	if (!file.open(QIODevice::WriteOnly | QIODevice::Text))    
	{        
		QMessageBox::critical(this, tr("Error Writing File - Quackle"), dialogText(tr("Could not open %1 for writing.")).arg(file.fileName()));
		return;    
	}

	QTextStream stream(&file);
	SET_QTEXTSTREAM_TO_UTF8(stream);
	//stream << printer.html() << "\n";

	file.close();

	statusMessage(tr("`%1' written.").arg(filename));
}

void TopLevel::firstTimeRun()
{
	switchToTab(SettingsTabIndex);
	QMessageBox::information(this, tr("Welcome - Quackle"), dialogText(tr("Welcome to Quackle! To get started, configure a board by clicking Add Board in the Settings tab, add some bonus squares, and then start a new game. Also check out the Helpful Hints from the Help menu, and keep your eye out for the aidant messages in the status bar at the bottom of the window.")));
}

void TopLevel::about()
{
	QString aboutText = tr(
"<p><b>Quackle</b> 1.0.4.1 is a crossword game playing, analysis, and study tool. Visit the Quackle homepage at <tt><a href=\"http://quackle.org\">http://quackle.org</a></tt> for more information.</p>"
"<p>Quackle was written by Jason Katz-Brown, John O'Laughlin, John Fultz, Matt Liberty, and Anand Buddhdev. We thank the anonymous donor who made this software free.</p>"
"<p>Copyright 2005-2019 by</p>"
"<ul>"
"<li>Jason Katz-Brown &lt;jasonkatzbrown@gmail.com&gt;</li>"
"<li>John O'Laughlin &lt;olaughlin@gmail.com&gt;</li>"
"<li>John Fultz &lt;jfultz@wolfram.com&gt;</li>"
"</ul>"
"<p>Quackle is free, open-source software licensed under the terms of the GNU General Public License Version 3. See</p>"
"<p><tt><a href=\"http://quackle.org/LICENSE\">http://quackle.org/LICENSE</a></tt></p>"
"<p>Dictionary copyrights</p><ul>"
);

	FILE* file = fopen(QUACKLE_DATAMANAGER->makeDataFilename("lexica", "copyrights.txt", false).c_str(), "r");
	if (file)
	{
		QTextStream strm(file);
		SET_QTEXTSTREAM_TO_UTF8(strm);
		QString line = strm.readLine();
		while (!line.isNull())
		{
			qsizetype startPos = line.indexOf(':');
			if (startPos != -1 && startPos + 1 < line.size())
			{
				line = line.mid(startPos + 1);
				qsizetype endPos = line.indexOf(':');
				line = line.mid(0, endPos);
				// Only include lines with a copyright (the word or the symbol) in them
				if (line.indexOf("copyright", 0, Qt::CaseInsensitive) != -1 || line.indexOf(QChar(0xA9)) != -1)
					aboutText += "<li>" + line + "</li>";
			}
			line = strm.readLine();
		}
		fclose(file);
		aboutText += "</ul>";
	}
	QMessageBox::about(this, tr("About Quackle 1.0.4"), dialogText(aboutText));
}

void TopLevel::hints()
{
	QMessageBox::information(this, tr("Helpful Hints - Quackle"), dialogText(tr(
"<ul>"
"<li>Press Shift-Enter after typing your word on the board to enter and commit your move quickly.</li>"
"<li>Double-click at any time during a game on any item in the History table to analyze that position. If you then commit a play, you will restart the game from that point and future plays will be lost.</li>"
"<li>To analyze a real-life game, start a two-player game with two \"Human With Unknown Rack\" players. For one player, for each turn set the rack to the rack you had in the game and then analyze the position and commit the play that you made in real life. For the other player, commit your oppo's real-life plays.</li>"
"<li>Stop simulations by unchecking \"Simulate\" in the Move menu. Sims can be stopped and restarted without losing their state, and sims of different plies can be combined. Check out the sim details during a simulation by choosing \"Show simulation details\" from the Move menu!</li>"
"</ul>"
"<p>Have fun using Quackle. We'd love your help developing it, especially if you can code, but we like suggestions too! Please join the Quackle Yahoo! group at</p>"
"<p><tt>http://games.groups.yahoo.com/group/quackle/</tt></p>"
)));
}

void TopLevel::showConfigDialog()
{
	ConfigDialog dialog;
	connect(&dialog, SIGNAL(refreshViews()), this, SLOT(updateAllViews()));
	dialog.exec();
}

void TopLevel::saveSettings()
{
	CustomQSettings settings;

	settings.setValue("quackle/initial-directory", m_initialDirectory);
	settings.setValue("quackle/window-size", size());
	settings.setValue("quackle/splitter-sizes", m_splitter->saveState());
	settings.setValue("quackle/window-state", saveState(0));
	settings.setValue("quackle/plies", m_plies);
	settings.setValue("quackle/ignoreoppos", m_ignoreOpposCheck->isChecked());
	settings.setValue("quackle/logfileEnabled", isLogfileEnabled());
	settings.setValue("quackle/logfile", userSpecifiedLogfile());
	settings.setValue("quackle/partialopporackenabled", isPartialOppoRackEnabled());
	settings.setValue("quackle/partialopporack", userSpecifiedPartialOppoRack());

	settings.setValue("quackle/hasBeenRun", true);

	QuackerSettings::self()->writeSettings();
}

void TopLevel::loadSettings()
{
	CustomQSettings settings;

	m_initialDirectory = settings.value("quackle/initial-directory", QString("")).toString();
	resize(settings.value("quackle/window-size", QSize(800, 600)).toSize());

	if (settings.contains("quackle/splitter-sizes"))
		m_splitter->restoreState(settings.value("quackle/splitter-sizes").toByteArray());

	if (settings.contains("quackle/window-state"))
		restoreState(settings.value("quackle/window-state").toByteArray(), 0);

	m_plies = settings.value("quackle/plies", 2).toInt();
	updatePliesCombo();

	m_ignoreOpposCheck->setChecked(settings.value("quackle/ignoreoppos", false).toBool());

	m_logfileEdit->setText(settings.value("quackle/logfile", QString("")).toString());
	const bool logfileEnabled = settings.value("quackle/logfileEnabled", false).toBool();
	m_logfileEnable->setChecked(logfileEnabled);
	setLogfileEnabled(logfileEnabled);

	m_partialOppoRackEdit->setText(settings.value("quackle/partialopporack", QString("")).toString());
	const bool partialOppoRackEnabled = settings.value("quackle/partialopporackenabled", false).toBool();
	m_partialOppoRackEnable->setChecked(partialOppoRackEnabled);
	setPartialOppoRackEnabled(partialOppoRackEnabled);

	QuackerSettings::self()->readSettings();
}

QString TopLevel::dialogText(const QString &text)
{
	return QString("<html>%1</html>").arg(text);
}

void TopLevel::startBirthday()
{
	if (!m_game || !m_game->hasPositions())
	{
		return;
	}

	// Happy Birthday, Ong Suanne.

	bool isBirthday = false;
	const Quackle::PlayerList &players = m_game->currentPosition().players();
	for (Quackle::PlayerList::const_iterator it = players.begin(); it != players.end(); ++it)
	{
		if ((*it).name() == "zorbonauts")
		{
			isBirthday = true;
			break;
		}
	}

	if (!isBirthday)
	{
		m_birthdayTimer->stop();
		return;
	}
	
	m_birthdayIndex = 0;
	m_birthdayTimer->start(800);
}

void TopLevel::birthdayBash()
{
	birthdayGram(m_birthdayIndex, /* off */ false);
	++m_birthdayIndex;
	if (m_birthdayIndex >= 11)
		m_birthdayIndex = 0;
	birthdayGram(m_birthdayIndex, /* on */ true);
}

void TopLevel::birthdayGram(int index, bool on)
{
	switch (index)
	{
	case 0:
		setCaption(tr("HURRRRRRRRRRRRRRRRRRRRRRRRR"));
		break;
	case 1:
		m_newAction->setIconText(on? tr("HAPPY") : tr("New game"));
		break;
	case 2:
		m_generateAction->setIconText(on? tr("BIRTHDAY") : tr("Generate word list"));
		break;
	case 3:
		m_kibitzAction->setIconText(on? tr("ONG") : tr("Generate choices"));
		break;
	case 4:
		m_nextPositionAction->setIconText(on? tr("! ! ! ! ! ! ! ! !") : tr("Forward"));
		break;
	case 5:
		m_kibitzAsActions->actions().front()->setIconText(on? tr("SUANNE") : tr("Ask Championship Player"));
		break;
	case 6:
		m_simulateAction->setIconText(on? tr("! ! ! ! ! ! ! ! !") : tr("Simulate"));
		break;
	case 7:
		m_tabWidget->setTabText(0, on? tr("HAVE A") : tr("Histor&y"));
		break;
	case 8:
		m_tabWidget->setTabText(1, on? tr("SUPERPIMP") : tr("&Choices"));
		break;
	case 9:
		m_tabWidget->setTabText(2, on? tr("YEAR") : tr("Se&ttings"));
		break;
	case 10:
		statusMessage(on? tr("you're awesome don't ever change") : tr(""));
		break;
	default:
		break;
	}
}

//////////////

KibitzerListener::KibitzerListener(Quackle::ComputerPlayer *computerPlayer, QObject *parent)
	: QObject(parent), m_computerPlayer(computerPlayer), m_slowPlayerWarningTriggered(false)
{
}

void KibitzerListener::kibitzTriggered()
{
	if (slownessCheck())
		emit kibitzAs(m_computerPlayer);
}

void KibitzerListener::reportTriggered()
{
	if (slownessCheck())
		emit reportAs(m_computerPlayer);
}

bool KibitzerListener::slownessCheck()
{
	return true;
}

