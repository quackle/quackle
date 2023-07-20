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

#ifndef QUACKER_QUACKER_H
#define QUACKER_QUACKER_H

// #define ENABLE_GRAPHICAL_REPORT

#include <QMainWindow>
#include <QMap>
#include <QMessageBox>

#include <datamanager.h>
#include "oppothread.h"
#include <sim.h>

#include <quackleio/dictimplementation.h>

class QAction;
class QActionGroup;
class QComboBox;
class QCheckBox;
class QFrame;
class QGroupBox;
class QLineEdit;
class QPushButton;
class QSplitter;
class QTabWidget;
class QTimer;
class QVBoxLayout;

class OppoThreadProgressBar;

namespace Quackle
{
	class ComputerPlayer;
	class Game;
	class GamePosition;
	class History;
	class HistoryLocation;
	class Move;
	class Rack;
}

namespace QuackleIO
{
	class Logania;
}

class BaseView;
class HistoryView;
class Letterbox;
class ListerDialog;
class QuackerSettings;
class Settings;
class SimViewer;
class View;

class TopLevel : public QMainWindow
{
Q_OBJECT

public:
	TopLevel(QWidget *parent = 0);
	~TopLevel();

	void closeEvent(QCloseEvent *closeEvent);

	// returns text wrapped in an HTML tag to ensure dialog wrapping
	static QString dialogText(const QString &text);

public slots:
	void open();
	void newGame();

	// like loadFile, but set the filename, initial directory,
	// and other things
	void openFile(const QString &filename);

	// actually loads the file
	void loadFile(const QString &filename);

	void save();
	void saveAs();
	void writeFile(const QString &filename);
	void generateList();
	void letterbox();

	void kibitz();
	void kibitz(int numberOfPlays, Quackle::ComputerPlayer *computerPlayer = 0);
	void kibitzFifty();
	void kibitzAll();
	void kibitzAs(Quackle::ComputerPlayer *computerPlayer);

	void firstPosition();
	void nextPosition();
	void previousPosition();

	void reportAs(Quackle::ComputerPlayer *computerPlayer);
	void htmlReport();
	void graphicalReport();
	void commitTopChoice();
	void simulate(bool startSimulation);
	void simulateToggled(bool startSimulation);
	void clearSimulationResults();

	void showAscii();
	void writeAsciiToFile(const QString &text, const QString &filename);
	void copyToClipboard(const QString &text);
	void print();
	void firstTimeRun();
	void about();
	void hints();

	void showConfigDialog();

	// set up our game object based on a shuffled playerList
	void initializeGame(const Quackle::PlayerList *players);

	// call timerControl, and tell user about it
	void pause(bool paused);

protected slots:
	// final set up of initial data structures
	// and an example game
	void finishInitialization();

	// say hi
	void introduceToUser();

	// prepares current position and spawns computer player thread
	// if necessary then sends orders to update the UI
	void advanceGame();

	// asks oppo thread to find best move
	void startOppoThread();

	// called by oppo thread when it's done
	void computerPlayerDone();

	// called by player thread when it's done
	void kibitzThreadFinished();

	// called by a oppo/player thread when it has a status update
	void playerFractionDone(double fraction, OppoThread *thread);

	// update both positional and history views,
	// then wait for human to make a play
	void showToHuman();

	// use this to control timer!
	void timerControl(bool paused);

	// commit candidate play then advance the game
	void commit();

	// add a pass candidate
	void pass();

	// handle an overdraw
	void overdraw();

	void statusMessage(const QString &message);

	// set game's candidate to move and update views
	void setCandidateMove(const Quackle::Move *move, bool *carryOnPtr = nullptr);
	void removeCandidateMoves(const Quackle::MoveList *moves);

	// set current player's rack and update views
	void setRack(const Quackle::Rack &rack);

	// set current position's explanatory note
	void setNote(const UVString &note);

	// set history location to view
	void goToHistoryLocation(const Quackle::HistoryLocation *location);

	// stop simulation, opponenent thread, etc
	void stopEverything();

	// update all views (usually because of a settings change)
	void updateAllViews();

	// update *positional* views - emit positionChanged
	void updatePositionViews();

	// updates move views from either simulation results if available
	// or kibitzed moves
	void updateMoveViews();

	// update history views when a new position is added
	void updateHistoryViews();

	void setCaption(const QString &text = QString());
	void setModified(bool modified);

	// main timer
	void timeout();

	// simulation timer
	void incrementSimulation();
	void updateSimViews();

	// simulator settings:
	void pliesSet(int plyIndex);
	void ignoreOpposChanged();
	void updatePliesCombo();
	void logfileEnabled(bool on);
	void logfileChanged();
	void chooseLogfile();
	void partialOppoRackEnabled(bool on);
	void partialOppoRackChanged();
	void showSimulationDetails();

	// Birthday
	void startBirthday();
	void birthdayBash();
	void birthdayGram(int index, bool on);

signals:
	// emitted when views (eg board) should update based on the
	// current position (includes board information, current candidate play
	// that should be shown)
	void positionChanged(const Quackle::GamePosition *position);

	void movesChanged(const Quackle::MoveList *moves);

	// emitted when views of history must update
	void historyChanged(const Quackle::History &history);

protected:
	Quackle::DataManager m_dataManager;
	Quackle::Game *m_game;
	Quackle::Simulator *m_simulator;

private:
	void saveSettings();
	void loadSettings();

	// returns 0 for save, 1 for discard, 2 for cancel
	QMessageBox::StandardButton askToSave();

	// returns true if user wants to make play anyway
	bool askToCarryOn(const QString &text);

	// used to know when to update UI when performing heavy calculation
	bool m_initializationChuu;

	// are dictionaries etc properly set up?
	bool setupCheck();

	// hook up signals and slots associated with a view
	void plugIntoBaseMatrix(BaseView *view);
	void plugIntoMatrix(View *view);
	void plugIntoPositionMatrix(View *view);
	void plugIntoMoveMatrix(View *view);
	void plugIntoHistoryMatrix(HistoryView *view);

	void parseCommandLineOptions();

	bool isPlayerOnTurnComputer() const;
	bool isCommitAllowed() const;
	bool shouldOutcraftyCurrentPlayer() const;
	void startOutcraftyingCurrentPlayer();
	void stopOutcraftyingCurrentPlayer();

	// Used to sanitize a move for a player with unknown racks.
	// Changes exchanges to exchange tiles on the current rack.
	// If move is a place, changes current player's rack to something
	// that includes the used tiles.
	// Returns true if the validization was successful.
	bool validifyMove(Quackle::Move &move);

	void kibitzFinished();

	OppoThreadProgressBar *createProgressBarForThread(OppoThread *thread);
	void removeProgressIndicators();
	void removeProgressIndicator(OppoThread *thread);
	OppoThreadProgressBar *progressBarOfThread(OppoThread *thread);

	// generic game title
	QString gameTitle();

	UVString m_firstPlayerName;
	QString playerString() const;

	QList<OppoThread *> m_oppoThreads;
	QList<OppoThread *> m_otherOppoThreads;

	QTimer *m_timer;
	QTimer *m_simulationTimer;

	QMap<OppoThread *, OppoThreadProgressBar *> m_progressIndicators;

	View *m_brb;

	QSplitter *m_splitter;

	QVBoxLayout *m_leftSideLayout;

	enum TabIndex { HistoryTabIndex = 0, ChoicesTabIndex = 1, SettingsTabIndex = 2};
	QTabWidget *m_tabWidget;

	HistoryView *m_history;
	HistoryView *m_dashboard;

	QWidget *m_choicesWidget;
	View *m_moveBox;
	View *m_noteEditor;
	QFrame *m_frameWidget;

	Settings *m_settings;

	ListerDialog *m_listerDialog;
	void updateListerDialogWithRack();

	Letterbox *m_letterbox;
	QuackerSettings *m_quackerSettings;

// encapsulated simulator settings widget
	QGroupBox *m_simulatorWidget;
	QPushButton *m_showDetailsButton;
	QLineEdit *m_logfileEdit;
	QGroupBox *m_logfileEnable;
	QPushButton *m_logfileChooser;
	QLineEdit *m_partialOppoRackEdit;
	QGroupBox *m_partialOppoRackEnable;
	QCheckBox *m_ignoreOpposCheck;

	static const int m_pliesToOffer = 6;
	QComboBox *m_pliesCombo;

	SimViewer *m_simViewer;

	void setLogfileEnabled(bool enabled);
	bool isLogfileEnabled() const;

	// the value of the file-choosing lineedit
	QString userSpecifiedLogfile() const;

	// what user specified, or empty string if logging disabled
	QString logfile() const;

	void setPartialOppoRackEnabled(bool enabled);
	bool isPartialOppoRackEnabled() const;

	// the value of the partial oppo rack-choosing lineedit
	QString userSpecifiedPartialOppoRack() const;

	// what user specified, or empty string if logging disabled
	QString partialOppoRack() const;

	int m_plies;
// end encapsulation, hah

	QString m_filename;
	QuackleIO::Logania *m_logania;
	bool m_modified;
	QString m_ourCaption;

	QString getInitialDirectory() const;
	void setInitialDirectory(const QString &filename);
	QString m_initialDirectory;
	QString gameFileFilters() const;
	QString defaultGameFileFilter() const;

	QActionGroup *m_kibitzAsActions;
	QActionGroup *m_reportAsActions;
	QAction *m_htmlReportAction;
	QAction *m_showAsciiAction;
	QAction *m_generateAction;
	QAction *m_newAction;
	QAction *m_openAction;
	QAction *m_saveAction;
	QAction *m_saveAsAction;
	QAction *m_pauseAction;
	QAction *m_firstPositionAction;
	QAction *m_nextPositionAction;
	QAction *m_previousPositionAction;
	QAction *m_commitAction;
	QAction *m_passAction;
	QAction *m_overdrawAction;
	QAction *m_kibitzAction;
	QAction *m_kibitzFiftyAction;
	QAction *m_kibitzAllAction;
	QAction *m_commitTopChoiceAction;
	QAction *m_simulateAction;
	QAction *m_simulateClearAction;
	QAction *m_simulateDetailsAction;
	QAction *m_preferencesAction;

#ifdef ENABLE_GRAPHICAL_REPORT
	QAction *m_graphicalReportAction;
#endif

	// make sure moves in our simulator match those in current position
	void ensureUpToDateSimulatorMoveList();

	void createMenu();
	void createWidgets();
	void switchToTab(TabIndex index);

	// Birthday
	QTimer *m_birthdayTimer;
	int m_birthdayIndex;
};

class KibitzerListener : public QObject
{
Q_OBJECT

public:
	KibitzerListener(Quackle::ComputerPlayer *computerPlayer, QObject *parent);

public slots:
	void kibitzTriggered();
	void reportTriggered();

signals:
	void kibitzAs(Quackle::ComputerPlayer *computerPlayer);
	void reportAs(Quackle::ComputerPlayer *computerPlayer);

private:
	// returns true if the action should still be carried out despite slowness
	bool slownessCheck();

	Quackle::ComputerPlayer *m_computerPlayer;
	bool m_slowPlayerWarningTriggered;
};

#endif
