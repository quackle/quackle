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

#ifndef QUACKER_NEWGAME_H
#define QUACKER_NEWGAME_H

#include <QDialog>
#include <QMap>

#include <player.h>
#include <playerlist.h>

class QComboBox;
class QGroupBox;
class QLineEdit;
class QTabWidget;
class QTreeWidget;
class QTreeWidgetItem;
class QPushButton;
class QSettings;
class PlayerTab;

namespace Quackle
{
	class ComputerPlayer;
}

class NewGameDialog : public QDialog
{
Q_OBJECT

public:
	NewGameDialog(QWidget *parent = 0);

	Quackle::PlayerList players() const;

public slots:
	void accept();

protected:
	void saveSettings();
	void loadSettings();

private:
	QTabWidget *m_tabs;
	PlayerTab *m_playerTab;
};

class PlayerTab : public QWidget 
{
Q_OBJECT

public:
	PlayerTab(QWidget *parent = 0);

	Quackle::PlayerList players() const;

public slots:
	void saveSettings();

private slots:
	void addPlayer();
	void removePlayer();
	void selectionChanged();
	void playerEdited();
	void populatePlayers();

private:
	bool hasSelection();

	void addPlayer(const Quackle::Player &player);
	void setItem(QTreeWidgetItem *item, const Quackle::Player &player);

	QString stringForPlayer(const Quackle::Player &player);
	void updatePlayerFromString(const QString &typeString, Quackle::Player &player);

	Quackle::ComputerPlayer *defaultComputerPlayer() const;

	enum PlayerTreeRows { PlayerName = 0, PlayerType = 1 };

	Quackle::Player getLastPlayer();

	QTreeWidget *m_playersTreeWidget;
	QPushButton *m_addPlayerButton;
	QPushButton *m_removePlayerButton;

	QGroupBox *m_editGroup;
	QLineEdit *m_nameEdit;
	QComboBox *m_playerType;
	bool m_changingEditorManually;

	QMap<Quackle::Player, QTreeWidgetItem *> m_playerMap;
};

#endif
