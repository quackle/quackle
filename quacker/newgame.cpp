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

#include <QtWidgets>

#include <computerplayer.h>
#include <datamanager.h>
#include <quackleio/util.h>

#include "newgame.h"
#include "customqsettings.h"

using namespace std;

NewGameDialog::NewGameDialog(QWidget *parent)
	: QDialog(parent)
{
	m_tabs = new QTabWidget;

	m_playerTab = new PlayerTab;
	m_tabs->addTab(m_playerTab, tr("&Players"));

	QPushButton *okButton = new QPushButton(tr("&OK"));
	QPushButton *cancelButton = new QPushButton(tr("&Cancel"));

	connect(okButton, SIGNAL(clicked()), this, SLOT(accept()));
	connect(cancelButton, SIGNAL(clicked()), this, SLOT(reject()));

	QHBoxLayout *buttonLayout = new QHBoxLayout;
	buttonLayout->addStretch(1);
	buttonLayout->addWidget(okButton);
	buttonLayout->addWidget(cancelButton);

	QVBoxLayout *topLayout = new QVBoxLayout;
	topLayout->addWidget(m_tabs);
	topLayout->addLayout(buttonLayout);
	setLayout(topLayout);

	loadSettings();

	setWindowTitle(tr("New Game - Quackle"));
	okButton->setDefault(true);
}

Quackle::PlayerList NewGameDialog::players() const
{
	return m_playerTab->players();
}

void NewGameDialog::accept()
{
	saveSettings();
	m_playerTab->applyThreadSettings();
	QDialog::accept();
}

void NewGameDialog::saveSettings()
{
	m_playerTab->saveSettings();
}

void NewGameDialog::loadSettings() {}

/////////////

PlayerTab::PlayerTab(QWidget *parent)
	: QWidget(parent), m_changingEditorManually(false), m_editedPlayerId(-1)
{
	m_addPlayerButton = new QPushButton(tr("&Add New Player"));
	m_removePlayerButton = new QPushButton(tr("&Remove Player"));

	connect(m_addPlayerButton, SIGNAL(clicked()), this, SLOT(addPlayer()));
	connect(m_removePlayerButton, SIGNAL(clicked()), this, SLOT(removePlayer()));

	QHBoxLayout *buttonLayout = new QHBoxLayout;
	buttonLayout->addWidget(m_addPlayerButton);
	buttonLayout->addWidget(m_removePlayerButton);

	m_playersTreeWidget = new QTreeWidget;

	// probably too confusing to be able to select multiple players at oncet?
	// all functionality for it is implemented wrt removing multiple players at oncet
	// m_playersTreeWidget->setSelectionMode(QTreeWidget::ExtendedSelection);

	connect(m_playersTreeWidget, SIGNAL(itemSelectionChanged()), this, SLOT(selectionChanged()));
	QStringList headers;
	headers << tr("Name") << tr("Controller");
	m_playersTreeWidget->setHeaderLabels(headers);

	m_editGroup = new QGroupBox(tr("Player &Information"));
	QVBoxLayout *editLayout = new QVBoxLayout(m_editGroup);

	QHBoxLayout *identityLayout = new QHBoxLayout;

	m_nameEdit = new QLineEdit;
	connect(m_nameEdit, SIGNAL(textEdited(const QString &)), this, SLOT(playerEdited()));
	identityLayout->addWidget(m_nameEdit);

	m_playerType = new QComboBox;

	QStringList playerTypes(tr("Human"));
	playerTypes.push_back(tr("Human With Unknown Racks"));
	for (Quackle::PlayerList::const_reverse_iterator it = QUACKLE_COMPUTER_PLAYERS.rbegin(); it != QUACKLE_COMPUTER_PLAYERS.rend(); ++it)
	{
		if ((*it).computerPlayer()->isUserVisible())
			playerTypes.push_back(QuackleIO::Util::uvStringToQString((*it).name()));
	}
	m_playerType->addItems(playerTypes);

	connect(m_playerType, SIGNAL(activated(int)), this, SLOT(playerEdited()));
	identityLayout->addWidget(m_playerType);

	editLayout->addLayout(identityLayout);
	editLayout->addWidget(createThreadSettingsWidget());

	QVBoxLayout *topLayout = new QVBoxLayout;
	topLayout->addWidget(m_playersTreeWidget);
	topLayout->addWidget(m_editGroup);
	topLayout->addStretch();
	topLayout->addLayout(buttonLayout);

	setLayout(topLayout);

	QTimer::singleShot(0, this, SLOT(populatePlayers()));
}

QWidget *PlayerTab::createThreadSettingsWidget()
{
	m_threadSettingsWidget = new QWidget;

	QFormLayout *threadLayout = new QFormLayout(m_threadSettingsWidget);
	threadLayout->setContentsMargins(0, 0, 0, 0);
	threadLayout->setFieldGrowthPolicy(QFormLayout::FieldsStayAtSizeHint);

	m_threadQoSCombo = new QComboBox;
	m_threadQoSCombo->addItem(tr("Performance"), static_cast<int>(Quackle::ThreadQoS::Performance));
	m_threadQoSCombo->addItem(tr("Balanced"), static_cast<int>(Quackle::ThreadQoS::Balanced));
	m_threadQoSCombo->addItem(tr("Energy efficient"), static_cast<int>(Quackle::ThreadQoS::EnergyEfficient));
	m_threadQoSCombo->setToolTip(tr("How hard this player's sim threads compete for the machine.\n"
									"Changing this refills the thread count."));
	connect(m_threadQoSCombo, SIGNAL(activated(int)), this, SLOT(threadQoSSet()));

	m_threadCountEdit = new QLineEdit;
	m_threadCountEdit->setMaximumWidth(m_threadCountEdit->fontMetrics().horizontalAdvance(QString(5, QLatin1Char('0'))));
	m_threadCountEdit->setToolTip(tr("Worker threads running this player's sim."));
	connect(m_threadCountEdit, SIGNAL(editingFinished()), this, SLOT(threadCountEdited()));

	m_threadCountDetector = new QPushButton(tr("A&uto-detect"));
	m_threadCountDetector->setToolTip(tr("Fill in the count advised for this machine at the selected priority."));
	connect(m_threadCountDetector, SIGNAL(clicked()), this, SLOT(autoDetectThreadCount()));

	QHBoxLayout *threadCountLayout = new QHBoxLayout;
	threadCountLayout->addWidget(m_threadCountEdit);
	threadCountLayout->addWidget(m_threadCountDetector);
	threadCountLayout->addStretch();

	QLabel *threadQoSLabel = new QLabel(tr("CPU &priority:"));
	threadQoSLabel->setBuddy(m_threadQoSCombo);

	// addRow() only sets a buddy when the field is a widget, and a QLabel keeps
	// the ampersand it can't spend on one.
	QLabel *threadCountLabel = new QLabel(tr("Number of t&hreads:"));
	threadCountLabel->setBuddy(m_threadCountEdit);

	threadLayout->addRow(threadQoSLabel, m_threadQoSCombo);
	threadLayout->addRow(threadCountLabel, threadCountLayout);

	m_threadSettingsWidget->setVisible(false);

	return m_threadSettingsWidget;
}

int PlayerTab::recommendedThreadCount(Quackle::ThreadQoS qos)
{
	return (int)Quackle::Simulator::recommendedThreadCount(qos);
}

Quackle::ThreadQoS PlayerTab::selectedThreadQoS() const
{
	return static_cast<Quackle::ThreadQoS>(m_threadQoSCombo->currentData().toInt());
}

PlayerTab::ThreadSettings &PlayerTab::threadSettings(int playerId)
{
	QMap<int, ThreadSettings>::iterator it = m_threadSettings.find(playerId);

	if (it == m_threadSettings.end())
	{
		ThreadSettings fresh;
		fresh.qos = Quackle::ThreadQoS::Performance;
		fresh.count = recommendedThreadCount(fresh.qos);
		it = m_threadSettings.insert(playerId, fresh);
	}

	return *it;
}

void PlayerTab::showThreadSettings(const Quackle::Player &player)
{
	const bool isComputer = player.type() == Quackle::Player::ComputerPlayerType;
	m_threadSettingsWidget->setVisible(isComputer);

	m_editedPlayerId = isComputer ? player.id() : -1;
	if (!isComputer)
		return;

	const ThreadSettings &settings = threadSettings(player.id());

	const int qosIndex = m_threadQoSCombo->findData(static_cast<int>(settings.qos));
	m_threadQoSCombo->setCurrentIndex(qosIndex == -1 ? 0 : qosIndex);
	m_threadCountEdit->setText(QString::number(settings.count));
}

void PlayerTab::setThreadCount(int count)
{
	m_threadCountEdit->setText(QString::number(count));

	if (m_editedPlayerId >= 0)
		threadSettings(m_editedPlayerId).count = count;
}

void PlayerTab::threadQoSSet()
{
	if (m_editedPlayerId < 0)
		return;

	threadSettings(m_editedPlayerId).qos = selectedThreadQoS();
	setThreadCount(recommendedThreadCount(selectedThreadQoS()));
}

void PlayerTab::threadCountEdited()
{
	if (m_changingEditorManually || m_editedPlayerId < 0)
		return;

	bool isNumber = false;
	const int count = m_threadCountEdit->text().toInt(&isNumber);

	// Anything unusable puts the last good value back.
	setThreadCount(isNumber && count >= 1 ? count : threadSettings(m_editedPlayerId).count);
}

void PlayerTab::autoDetectThreadCount()
{
	setThreadCount(recommendedThreadCount(selectedThreadQoS()));
}

void PlayerTab::applyThreadSettings()
{
	const QList<Quackle::Player> playerList(m_playerMap.keys());

	for (QList<Quackle::Player>::const_iterator it = playerList.begin(); it != playerList.end(); ++it)
	{
		if ((*it).type() != Quackle::Player::ComputerPlayerType || !(*it).computerPlayer())
			continue;

		const ThreadSettings &settings = threadSettings((*it).id());
		(*it).computerPlayer()->setThreadCount(settings.count, settings.qos);
	}
}

void PlayerTab::saveSettings()
{
	CustomQSettings settings;

	QList<QVariant> playerIds;

	QList<Quackle::Player> playerList(m_playerMap.keys());

	for (QList<Quackle::Player>::iterator it = playerList.begin(); it != playerList.end(); ++it)
	{
		playerIds.push_back((*it).id());
		settings.setValue(
			QString("quackle/newgame/players/%1").arg((*it).id()), QuackleIO::Util::uvStringToQString((*it).storeInformationToString()));

		const ThreadSettings &threads = threadSettings((*it).id());
		settings.setValue(QString("quackle/newgame/playerthreads/%1/qos").arg((*it).id()), static_cast<int>(threads.qos));
		settings.setValue(QString("quackle/newgame/playerthreads/%1/count").arg((*it).id()), threads.count);
	}

	settings.setValue("quackle/newgame/playerIds", playerIds);

	// this is a workaround for one item lists getting saved oddly
	if (playerIds.size() == 1)
		settings.setValue("quackle/newgame/playerId", playerIds.front().toInt());
	else
		settings.setValue("quackle/newgame/playerId", -1);
}

void PlayerTab::populatePlayers()
{
	CustomQSettings settings;

	// this is a workaround for one item lists getting saved oddly
	int playerId = settings.value("quackle/newgame/playerId", -1).toInt();

	QList<QVariant> playerIds;
	if (playerId >= 0)
		playerIds.push_back(QVariant(playerId));
	else
		playerIds = settings.value("quackle/newgame/playerIds", QList<QVariant>()).toList();

	for (const auto &it : playerIds)
	{
		int id = it.toInt();

		QString infoString = settings.value(QString("quackle/newgame/players/%1").arg(id)).toString();
		if (infoString.isNull())
			continue;

		Quackle::Player player(Quackle::Player::makePlayerFromString(QuackleIO::Util::qstringToString(infoString)));
		if (!player.computerPlayer())
		{
			// this is required if the computer id in the settings is no longer
			// offered by Quackle
			player.setComputerPlayer(defaultComputerPlayer());
		}

		ThreadSettings &threads = threadSettings(id);
		const int qos = settings.value(QString("quackle/newgame/playerthreads/%1/qos").arg(id), static_cast<int>(threads.qos)).toInt();
		if (qos >= static_cast<int>(Quackle::ThreadQoS::EnergyEfficient) && qos <= static_cast<int>(Quackle::ThreadQoS::Performance))
			threads.qos = static_cast<Quackle::ThreadQoS>(qos);

		const int count = settings.value(QString("quackle/newgame/playerthreads/%1/count").arg(id), 0).toInt();
		threads.count = count >= 1 ? count : recommendedThreadCount(threads.qos);

		addPlayer(player);
	}

	if (m_playerMap.empty())
	{
		// one computer
		Quackle::Player newPlayer(QuackleIO::Util::qstringToString(tr("Quackle")), Quackle::Player::ComputerPlayerType, 0);
		newPlayer.setComputerPlayer(defaultComputerPlayer());
		addPlayer(newPlayer);

		// and a human
		addPlayer();
	}

	selectionChanged();
}

Quackle::PlayerList PlayerTab::players() const
{
	// this is all silly to do but oh well
	QList<Quackle::Player> playerList(m_playerMap.keys());
	Quackle::PlayerList ret;

	for (QList<Quackle::Player>::const_iterator it = playerList.begin(); it != playerList.end(); ++it)
	{
		ret.push_back(*it);

		if (ret.back().name().empty())
			ret.back().setName(QuackleIO::Util::qstringToString(tr("No Name")));

		QStringList splitName = QuackleIO::Util::uvStringToQString(ret.back().name()).split(QRegularExpression("\\s+"));
		ret.back().setAbbreviatedName(QuackleIO::Util::qstringToString(splitName.join("_")));
	}

	return ret;
}

void PlayerTab::addPlayer(const Quackle::Player &player)
{
	QTreeWidgetItem *item = new QTreeWidgetItem(m_playersTreeWidget);
	m_playerMap.insert(player, item);
	setItem(item, player);

	m_playersTreeWidget->clearSelection();
	item->setSelected(true);
}

Quackle::ComputerPlayer *PlayerTab::defaultComputerPlayer() const
{
	for (Quackle::PlayerList::const_reverse_iterator it = QUACKLE_COMPUTER_PLAYERS.rbegin(); it != QUACKLE_COMPUTER_PLAYERS.rend(); ++it)
		if ((*it).computerPlayer()->isUserVisible() && !(*it).computerPlayer()->isSlow())
			return (*it).computerPlayer();

	return QUACKLE_COMPUTER_PLAYERS.back().computerPlayer();
}

void PlayerTab::setItem(QTreeWidgetItem *item, const Quackle::Player &player)
{
	item->setText(PlayerName, QuackleIO::Util::uvStringToQString(player.name()));
	item->setText(PlayerType, stringForPlayer(player));
}

void PlayerTab::addPlayer()
{
	QString name(tr("New Player %1"));
	QString unusedName;
	int unusedId;

	for (int i = 1;; ++i)
	{
		unusedName = name.arg(i);
		UVString nameString = QuackleIO::Util::qstringToString(unusedName);

		bool found = false;
		for (QMap<Quackle::Player, QTreeWidgetItem *>::iterator it = m_playerMap.begin(); it != m_playerMap.end(); ++it)
		{
			if (it.key().name() == nameString)
			{
				found = true;
				break;
			}
		}

		if (!found)
			break;
	}

	for (unusedId = 0;; ++unusedId)
	{
		bool found = false;
		for (QMap<Quackle::Player, QTreeWidgetItem *>::iterator it = m_playerMap.begin(); it != m_playerMap.end(); ++it)
		{
			if (it.key().id() == unusedId)
			{
				found = true;
				break;
			}
		}

		if (!found)
			break;
	}

	Quackle::Player player(QuackleIO::Util::qstringToString(unusedName), Quackle::Player::HumanPlayerType, unusedId);
	addPlayer(player);
}

void PlayerTab::removePlayer()
{
	QList<QTreeWidgetItem *> items = m_playersTreeWidget->selectedItems();
	for (auto &it : items)
	{
		delete it;

		QList<Quackle::Player> correspondingPlayers(m_playerMap.keys(it));

		if (correspondingPlayers.size() > 0)
		{
			m_threadSettings.remove(correspondingPlayers.front().id());
			m_playerMap.remove(correspondingPlayers.front());
		}
	}

	selectionChanged();
}

void PlayerTab::playerEdited()
{
	if (m_changingEditorManually || !hasSelection())
		return;

	Quackle::Player lastPlayer(getLastPlayer());
	if (lastPlayer.id() < 0)
		return;

	lastPlayer.setName(QuackleIO::Util::qstringToString(m_nameEdit->text()));
	updatePlayerFromString(m_playerType->currentText(), lastPlayer);

	// please excuse the HORRIBLE use of QMap here
	QTreeWidgetItem *item = m_playerMap.value(lastPlayer);
	m_playerMap.remove(lastPlayer);
	m_playerMap.insert(lastPlayer, item);

	setItem(item, lastPlayer);

	m_changingEditorManually = true;
	showThreadSettings(lastPlayer);
	m_changingEditorManually = false;
}

void PlayerTab::selectionChanged()
{
	bool hasSel = hasSelection();
	m_removePlayerButton->setEnabled(hasSel);
	m_editGroup->setEnabled(hasSel);

	if (!hasSel)
	{
		m_threadSettingsWidget->setVisible(false);
		m_editedPlayerId = -1;
		return;
	}

	Quackle::Player lastPlayer(getLastPlayer());

	if (lastPlayer.id() < 0)
	{
		UVcout << "last player id < 0" << endl;
		return;
	}

	m_changingEditorManually = true;

	m_nameEdit->setText(QuackleIO::Util::uvStringToQString(lastPlayer.name()));
	m_playerType->setCurrentIndex(m_playerType->findText(stringForPlayer(lastPlayer)));
	showThreadSettings(lastPlayer);

	m_changingEditorManually = false;
}

bool PlayerTab::hasSelection()
{
	return !m_playersTreeWidget->selectedItems().empty();
}

Quackle::Player PlayerTab::getLastPlayer()
{
	QTreeWidgetItem *lastItem = m_playersTreeWidget->selectedItems().back();
	Quackle::Player lastPlayer;
	for (QMap<Quackle::Player, QTreeWidgetItem *>::iterator it = m_playerMap.begin(); it != m_playerMap.end(); ++it)
		if (it.value() == lastItem)
			return it.key();

	// urp!
	return Quackle::Player();
}

QString PlayerTab::stringForPlayer(const Quackle::Player &player)
{
	switch (player.type())
	{
	case Quackle::Player::HumanPlayerType:
	{
		if (player.racksAreKnown())
			return tr("Human");
		else
			return tr("Human With Unknown Racks");
	}

	case Quackle::Player::ComputerPlayerType:
		if (player.computerPlayer())
			return QuackleIO::Util::uvStringToQString(player.computerPlayer()->name());

	default:
		return tr("Unknown Player");
	}
}

void PlayerTab::updatePlayerFromString(const QString &typeString, Quackle::Player &player)
{
	if (typeString == tr("Human"))
	{
		player.setType(Quackle::Player::HumanPlayerType);
		player.setRacksAreKnown(true);
		player.setComputerPlayer(0);
		return;
	}

	if (typeString == tr("Human With Unknown Racks"))
	{
		player.setType(Quackle::Player::HumanPlayerType);
		player.setRacksAreKnown(false);
		player.setComputerPlayer(0);
		return;
	}

	player.setType(Quackle::Player::ComputerPlayerType);

	bool found = false;
	const Quackle::Player &modelPlayer = QUACKLE_COMPUTER_PLAYERS.playerForName(QuackleIO::Util::qstringToString(typeString), found);

	if (found)
		player.setComputerPlayer(modelPlayer.computerPlayer());
	else
		player.setComputerPlayer(0);
}
