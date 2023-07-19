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

#include <QtWidgets>

#include <game.h>
#include <quackleio/util.h>

#include "history.h"
#include "geometry.h"

History::History(QWidget *parent)
	: HistoryView(parent)
{
	m_vlayout = new QVBoxLayout(this);
	Geometry::setupFramedLayout(m_vlayout);

	m_tableWidget = new QTableWidget(this);
	m_tableWidget->setHorizontalScrollMode(QTableView::ScrollPerPixel);
	connect(m_tableWidget, SIGNAL(itemActivated(QTableWidgetItem *)), this, SLOT(itemActivated(QTableWidgetItem *)));

	setSizePolicy(QSizePolicy::Maximum, QSizePolicy::Maximum);

	m_vlayout->addWidget(m_tableWidget);
}

void History::historyChanged(const Quackle::History &history)
{
	m_tableWidget->clear();
	const Quackle::PlayerList players(history.players());

	QStringList columnLabels;
	QStringList rowLabels;

	for (int i = 1; i <= history.maximumTurnNumber(); ++i)
		rowLabels.push_back(QString::number(i));
	
	Quackle::GamePosition lastPosition(history.lastPosition());
	lastPosition.setCurrentPlayer(lastPosition.playerOnTurn().id());
	const bool gameOver = lastPosition.gameOver();
	if (gameOver)
		rowLabels.push_back(tr("Final"));

	m_tableWidget->setRowCount(int(rowLabels.size()));
	m_tableWidget->setColumnCount(int(players.size()));

	Quackle::PlayerList currentScores(lastPosition.endgameAdjustedScores());

	QTableWidgetItem *currentItem = 0;
	int playerCountFromZero = 0;
	Quackle::PlayerList::const_iterator currentScoresIt = currentScores.begin();
	for (Quackle::PlayerList::const_iterator it = players.begin(); it != players.end(); ++it, ++currentScoresIt, ++playerCountFromZero)
	{
		columnLabels.push_back(QuackleIO::Util::uvStringToQString((*it).name()));

		const Quackle::PositionList positions(history.positionsFacedBy((*it).id()));
		for (Quackle::PositionList::const_iterator pit = positions.begin(); pit != positions.end(); ++pit)
		{
			QTableWidgetItem *item = createItem(*pit, history.currentPosition().currentPlayer());

			const int row = /* make zero indexed */ (*pit).turnNumber() - 1;
			const int column = playerCountFromZero;
			m_tableWidget->setItem(row, column, item);

			Quackle::HistoryLocation location((*it).id(), (*pit).turnNumber());
			m_locationMap.insert(location, item);

			if (location == history.currentLocation())
			{
				currentItem = m_tableWidget->item(row, column);
				m_tableWidget->setCurrentItem(currentItem);
			}
		}

		if (gameOver)
		{
			QString scoreString(QString::number((*currentScoresIt).score()));

			QTableWidgetItem *item = createPlainItem(scoreString);
			m_tableWidget->setItem(int(rowLabels.size()) - 1, playerCountFromZero, item);

			if (history.currentPosition().gameOver())
				currentItem = item;
		}

		if (!positions.empty())
			m_tableWidget->resizeColumnToContents(playerCountFromZero);
	}

	m_tableWidget->setHorizontalHeaderLabels(columnLabels);
	m_tableWidget->setVerticalHeaderLabels(rowLabels);

	m_tableWidget->scrollToItem(currentItem);
}

QTableWidgetItem *History::createItem(const Quackle::GamePosition &position, const Quackle::Player &currentPlayer)
{
	const Quackle::Move committedMove(position.committedMove());
	QString contentString;

	if (committedMove.action == Quackle::Move::Nonmove)
		contentString = tr("*TO PLAY*");
	else
	{
		const QString moveString = (position.currentPlayer() == currentPlayer?  QuackleIO::Util::moveToDetailedString(committedMove) : QuackleIO::Util::moveToSensitiveString(committedMove));

		const int score = position.currentPlayer().score() + committedMove.effectiveScore();

		const QString scoreString = QString("+%2/%3").arg(committedMove.effectiveScore()).arg(score);

    // Pad scoreString to 8 chars, because '+117/381' is 8 chars, and this
    // helps the words kinda line up with each other.
    contentString = QString("%1 %2").arg(moveString).arg(scoreString, 8);
	}

	return createPlainItem(contentString);
}

QTableWidgetItem *History::createPlainItem(const QString &contentString)
{
	QTableWidgetItem *ret = new QTableWidgetItem(contentString);
	ret->setFlags(Qt::ItemIsEnabled | Qt::ItemIsSelectable);

	// make scores line up with each other
	ret->setTextAlignment(Qt::AlignRight);

	return ret;
}

void History::itemActivated(QTableWidgetItem *item)
{
	for (QMap<Quackle::HistoryLocation, QTableWidgetItem *>::iterator it = m_locationMap.begin(); it != m_locationMap.end(); ++it)
	{
		if (it.value() == item)
		{
			emit goToHistoryLocation(&it.key());
			break;
		}
	}
}

