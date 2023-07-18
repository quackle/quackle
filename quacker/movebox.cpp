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

#include <game.h>
#include <quackleio/util.h>

#include "geometry.h"
#include "movebox.h"

MoveBox::MoveBox(QWidget *parent)
	: View(parent)
{
	QVBoxLayout *vlayout = new QVBoxLayout(this);
	Geometry::setupInnerLayout(vlayout);
	vlayout->setSpacing(0);

	m_treeWidget = new QTreeWidget(this);

	m_treeWidget->setColumnCount(3);
	m_treeWidget->setSelectionMode(QTreeWidget::ExtendedSelection);
	QStringList headers;
	headers << tr("Move") << tr("Score") << tr("Leave") << tr("Win %") << tr("Valuation");
	m_treeWidget->setHeaderLabels(headers);
	
	QHBoxLayout *buttonLayout = new QHBoxLayout;
	Geometry::setupInnerLayout(buttonLayout);
	m_removeButton = new QPushButton(tr("Remove"));
	connect(m_removeButton, SIGNAL(clicked()), this, SLOT(removeMove()));
	buttonLayout->addWidget(m_removeButton);

	m_commitButton = new QPushButton(tr("Commit"));
	connect(m_commitButton, SIGNAL(clicked()), this, SIGNAL(commit()));
	buttonLayout->addWidget(m_commitButton);

	setSelectionWatchingEnabled(true);

	vlayout->addWidget(m_treeWidget);
	vlayout->addLayout(buttonLayout);
}

void MoveBox::moveActivated(QTreeWidgetItem *item)
{
	if (item == 0)
	{
		Quackle::Move nonMove = Quackle::Move::createNonmove();
		emit setCandidateMove(&nonMove, nullptr);
		return;
	}

	// do nothing if no item was clicked or if more than one item is selected
	if (m_treeWidget->selectedItems().size() != 1)
		return;

	for (QMap<Quackle::Move, QTreeWidgetItem *>::iterator it = m_moveMap.begin(); it != m_moveMap.end(); ++it)
	{
		if (it.value() == item)
		{
			const auto& key = it.key();
			emit setCandidateMove(&key, nullptr);
			break;
		}
	}
}

void MoveBox::selectionChanged()
{
	const bool moveSelected = !m_treeWidget->selectedItems().empty();
	m_removeButton->setEnabled(moveSelected);
	m_commitButton->setEnabled(moveSelected);
}

void MoveBox::removeMove()
{
	QList<QTreeWidgetItem *> selectedItems = m_treeWidget->selectedItems();
	if (selectedItems.empty())
		return;
	
	Quackle::MoveList selectedMoves;

	for (const auto& it : selectedItems)
	{
		for (QMap<Quackle::Move, QTreeWidgetItem *>::iterator mapIt = m_moveMap.begin(); mapIt != m_moveMap.end(); ++mapIt)
		{
			if (mapIt.value() == it)
			{
				selectedMoves.push_back(mapIt.key());
				break;
			}
		}
	}

	emit removeCandidateMoves(&selectedMoves);

	// TODO make this code work to select the next item
	QTreeWidgetItem *prevLastSelection = m_moveMap.value(selectedMoves.back());
	QTreeWidgetItem *nextSelection = 0;
	const int numItems = m_treeWidget->topLevelItemCount();
	for (int i = 0; i < numItems; ++i)
	{
		if (m_treeWidget->topLevelItem(i) == prevLastSelection)
		{
			if (i != numItems - 1)
			{
				nextSelection = m_treeWidget->topLevelItem(i + 1);
				break;
			}
		}
	}

	if (nextSelection)
	{
		for (QMap<Quackle::Move, QTreeWidgetItem *>::iterator mapIt = m_moveMap.begin(); mapIt != m_moveMap.end(); ++mapIt)
		{
			if (mapIt.value() == nextSelection)
			{
				const auto& key = mapIt.key();
				emit setCandidateMove(&key, nullptr);
				break;
			}
		}
	}
}

// This is complex as it tries to do as little as possible when
// the move list hasn't changed and is sorted the same way,
// so simulations can go as fast as possible.
// Nevertheless, TODO clean this up
void MoveBox::setMoves(const Quackle::MoveList &moves, const Quackle::Move &selectedMove)
{
	bool resorted = false;
	if (m_previousMoves.size() == moves.size())
	{
		Quackle::MoveList::const_iterator prevIt = m_previousMoves.begin();
		const Quackle::MoveList::const_iterator end = moves.end();
		for (Quackle::MoveList::const_iterator it = moves.begin(); it != end; ++it, ++prevIt)
		{
			if (!(*prevIt == *it))
			{
				resorted = true;
				break;
			}
		}
	}
	else
	{
		resorted = true;
	}

	bool hasNewItems = false;

	Quackle::MoveList::const_iterator end(moves.end());
	for (Quackle::MoveList::const_iterator it = moves.begin(); it != end; ++it)
	{
		QMap<Quackle::Move, QTreeWidgetItem *>::const_iterator mapEnd(m_moveMap.end());
		for (QMap<Quackle::Move, QTreeWidgetItem *>::const_iterator mapIt = m_moveMap.begin(); mapIt != mapEnd; ++mapIt)
		{
			if (mapIt.key() == *it)
			{
				mapIt.value()->setText(WinPercentageColumn, formatWinPercentage((*it).win));
				mapIt.value()->setText(EquityColumn, formatValuation((*it).equity));

				if (resorted)
				{
					m_treeWidget->addTopLevelItem(m_treeWidget->takeTopLevelItem(m_treeWidget->indexOfTopLevelItem(mapIt.value())));
				}

				goto foundFirstPass;
			}
		}

		hasNewItems = true;
		m_moveMap.insert(*it, createItem(*it));

		foundFirstPass:
		continue;
	}

	if (resorted)
	{
		for (QMutableMapIterator<Quackle::Move, QTreeWidgetItem *> mapIt(m_moveMap); mapIt.hasNext(); )
		{
			mapIt.next();

			for (Quackle::MoveList::const_iterator it = moves.begin(); it != end; ++it)
				if (mapIt.key() == *it)
					goto found;

			delete mapIt.value();
			mapIt.remove();

			found:
			continue;
		}
	}

	if (moves.contains(selectedMove) && m_moveMap.contains(selectedMove))
	{
		m_treeWidget->setCurrentItem(m_moveMap.value(selectedMove));
	}

	selectionChanged();

	if (hasNewItems)
		QTimer::singleShot(0, this, SLOT(checkGeometry()));

	m_previousMoves = moves;
	m_previousSelection = selectedMove;
}

void MoveBox::checkGeometry()
{
	m_treeWidget->resizeColumnToContents(EquityColumn);
	m_treeWidget->resizeColumnToContents(WinPercentageColumn);
	m_treeWidget->resizeColumnToContents(LeaveColumn);
	m_treeWidget->resizeColumnToContents(ScoreColumn);
	m_treeWidget->resizeColumnToContents(PlayColumn);
}

void MoveBox::positionChanged(const Quackle::GamePosition *position)
{
	if (m_rack.tiles() != position->currentPlayer().rack().tiles())
	{
		for (auto& mapIt : m_moveMap)
			delete mapIt;

		m_moveMap.clear();
	}

	m_rack = position->currentPlayer().rack();
	setMoves(position->moves(), position->moveMade());
}

void MoveBox::movesChanged(const Quackle::MoveList *moves)
{
	setMoves(*moves, m_previousSelection);
}

QTreeWidgetItem *MoveBox::createItem(const Quackle::Move &move)
{
	QTreeWidgetItem *item = new QTreeWidgetItem(m_treeWidget);
	item->setText(PlayColumn, QuackleIO::Util::moveToDetailedString(move));
	item->setText(ScoreColumn, QString::number(move.effectiveScore()));
	item->setText(LeaveColumn, QuackleIO::Util::letterStringToQString(QuackleIO::Util::arrangeLettersForUser(m_rack - move)));
	item->setText(WinPercentageColumn, formatWinPercentage(move.win));
	item->setText(EquityColumn, formatValuation(move.equity));

	return item;
}

QString MoveBox::formatValuation(double valuation)
{
	return QString::number(valuation, 'f', 1);
}

QString MoveBox::formatWinPercentage(double winPercentage)
{
	return QString::number(winPercentage * 100.0, 'f', 2);
}

void MoveBox::setSelectionWatchingEnabled(bool enabled)
{
	if (enabled)
	{
		connect(m_treeWidget, SIGNAL(itemSelectionChanged()), this, SLOT(selectionChanged()));

		// the former is single-click to select on all platforms,
		// latter is always double-click to select on most platforms
		connect(m_treeWidget, SIGNAL(itemClicked(QTreeWidgetItem *, int)), this, SLOT(moveActivated(QTreeWidgetItem *)));
		//connect(m_treeWidget, SIGNAL(itemActivated(QTreeWidgetItem *, int)), this, SLOT(moveActivated(QTreeWidgetItem *)));

		// to allow the arrow keys to be used to select moves
		// connect(m_treeWidget, SIGNAL(currentItemChanged(QTreeWidgetItem *, QTreeWidgetItem *)), this, SLOT(moveActivated(QTreeWidgetItem *)));
	}
	else
	{
		disconnect(m_treeWidget, SIGNAL(itemSelectionChanged()), this, SLOT(selectionChanged()));
		disconnect(m_treeWidget, SIGNAL(itemClicked(QTreeWidgetItem *, int)), this, SLOT(moveActivated(QTreeWidgetItem *)));
		// disconnect(m_treeWidget, SIGNAL(currentItemChanged(QTreeWidgetItem *, QTreeWidgetItem *)), this, SLOT(moveActivated(QTreeWidgetItem *)));
	}
}

