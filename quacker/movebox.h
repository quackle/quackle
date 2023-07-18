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

#ifndef QUACKER_MOVEBOX_H
#define QUACKER_MOVEBOX_H

#include <QMap>

#include <move.h>
#include <rack.h>

#include "view.h"

class QPushButton;
class QTreeWidget;
class QTreeWidgetItem;

class MoveBox : public View
{
Q_OBJECT

public:
	MoveBox(QWidget *parent = 0);

	// Sets the moves shown in the movebox and the current (selected) move
	void setMoves(const Quackle::MoveList &moves, const Quackle::Move &selectedMove);

public slots:
	virtual void positionChanged(const Quackle::GamePosition *position);
	virtual void movesChanged(const Quackle::MoveList *moves);

private slots:
	void moveActivated(QTreeWidgetItem *item);
	void selectionChanged();
	void removeMove();
	void checkGeometry();

protected:
	QTreeWidgetItem *createItem(const Quackle::Move &move);

	QString formatWinPercentage(double winPercentage);
	QString formatValuation(double valuation);

	void setSelectionWatchingEnabled(bool enabled);

	QMap<Quackle::Move, QTreeWidgetItem *> m_moveMap;
	Quackle::MoveList m_previousMoves;
	Quackle::Move m_previousSelection;
	Quackle::Rack m_rack;

	enum Columns { PlayColumn = 0, ScoreColumn = 1, LeaveColumn = 2, WinPercentageColumn = 3, EquityColumn = 4 };
	QTreeWidget *m_treeWidget;
	QPushButton *m_removeButton;
	QPushButton *m_commitButton;
};

#endif
