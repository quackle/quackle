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

#include "view.h"

BaseView::BaseView(QWidget *parent)
	: QFrame(parent)
{
}

BaseView::~BaseView()
{
}

/////////

View::View(QWidget *parent)
	: BaseView(parent)
{
}

View::~View()
{
}

void View::grabFocus()
{
}

void View::positionChanged(const Quackle::GamePosition *position)
{
	for (QList<View *>::iterator it = m_subviews.begin(); it != m_subviews.end(); ++it)
		(*it)->positionChanged(position);
}

void View::movesChanged(const Quackle::MoveList *moves)
{
	for (auto& it : m_subviews)
		it->movesChanged(moves);
}

void View::connectSubviewSignals()
{
	for (auto& it : m_subviews)
	{
		connect(it, SIGNAL(statusMessage(const QString &)), this, SIGNAL(statusMessage(const QString &)));
		connect(it, SIGNAL(setCandidateMove(const Quackle::Move *, bool *)), this, SIGNAL(setCandidateMove(const Quackle::Move *, bool *)));
		connect(it, SIGNAL(removeCandidateMoves(const Quackle::MoveList *)), this, SIGNAL(removeCandidateMoves(const Quackle::MoveList *)));
		connect(it, SIGNAL(commit()), this, SIGNAL(commit()));
		connect(it, SIGNAL(setRack(const Quackle::Rack &)), this, SIGNAL(setRack(const Quackle::Rack &)));
	}
}

/////////

HistoryView::HistoryView(QWidget *parent)
	: BaseView(parent)
{
}

HistoryView::~HistoryView()
{
}

void HistoryView::historyChanged(const Quackle::History & /* history */)
{
}

