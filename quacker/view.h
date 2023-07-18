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

#ifndef QUACKER_VIEW_H
#define QUACKER_VIEW_H

#include <QFrame>
#include <QList>

#include <alphabetparameters.h>

namespace Quackle
{
	class GamePosition;
	class History;
	class HistoryLocation;
	class Move;
	class MoveList;
	class Rack;
}

class BaseView : public QFrame
{
Q_OBJECT

public:
	BaseView(QWidget *parent = 0);
	virtual ~BaseView();

signals:
	// tell user of a message, usually via status bar
	void statusMessage(const QString &message);
};

class View : public BaseView
{
Q_OBJECT

public:
	View(QWidget *parent = 0);
	virtual ~View();

signals:
	// emit to alert the rest of the application to show this
	// as candidate move - may eventually trigger positionChanged
	// in response
	void setCandidateMove(const Quackle::Move *move, bool *carryOnPtr = nullptr);
	void removeCandidateMoves(const Quackle::MoveList *moves);
	void commit();

	// emit to alert the rest of the application to reset the current
	// player's rack - may eventually trigger positionChanged
	// in response
	void setRack(const Quackle::Rack &rack);

	// Sets the current position's explanatory note.
	// Does *not* alert the rest of the application.
	void setNote(const UVString &note);

public slots:
	// called whenever game position changes; the board only changes when a
	// move is made, but the candidate move (accessible from position.moveMade(),
	// and the resulting board from position.boardAfterMoveMade()) change alone too.
	// This is called in both cases.
	// 
	// The default implementation calls positionChanged(position) for all subviews
	// in m_subviews.
	virtual void positionChanged(const Quackle::GamePosition *position);

	// called when user starts a simulation and this move list should
	// supercede that from the position
	virtual void movesChanged(const Quackle::MoveList *moves);

	virtual void grabFocus();

protected:
	// keep a list of View subclasses in m_subviews
	// and call this so their signals are emitted from this object
	void connectSubviewSignals();
	QList<View *> m_subviews;
};

class HistoryView : public BaseView
{
Q_OBJECT

public:
	HistoryView(QWidget *parent = 0);
	virtual ~HistoryView();

signals:
	// emit to alert the rest of the application to show this
	// as candidate move - may eventually trigger positionChanged
	// in response
	void goToHistoryLocation(const Quackle::HistoryLocation *location);

public slots:
	// called whenever history is added to
	virtual void historyChanged(const Quackle::History &history);
};

#endif
