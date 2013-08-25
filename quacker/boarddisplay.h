/*
 *  Quackle -- Crossword game artificial intelligence and analysis tool
 *  Copyright (C) 2005-2006 Jason Katz-Brown and John O'Laughlin.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  
 *  02110-1301  USA
 */

#ifndef QUACKER_BOARDDISPLAY_H
#define QUACKER_BOARDDISPLAY_H

#include <move.h>

#include "view.h"

class QLineEdit;
class QPushButton;
class QTextEdit;
class QVBoxLayout;

namespace Quackle
{
	class GamePosition;
}

class BoardWithQuickEntry : public View
{
Q_OBJECT

public:
	BoardWithQuickEntry(QWidget *parent = 0);
	virtual ~BoardWithQuickEntry();

public slots:
	virtual void positionChanged(const Quackle::GamePosition &position);

protected slots:
	void setLocalCandidate(const Quackle::Move &candidate);

private slots:
	void quickEditReturnPressed();
	void plusFive();
	void performCommit();
	void reset();

protected:
	virtual void processCommand(const QString &command);
	void provideHelp();

	QVBoxLayout *m_vlayout;

private:
	QLineEdit *m_lineEdit;
	QPushButton *m_commitButton;
	Quackle::Move m_localCandidateMove;
};

class TextBoard : public BoardWithQuickEntry
{
Q_OBJECT

public:
	TextBoard(QWidget *parent = 0);

public slots:
	virtual void positionChanged(const Quackle::GamePosition &position);

private:
	QTextEdit *m_textEdit;
};

#endif
