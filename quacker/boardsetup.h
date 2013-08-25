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

#ifndef QUACKER_BOARDSETUP_H
#define QUACKER_BOARDSETUP_H

#include "graphicalboard.h"

class BoardSetupFrame;

class BoardSetup : public View
{
Q_OBJECT

public:
	BoardSetup(QWidget *parent = 0);
	~BoardSetup() {};

	BoardSetupFrame *boardFrame() { return m_boardFrame; }

protected slots:
	virtual void expandToFullWidth();
	virtual void resizeEvent(QResizeEvent *event);

private:
	BoardSetupFrame *m_boardFrame;
	QWidget *m_boardWrapper;
	QVBoxLayout *m_vlayout;
};

class BoardSetupFrame : public GraphicalBoardFrame
{
Q_OBJECT

public:
	BoardSetupFrame(QWidget *parent = 0);
	~BoardSetupFrame();
	void setBoard(const Quackle::Board &board);
	void setSymmetry(bool horizontal, bool vertical);
	void parametersChanged();

public slots:
	virtual void positionChanged(const Quackle::GamePosition & /* position */) {};

protected slots:
	virtual void tileClicked(const QSize &tileLocation, const QMouseEvent *event);

protected:
	virtual void keyPressEvent(QKeyEvent * /* e */) {};

	virtual bool wantMousePressEvent(const QMouseEvent *event) const;

private:
	bool horizontalSymmetry;
	bool verticalSymmetry;

	void setMultipliers(int row, int col, int word, int letter);
};

#endif
