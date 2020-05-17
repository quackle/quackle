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
#include <math.h>

#include <QtWidgets>

#include <game.h>
#include <move.h>
#include <boardparameters.h>
#include <quackleio/util.h>

#include "boardsetup.h"
#include "geometry.h"

BoardSetup::BoardSetup(QWidget *parent)
	: View(parent)
{
	m_vlayout = new QVBoxLayout(this);
	Geometry::setupInnerLayout(m_vlayout);

	m_boardFrame = new BoardSetupFrame;
	m_boardWrapper = new QWidget;

	QLabel *helperLabel = new QLabel(tr("Click or right-click on a board square to cycle through bonuses. Shift-click on a square to designate it as the square that starts the game."));
	helperLabel->setWordWrap(true);

	QVBoxLayout *helperLayout = new QVBoxLayout(m_boardWrapper);
	Geometry::setupInnerLayout(helperLayout);

	m_vlayout->addWidget(helperLabel);
	m_vlayout->addWidget(m_boardWrapper);
	helperLayout->addWidget(m_boardFrame);
	m_vlayout->setStretchFactor(m_boardWrapper, 10);

	m_subviews.push_back(m_boardFrame);
	connectSubviewSignals();
}

void BoardSetup::expandToFullWidth()
{
	m_boardFrame->expandToSize(m_boardWrapper->size());
}

void BoardSetup::resizeEvent(QResizeEvent * /* event */)
{
	QTimer::singleShot(0, this, SLOT(expandToFullWidth()));
}

///////////////////

BoardSetupFrame::BoardSetupFrame(QWidget *parent)
	: GraphicalBoardFrame(parent)
{
	m_alwaysShowVerboseLabels = true;
}

BoardSetupFrame::~BoardSetupFrame()
{
}


void BoardSetupFrame::setBoard(const Quackle::Board &board)
{
	m_board = board;
}

void BoardSetupFrame::setSymmetry(bool horizontal, bool vertical, bool diagonal)
{
	horizontalSymmetry = horizontal;
	verticalSymmetry = vertical;
	diagonalSymmetry = diagonal;
}

void BoardSetupFrame::parametersChanged()
{
	flushPixmapsAndRedraw();
}

void BoardSetupFrame::tileClicked(const QSize &tileLocation, const QMouseEvent *event)
{
	const int row = tileLocation.height();
	const int col = tileLocation.width();
	
	// set starting point...
	if (event->button() == Qt::LeftButton && (event->modifiers() & Qt::SHIFT) != 0)
	{
		QUACKLE_BOARD_PARAMETERS->setStartRow(row);
		QUACKLE_BOARD_PARAMETERS->setStartColumn(col);
		prepare();
		return;
	}
	
	// or change the value of a square
	const int maxLetterMultiplier = (int) Quackle::BoardParameters::lsCount;
	const int maxWordMultiplier = (int) Quackle::BoardParameters::wsCount;
	const int maxMultiplier = maxLetterMultiplier + maxWordMultiplier - 1;
	int wordMultiplier = QUACKLE_BOARD_PARAMETERS->wordMultiplier(row, col);
	int letterMultiplier = QUACKLE_BOARD_PARAMETERS->letterMultiplier(row, col);
	int combinedMultipliers = (wordMultiplier == 1) ? letterMultiplier : (maxLetterMultiplier + wordMultiplier - 1);

	if (event->button() == Qt::LeftButton)
		combinedMultipliers++;
	else
		combinedMultipliers--;

	if (combinedMultipliers < 1)
		combinedMultipliers = maxMultiplier;
	else if (combinedMultipliers > maxMultiplier)
		combinedMultipliers = 1;

	if (combinedMultipliers <= maxLetterMultiplier)
	{
		wordMultiplier = 1;
		letterMultiplier = combinedMultipliers;
	}
	else
	{
		wordMultiplier = combinedMultipliers - maxLetterMultiplier + 1;
		letterMultiplier = 1;
	}

	setMultipliers(row, col, wordMultiplier, letterMultiplier);

	const int height = QUACKLE_BOARD_PARAMETERS->height();
	const int width = QUACKLE_BOARD_PARAMETERS->width();

	if (horizontalSymmetry)
		setMultipliers(row, width - 1 - col, wordMultiplier, letterMultiplier);
	if (verticalSymmetry)
		setMultipliers(height - 1 - row, col, wordMultiplier, letterMultiplier);
	if (horizontalSymmetry && verticalSymmetry)
		setMultipliers(height - 1 - row, width - 1 - col, wordMultiplier, letterMultiplier);
	if (diagonalSymmetry && (row != col) && (height - 1 - row != col))
	{
		setMultipliers(col, row, wordMultiplier, letterMultiplier);
		setMultipliers(col, width - 1 - row, wordMultiplier, letterMultiplier);
		setMultipliers(height - 1 - col, row, wordMultiplier, letterMultiplier);
		setMultipliers(height - 1 - col, width - 1 - row, wordMultiplier, letterMultiplier);
	}
		
	prepare();
}

bool BoardSetupFrame::wantMousePressEvent(const QMouseEvent *event) const
{
	return (event->button() == Qt::LeftButton || event->button() == Qt::RightButton);
}

void BoardSetupFrame::setMultipliers(int row, int col, int word, int letter)
{
	QUACKLE_BOARD_PARAMETERS->setWordMultiplier(row, col, (Quackle::BoardParameters::WordMultiplier) word);
	QUACKLE_BOARD_PARAMETERS->setLetterMultiplier(row, col, (Quackle::BoardParameters::LetterMultiplier) letter);
}
