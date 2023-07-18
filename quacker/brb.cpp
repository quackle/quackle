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

#include "bagdisplay.h"
#include "brb.h"
#include "boarddisplay.h"
#include "geometry.h"
#include "rackdisplay.h"
#include "widgetfactory.h"

BRB::BRB(WidgetFactory *widgetFactory, QWidget *parent)
	: View(parent)
{
	QHBoxLayout *topHorizontalLayout = new QHBoxLayout;
	Geometry::setupFramedLayout(topHorizontalLayout);
	setLayout(topHorizontalLayout);

	QVBoxLayout *leftVerticalLayout = new QVBoxLayout;
	Geometry::setupInnerLayout(leftVerticalLayout);
	topHorizontalLayout->addLayout(leftVerticalLayout);
	QVBoxLayout *rightVerticalLayout = new QVBoxLayout;
	Geometry::setupInnerLayout(rightVerticalLayout);
	topHorizontalLayout->addLayout(rightVerticalLayout);

	m_boardDisplay = widgetFactory->createBoardDisplay();
	leftVerticalLayout->addWidget(m_boardDisplay);

	m_rackDisplay = widgetFactory->createRackDisplay();
	leftVerticalLayout->addWidget(m_rackDisplay);

	topHorizontalLayout->setStretchFactor(leftVerticalLayout, 10);

	m_bagDisplay = widgetFactory->createBagDisplay();
	rightVerticalLayout->addWidget(m_bagDisplay);

	m_subviews.push_back(m_boardDisplay);
	m_subviews.push_back(m_rackDisplay);
	m_subviews.push_back(m_bagDisplay);
	connectSubviewSignals();
}

BRB::~BRB()
{
}

View * BRB::getBoardView() const
{
	return m_boardDisplay;
}

void BRB::grabFocus()
{
	m_rackDisplay->grabFocus();
}

void BRB::positionChanged(const Quackle::GamePosition *position)
{
	View::positionChanged(position);
}

