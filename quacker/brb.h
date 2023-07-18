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

#ifndef QUACKER_BRB_H
#define QUACKER_BRB_H

#include <QWidget>

#include "view.h"

namespace Quackle
{
	class GamePosition;
}

class WidgetFactory;

class BRB : public View
{
Q_OBJECT

public:
	BRB(WidgetFactory *widgetFactory, QWidget *parent = 0);
	virtual ~BRB();
	View * getBoardView() const;

public slots:
	virtual void positionChanged(const Quackle::GamePosition *position);
	virtual void grabFocus();

private:
	View *m_boardDisplay;
	View *m_rackDisplay;
	View *m_bagDisplay;
};

#endif
