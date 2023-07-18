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

#ifndef QUACKER_BAGDISPLAY_H
#define QUACKER_BAGDISPLAY_H

#include "alphabetparameters.h"
#include "view.h"

class QLabel;
class QTextEdit;

class BagDisplay : public View
{
Q_OBJECT

public:
	BagDisplay(QWidget *parent = 0);
	virtual ~BagDisplay();

public slots:
	virtual void positionChanged(const Quackle::GamePosition *position);

protected slots:
	virtual void showTiles(const Quackle::LongLetterString &tiles);

private:
	QLabel *m_label;
	QTextEdit *m_textEdit;
};

#endif
