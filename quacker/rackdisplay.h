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

#ifndef QUACKER_RACKDISPLAY_H
#define QUACKER_RACKDISPLAY_H

#include "view.h"
#include "alphabetparameters.h"

class QFont;
class QLabel;
class QLineEdit;
class QHBoxLayout;
class GraphicalRack;
class RackTileWidget;

class QuickEntryRack : public View
{
Q_OBJECT

public:
	QuickEntryRack(QWidget *parent = 0);
	virtual ~QuickEntryRack();

public slots:
	virtual void positionChanged(const Quackle::GamePosition *position);
	virtual void grabFocus();

private slots:
	void quickEditReturnPressed();
	void shuffle();

protected:
	virtual void processRack(const QString &rack);

private:
	QLabel *m_label;
	QLineEdit *m_lineEdit;
	GraphicalRack *m_tiles;
	Quackle::LetterString m_rackTiles;
};

class GraphicalRack : public QFrame
{
	Q_OBJECT

public:
    GraphicalRack(QWidget * parent = 0);

public slots:
    virtual void setText(const Quackle::LetterString &text);

protected:
    void dragEnterEvent (QDragEnterEvent* event);
    void dropEvent (QDropEvent* event);
    void mousePressEvent (QMouseEvent* event);

private:
	QHBoxLayout *m_layout;
	static const QString mime_type;
};

#endif
