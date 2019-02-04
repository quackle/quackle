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

#ifndef QUACKER_SIMVIEWER_H
#define QUACKER_SIMVIEWER_H

#include <QDialog>

#include <sim.h>

class QPushButton;
class QTabWidget;
class QTextEdit;
class AveragesTab;

class SimViewer : public QDialog
{
Q_OBJECT

public:
	SimViewer(QWidget *parent = 0);

	virtual QSize sizeHint() const;

public slots:
	void setSimulator(const Quackle::Simulator &simulator);

private:
	QTabWidget *m_tabs;
	AveragesTab *m_averagesTab;
};

class AveragesTab : public QWidget 
{
Q_OBJECT

public:
	AveragesTab(QWidget *parent = 0);

public slots:
	void setSimulator(const Quackle::Simulator &simulator);

	QString statisticTable(const Quackle::Simulator &simulator);

	void explain();

private:
	QTextEdit *m_textEdit;
};

#endif
