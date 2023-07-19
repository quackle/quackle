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

#ifndef QUACKER_BOARDSETUPDIALOG_H
#define QUACKER_BOARDSETUPDIALOG_H

#include <string>
#include <game.h>

#include <QWidget>
#include <QDialog>

class QCheckBox;
class QComboBox;
class QLineEdit;
class QPushButton;

class BoardSetupFrame;

class BoardSetupDialog : public QDialog
{
Q_OBJECT

public:
	BoardSetupDialog(QWidget *parent = 0);
	~BoardSetupDialog();
	virtual void accept();
	virtual void reject();

protected slots:
	void parametersChanged(int unused);
	void parametersChanged(const QString &);
	void symmetryChanged();
	void undoAllChanges();
	void deleteBoard();

private:
	QCheckBox *m_horizontalSymmetry;
	QCheckBox *m_verticalSymmetry;
	QCheckBox *m_diagonalSymmetry;
	
	QComboBox *m_horizontalDimension;
	QComboBox *m_verticalDimension;
	
	QLineEdit *m_boardName;
	
	QPushButton *m_saveChanges;
	QPushButton *m_cancel;
	QPushButton *m_undoAll;
	QPushButton *m_deleteBoard;
	
	Quackle::Game m_game;
	BoardSetupFrame * m_boardFrame;

	QString m_originalName;
	
	string m_serializedOriginalBoard;

	QComboBox * constructDimensionComboBox(int defaultDimension);
	void initializeBoardName();
};

#endif
