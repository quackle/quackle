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

#ifndef QUACKER_SETTINGS_H
#define QUACKER_SETTINGS_H

#include <string>

#include <QWidget>
#include <QSettings>

class QComboBox;
class QCheckBox;
class QPushButton;

using namespace std;

class Settings : public QWidget
{
Q_OBJECT

public:
	Settings(QWidget *parent = 0);

	static Settings *self();

signals:
	void refreshViews();

public slots:
	// called before anything else to initialize quackle generally
	void preInitialize();

	// called to set up libquackle data structures and our internal
	// data structures based on stored user settings
	void initialize();

	// called to set widgets to display current settings based
	// on libquackle data structures and our internal data structures
	void load();

	void createGUI();

protected slots:
	void lexiconChanged(const QString &lexiconName);
	void alphabetChanged(const QString &alphabetName);
	void boardChanged(const QString &boardName);

	void addBoard();
	void editBoard();
	void deleteBoard();
	
	void setQuackleToUseLexiconName(const string &lexiconName);
	void setQuackleToUseAlphabetName(const string &alphabetName);
	void setQuackleToUseBoardName(const QString &lexiconName);

protected:
	QComboBox *m_lexiconNameCombo;
	QComboBox *m_alphabetNameCombo;
	QComboBox *m_boardNameCombo;
	QPushButton *m_addBoard;
	QPushButton *m_editBoard;
	QPushButton *m_deleteBoard;

private:
	// populate the popup based on what's in QSettings
	void loadBoardNameCombo();
	
	static Settings *m_self;
};

#endif
