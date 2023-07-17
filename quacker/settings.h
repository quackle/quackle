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

#ifndef QUACKER_SETTINGS_H
#define QUACKER_SETTINGS_H

#include <string>

#include <QWidget>
#include <QSettings>

#include "quackleio/gaddagfactory.h"

class QComboBox;
class QCheckBox;
class QPushButton;
class QLabel;

class Settings : public QWidget
{
Q_OBJECT

public:
	Settings(QWidget *parent = 0);

	static Settings *self();

	// load up an item list based on a list of filenames
	static void populateComboFromFilenames(QComboBox* combo, const QString &path, const QString &extension, const QString &label);

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
	void lexiconChanged(int lexiconIndex);
	void alphabetChanged(int alphabetIndex);
	void themeChanged(int themeIndex);
	void boardChanged(int boardIndex);

	void addBoard();
	void editBoard();
	
	void editLexicon();
	void editAlphabet();
	void editTheme();
	void buildGaddag();

	void setQuackleToUseLexiconName(const QString &lexiconName);
	void setQuackleToUseAlphabetName(const QString &alphabetName);
	void setQuackleToUseThemeName(const QString &themeName);
	void setQuackleToUseBoardName(const QString &lexiconName);

protected:
	QComboBox *m_lexiconNameCombo;
	QComboBox *m_alphabetNameCombo;
	QComboBox *m_themeNameCombo;
	QComboBox *m_boardNameCombo;
	QPushButton *m_editLexicon;
	QPushButton *m_editAlphabet;
	QPushButton *m_editTheme;
	QPushButton *m_editBoard;
	QPushButton *m_buildGaddag;
	QLabel *m_logoLabel;
	QLabel *m_copyrightLabel;
	QLabel *m_buildGaddagLabel;
	QLabel *m_separatorLabel;
	QString m_appDataDir;
	QString m_userDataDir;
	QString m_themeName;

private:
	// populate the popup based on what's in QSettings
	void loadBoardNameCombo();

	void setGaddagLabel();
	void setGaddagLabel(const QString &label);
	void pushIndex(GaddagFactory &factory, Quackle::LetterString &word, int index, int &wordCount);

	void lexiconChanged(const QString &lexiconName);
	void boardChanged(const QString &boardName);

	static Settings *m_self;
	int m_lastGoodLexiconValue;
	int m_lastGoodBoardValue;
};

#endif
