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

#ifndef QUACKLE_LISTER_H
#define QUACKLE_LISTER_H

#include <qdialog.h>
#include <qframe.h>

#include <quackleio/dict.h>

class QCheckBox;
class QLabel;
class QLineEdit;
class QListWidget;
class QListWidgetItem;
class QPushButton;
class QSettings;
class QSpinBox;
class QVBoxLayout;

class Filter;

class ListerDialog : public QDialog
{
Q_OBJECT

public:
	enum ListerFlags { FullLister = 0x0000, IgnoreBritishness = 0x0001, ProbabilityInsteadOfPlayability = 0x0002, NothingToReturn = 0x0004 };

	ListerDialog(QWidget *parent, const QString &settingsGroup, const QString &appName = QString(), int flags = FullLister);
	~ListerDialog();

	// use this for modal running! settingsGroup is like "letterbox"
	static QString run(QWidget *parent, const QString &settingsGroup, const QString &appName = QString(), int flags = FullLister);

	Dict::WordList &wordList();
	void setWordList(Dict::WordList list);

	// map of alphagrams to words
	QMap<QString, Dict::WordList> anagramMap();

	QString writeList(bool alphagrams);
	QString filename() { return m_filename; }

	int flags() { return m_flags; }

	static QSpinBox *makeSpinBox(int minimum, int maximum, int singleStep);

public slots:
	void setQuery(const QString &query);
	void queryGo();
	void setRemoveSowpods(bool on);

	// makes filter widget and save/load settings exactly once
	void showFilter(const QString &filterName);
	void showFilter(QListWidgetItem *item);

	void chooseFilename();
	void filenameChanged(const QString &);

	void clear();
	void openFile();

	void writeButtonClicked();
	void writeNormalButtonClicked();
	void studyButtonClicked();

	QString windowTitleWithAppName(const QString &windowTitle);

	void accept();

protected:
	Dict::WordList m_wordList;
	QString m_settingsGroup;
	QString m_appName;
	int m_flags;

	void saveSettings();
	void loadSettings();

	void resetFocus();
	void populateListBox();

private:

	// left side
	QLineEdit *m_queryEdit;
	QPushButton *m_queryButton;
	QPushButton *m_closeButton;
	QLabel *m_numResultsLabel;
	QCheckBox *m_sowpodsChecker;
	QCheckBox *m_buildChecker;

	// right side
	QListWidget *m_filtersBox;
	QVBoxLayout *m_filtersLayout;
	Filter *m_currentFilter;
	QPushButton *m_applyButton;

	// bottom
	QLineEdit *m_filenameEdit;
	QPushButton *m_writeButton;
	QPushButton *m_writeNormalButton;
	QPushButton *m_studyThisButton;
	QListWidget *m_listBox;

	QString m_filename;
};

class Filter : public QFrame
{
Q_OBJECT

public:
	Filter(ListerDialog *dialog);

public slots:
	virtual void apply();
	virtual void saveSettings(QSettings *settings);
	virtual void loadSettings(QSettings *settings);

protected:
	ListerDialog *m_dialog;
	QVBoxLayout *m_vbox;
};

class RegexFilter : public Filter
{
Q_OBJECT

public:
	RegexFilter(ListerDialog *dialog);

public slots:
	virtual void apply();

private:
	QLineEdit *m_lineEdit;
};

class PlayabilityFilter : public Filter
{
Q_OBJECT

public:
	PlayabilityFilter(ListerDialog *dialog);

public slots:
	virtual void apply();
	virtual void saveSettings(QSettings *settings);
	virtual void loadSettings(QSettings *settings);

private:
	QSpinBox *m_minRankSpinner;
	QSpinBox *m_maxRankSpinner;
};

class NumAnagramsFilter : public Filter
{
Q_OBJECT

public:
	NumAnagramsFilter(ListerDialog *dialog);

public slots:
	virtual void apply();
	virtual void saveSettings(QSettings *settings);
	virtual void loadSettings(QSettings *settings);

private:
	QSpinBox *m_twlAnagramsSpinner;
	QSpinBox *m_oswOnlyAnagramsSpinner;
};

class KeepBritishFilter : public Filter
{
Q_OBJECT

public:
	KeepBritishFilter(ListerDialog *dialog);

public slots:
	virtual void apply();
};

#endif
