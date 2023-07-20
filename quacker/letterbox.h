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

#ifndef QUACKER_LETTERBOX_H
#define QUACKER_LETTERBOX_H

#include <QElapsedTimer>
#include <QMainWindow>
#include <QMessageBox>
#include <QValidator>
#include <QTextEdit>
#include <QTime>

#include <quackleio/dict.h>

class QAction;
class QLineEdit;
class QTimer;
class Letterbox;
class HTMLRepresentation;
class ListerDialog;

class WordResult
{
public:
	WordResult();
	WordResult(QString w);
	void resetStats();
	QString word;
	qint64 time;
	bool missed;
	int keystrokes;
};

typedef QList<WordResult> WordResultList;

class Clue
{
public:
	Clue();
	Clue(const QString &newClueString);
	QString clueString;
};

class ClueResult
{
public:
	ClueResult();
	ClueResult(const QString &newClue);
	Clue clue;

	void setWordList(Dict::WordList answers);	
	WordResultList words;		

	void resetStats();
};

typedef QList<ClueResult> ClueResultList;

class InputValidator : public QValidator
{
Q_OBJECT

public:
    InputValidator(QObject *parent = 0);
    ~InputValidator();

	QValidator::State validate(QString&, int&) const;
};

class Letterbox : public QMainWindow
{
Q_OBJECT

public:
	Letterbox(QWidget *parent, QAction *preferencesAction, ListerDialog *listerDialog);
	~Letterbox();

	static Letterbox *self();

	bool tryToClose();
	void closeEvent(QCloseEvent *closeEvent);

	// make alphagram
    QString alphagram(const QString &word);

	// make pattern of letters user wants
	QString arrangeLettersForUser(const QString &word);

	// make an anahook expression where one letter is added (if not possible, return alphagram)
	Clue mathClue(const QString &word);

	// Letterbox clue giver
	Clue clueFor(const QString &word);

	// calculate an anahook's score as an anagram steal with John Chew's system
	int chewScore(const QString &word, const QString &steal);

	void updateDuringQuery();

public slots:
	void open();
	void openParticularFile(const QString &filename);
	void generateList();
	void writeFile();
	void print();
	void printStudy();
	void about();

	void loadFile();

	// call timerControl, and tell user about it
	void pause(bool paused);

	void markLastAsMissed();
	void skip();

	void increment();
	void prepareQuiz();

	// only adjusts iterators; does not prepareQuiz()
	void jumpTo(int index);

	// asks user where to jump to
	void jumpTo();

protected slots:
	void finishInitialization();
	void lineEditReturnPressed();
	void mistakeDetector(const QString &text);
	void timeout();

	void setCaption(const QString &text = QString());
	void setModified(bool modified);

protected:
	void saveSettings();

	// use this to control timer!
	void timerControl(bool paused);

	// also calls createWidgets()
	void loadSettings();

	// opens a word list for new users
	void loadExampleList();

	// all anagrams of letters
	Dict::WordList answersFor(const QString &word);

	// if answer correct, tell user and keep note.
	// if user has given all correct answers, increment()
	void processAnswer(const QString &answer);

	void updateViews();

	// let user know he can stick a fork in this list.
	// perhaps later record the history of his misery?
	void listFinished();

	// returns true if there is an alphagram being shown to be answered.
	bool isInQuiz() const;

	// outputs results to the same file words were loaded from
	void outputResults();

	QString generateStudySheet(Dict::WordListList::ConstIterator start, Dict::WordListList::ConstIterator end);

	// returns length of time user gets to answer
	// based on m_msecWaitBase and m_msecWaitExtraPerSolution
	int timerLength();

	bool dictCheck();

	static Letterbox *m_self;

	// returns 0 for save, 1 for discard, 2 for cancel
	QMessageBox::StandardButton askToSave();

	// used to know when to update UI when querying anagrammer
	bool m_initializationChuu;

private:
	QString m_filename;
	bool m_modified;
	QString m_ourCaption;

	QLineEdit *m_lineEdit;
	bool m_mistakeMade;

	ListerDialog *m_listerDialog;

	QTimer *m_timer;
	QElapsedTimer m_time;
	QElapsedTimer m_pauseTime;
	int m_pauseMs;
	int m_keystrokes;

	QStringList m_list;
	Dict::WordListList m_answers;

	Dict::WordListList::iterator m_answersIterator;
	QStringList::iterator m_queryIterator;

	int m_numberIterator;

	ClueResultList m_clueResults;
	ClueResultList::iterator m_clueResultsIterator;

	// returns empty clue result if empty comment
	ClueResult parseComment(const QString &comment);

	QStringList m_submittedAnswers;

	HTMLRepresentation *m_solutionsView;
	HTMLRepresentation *m_upcomingView;

	QAction *m_pauseAction;
	QAction *m_preferencesAction;

	void createMenu();
	void createWidgets();

	QString getInitialDirectory() const;
	void setInitialDirectory(const QString &filename);
	QString m_initialDirectory;
	QString studyListFileFilters() const;
	QString defaultStudyListFileFilter() const;
};

class HTMLRepresentation
{
public:
	HTMLRepresentation();
	virtual ~HTMLRepresentation();

	// set solutions!
	void setWords(Dict::WordListList::ConstIterator start, Dict::WordListList::ConstIterator end, bool revers = false);

	// set upcoming!
	void setWords(ClueResultList::ConstIterator start, ClueResultList::ConstIterator end, bool revers = false);

	// add something to list at bottom!
	void addSubmission(const QString &submission);

	enum ContentType { Content_Solutions, Content_Upcoming };

	virtual void htmlUpdated(ContentType type);

	void setHTML(const QString &text, ContentType type);
	QString html();

	static QString prettyExtensionList(const Dict::ExtensionList &list, bool html = true);

protected:
	QString htmlForPlainWord(const QString &word);
	QString htmlForSolution(const Dict::Word &word);
	QString htmlForWordList(const Dict::WordList &wordList);
	QString tableOfExtensions(const Dict::ExtensionList &list);

	int m_shownSolutions;
	int m_shownClues;
	int m_tablePadding;
	int m_tableSpacing;

	QString m_html;
};

class WordView : public QTextEdit, public HTMLRepresentation
{
Q_OBJECT

public:
	WordView(QWidget *parent = 0);
	virtual ~WordView();

	virtual void htmlUpdated(ContentType type);
};

#endif
