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

#include <quackleio/dictfactory.h>
#include <quackleio/util.h>

#include "letterbox.h"
#include "customqsettings.h"
#include "quackersettings.h"
#include "letterboxsettings.h"
#include "lister.h"

Letterbox *Letterbox::m_self = 0;
Letterbox *Letterbox::self()
{
	return m_self;
}

Letterbox::Letterbox(QWidget *parent, QAction *preferencesAction, ListerDialog *listerDialog)
	: QMainWindow(parent), m_initializationChuu(false), m_modified(false), m_mistakeMade(false), m_listerDialog(listerDialog), m_pauseMs(0), m_keystrokes(0), m_numberIterator(0), m_preferencesAction(preferencesAction)
{
	m_self = this;

	createWidgets();
	createMenu();
	loadSettings();

	setCaption(tr("No List"));

	QTimer::singleShot(0, this, SLOT(finishInitialization()));
}

Letterbox::~Letterbox()
{
	saveSettings();
}

void Letterbox::closeEvent(QCloseEvent *closeEvent)
{
	closeEvent->setAccepted(tryToClose());
}

bool Letterbox::tryToClose()
{
	pause(true);

	if (m_modified)
	{
		switch (askToSave())
		{
		case QMessageBox::Save:
			qApp->processEvents();
			writeFile();

			// fall through

		case QMessageBox::Discard:
			return true;

		case QMessageBox::Cancel:
		default:
			return false;
		}
	}

	return true;
}

void Letterbox::finishInitialization()
{
	// ensure no dangling iterators
	m_clueResultsIterator = m_clueResults.begin();
	m_answersIterator = m_answers.begin();
	m_queryIterator = m_list.begin();	

	m_timer = new QTimer(this);
	connect(m_timer, SIGNAL(timeout()), this, SLOT(timeout()));

	m_clueResults.clear();

	loadFile();

	if (m_filename.isEmpty())
	{
		statusBar()->showMessage(tr("[Paused]") + QString(" ") + tr("Enjoy your letterboxings."));
		loadExampleList();
	}
	else
	{
		statusBar()->showMessage(tr("[Paused]") + QString(" ") + tr("Enjoy your letterboxings on %1.").arg(m_filename.right(m_filename.length() - m_filename.lastIndexOf("/") - 1)));
	}
}

void Letterbox::open()
{
	pause(true);

	if (m_modified)
	{
		switch (askToSave())
		{
		case QMessageBox::Save:
			writeFile();

		case QMessageBox::Discard:
			break;

		case QMessageBox::Cancel:
		default:
			return;
		}
	}
	
	QString defaultFilter = defaultStudyListFileFilter();
	QString filename = QFileDialog::getOpenFileName(this, tr("Choose Letterbox file to open"), getInitialDirectory(), studyListFileFilters(), &defaultFilter);
	if (!filename.isEmpty())
	{
		setInitialDirectory(filename);
		m_filename = filename;
		loadFile();
		saveSettings();
	}
}

void Letterbox::openParticularFile(const QString &filename)
{
	if (!filename.isEmpty())
	{
		if (m_modified)
		{
			switch (askToSave())
			{
				case QMessageBox::Save:
					writeFile();

				case QMessageBox::Discard:
					break;

				case QMessageBox::Cancel:
				default:
					return;
			}
		}

		m_filename = filename;
		loadFile();
	}
}

void Letterbox::setCaption(const QString &text)
{
	if (!text.isNull())
		m_ourCaption = text;

	setWindowTitle(QString("%1[*] - Quackle Letterbox").arg(m_ourCaption));
}

void Letterbox::setModified(bool modified)
{
	m_modified = modified;
	setWindowModified(m_modified);
}

bool Letterbox::dictCheck()
{
	if (!QuackleIO::DictFactory::querier()->isLoaded())
	{
		QMessageBox::critical(this, tr("No Dictionary Loaded - Quackle Letterbox"), tr("Please open a dictionary with Settings->Set Dictionary."));
		return false;
	}

	return true;
}

QMessageBox::StandardButton Letterbox::askToSave()
{
	return QMessageBox::warning(this, tr("Unsaved Results - Quackle Letterbox"), tr("There are unsaved results in the current Letterbox list. Save them?"), QMessageBox::Save | QMessageBox::Discard | QMessageBox::Cancel, QMessageBox::Save);
}

void Letterbox::generateList()
{
	pause(true);

	if (m_listerDialog)
	{
		m_listerDialog->show();
		m_listerDialog->raise();
	}
}

void Letterbox::loadFile()
{
	if (m_filename.isEmpty())
		return;

	QString filename(m_filename.right(m_filename.length() - m_filename.lastIndexOf("/") - 1));
	statusBar()->showMessage(tr("Loading %1...").arg(filename));
	qApp->processEvents();
	
	m_list.clear();
	m_answers.clear();
	m_clueResults.clear();

	QFile file(m_filename);
	if (!file.exists())
	{
		QMessageBox::critical(this, tr("Error Loading Letterbox List - Quackle Letterbox"), tr("Filename %1 does not exist").arg(m_filename));
		return;
	}

	if (!file.open(QIODevice::ReadOnly | QIODevice::Text))
	{
		QMessageBox::critical(this, tr("Error Loading Letterbox List - Quackle Letterbox"), tr("%1 cannot be opened.").arg(filename));
		return;
	}

	int startAt = 0;

	QTextStream stream(&file);
	QString line;
	SET_QTEXTSTREAM_TO_UTF8(stream);

	m_initializationChuu = true;

	bool firstLine = true;

	while (!stream.atEnd())
	{
		line = stream.readLine().trimmed().toUpper();

		if (firstLine)
		{
			if (line.left(10) == "\" RESUME: ")
				startAt = line.right(line.length() - 10).toInt();

			firstLine = false;
		}

		line.remove('#');

		QString letters = line;
		QString comment;

		int quoteMarkIndex = int(line.indexOf("\""));
		if (quoteMarkIndex >= 0)
		{
			letters = line.left(quoteMarkIndex).trimmed();
			comment = line.right(line.length() - quoteMarkIndex - 1).trimmed();
		}

		if (letters.isEmpty())
			continue;

		m_list += letters;

		m_clueResults.append(parseComment(comment));
		m_clueResults.last().clue = clueFor(letters);
	}

	file.close();

	if (!dictCheck())
		return;

	jumpTo(startAt);

	statusBar()->showMessage(tr("Loaded list `%1' of length %2.").arg(filename).arg(m_clueResults.count()));
	setCaption(filename);

	m_initializationChuu = false;
	setModified(false);

	prepareQuiz();
	pause(true);
}

void Letterbox::loadExampleList()
{
	// TODO; load an interesting list, like sixes in playability order
}

void Letterbox::writeFile()
{
	outputResults();

	setModified(false);
	statusBar()->showMessage(tr("Saved results to file."));
}

void Letterbox::jumpTo()
{
	pause(true);

	bool ok;
	int index = QInputDialog::getInt(this, tr("Jump to word - Quackle Letterbox"), tr("Index to which to jump:"), m_numberIterator + 1, 1, int(m_clueResults.count()), 1, &ok);
	if (ok)
	{
		jumpTo(index);
	}

	prepareQuiz();
	pause(true);
}

void Letterbox::jumpTo(int index)
{
	if (m_numberIterator != index)
		setModified(true);

	m_numberIterator = index;

	if (index >= m_clueResults.size())
	{
		m_answersIterator = m_answers.end();
		m_clueResultsIterator = m_clueResults.end();
		m_queryIterator = m_list.end();
		return;
	}

	while (m_answers.count() <= m_numberIterator)
	{
		m_answers.append(answersFor(m_list.at(m_answers.count())));
		m_clueResults[m_answers.count() - 1].setWordList(m_answers.at(m_answers.count() - 1));
	}

	m_clueResultsIterator = m_clueResults.begin();
	m_answersIterator = m_answers.begin();
	for (int i = 0; i < index; ++i)
	{
		++m_clueResultsIterator;
		++m_answersIterator;
	}
	(*m_clueResultsIterator).resetStats();

	m_queryIterator = m_list.begin();
	for (int i = 0; i < index + 1; ++i)
	{
		++m_queryIterator;
	}
}

ClueResult Letterbox::parseComment(const QString &comment)
{
	if (comment.isEmpty())
		return ClueResult();

#if (QT_VERSION >= QT_VERSION_CHECK(5, 14, 0))
	QStringList items = comment.split(" ", Qt::SkipEmptyParts);
#else
	QStringList items = comment.split(" ", QString::SkipEmptyParts);
#endif

	ClueResult ret;
		
	for (QStringList::iterator it = items.begin(); it != items.end(); ++it)
	{
		WordResult word;
		word.word = *it;

		++it;
		word.time = (*it).toInt();

		++it;
		word.keystrokes = (*it).toInt();
		word.missed = (word.time == 0);

		ret.words.append(word);
	}

	return ret;
}

void Letterbox::increment()
{
	jumpTo(m_numberIterator + 1);

    if (m_numberIterator >= m_clueResults.size())
    { 
		updateViews();
        listFinished();
		m_timer->stop();
        return;
    }

    prepareQuiz();
}   

void Letterbox::pause(bool paused)
{
	timerControl(paused);

	if (m_pauseAction->isChecked() != paused)
		m_pauseAction->setChecked(paused);

	if (!paused)
		m_lineEdit->setFocus();

	if (paused)
	{
		m_pauseTime.start();
		statusBar()->showMessage(tr("Paused on #%1 of %2 total.").arg(m_numberIterator + 1).arg(m_clueResults.count()));
	}
	else
	{
		m_pauseMs += m_pauseTime.elapsed();
		statusBar()->showMessage(tr("Resuming..."));
	}
}

void Letterbox::timerControl(bool paused)
{
	if (paused)
	{
		m_timer->stop();
	}
	else if (isInQuiz())
	{
		m_timer->setSingleShot(true);
		m_timer->start(timerLength());
	}
}

void Letterbox::markLastAsMissed()
{
	ClueResultList::iterator it(m_clueResultsIterator);
	(*(--it)).resetStats();

	if (!m_pauseAction->isChecked())
	{
		// reset clock
		timerControl(true);
		timerControl(false);
	}

	statusBar()->showMessage(tr("%1 marked as missed.").arg((*it).clue.clueString));
}

void Letterbox::skip()
{
	for (auto& it : m_clueResultsIterator->words)
	{
		it.missed = false;
		it.keystrokes = int(it.word.length());
		it.time = timerLength();
	}

	m_mistakeMade = false;
	increment();
}

void Letterbox::prepareQuiz()
{
	updateViews();
	
	m_submittedAnswers.clear();

	m_lineEdit->clear();

	m_mistakeMade = false;

	if (m_numberIterator == m_clueResults.count())
	{
		statusBar()->clearMessage();
		return;
	}

	m_lineEdit->setFocus();
	m_pauseAction->setChecked(false);

	statusBar()->showMessage(tr("Word #%1 of %2 total.").arg(m_numberIterator + 1).arg(m_clueResults.count()));

	timerControl(true);
	timerControl(false);

	m_time.start();
	m_keystrokes = 0;
	m_pauseMs = 0;
}

int Letterbox::timerLength()
{
	if (!isInQuiz())
		return 0;

	return LetterboxSettings::self()->msecWaitBase + LetterboxSettings::self()->msecWaitExtraPerSolution * int(m_answersIterator->count());
}

void Letterbox::listFinished()
{
	writeFile();
	statusBar()->showMessage(tr("List is finished, results saved."));
	m_timer->stop();
}

bool Letterbox::isInQuiz() const
{
    return m_clueResults.size() > 0 && m_numberIterator < m_clueResults.size();
}

void Letterbox::outputResults()
{
	QFile file(m_filename);
	 
	if (!file.open(QIODevice::WriteOnly | QIODevice::Text))
	{        
		QMessageBox::critical(this, tr("Error Writing File - Quackle Letterbox"), tr("Could not open %1 for writing.").arg(m_filename));        
		return;    
	}    

	QTextStream stream(&file);
	SET_QTEXTSTREAM_TO_UTF8(stream);

	if (m_numberIterator < m_clueResults.count())
		stream << "\" Resume: " << m_numberIterator << "\n";

	QStringList::iterator listIt = m_list.begin();
    for (const auto& it : m_clueResults)
	{
		stream << *listIt;

		if (it.words.count() > 0)
		{
			stream << " \"";
			for (const auto& word : it.words)
				stream << " " << word.word << " " << word.time << " " << word.keystrokes;
		}

		stream << "\n";

		++listIt;
	}

	file.close();

	if (LetterboxSettings::self()->newMissesFile)
	{
		QString missesFilename = m_filename + QString("-misses");
		QFile missesFile(missesFilename);
		 
		if (!file.open(QIODevice::WriteOnly | QIODevice::Text))
		{        
			QMessageBox::critical(this, tr("Error writing misses file"), tr("Could not open %1 for writing.").arg(missesFilename));        
			return;    
		}    

		QTextStream stream(&missesFile);
		SET_QTEXTSTREAM_TO_UTF8(stream);

    	for (const auto& it : m_clueResults)
		{
			for (const auto& word : it.words)
			{
				if (word.time == 0)
				{
					stream << it.clue.clueString << "\n";
					break;
				}
			}
		}

		missesFile.close();
	}
}

int Letterbox::chewScore(const QString & /*word*/, const QString & /*steal*/)
{
	int ret = 0;

	/*
	int wordpairs[26][26];
	for (int i = 0; i < 26; i++)
		for (int j = 0; j < 26; j++)
			wordpairs[i][j] = 0;

	if ((word.length() < 2) || (steal.length() < word.length()))
		return 0;

	for (unsigned int i = 0; i < word.length() - 1; i++)
	{
		int a = word.at(i).latin1() - 'A';
		int b = word.at(i + 1).latin1() - 'A';
		if ((a >= 0) && (a < 26) && (b >= 0) && (b < 26))
			wordpairs[a][b]++;
	}

	for (unsigned int i = 0; i < steal.length() - 1; i++)
	{
		int a = steal.at(i).latin1() - 'A';
		int b = steal.at(i + 1).latin1() - 'A';
		if ((a >= 0) && (a < 26) && (b >= 0) && (b < 26))
		{	
			if (wordpairs[a][b] == 0)
				ret++;
			else
				wordpairs[a][b]--;
		}
	}

*/
	return ret;
}

void Letterbox::updateDuringQuery()
{
	update();
}

Clue Letterbox::mathClue(const QString &clue)
{
	Dict::WordList clueWords = QuackleIO::DictFactory::querier()->query(clue);
	QString word = (*clueWords.begin()).word;

	int bestChew = 0;
	QString ret;

	for (int i = 0; i < word.length(); i++)
	{
		QString query = word.left(i) + word.right(word.length() - i - 1);
		Dict::WordList words = QuackleIO::DictFactory::querier()->query(query);
		for (const auto& it : words)
		{
			int chew = chewScore(it.word, word);
			if (chew > bestChew)
			{
				bestChew = chew;
				ret = it.word + " + " + word.at(i);
			}
		}
	}

	if (bestChew > 2)
		return Clue(ret);

	return Clue(arrangeLettersForUser(word));
}

QString Letterbox::alphagram(const QString &word) 
{
	return QuackleIO::Util::alphagram(word);
}

QString Letterbox::arrangeLettersForUser(const QString &word) 
{
	return QuackleIO::Util::arrangeLettersForUser(word);
}

Clue Letterbox::clueFor(const QString &word)
{
	if (word.isNull())
		return Clue(QString());

	if (LetterboxSettings::self()->mathMode)
		return mathClue(word);

	return Clue(arrangeLettersForUser(word));
}

Dict::WordList Letterbox::answersFor(const QString &word)
{
	Dict::WordList results;

	if (word.isNull())
		return results;

	// no updates during initialization of lists, but with extensions
	results = QuackleIO::DictFactory::querier()->query(word, (m_initializationChuu? Dict::Querier::None : Dict::Querier::CallUpdate) | Dict::Querier::WithExtensions);

	return results;
}

void Letterbox::mistakeDetector(const QString &text)
{
	m_keystrokes++;

	QString upperText(text.toUpper());

	if (upperText.length() == 0)
		return;

	for (const auto& it : *m_answersIterator)
	{
		if (LetterboxSettings::self()->spaceComplete)
		{
			if (upperText[0] == ' ')
			{
				if (!m_submittedAnswers.contains(it.word))
				{
					processAnswer(it.word);
					return;
				}
			}
		}
		else
		{
			if (it.word.startsWith(upperText) && !m_submittedAnswers.contains(it.word))
			{
				if (m_mistakeMade)
					statusBar()->clearMessage();

				if (LetterboxSettings::self()->autoCompleteLength > 0)  
					if (upperText.length() == LetterboxSettings::self()->autoCompleteLength)
						processAnswer(it.word);

				return;
			}
		}
	}

	m_mistakeMade = true;	
	
	statusBar()->showMessage(tr("MISTAKE DETECTED!"), /* show for 2 seconds */ 2000);
}

void Letterbox::lineEditReturnPressed()
{
	processAnswer(m_lineEdit->text());
}

void Letterbox::processAnswer(const QString &answer)
{
	if (!isInQuiz())
		return;

	QString upperAnswer(answer.toUpper());

	if (m_submittedAnswers.contains(upperAnswer))
	{
		statusBar()->showMessage(tr("You already submitted %1.").arg(upperAnswer));
		m_lineEdit->clear();
		return;
	}

	for (auto& it : m_clueResultsIterator->words)
	{
		if (it.word == upperAnswer)
		{
			it.missed = false;
			it.keystrokes = m_keystrokes;
			it.time = m_time.elapsed() - m_pauseMs;
		}
	}

	for (const auto& it : *m_answersIterator)
	{
		if (it.word == upperAnswer)
		{
			m_submittedAnswers.append(upperAnswer);
			m_lineEdit->clear();
			m_mistakeMade = false;

			if (m_submittedAnswers.count() >= (*m_answersIterator).count())
			{
				increment();
				return;
			}
			else
			{
				statusBar()->showMessage(tr("%1 is correct.").arg(upperAnswer));
				m_solutionsView->addSubmission(upperAnswer);
				break;
			}
		}
	}

	m_lineEdit->clear();
	m_keystrokes = 0;
	m_pauseMs = 0;
}

void Letterbox::timeout()
{
	increment();
}

void Letterbox::updateViews()
{
	m_solutionsView->setWords(m_answers.begin(), m_answersIterator);
	m_upcomingView->setWords(m_clueResultsIterator, m_clueResults.end());
}

void Letterbox::createMenu()
{
	QMenu *file = menuBar()->addMenu(tr("&File"));
	QMenu *go = menuBar()->addMenu(tr("&Go"));
	QMenu *settings = menuBar()->addMenu(tr("&Settings"));
	QMenu *help = menuBar()->addMenu(tr("&Help"));

	QAction *generateAction = new QAction(tr("&Generate new list..."), this);
	generateAction->setShortcut(tr("Ctrl+L"));
	file->addAction(generateAction);
	connect(generateAction, SIGNAL(activated()), this, SLOT(generateList()));

	QAction *openAction = new QAction(tr("&Open..."), this);
	openAction->setShortcut(tr("Ctrl+O"));
	file->addAction(openAction);
	connect(openAction, SIGNAL(activated()), this, SLOT(open()));

	QAction *saveAction = new QAction(tr("&Save"), this);
	saveAction->setShortcut(tr("Ctrl+S"));
	file->addAction(saveAction);
	connect(saveAction, SIGNAL(activated()), this, SLOT(writeFile()));

	file->addSeparator();

	QAction *printAction = new QAction(tr("&Print"), this);
	file->addAction(printAction);
	connect(printAction, SIGNAL(activated()), this, SLOT(print()));

	QAction *printStudyAction = new QAction(tr("&Print Study Sheet"), this);
	file->addAction(printStudyAction);
	connect(printStudyAction, SIGNAL(activated()), this, SLOT(printStudy()));

	file->addSeparator();

	QAction *quitAction = new QAction(tr("&Close"), this);
	quitAction->setShortcut(tr("Ctrl+W"));
	file->addAction(quitAction);
	connect(quitAction, SIGNAL(activated()), this, SLOT(close()));

	QAction *jumpAction = new QAction(tr("&Jump to..."), this);
	jumpAction->setShortcut(tr("Ctrl+J"));
	go->addAction(jumpAction);
	connect(jumpAction, SIGNAL(activated()), this, SLOT(jumpTo()));

	QAction *markAsMissedAction = new QAction(tr("Mark last as &missed"), this);
	markAsMissedAction->setShortcut(tr("Ctrl+M"));
	go->addAction(markAsMissedAction);
	connect(markAsMissedAction, SIGNAL(activated()), this, SLOT(markLastAsMissed()));

	QAction *skipAction = new QAction(tr("S&kip"), this);
	skipAction->setShortcut(tr("Ctrl+K"));
	go->addAction(skipAction);
	connect(skipAction, SIGNAL(activated()), this, SLOT(skip()));

	go->addSeparator();

	m_pauseAction = new QAction(tr("&Pause"), this);
	m_pauseAction->setShortcut(tr("Ctrl+P"));
	m_pauseAction->setCheckable(true);
	go->addAction(m_pauseAction);
	connect(m_pauseAction, SIGNAL(toggled(bool)), this, SLOT(pause(bool)));

	QAction *focusAction = new QAction(tr("&Focus entry widget"), this);
	focusAction->setShortcut(tr("Ctrl+F"));
	go->addAction(focusAction);
	connect(focusAction, SIGNAL(activated()), m_lineEdit, SLOT(setFocus()));

	settings->addAction(m_preferencesAction);

	settings->addSeparator();

	QAction *aboutAction = new QAction(tr("&About Letterbox"), this);
	help->addAction(aboutAction);
	connect(aboutAction, SIGNAL(activated()), this, SLOT(about()));
}

void Letterbox::createWidgets()
{
	QWidget *centralWidget = new QWidget(this);
	QVBoxLayout *centralLayout = new QVBoxLayout(centralWidget);

	setCentralWidget(centralWidget);

	WordView *solutionsView = new WordView(centralWidget);
	WordView *upcomingView = new WordView(centralWidget);

	m_lineEdit = new QLineEdit(centralWidget);
	m_lineEdit->setAlignment(Qt::AlignHCenter);
	m_lineEdit->setValidator(new InputValidator(m_lineEdit));
	connect(m_lineEdit, SIGNAL(returnPressed()), this, SLOT(lineEditReturnPressed()));
	connect(m_lineEdit, SIGNAL(textChanged(const QString &)), this, SLOT(mistakeDetector(const QString &)));

	centralLayout->addWidget(solutionsView);
	centralLayout->addWidget(m_lineEdit);
	centralLayout->addWidget(upcomingView);

	centralLayout->setStretchFactor(solutionsView, 3);

	m_solutionsView = solutionsView;
	m_upcomingView = upcomingView;
}

void Letterbox::print()
{
	pause(true);

	QString filename = QFileDialog::getSaveFileName(this, tr("Choose HTML file to which to save pretty word list"), m_filename + ".html", "*.html");

	if (filename.isEmpty())
		return;

	QFile file(filename);
	 
	if (!file.open(QIODevice::WriteOnly | QIODevice::Text))
	{        
		QMessageBox::critical(this, tr("Error Writing File - Quackle Letterbox"), tr("Could not open %1 for writing.").arg(filename));        
		return;    
	}

	HTMLRepresentation printer;

	bool wasModified = m_modified;
	int previousNumber = m_numberIterator;
	statusBar()->showMessage(tr("Generating HTML..."));
	jumpTo(int(m_clueResults.size() - 1));

	printer.setWords(m_answers.begin(), m_answers.end());

	jumpTo(previousNumber);
	setModified(wasModified);

	QTextStream stream(&file);
	SET_QTEXTSTREAM_TO_UTF8(stream);
	stream << printer.html() << "\n";

	file.close();

	statusBar()->showMessage(tr("%1 written.").arg(filename));
}

void Letterbox::printStudy()
{
	pause(true);

	QString filename = QFileDialog::getSaveFileName(this, tr("Choose file to which to save study sheet"), m_filename + "-study");

	if (filename.isEmpty())
		return;

	QFile file(filename);
	 
	if (!file.open(QIODevice::WriteOnly | QIODevice::Text))
	{        
		QMessageBox::critical(this, tr("Error Writing File - Quackle Letterbox"), tr("Could not open %1 for writing.").arg(filename));        
		return;    
	}

	bool wasModified = m_modified;
	int previousNumber = m_numberIterator;
	statusBar()->showMessage(tr("Generating study sheet..."));
	jumpTo(int(m_clueResults.size() - 1));

	QTextStream stream(&file);
	SET_QTEXTSTREAM_TO_UTF8(stream);
	stream << generateStudySheet(m_answers.begin(), m_answers.end()) << "\n";

	file.close();

	jumpTo(previousNumber);
	setModified(wasModified);

	statusBar()->showMessage(tr("%1 written.").arg(filename));
}

QString Letterbox::generateStudySheet(Dict::WordListList::ConstIterator start, Dict::WordListList::ConstIterator end)
{
	QString ret;

	int prevLengthOfExtensions = LetterboxSettings::self()->lengthOfExtensions;
	LetterboxSettings::self()->lengthOfExtensions = 1;

	for (Dict::WordListList::ConstIterator it = start; it != end; ++it) 
	{
		int length = int((*it).front().word.length());
		QString pad = "  ";
		for (int i = 0; i < length; ++i)
			pad += " ";

		ret += arrangeLettersForUser((*it).front().word) + " ";
		bool first = true;
		for (Dict::WordList::ConstIterator wit = (*it).begin(); wit != (*it).end(); ++wit)
		{
			if (first)
			{
				first = false;
			}
			else
			{
				ret += pad;
			}

			ret += HTMLRepresentation::prettyExtensionList((*wit).getFrontExtensionList(), false);
			ret += " ";
			ret += (*wit).word;
			ret += " ";
			ret += HTMLRepresentation::prettyExtensionList((*wit).getBackExtensionList(), false);
			ret += "\n";
		}
	}

	LetterboxSettings::self()->lengthOfExtensions = prevLengthOfExtensions;
	
	return ret;
}

void Letterbox::about()
{
	QMessageBox::about(this, tr("About Quackle Letterbox"), "<p><b>Letterbox</b> is a lexical study tool, that is now part of Quackle.</p><p>Copyright 2005-2007 by<ul><li>John O'Laughlin &lt;olaughlin@gmail.com&gt;</li><li>Jason Katz-Brown &lt;jasonkatzbrown@gmail.com&gt;</li></ul>");
}

void Letterbox::saveSettings()
{
	LetterboxSettings::self()->writeSettings();

	CustomQSettings settings;

	settings.setValue("quackle/letterbox/most-recent-list", m_filename);
	settings.setValue("quackle/letterbox/window-size", size());
}

void Letterbox::loadSettings()
{
	LetterboxSettings::self()->readSettings();

	CustomQSettings settings;
	m_filename = settings.value("quackle/letterbox/most-recent-list", QString("")).toString();
	resize(settings.value("quackle/letterbox/window-size", QSize(800, 600)).toSize());
}

//////////

WordResult::WordResult()
{
	resetStats();
}

WordResult::WordResult(QString w)
{
	word = w;
	resetStats();
}

void WordResult::resetStats()
{
	missed = true;
	keystrokes = 0;
	time = 0;
}

//////////

ClueResult::ClueResult()
{
}

ClueResult::ClueResult(const QString &newClue)
	: clue(newClue)
{
}

void ClueResult::setWordList(Dict::WordList answers)
{
	for (const auto& it : answers)
	{
		bool isAnswerInOurWords = false;
		for (const auto& wrIt : words)
			isAnswerInOurWords = isAnswerInOurWords || (it.word == wrIt.word);

		if (!isAnswerInOurWords)
			words.append(WordResult(it.word));	
	}
}

void ClueResult::resetStats()
{
	for (auto& wrIt : words)
		wrIt.resetStats();
}

Clue::Clue()
{
}

Clue::Clue(const QString &newClueString)
	: clueString(newClueString)
{
}

//////////

InputValidator::InputValidator(QObject *parent) : QValidator(parent)
{
}

InputValidator::~InputValidator()
{
}

QValidator::State InputValidator::validate(QString & /* input */, int &) const
{
	return Acceptable;
}

/////////

WordView::WordView(QWidget *parent)
	: QTextEdit(parent)
{
	m_shownSolutions = 16;
	m_shownClues = 30;
	m_tablePadding = 0;
	m_tableSpacing = 0;

	setReadOnly(true);

	setVerticalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
	setHorizontalScrollBarPolicy(Qt::ScrollBarAlwaysOff);

	// TODO(jasonkb) background/foreground colors
	//setPaper(QBrush(QColor(LetterboxSettings::self()->backgroundColor)));
	//setColor(LetterboxSettings::self()->foregroundColor);
}

WordView::~WordView()
{
}

void WordView::htmlUpdated(ContentType type)
{
	setText(html());

	switch (type)
	{
	case Content_Upcoming:
		scrollToAnchor("top");
		break;

	case Content_Solutions:
		QTextCursor cursor = textCursor();
		cursor.movePosition(QTextCursor::End);
		setTextCursor(cursor);
		ensureCursorVisible();
		break;
	}
}

////////

HTMLRepresentation::HTMLRepresentation()
	: m_shownSolutions(INT_MAX), m_shownClues(INT_MAX), m_tablePadding(4), m_tableSpacing(0)
{
}

HTMLRepresentation::~HTMLRepresentation()
{
}

void HTMLRepresentation::htmlUpdated(ContentType /* type */ )
{
}

void HTMLRepresentation::setHTML(const QString &text, ContentType type)
{
	m_html = text;
	htmlUpdated(type);
}

QString HTMLRepresentation::html()
{
	return QString("<html>\n<head>\n<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\">\n</head>\n<body>") +
			m_html +
			QString("</body></html>");
}

void HTMLRepresentation::setWords(ClueResultList::ConstIterator start, ClueResultList::ConstIterator end, bool revers)
{
	QString html("<a name=top>");
	
	int shown = 0;

	html += "<center>";

	if (revers)
	{
		ClueResultList::ConstIterator it = end;
		while (shown < m_shownClues)
		{
			if (it == start)
				break;

			--it;

			if (shown == 0)
				html += "<font size=6>";
			else
				html += "<font size=6>";

			html += htmlForPlainWord((*it).clue.clueString) + "<br>";

			html += "</font>";

			shown++;
		}
	}
	else
	{
		for (ClueResultList::ConstIterator it = start; (it != end) && (shown < 30); ++it) 
		{
			if (shown == 0)
				html += "<font size=6>";
			else
				html += "<font size=6>";

			html += htmlForPlainWord((*it).clue.clueString) + "<br>";

			if (shown == 0)
				html += "</font>";

			shown++;
		}
	}

	html += "</center>";

	setHTML(html, Content_Upcoming);
}

void HTMLRepresentation::setWords(Dict::WordListList::ConstIterator start, Dict::WordListList::ConstIterator end, bool revers)
{
    Dict::WordListList::ConstIterator newStart = end;
	for (int i = 0; i < m_shownSolutions; i++)
		if (newStart != start)
			newStart--;
	start = newStart;

	QString html("<a name=top>");

	if (revers)
	{
		Dict::WordListList::ConstIterator it = end;
		while (true)
		{
			if (it == start)
				break;

			--it;
			html += htmlForWordList(*it);
		}
	}
	else
	{
		for (Dict::WordListList::ConstIterator it = start; it != end; ++it)
			html += htmlForWordList(*it);
	}

	setHTML(html, Content_Solutions);
}

QString HTMLRepresentation::htmlForWordList(const Dict::WordList &wordList)
{
	if (wordList.isEmpty())
		return QString();

	QString html = QString("<center><table border=1 cellspacing=%1 cellpadding=%2>").arg(m_tableSpacing).arg(m_tablePadding);

	bool hasFrontExtensionColumn = false;
	bool hasBackExtensionColumn = false;

	for (const auto& it : wordList)
	{
		if (!(tableOfExtensions(it.getFrontExtensionList()).isEmpty()))		
			hasFrontExtensionColumn = true;
		if (!(tableOfExtensions(it.getBackExtensionList()).isEmpty()))		
			hasBackExtensionColumn = true;
	}

	for (const auto& it : wordList)
	{
		html += "<tr>";

		if (hasFrontExtensionColumn)
			html += "<td align=right>" + tableOfExtensions(it.getFrontExtensionList()) + "</td>";

		html += "<td align=center>" + htmlForSolution(it) + "</td>";

		if (hasBackExtensionColumn)
			html += "<td align=left>" + tableOfExtensions(it.getBackExtensionList()) + "</td>";

		html += "</tr>";
	}
	html += "</table></center><font size=\"-7\"><br></font>\n";
     	
	return html;
}

QString HTMLRepresentation::htmlForPlainWord(const QString &word)
{
	QString fontArgs("color=\"%1\"");

	QString html("<font " + fontArgs.arg(LetterboxSettings::self()->foregroundColor) + ">%1</font>");

	return html.arg(word);
}

QString HTMLRepresentation::htmlForSolution(const Dict::Word &word)
{
	QString fontArgs("size=\"+5\" color=\"%1\"");

	QString html("<b><font " + fontArgs.arg(word.british? LetterboxSettings::self()->sowpodsColor : LetterboxSettings::self()->foregroundColor) + ">%1</font></b>");
	
	return html.arg(word.word + (word.british? "#" : ""));
}

QString HTMLRepresentation::tableOfExtensions(const Dict::ExtensionList &list)
{
	QString html;

	bool first = true;

	for (int i = 1; i <= LetterboxSettings::self()->lengthOfExtensions; ++i)
	{
		Dict::ExtensionList extensions(Dict::Word::extensionsByLength(i, list));
		if (extensions.count() > 0)
		{
			QString extensionHtml = HTMLRepresentation::prettyExtensionList(extensions);
			
			if (!extensionHtml.isEmpty())
			{
				if (!first)
					html += "<br>";
				else
					first = false;
			
				if (i == 1)
				{
					html += "<font size=5>";
				}
				else
				{
					html += "<font size=3>";
				}

				html += htmlForPlainWord(extensionHtml);

				html += "</font>";
			}
		}
	}

	return html;
}

void HTMLRepresentation::addSubmission(const QString &submission)
{
	QString newHtml(html());
	QString item(QString("%1").arg(submission));

	if (newHtml.endsWith("</p>"))
		newHtml.insert(newHtml.length() - 4, QString("<br>") + item);
	else
		newHtml += QString("<center><h2>%1</h2><p>%2</p>").arg(qApp->tr("Correct Answers")).arg(item);

	setHTML(newHtml, Content_Solutions);
}

QString HTMLRepresentation::prettyExtensionList(const Dict::ExtensionList &list, bool make_html)
{
	QString ret;

	int extensionChars = 0;
	QString space = make_html? "&nbsp;" : " ";

	for (Dict::ExtensionList::ConstIterator it = list.begin(); it != list.end(); ++it)
	{
		QString fontArgs("color=\"%1\"");

		QString color = (*it).british? LetterboxSettings::self()->sowpodsColor : LetterboxSettings::self()->foregroundColor;
  
		QString html;
		
		if (make_html)
			html += "<font " + fontArgs.arg(color) + ">%1</font>";
		else
			html += "%1";

		QString wordHtml;

		if (LetterboxSettings::self()->numExtensionChars > 0)
		{
			if (extensionChars >= LetterboxSettings::self()->numExtensionChars)
			{
				int numExtensions = int(list.size());
				
				ret += QString("...%1(%2)").arg(space).arg(numExtensions);
				return ret;
			}
		}

		wordHtml += (*it).word;
		extensionChars += (*it).word.length();

		// this is super clumsy, working around the fact that it doesn't show leading nbsps
		bool displayableTokensRemain = false;
	
		for (Dict::ExtensionList::ConstIterator lookAhead = it; lookAhead != list.end(); ++lookAhead)
		{
			if (lookAhead == it)
				continue;

			displayableTokensRemain = true;
		}

		if (displayableTokensRemain)
			wordHtml += space;
		
		ret += html.arg(wordHtml);
		
		if (!displayableTokensRemain)
			return ret;
	}

	return ret;
}

QString Letterbox::getInitialDirectory() const
{
	return m_initialDirectory.isEmpty()? QDir::homePath() : m_initialDirectory;
}

void Letterbox::setInitialDirectory(const QString &filename)
{
	QFileInfo file(filename);
	m_initialDirectory = file.path();
}

QString Letterbox::studyListFileFilters() const
{
	return tr("Letterbox files (%1);;All files (*)").arg("*.letterbox");
}

QString Letterbox::defaultStudyListFileFilter() const
{
	return "*.letterbox";
}

