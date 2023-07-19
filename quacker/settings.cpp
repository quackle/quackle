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

#include <iostream>
#include <sstream>

#include <QtWidgets>

#ifdef Q_OS_MAC
#include <CoreFoundation/CoreFoundation.h>
#endif // Q_OS_MAC

#include "alphabetparameters.h"
#include "board.h"
#include "boardparameters.h"
#include "computerplayercollection.h"
#include "datamanager.h"
#include "game.h"
#include "lexiconparameters.h"
#include "rack.h"
#include "strategyparameters.h"

#include "quackleio/flexiblealphabet.h"
#include "quackleio/util.h"

#include "settings.h"
#include "boardsetupdialog.h"
#include "customqsettings.h"
#include "graphicalboard.h"
#include "lexicondialog.h"

using namespace std;

Settings *Settings::m_self = 0;
Settings *Settings::self()
{
	return m_self;
}

Settings::Settings(QWidget *parent)
	: QWidget(parent), m_lexiconNameCombo(0), m_alphabetNameCombo(0), m_themeNameCombo(0)
{
	m_self = this;
	QDir directory = QFileInfo(qApp->arguments().at(0)).absoluteDir();

 #ifdef Q_OS_MAC
	if (CFBundleGetMainBundle())
	{
		 CFURLRef dataUrlRef = CFBundleCopyResourceURL(CFBundleGetMainBundle(), CFSTR("data"), NULL, NULL);
		 if (dataUrlRef)
		 {
		 	 CFStringRef macPath = CFURLCopyFileSystemPath(dataUrlRef, kCFURLPOSIXPathStyle);
			 size_t sizeOfBuf = CFStringGetMaximumSizeOfFileSystemRepresentation(macPath);
			 char* buf = (char*) malloc(sizeOfBuf);

			 CFStringGetFileSystemRepresentation(macPath, buf, sizeOfBuf);
		 	 directory = QDir(buf);
		 	 directory.cdUp();

			 free(buf);
			 CFRelease(dataUrlRef);
			 CFRelease(macPath);
		 }
	}
 #endif // Q_OS_MAC

	if (QFile::exists("data"))
		m_appDataDir = "data";
	else if (QFile::exists("../data"))
		m_appDataDir = "../data";
	else if (QFile::exists("Quackle.app/Contents/data"))
		m_appDataDir = "Quackle.app/Contents/data";
	else
	{
		if (!directory.cd("data") || !directory.cd("../data"))
			QMessageBox::critical(0, tr("Error Initializing Data Files - Quacker"), tr("<p>Could not open data directory. Quackle will be useless. Try running the quacker executable with quackle/quacker/ as the current directory.</p>"));
		m_appDataDir = directory.absolutePath();
	}
	m_userDataDir = QStandardPaths::writableLocation(QStandardPaths::AppLocalDataLocation);
	QDir qdir(m_userDataDir);
	qdir.mkpath("lexica");
}

void Settings::createGUI()
{
	if (m_lexiconNameCombo != 0)
		return;

	QGridLayout *layout = new QGridLayout(this);
	QMargins margins = layout->contentsMargins();
	margins.setBottom(0); // let logo image flow off of bottom
	layout->setContentsMargins(margins);
	layout->setVerticalSpacing(2);

	m_lexiconNameCombo = new QComboBox;
	connect(m_lexiconNameCombo, SIGNAL(activated(int)), this, SLOT(lexiconChanged(int)));

	populateComboFromFilenames(m_lexiconNameCombo, "lexica", ".dawg", "lexicon");

	QLabel *lexiconNameLabel = new QLabel(tr("&Lexicon:"));
	lexiconNameLabel->setBuddy(m_lexiconNameCombo);
	m_editLexicon = new QPushButton(tr("Edit..."));
	m_editLexicon->setMaximumWidth(60);
	connect(m_editLexicon, SIGNAL(clicked()), this, SLOT(editLexicon()));

	m_alphabetNameCombo = new QComboBox;
	connect(m_alphabetNameCombo, SIGNAL(activated(int)), this, SLOT(alphabetChanged(int)));

	populateComboFromFilenames(m_alphabetNameCombo, "alphabets", ".quackle_alphabet", "");

	QLabel *alphabetNameLabel = new QLabel(tr("&Alphabet:"));
	alphabetNameLabel->setBuddy(m_alphabetNameCombo);
	m_editAlphabet = new QPushButton(tr("Edit..."));
	m_editAlphabet->setMaximumWidth(60);
	connect(m_editAlphabet, SIGNAL(clicked()), this, SLOT(editAlphabet()));

	m_themeNameCombo = new QComboBox;
	connect(m_themeNameCombo, SIGNAL(activated(int)), this, SLOT(themeChanged(int)));

	populateComboFromFilenames(m_themeNameCombo, "themes", ".ini", "");

	QLabel *themeNameLabel = new QLabel(tr("&Theme:"));
	themeNameLabel->setBuddy(m_themeNameCombo);
	m_editTheme = new QPushButton(tr("Edit..."));
	m_editTheme->setMaximumWidth(60);
	connect(m_editTheme, SIGNAL(clicked()), this, SLOT(editTheme()));

	m_boardNameCombo = new QComboBox;
	connect(m_boardNameCombo, SIGNAL(activated(int)), this, SLOT(boardChanged(int)));

	populateComboFromFilenames(m_boardNameCombo, "boards", "", "board");

	QLabel *boardNameLabel = new QLabel(tr("&Board:"));
	boardNameLabel->setBuddy(m_boardNameCombo);
	m_editBoard = new QPushButton(tr("Edit..."));
	m_editBoard->setMaximumWidth(60);
	connect(m_editBoard, SIGNAL(clicked()), this, SLOT(editBoard()));

	m_buildGaddag = new QPushButton(tr("Build lexicon database..."));
	connect(m_buildGaddag, SIGNAL(clicked()), this, SLOT(buildGaddag()));

	m_buildGaddagLabel = new QLabel();
	m_buildGaddagLabel->setWordWrap(true);
	m_logoLabel = new QLabel();
	m_logoLabel->setAlignment(Qt::AlignTop | Qt::AlignHCenter);
	m_copyrightLabel = new QLabel();
	m_copyrightLabel->setWordWrap(true);
	m_separatorLabel = new QLabel();
	m_separatorLabel->setFrameStyle(QFrame::HLine | QFrame::Sunken);

	layout->addWidget(lexiconNameLabel, 0, 0, Qt::AlignRight);
	layout->addWidget(m_lexiconNameCombo, 0, 1);
	layout->addWidget(m_editLexicon, 0, 2);
	layout->addWidget(alphabetNameLabel, 1, 0, Qt::AlignRight);
	layout->addWidget(m_alphabetNameCombo, 1, 1);
	// layout->addWidget(m_editAlphabet, 1, 2);
	layout->addWidget(themeNameLabel, 2, 0, Qt::AlignRight);
	layout->addWidget(m_themeNameCombo, 2, 1);
	// layout->addWidget(m_editTheme, 2, 2);
	layout->addWidget(boardNameLabel, 3, 0, Qt::AlignRight);
	layout->addWidget(m_boardNameCombo, 3, 1);
	layout->addWidget(m_editBoard, 3, 2);
	layout->addWidget(m_buildGaddag, 4, 1);
	layout->addWidget(m_buildGaddagLabel, 5, 1);
	layout->addWidget(m_separatorLabel, 6, 0, 1, -1);
	layout->addWidget(m_copyrightLabel, 7, 0, 1, -1, Qt::AlignTop);
	layout->addWidget(m_logoLabel, 8, 0, 1, -1, Qt::AlignTop | Qt::AlignHCenter);

	layout->setColumnMinimumWidth(3, 0);
	layout->setColumnStretch(3, 1);
	layout->setRowStretch(8, 1);


	load();
}

void Settings::load()
{
	m_lexiconNameCombo->setCurrentIndex(m_lexiconNameCombo->findText(QString::fromUtf8(QUACKLE_LEXICON_PARAMETERS->lexiconName().c_str())));
	if (m_lexiconNameCombo->currentIndex() == -1)
		m_lexiconNameCombo->setCurrentIndex(m_lexiconNameCombo->findText(QString::fromUtf8(QUACKLE_LEXICON_PARAMETERS->lexiconName().c_str()) + "*"));
	m_lastGoodLexiconValue = m_lexiconNameCombo->currentIndex();
	m_alphabetNameCombo->setCurrentIndex(m_alphabetNameCombo->findText(QString::fromUtf8(QUACKLE_ALPHABET_PARAMETERS->alphabetName().c_str())));
	m_themeNameCombo->setCurrentIndex(m_themeNameCombo->findText(m_themeName));
	m_boardNameCombo->setCurrentIndex(m_boardNameCombo->findText(QuackleIO::Util::uvStringToQString(QUACKLE_BOARD_PARAMETERS->name())));
	m_lastGoodBoardValue = m_boardNameCombo->currentIndex();
	//m_logoLabel->setPixmap(QPixmap());
	m_copyrightLabel->setText(QString::fromUtf8(QUACKLE_LEXICON_PARAMETERS->copyrightString().c_str()));
	setGaddagLabel();
}

void Settings::preInitialize()
{
	// load computer players
	QUACKLE_DATAMANAGER->setComputerPlayers(Quackle::ComputerPlayerCollection::fullCollection());
}

void Settings::initialize()
{
	CustomQSettings settings;

	QUACKLE_DATAMANAGER->setAppDataDirectory(m_appDataDir.toStdString());
	QUACKLE_DATAMANAGER->setUserDataDirectory(m_userDataDir.toStdString());

	QString lexiconName = settings.value("quackle/settings/lexicon-name", QString("nwl18")).toString();

	// Handle Collins update.
	if (lexiconName == "cswfeb07" || lexiconName == "cswapr07")
		lexiconName = "csw07";

	setQuackleToUseLexiconName(lexiconName);
	setQuackleToUseAlphabetName(settings.value("quackle/settings/alphabet-name", QString("english")).toString());
	setQuackleToUseThemeName(settings.value("quackle/settings/theme-name", QString("traditional")).toString());
	setQuackleToUseBoardName(settings.value("quackle/settings/board-name", QString("")).toString());
}

void Settings::setGaddagLabel()
{
	QString gaddagLabelString;
	if (!QUACKLE_LEXICON_PARAMETERS->hasGaddag())
	{
		gaddagLabelString = tr("Lexicon database is not up to date.  Press the button to begin building the database.  This may take several minutes to complete.");
		m_buildGaddag->setEnabled(true);
	}
	else
	{
		gaddagLabelString = tr("Lexicon database is up to date.");
		m_buildGaddag->setEnabled(false);
	}

	m_buildGaddagLabel->setText(gaddagLabelString);
}

void Settings::setGaddagLabel(const QString &label)
{
	m_buildGaddagLabel->setText(label);
	qApp->processEvents();
}

void Settings::buildGaddag()
{
	const string gaddagFile(QUACKLE_DATAMANAGER->makeDataFilename("lexica", QUACKLE_LEXICON_PARAMETERS->lexiconName() + ".gaddag", true));
	GaddagFactory factory((UVString()));
	Quackle::LetterString word;
	int wordCount = 0;

	setGaddagLabel(tr("Words processed: 0"));
	pushIndex(factory, word, 1, wordCount);
	if (wordCount < QUACKLE_MAX_GADDAG_WORDCOUNT)
	{
		setGaddagLabel(QString(tr("Lexicon total: %1 words.  Compressing...")).arg(wordCount));
		factory.generate();
		setGaddagLabel(QString(tr("Lexicon total: %1 words.  Writing to disk...")).arg(wordCount));
		factory.writeIndex(gaddagFile);
		QUACKLE_LEXICON_PARAMETERS->loadGaddag(gaddagFile);
		setGaddagLabel();
	}
	else
		setGaddagLabel(tr("Your lexicon is too large to be represented using the internal database format.  Operation aborted."));
}

void Settings::pushIndex(GaddagFactory &factory, Quackle::LetterString &word, int index, int &wordCount)
{
	unsigned int p;
	Quackle::Letter letter;
	bool t;
	bool lastchild;
	bool british;
	int playability;

	do
	{
		QUACKLE_LEXICON_PARAMETERS->dawgAt(index, p, letter, t, lastchild, british, playability);
		word.push_back(letter);
		if (t)
		{
			factory.pushWord(word);
			wordCount++;
			if (wordCount % 1000 == 0)
				setGaddagLabel(QString(tr("Words processed: %1")).arg(wordCount));
			if (wordCount > QUACKLE_MAX_GADDAG_WORDCOUNT)
				return;
		}
		if (p)
		{
			pushIndex(factory, word, p, wordCount);
			if (wordCount > QUACKLE_MAX_GADDAG_WORDCOUNT)
				return;
		}
		index++;
		word.pop_back();
	} while (!lastchild);
}


void Settings::setQuackleToUseLexiconName(const QString &lexiconName)
{
	QUACKLE_DATAMANAGER->setBackupLexicon("default");

	string lexiconNameStr = lexiconName.toStdString();
	if (QUACKLE_LEXICON_PARAMETERS->lexiconName() != lexiconNameStr)
	{
		QUACKLE_LEXICON_PARAMETERS->setLexiconName(lexiconNameStr);

		string dawgFile = Quackle::LexiconParameters::findDictionaryFile(lexiconNameStr + ".dawg");
		if (dawgFile.empty())
		{
			UVcout << "Dawg for lexicon '" << lexiconNameStr << "' does not exist." << endl;
			QUACKLE_LEXICON_PARAMETERS->unloadDawg();
		}
		else
			QUACKLE_LEXICON_PARAMETERS->loadDawg(dawgFile);

		if (!QUACKLE_LEXICON_PARAMETERS->hasDawg())
		{
			QUACKLE_LEXICON_PARAMETERS->unloadGaddag();
			return;
		}

		string gaddagFile = Quackle::LexiconParameters::findDictionaryFile(lexiconNameStr + ".gaddag");
		if (gaddagFile.empty())
		{
			UVcout << "Gaddag for lexicon '" << lexiconNameStr << "' does not exist." << endl;
			QUACKLE_LEXICON_PARAMETERS->unloadGaddag();
		}
		else
			QUACKLE_LEXICON_PARAMETERS->loadGaddag(gaddagFile);

		// Dirty test to see if we're working with an English-like dictionary, and if so, beef up
		// strategy files with twl06 ones (until I can start generating better).  It's an imperfect
		// test...it captures the ODS dictionary, for example, which seems pretty wrong.  But I
		// don't want to hard-code lexicon names here, so this is about as good as I can do.
		const vector<string> & alphabet = QUACKLE_LEXICON_PARAMETERS->utf8Alphabet();
		if (alphabet.size() == 26)
		{
			vector<string>::const_iterator it;
			for (it = alphabet.begin(); it != alphabet.end(); it++)
			{
				if (it->size() != 1)
					break;
				if (it->c_str()[0] < 'A' || it->c_str()[0] > 'Z')
					break;
			}
			if (it == alphabet.end())
				QUACKLE_DATAMANAGER->setBackupLexicon("default_english");
		}
		QUACKLE_STRATEGY_PARAMETERS->initialize(lexiconNameStr);
		string logoFileName = QUACKLE_LEXICON_PARAMETERS->logoFileName();
		if (logoFileName.empty())
			m_logoLabel->setPixmap(QPixmap());
		else
			m_logoLabel->setPixmap(QPixmap(QString(logoFileName.c_str())));
		m_copyrightLabel->setText(QString::fromUtf8(QUACKLE_LEXICON_PARAMETERS->copyrightString().c_str()));
		setGaddagLabel();
	}
}

void Settings::setQuackleToUseAlphabetName(const QString &alphabetName)
{
	string alphabetNameStr = alphabetName.toStdString();
	if (QUACKLE_ALPHABET_PARAMETERS->alphabetName() != alphabetNameStr)
	{
		QString alphabetFileStr = QuackleIO::Util::stdStringToQString(Quackle::AlphabetParameters::findAlphabetFile(alphabetNameStr));

		QuackleIO::FlexibleAlphabetParameters *flexure = new QuackleIO::FlexibleAlphabetParameters;
		flexure->setAlphabetName(alphabetNameStr);
		if (flexure->load(alphabetFileStr))
		{
			if (flexure->length() != QUACKLE_ALPHABET_PARAMETERS->length() && QUACKLE_ALPHABET_PARAMETERS->alphabetName() != "default")
			{
				QMessageBox::warning(this, tr("Alphabet mismatch - Quackle"), QString("<html>%1</html>").arg(tr("%1 has a different number of letters than %2, so please start a new game or else Quackle will crash or act strangely.").arg(QuackleIO::Util::stdStringToQString(flexure->alphabetName())).arg(QuackleIO::Util::stdStringToQString(QUACKLE_ALPHABET_PARAMETERS->alphabetName()))));
			}

			QUACKLE_DATAMANAGER->setAlphabetParameters(flexure);
		}
		else
		{
			UVcerr << "Couldn't load alphabet!" << endl;
			delete flexure;
		}
	}
}

void Settings::setQuackleToUseThemeName(const QString &themeName)
{
	m_themeName = themeName;
	QString themeFile = m_userDataDir + "/themes/" + themeName + ".ini";
	if (!QFile::exists(themeFile))
		themeFile = m_appDataDir + "/themes/" + themeName + ".ini";
	if (!QFile::exists(themeFile))
	{
		m_themeName = "traditional";
		themeFile = m_appDataDir + "/themes/traditional.ini";
	}
	PixmapCacher::self()->readTheme(themeFile);
}

void Settings::setQuackleToUseBoardName(const QString &boardName)
{
	CustomQSettings settings;
	settings.beginGroup("quackle/boardparameters");

	if (boardName.isEmpty() || !settings.contains(boardName))
		QUACKLE_DATAMANAGER->setBoardParameters(new Quackle::BoardParameters());
	else
	{
		QByteArray boardParameterBytes = qUncompress(settings.value(boardName).toByteArray());
		string boardParameterBuf;
		boardParameterBuf.assign((const char *) boardParameterBytes, boardParameterBytes.size());
		istringstream boardParameterStream(boardParameterBuf);

		QUACKLE_DATAMANAGER->setBoardParameters(Quackle::BoardParameters::Deserialize(boardParameterStream));
	}
	QUACKLE_BOARD_PARAMETERS->setName(QuackleIO::Util::qstringToString(boardName));
	loadBoardNameCombo();
}

void Settings::lexiconChanged(int lexiconIndex)
{
	if (m_lexiconNameCombo->currentIndex() == m_lexiconNameCombo->count() - 1)
	{
		editLexicon();
		if (m_lexiconNameCombo->currentIndex() == m_lexiconNameCombo->count() - 1 &&
			m_lexiconNameCombo->currentIndex() != 0)
			m_lexiconNameCombo->setCurrentIndex(m_lastGoodLexiconValue);
		return;
	}
	lexiconChanged(m_lexiconNameCombo->currentText());
}

void Settings::lexiconChanged(const QString &lexiconName)
{
	QString lexicon = lexiconName;
	if (lexicon.endsWith("*"))
		lexicon.truncate(lexicon.size() - 1);

	setQuackleToUseLexiconName(lexicon);
	m_lastGoodLexiconValue = m_lexiconNameCombo->currentIndex();

	CustomQSettings settings;
	settings.setValue("quackle/settings/lexicon-name", lexicon);

	emit refreshViews();
}

void Settings::alphabetChanged(int alphabetIndex)
{
	// Uncomment when we support an add/edit alphabet dialog
	// if (m_alphabetNameCombo->currentIndex() == m_alphabetNameCombo->count() - 1)
	// {
	// 	editAlphabet();
	// 	return;
	// }
	QString alphabetName = m_alphabetNameCombo->currentText();
	setQuackleToUseAlphabetName(alphabetName);

	CustomQSettings settings;
	settings.setValue("quackle/settings/alphabet-name", alphabetName);

	emit refreshViews();
}

void Settings::themeChanged(int themIndex)
{
	// Uncomment when we support an add/edit theme dialog
	// if (m_themeNameCombo->currentIndex() == m_themeNameCombo->count() - 1)
	// {
	// 	editTheme();
	// 	return;
	// }
	setQuackleToUseThemeName(m_themeNameCombo->currentText());

	CustomQSettings settings;
	settings.setValue("quackle/settings/theme-name", m_themeNameCombo->currentText());

	emit refreshViews();
}

void Settings::boardChanged(int boardIndex)
{
	if (m_boardNameCombo->currentIndex() == m_boardNameCombo->count() - 1)
	{
		addBoard();
		if (m_boardNameCombo->currentIndex() == m_boardNameCombo->count() - 1 &&
			m_boardNameCombo->currentIndex() != 0)
			m_boardNameCombo->setCurrentIndex(m_lastGoodBoardValue);
		return;
	}
	boardChanged(m_boardNameCombo->currentText());
}

void Settings::boardChanged(const QString &boardName)
{
	CustomQSettings settings;
	settings.setValue("quackle/settings/board-name", boardName);

	setQuackleToUseBoardName(boardName);
	emit refreshViews();
}

void Settings::addBoard()
{
	QUACKLE_DATAMANAGER->setBoardParameters(new Quackle::BoardParameters());
	QUACKLE_BOARD_PARAMETERS->setName(MARK_UV(""));

	CustomQSettings settings;
	BoardSetupDialog dialog(this);
	if (dialog.exec())
	{
		QString boardName = QuackleIO::Util::uvStringToQString(QUACKLE_BOARD_PARAMETERS->name());
		settings.beginGroup("quackle/boardparameters");

		ostringstream boardParameterStream;
		QUACKLE_BOARD_PARAMETERS->Serialize(boardParameterStream);

		QByteArray boardParameterBytes = qCompress(
							(const uchar *)boardParameterStream.str().data(),
							(int)boardParameterStream.str().size());
		settings.setValue(boardName, QVariant(boardParameterBytes));
		m_boardNameCombo->setCurrentIndex(-1);
		boardChanged(boardName);
	}
	else
		setQuackleToUseBoardName(settings.value("quackle/settings/board-name", QString("")).toString());

	PixmapCacher::self()->invalidate();
}

void Settings::editBoard()
{
	QString oldBoardName = m_boardNameCombo->currentText();
	QUACKLE_BOARD_PARAMETERS->setName(QuackleIO::Util::qstringToString(oldBoardName));

	BoardSetupDialog dialog(this);
	if (dialog.exec())
	{
		QString newBoardName = QuackleIO::Util::uvStringToQString(QUACKLE_BOARD_PARAMETERS->name());
		CustomQSettings settings;
		settings.beginGroup("quackle/boardparameters");

		if (newBoardName != oldBoardName)
			settings.remove(oldBoardName);

		ostringstream boardParameterStream;
		QUACKLE_BOARD_PARAMETERS->Serialize(boardParameterStream);

		QByteArray boardParameterBytes = qCompress(
							(const char *)boardParameterStream.str().data(),
							int(boardParameterStream.str().size()));
		settings.setValue(newBoardName, QVariant(boardParameterBytes));
		boardChanged(newBoardName);
	}
	PixmapCacher::self()->invalidate();
	loadBoardNameCombo();
	emit refreshViews();
}

void Settings::loadBoardNameCombo()
{
	while (m_boardNameCombo->count() > 0)
		m_boardNameCombo->removeItem(0);

	CustomQSettings settings;
	settings.beginGroup("quackle/boardparameters");
	QStringList boardNames = settings.childKeys();
	boardNames.sort();
	m_boardNameCombo->addItems(boardNames);
	m_boardNameCombo->addItem("Add new board...");
	settings.endGroup();

	m_editBoard->setEnabled(!boardNames.empty());

	QString currentItem = settings.value("quackle/settings/board-name", QString("")).toString();
	int currentItemIndex = m_boardNameCombo->findText(currentItem);
	if (m_boardNameCombo->count() > 0 && currentItemIndex < 0)
		currentItemIndex = 0;
	m_boardNameCombo->setCurrentIndex(currentItemIndex);
}

void Settings::editLexicon()
{
	QString name = m_lexiconNameCombo->currentText();
	if (name.endsWith("*"))
		name.truncate(name.size() - 1);
	if (m_lexiconNameCombo->currentIndex() == m_lexiconNameCombo->count() - 1)
		name = "";
	LexiconDialog dialog(this, name);
	if (dialog.exec())
	{
		populateComboFromFilenames(m_lexiconNameCombo, "lexica", ".dawg", "lexicon");
		qApp->processEvents();
		if (dialog.itemWasDeleted())
		{
			m_lexiconNameCombo->setCurrentIndex(m_lexiconNameCombo->findText(name));
			QUACKLE_LEXICON_PARAMETERS->setLexiconName(""); // force lexicon to reload
			QUACKLE_LEXICON_PARAMETERS->unloadAll();
			if (m_lexiconNameCombo->currentIndex() != -1)
				setQuackleToUseLexiconName(name);
		}
		else if (!dialog.lexiconName().isEmpty())
		{
			QUACKLE_LEXICON_PARAMETERS->setLexiconName(""); // force lexicon to reload
			QUACKLE_LEXICON_PARAMETERS->unloadAll();
			setQuackleToUseLexiconName(dialog.lexiconName());
			m_lexiconNameCombo->setCurrentIndex(m_lexiconNameCombo->findText(name + "*"));
		}
		load();
	}
}

void Settings::editAlphabet()
{
#if 0
	QString name = m_alphabetNameCombo->currentText();
	if (m_alphabetNameCombo->currentIndex() == m_alphabetNameCombo->count() - 1)
		name = "";
	AlphabetDialog dialog(this);
	if (dialog.exec())
	{
		populateComboFromFilenames(m_alphabetNameCombo, "alphabets", ".quackle_alphabet", "alphabet");
		load();
	}
#endif // 0
}

void Settings::editTheme()
{
#if 0
	QString name = m_themeNameCombo->currentText();
	if (m_themeNameCombo->currentIndex() == m_themeNameCombo->count() - 1)
		name = "";
	ThemeDialog dialog(this);
	if (dialog.exec())
	{
		populateThemeFromFilenames(m_themeNameCombo, "themes", "theme");
		load();
	}
#endif // 0
}

void Settings::populateComboFromFilenames(QComboBox* combo, const QString &path, const QString &extension, const QString &label)
{
	while (combo->count() > 0)
		combo->removeItem(0);
	
	QStringList fileList;
	QDir dir(self()->m_appDataDir);
	if (dir.cd(path))
		fileList << dir.entryList(QDir::Files | QDir::Readable, QDir::Name);
	dir = QDir(self()->m_userDataDir);
	if (dir.cd(path))
		fileList << dir.entryList(QDir::Files | QDir::Readable, QDir::Name);

	QStringList::iterator i;
	QString fileName;
	QStringList list;
	qsizetype periodPos;

	for (i = fileList.begin(); i != fileList.end(); ++i)
	{
		fileName = *i;
		if (!fileName.endsWith(extension))
			continue;
		periodPos = fileName.indexOf('.');
		if (periodPos)
		{
			fileName.truncate(periodPos);
			list << fileName;
		}
	}

	for (i = list.begin(); i != list.end(); ++i)
	{
		for (QStringList::iterator j = i + 1; j != list.end(); ++j)
		{
			if (*i == *j)
			{
				*i = *i + "*";
				list.erase(j);
				break;
			}
		}
	}

	combo->addItems(list);
	if (label.size() > 0)
		combo->addItem(QString(tr("Add new ")).append(label).append("..."));
}
