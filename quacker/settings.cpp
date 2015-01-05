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

#include <iostream>
#include <sstream>

#include <QtGui>
#include <QMessageBox>

#ifdef Q_WS_MAC
#include <CoreFoundation/CoreFoundation.h>
#endif // Q_WS_MAC

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

 #ifdef Q_WS_MAC
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
 #endif

	if (QFile::exists("data"))
		m_dataDir = "data";
	else if (QFile::exists("../data"))
		m_dataDir = "../data";
	else if (QFile::exists("Quackle.app/Contents/data"))
		m_dataDir = "Quackle.app/Contents/data";
	else
	{
		if (!directory.cd("data") || !directory.cd("../data"))
			QMessageBox::critical(0, tr("Error Initializing Data Files - Quacker"), tr("<p>Could not open data directory. Quackle will be useless. Try running the quacker executable with quackle/quacker/ as the current directory.</p>"));
		m_dataDir = directory.absolutePath();
	}
}

void Settings::createGUI()
{
	if (m_lexiconNameCombo != 0)
		return;

	QVBoxLayout *vlayout = new QVBoxLayout(this);

	m_lexiconNameCombo = new QComboBox;
	connect(m_lexiconNameCombo, SIGNAL(activated(const QString &)), this, SLOT(lexiconChanged(const QString &)));

	QStringList lexiconItems;
	populateListFromFilenames(lexiconItems, "lexica");
	m_lexiconNameCombo->addItems(lexiconItems);

	QHBoxLayout *lexiconLayout = new QHBoxLayout;
	QLabel *lexiconNameLabel = new QLabel(tr("&Lexicon:"));
	lexiconNameLabel->setBuddy(m_lexiconNameCombo);
	m_editLexicon = new QPushButton(tr("Edit..."));
	m_editLexicon->setMaximumWidth(60);
	connect(m_editLexicon, SIGNAL(clicked()), this, SLOT(editLexicon()));

	lexiconLayout->addWidget(lexiconNameLabel);
	lexiconLayout->addWidget(m_lexiconNameCombo);
	lexiconLayout->addWidget(m_editLexicon);

	m_alphabetNameCombo = new QComboBox;
	connect(m_alphabetNameCombo, SIGNAL(activated(const QString &)), this, SLOT(alphabetChanged(const QString &)));

	QStringList alphabetItems;
	populateListFromFilenames(alphabetItems, "alphabets");
	m_alphabetNameCombo->addItems(alphabetItems);

	QHBoxLayout *alphabetLayout = new QHBoxLayout;
	QLabel *alphabetNameLabel = new QLabel(tr("&Alphabet:"));
	alphabetNameLabel->setBuddy(m_alphabetNameCombo);
	m_editAlphabet = new QPushButton(tr("Edit..."));
	m_editAlphabet->setMaximumWidth(60);
	connect(m_editAlphabet, SIGNAL(clicked()), this, SLOT(editAlphabet()));

	alphabetLayout->addWidget(alphabetNameLabel);
	alphabetLayout->addWidget(m_alphabetNameCombo);
	alphabetLayout->addWidget(m_editAlphabet);

	m_themeNameCombo = new QComboBox;
	connect(m_themeNameCombo, SIGNAL(activated(const QString &)), this, SLOT(themeChanged(const QString &)));

	QStringList themeItems;
	populateListFromFilenames(themeItems, "themes");
	m_themeNameCombo->addItems(themeItems);

	QHBoxLayout *themeLayout = new QHBoxLayout;
	QLabel *themeNameLabel = new QLabel(tr("&Theme:"));
	themeNameLabel->setBuddy(m_themeNameCombo);
	m_editTheme = new QPushButton(tr("Edit..."));
	m_editTheme->setMaximumWidth(60);
	connect(m_editTheme, SIGNAL(clicked()), this, SLOT(editTheme));

	themeLayout->addWidget(themeNameLabel);
	themeLayout->addWidget(m_themeNameCombo);
	themeLayout->addWidget(m_editTheme);

	m_boardNameCombo = new QComboBox;
	connect(m_boardNameCombo, SIGNAL(activated(const QString &)), this, SLOT(boardChanged(const QString &)));

	QStringList boardItems;
	populateListFromFilenames(boardItems, "boards");
	m_boardNameCombo->addItems(boardItems);

	QHBoxLayout *boardLayout = new QHBoxLayout;
	QLabel *boardNameLabel = new QLabel(tr("&Board:"));
	boardNameLabel->setBuddy(m_boardNameCombo);
	m_editBoard = new QPushButton(tr("Edit..."));
	m_editBoard->setMaximumWidth(60);
	connect(m_editBoard, SIGNAL(clicked()), this, SLOT(editBoard));

	boardLayout->addWidget(boardNameLabel);
	boardLayout->addWidget(m_boardNameCombo);
	boardLayout->addWidget(m_editBoard);

	// m_addBoard = new QPushButton(tr("Add Board"));
	// connect(m_addBoard, SIGNAL(clicked()), this, SLOT(addBoard()));

	// m_editBoard = new QPushButton(tr("&Edit Board"));
	// connect(m_editBoard, SIGNAL(clicked()), this, SLOT(editBoard()));

	// m_deleteBoard = new QPushButton(tr("&Delete Board"));
	// connect(m_deleteBoard, SIGNAL(clicked()), this, SLOT(deleteBoard()));

	// loadBoardNameCombo();

	// QGroupBox *boardGroup = new QGroupBox("Game Board Definitions");
	// QGridLayout *boardLayout = new QGridLayout(boardGroup);
	// QLabel *boardNameLabel = new QLabel(tr("&Board:"));
	// boardNameLabel->setBuddy(m_boardNameCombo);

	// boardLayout->addWidget(boardNameLabel, 0, 0);
	// boardLayout->addWidget(m_boardNameCombo, 0, 1, 1, -1);
	// boardLayout->addWidget(m_addBoard, 1, 0);
	// boardLayout->addWidget(m_editBoard, 1, 1);
	// boardLayout->addWidget(m_deleteBoard, 1, 2);


	vlayout->addLayout(lexiconLayout);
	vlayout->addLayout(alphabetLayout);
	vlayout->addLayout(themeLayout);
	vlayout->addLayout(boardLayout);
	vlayout->addStretch();

	load();
}

void Settings::load()
{
	m_lexiconNameCombo->setCurrentIndex(m_lexiconNameCombo->findText(QuackleIO::Util::stdStringToQString(QUACKLE_LEXICON_PARAMETERS->lexiconName())));
	m_alphabetNameCombo->setCurrentIndex(m_alphabetNameCombo->findText(QuackleIO::Util::stdStringToQString(QUACKLE_ALPHABET_PARAMETERS->alphabetName())));
	m_themeNameCombo->setCurrentIndex(m_themeNameCombo->findText(m_themeName));
	m_boardNameCombo->setCurrentIndex(m_boardNameCombo->findText(QuackleIO::Util::uvStringToQString(QUACKLE_BOARD_PARAMETERS->name())));
}

void Settings::preInitialize()
{
	// load computer players
	QUACKLE_DATAMANAGER->setComputerPlayers(Quackle::ComputerPlayerCollection::fullCollection());
}

void Settings::initialize()
{
	CustomQSettings settings;

	QUACKLE_DATAMANAGER->setBackupLexicon("twl06");
	QUACKLE_DATAMANAGER->setDataDirectory(m_dataDir.toStdString());

	QString lexiconName = settings.value("quackle/settings/lexicon-name", QString("twl06")).toString();

	// Handle Collins update.
	if (lexiconName == "cswfeb07")
		lexiconName = "cswapr07";

	setQuackleToUseLexiconName(QuackleIO::Util::qstringToStdString(lexiconName));
	setQuackleToUseAlphabetName(QuackleIO::Util::qstringToStdString(settings.value("quackle/settings/alphabet-name", QString("english")).toString()));
	setQuackleToUseThemeName(settings.value("quackle/settings/theme-name", QString("traditional")).toString());
	setQuackleToUseBoardName(settings.value("quackle/settings/board-name", QString("")).toString());
}

void Settings::setQuackleToUseLexiconName(const string &lexiconName)
{
	if (QUACKLE_LEXICON_PARAMETERS->lexiconName() != lexiconName)
	{
		QUACKLE_LEXICON_PARAMETERS->setLexiconName(lexiconName);

		string gaddagFile = Quackle::LexiconParameters::findDictionaryFile(lexiconName + ".gaddag");

		if (gaddagFile.empty())
		{
			UVcout << "Gaddag for lexicon '" << lexiconName << "' does not exist." << endl;
			QUACKLE_LEXICON_PARAMETERS->unloadGaddag();
		}
		else
			QUACKLE_LEXICON_PARAMETERS->loadGaddag(gaddagFile);

		string dawgFile = Quackle::LexiconParameters::findDictionaryFile(lexiconName + ".dawg");
		if (dawgFile.empty())
		{
			UVcout << "Dawg for lexicon '" << lexiconName << "' does not exist." << endl;
			QUACKLE_LEXICON_PARAMETERS->unloadDawg();
		}
		else
			QUACKLE_LEXICON_PARAMETERS->loadDawg(dawgFile);

		QUACKLE_STRATEGY_PARAMETERS->initialize(lexiconName);
	}
}

void Settings::setQuackleToUseAlphabetName(const string &alphabetName)
{
	if (QUACKLE_ALPHABET_PARAMETERS->alphabetName() != alphabetName)
	{
		QString alphabetFile = QuackleIO::Util::stdStringToQString(Quackle::AlphabetParameters::findAlphabetFile(alphabetName + ".quackle_alphabet"));

		QuackleIO::FlexibleAlphabetParameters *flexure = new QuackleIO::FlexibleAlphabetParameters;
		flexure->setAlphabetName(alphabetName);
		if (flexure->load(alphabetFile))
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
	QString themeFile = m_dataDir + "/themes/" + themeName + ".ini";
	if (!QFile::exists(themeFile))
	{
		m_themeName = "traditional";
		themeFile = m_dataDir + "/themes/traditional.ini";
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

void Settings::lexiconChanged(const QString &lexiconName)
{
	string lexiconNameString = QuackleIO::Util::qstringToStdString(lexiconName);
	setQuackleToUseLexiconName(lexiconNameString);

	CustomQSettings settings;
	settings.setValue("quackle/settings/lexicon-name", lexiconName);

	emit refreshViews();
}

void Settings::alphabetChanged(const QString &alphabetName)
{
	string alphabetNameString = QuackleIO::Util::qstringToStdString(alphabetName);
	setQuackleToUseAlphabetName(alphabetNameString);

	CustomQSettings settings;
	settings.setValue("quackle/settings/alphabet-name", alphabetName);

	emit refreshViews();
}

void Settings::themeChanged(const QString &themeName)
{
	setQuackleToUseThemeName(themeName);

	CustomQSettings settings;
	settings.setValue("quackle/settings/theme-name", themeName);

	emit refreshViews();
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
							boardParameterStream.str().size());
		settings.setValue(boardName, QVariant(boardParameterBytes));
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
							boardParameterStream.str().size());
		settings.setValue(newBoardName, QVariant(boardParameterBytes));
		boardChanged(newBoardName);
	}
	PixmapCacher::self()->invalidate();
}

void Settings::deleteBoard()
{
	int oldIndex = m_boardNameCombo->currentIndex();
	QString boardName = m_boardNameCombo->currentText();
	QString message = "Do you really want to delete the game board \"";
	message += boardName;
	message += "\"?";
	if (QMessageBox::warning(NULL, QString("Confirm Deletion"), message,
			QMessageBox::Yes | QMessageBox::Default,
			QMessageBox::No | QMessageBox::Escape) == QMessageBox::Yes)
	{
		CustomQSettings settings;
		settings.beginGroup("quackle/boardparameters");
		settings.remove(boardName);
		loadBoardNameCombo();
		if (oldIndex != 0)
			oldIndex--;
		m_boardNameCombo->setCurrentIndex(oldIndex);
		boardChanged(m_boardNameCombo->currentText());
	}
}

void Settings::loadBoardNameCombo()
{
	if (m_lexiconNameCombo == 0)
		return;

	while (m_boardNameCombo->count() > 0)
		m_boardNameCombo->removeItem(0);

	CustomQSettings settings;
	settings.beginGroup("quackle/boardparameters");
	QStringList boardNames = settings.childKeys();
	boardNames.sort();
	m_boardNameCombo->addItems(boardNames);
	settings.endGroup();

	QString currentItem = settings.value("quackle/settings/board-name", QString("")).toString();
	m_boardNameCombo->setCurrentIndex(m_boardNameCombo->findText(currentItem));
}

void Settings::populateListFromFilenames(QStringList& list, const QString &path)
{
	QStringList fileList;
	QDir dir(m_dataDir);
	if (dir.cd(path))
		fileList << dir.entryList(QDir::Files | QDir::Readable, QDir::Name);
	dir = QDir(QDesktopServices::storageLocation(QDesktopServices::DataLocation));
	if (dir.cd(path))
		fileList << dir.entryList(QDir::Files | QDir::Readable, QDir::Name);

	QStringListIterator i(fileList);
	QString fileName;
	int periodPos;

	while (i.hasNext())
	{
		fileName = i.next();
		periodPos = fileName.indexOf('.');
		if (periodPos)
		{
			fileName.truncate(periodPos);
			list << fileName;
		}
	}
	list.removeDuplicates();
}
