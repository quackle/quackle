/*
 *  Quackle -- Crossword game artificial intelligence and analysis tool
 *  Copyright (C) 2005-2014 Jason Katz-Brown and John O'Laughlin.
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

#include <sstream>
#include <QtGui>
#include <datamanager.h>
#include <quackleio/util.h>

#include "lexicondialog.h"
#include "customqsettings.h"
#include "settings.h"
#include "geometry.h"
#include "quackleio/dawgfactory.h"

LexiconDialog::LexiconDialog(QWidget *parent, const QString &originalName) : QDialog(parent),
	m_wordFactory(NULL)
{
	m_originalName = originalName;

	resize(450,350);
	
	// construct the UI elements
	m_lexiconName = new QLineEdit();
	m_alphabetCombo = new QComboBox();

	m_addWordsFromFile = new QPushButton(tr("Add words from &file..."));
	m_clearAllWords = new QPushButton(tr("Clear &words and start again"));


	m_lexiconInformation = new QLabel("");
	m_lexiconInformation->setWordWrap(true);

	m_saveChanges = new QPushButton(tr("&Save Changes"));
	m_cancel = new QPushButton(tr("&Cancel"));
	m_deleteLexicon = new QPushButton(tr("&Delete Lexicon"));

	QLabel * lexiconNameLabel = new QLabel(tr("&Lexicon name:"));
	QLabel * alphabetLabel = new QLabel(tr("&Alphabet:"));
	lexiconNameLabel->setBuddy(m_lexiconName);
	alphabetLabel->setBuddy(m_alphabetCombo);

	QVBoxLayout * layout = new QVBoxLayout;
	Geometry::setupFramedLayout(layout);
	QHBoxLayout * lexiconRow = new QHBoxLayout;
	Geometry::setupInnerLayout(lexiconRow);
	QHBoxLayout * addRemoveWordsRow = new QHBoxLayout;
	Geometry::setupInnerLayout(addRemoveWordsRow);
	QHBoxLayout * buttonRow = new QHBoxLayout;
	Geometry::setupInnerLayout(buttonRow);
	QGroupBox * lexiconInformationGroup = new QGroupBox(tr("Lexicon information"));
	QVBoxLayout * lexiconInformationLayout = new QVBoxLayout(lexiconInformationGroup);

	// build the layout
	lexiconRow->addWidget(lexiconNameLabel);
	lexiconRow->addWidget(m_lexiconName);
	lexiconRow->addStretch();
	lexiconRow->addWidget(alphabetLabel);
	lexiconRow->addWidget(m_alphabetCombo);

	addRemoveWordsRow->addWidget(m_addWordsFromFile);
	addRemoveWordsRow->addWidget(m_clearAllWords);

	lexiconInformationLayout->addWidget(m_lexiconInformation);

	buttonRow->addWidget(m_deleteLexicon);
	buttonRow->addStretch();
	buttonRow->addWidget(m_cancel);
	buttonRow->addWidget(m_saveChanges);

	layout->addLayout(lexiconRow);
	layout->addLayout(addRemoveWordsRow);
	layout->addWidget(lexiconInformationGroup);
	layout->addStretch();
	layout->addLayout(buttonRow);

	setLayout(layout);
	m_saveChanges->setDefault(true);

	// hook up signals and slots
	//	connect(m_lexiconName, SIGNAL(textEdited(const QString &)), this, SLOT(parametersChanged(const QString &)));
	connect(m_addWordsFromFile, SIGNAL(clicked()), this, SLOT(addWordsFromFile()));
	connect(m_saveChanges, SIGNAL(clicked()), this, SLOT(accept()));
	connect(m_cancel, SIGNAL(clicked()), this, SLOT(reject()));
	connect(m_deleteLexicon, SIGNAL(clicked()), this, SLOT(deleteLexicon()));
	connect(m_alphabetCombo, SIGNAL(activated(const QString &)), this, SLOT(alphabetChanged(const QString &)));

	setWindowTitle(tr("Configure Lexicon - Quackle"));

	Settings::populateComboFromFilenames(m_alphabetCombo, "alphabets", "");
	alphabetChanged(m_alphabetCombo->currentText());
	updateLexiconInformation();

	// sync game board with control states and draw board
}

LexiconDialog::~LexiconDialog()
{

}

void LexiconDialog::deleteLexicon()
{
	delete m_wordFactory;
	m_wordFactory = NULL;
	updateLexiconInformation();
}

void LexiconDialog::addWordsFromFile()
{
	QFileDialog browser(this, tr("Choose a file containing words to be added to the lexicon..."));
	QStringList filters;
	filters << "Dictionary files (*.txt *.dawg *.raw)"
		<< "All files (*.*)";
	browser.setNameFilters(filters);
	browser.setFileMode(QFileDialog::ExistingFiles);
	browser.exec();

	QStringList files = browser.selectedFiles();
	for (QList<QString>::const_iterator it = files.begin(); it != files.end(); it++)
	{
		if (it->endsWith(".dawg", Qt::CaseInsensitive))
			addWordsFromDawgFile(*it, m_alphabetCombo->currentText());
		else
			addWordsFromTextFile(*it, m_alphabetCombo->currentText());
	}
	updateLexiconInformation();
}

void LexiconDialog::alphabetChanged(const QString &alphabet)
{
	delete m_wordFactory;
	m_wordFactory = NULL;
	updateLexiconInformation();
	m_alphabetFileName = QString::fromStdString(AlphabetParameters::findAlphabetFile(QuackleIO::Util::qstringToStdString(alphabet)));
}

void LexiconDialog::addWordsFromDawgFile(const QString &dawgfile, const QString &alphabetfile)
{
	if (!m_wordFactory)
		m_wordFactory = new DawgFactory(m_alphabetFileName);
	LexiconParameters lexParams;
	lexParams.loadDawg(QuackleIO::Util::qstringToStdString(dawgfile));
	if (!lexParams.hasDawg())
		return;

	Quackle::LetterString word;

	addWordsFromDawgRecursive(lexParams, word, 1);
}

void LexiconDialog::addWordsFromDawgRecursive(const LexiconParameters &lexParams, Quackle::LetterString &word, int index)
{
	unsigned int p;
	Quackle::Letter letter;
	bool t;
	bool lastchild;
	bool british;
	int playability;

	do
	{
		lexParams.dawgAt(index, p, letter, t, lastchild, british, playability);
		word.push_back(letter);
		if (t)
			m_wordFactory->pushWord(word, !british, playability);
		if (p)
			addWordsFromDawgRecursive(lexParams, word, p);
		index++;
		word.pop_back();
	} while (!lastchild);
}

void LexiconDialog::addWordsFromTextFile(const QString &textFile, const QString &alphabetfile)
{
	if (!m_wordFactory)
		m_wordFactory = new DawgFactory(m_alphabetFileName);

	QFile file(textFile);
	if (!file.open(QIODevice::ReadOnly | QIODevice::Text))
		return;

	QTextStream stream(&file);
	stream.setCodec("UTF-8");
	QString word;
	while (!stream.atEnd())
	{
		stream >> word;
		word = word.trimmed().toUpper();
		if (word.isEmpty())
			continue;
		QChar firstChar = word[0];
		if (firstChar < 'A')
			continue; // allows the usage of most punctuation characters as comments
		int playability = 0;
		for (int i = word.size() - 1; i > 0; i--)
		{
			if (word[i].isDigit())
				playability = playability * 10 + word[i].digitValue();
		}
		m_wordFactory->pushWord(QuackleIO::Util::qstringToString(word), true, playability);
	}
}

void LexiconDialog::accept()
{
	QDialog::accept();
}

void LexiconDialog::updateLexiconInformation()
{
	int wordCount = m_wordFactory ? m_wordFactory->wordCount() : 0;
	QByteArray hash = m_wordFactory ? QByteArray(m_wordFactory->hashBytes(), 16).toHex() : "";
	QString text;
	text.append(tr("File name: "));
	text.append(tr("\n\nFile size: "));
	text.append(tr("\n\nWord count: "));
	text.append(QString("%L1").arg(wordCount));
	text.append(tr("\n\nLexicon hash: "));
	text.append(hash);

	m_lexiconInformation->setText(text);
}
