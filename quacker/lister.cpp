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

#include <QtWidgets>

#include <quackleio/dictfactory.h>
#include <quackleio/util.h>

#include "lister.h"
#include "customqsettings.h"

using namespace std;

ListerDialog::ListerDialog(QWidget *parent, const QString &settingsGroup, const QString &appName, int flags)
	: QDialog(parent), m_settingsGroup(settingsGroup), m_appName(appName), m_flags(flags)
{
	setWindowTitle(windowTitleWithAppName(tr("Lister")));

    QVBoxLayout *vbox = new QVBoxLayout(this);

	QHBoxLayout *mainHorizontalLayout = new QHBoxLayout;
	vbox->addLayout(mainHorizontalLayout);

	QVBoxLayout *leftSideLayout = new QVBoxLayout;
	mainHorizontalLayout->addLayout(leftSideLayout);
	QVBoxLayout *rightSideLayout = new QVBoxLayout;
	mainHorizontalLayout->addLayout(rightSideLayout);
	mainHorizontalLayout->setStretchFactor(leftSideLayout, 3);

	// left side
	QHBoxLayout *queryLayout = new QHBoxLayout;
	leftSideLayout->addLayout(queryLayout);

	m_queryEdit = new QLineEdit;
	connect(m_queryEdit, SIGNAL(returnPressed()), this, SLOT(queryGo()));
	queryLayout->addWidget(m_queryEdit);

	m_queryButton = new QPushButton(tr("A-Z?* Query"));
	queryLayout->addWidget(m_queryButton);
	connect(m_queryButton, SIGNAL(clicked()), this, SLOT(queryGo()));

	QHBoxLayout *smallHLayout = new QHBoxLayout;
	leftSideLayout->addLayout(smallHLayout);

	m_numResultsLabel = new QLabel;
	smallHLayout->addWidget(m_numResultsLabel);
	smallHLayout->addStretch();

	m_buildChecker = new QCheckBox(tr("Buil&d"));
	smallHLayout->addWidget(m_buildChecker);

	m_sowpodsChecker = new QCheckBox(tr("Remove &British"));
	connect(m_sowpodsChecker, SIGNAL(toggled(bool)), this, SLOT(setRemoveSowpods(bool)));

	if (!(m_flags & IgnoreBritishness))
		smallHLayout->addWidget(m_sowpodsChecker);

	m_listBox = new QListWidget;
	leftSideLayout->addWidget(m_listBox);
	m_numResultsLabel->setBuddy(m_listBox);

	// right side
	
	QLabel *filtersLabel = new QLabel(tr("&Filters:"));
	rightSideLayout->addWidget(filtersLabel);
	
	m_filtersBox = new QListWidget;
	rightSideLayout->addWidget(m_filtersBox);
	connect(m_filtersBox, SIGNAL(itemClicked(QListWidgetItem *)), this, SLOT(showFilter(QListWidgetItem *)));
	filtersLabel->setBuddy(m_filtersBox);

	QStringList filters;

	if (m_flags & ProbabilityInsteadOfPlayability)
		filters << "Probability";
	else
		filters << "Playability";

	filters << "Regex";
	filters << "Num. Anagrams";

	if (!(m_flags & IgnoreBritishness))
		filters << "Keep British";

	m_filtersBox->addItems(filters);

	m_filtersLayout = new QVBoxLayout;
	rightSideLayout->addLayout(m_filtersLayout);
	m_currentFilter = 0;

	m_applyButton = new QPushButton(tr("&Apply"));
	rightSideLayout->addWidget(m_applyButton);

	// bottom

	QHBoxLayout *filenameLayout = new QHBoxLayout;
	vbox->addLayout(filenameLayout);
	QPushButton *filenameButton = new QPushButton(tr("Pic&k filename..."));
	filenameLayout->addWidget(filenameButton);
	connect(filenameButton, SIGNAL(clicked()), this, SLOT(chooseFilename()));
	m_filenameEdit = new QLineEdit;
	filenameLayout->addWidget(m_filenameEdit);

	QHBoxLayout *buttonBox = new QHBoxLayout;
	vbox->addLayout(buttonBox);
	QHBoxLayout *buttonBox2 = new QHBoxLayout;
	vbox->addLayout(buttonBox2);

	m_writeButton = new QPushButton(tr("&Write alphagram file"));
	m_writeNormalButton = new QPushButton(tr("&Write normal file"));
    m_studyThisButton = new QPushButton(tr("Write and &Study"));
	QPushButton *openButton = new QPushButton(tr("&Open File..."));
	QPushButton *clearButton = new QPushButton(tr("C&lear"));
    m_closeButton = new QPushButton(tr("&Close"));

	buttonBox->addWidget(m_writeButton);
	buttonBox->addWidget(m_writeNormalButton);

	if (!(m_flags & NothingToReturn))
		buttonBox->addWidget(m_studyThisButton);

	buttonBox->addWidget(openButton);
	buttonBox2->addWidget(clearButton);
	buttonBox2->addStretch();
	buttonBox2->addWidget(m_closeButton);

	filenameChanged(QString()); // disable write button initially

	connect(m_filenameEdit, SIGNAL(textChanged(const QString &)), this, SLOT(filenameChanged(const QString &)));
	connect(m_writeButton, SIGNAL(clicked()), this, SLOT(writeButtonClicked()));
	connect(m_writeNormalButton, SIGNAL(clicked()), this, SLOT(writeNormalButtonClicked()));
	connect(m_studyThisButton, SIGNAL(clicked()), this, SLOT(studyButtonClicked()));
	connect(m_closeButton, SIGNAL(clicked()), this, SLOT(accept()));
	connect(clearButton, SIGNAL(clicked()), this, SLOT(clear()));
	connect(openButton, SIGNAL(clicked()), this, SLOT(openFile()));

	showFilter(filters.first());
	//m_filtersBox->setCurrentItem(0);

	clear();
}

ListerDialog::~ListerDialog()
{
	saveSettings();
}

void ListerDialog::resetFocus()
{
	// Qt has some quirks so we're thorough
	m_closeButton->clearFocus();
	m_queryButton->setFocus();
	m_queryEdit->setFocus();
}

void ListerDialog::setQuery(const QString &query)
{
	m_queryEdit->setText(query);
	m_queryEdit->selectAll();
	resetFocus();
}

void ListerDialog::queryGo()
{
	if (m_queryEdit->text().length() >= 40)
	{
		QMessageBox::warning(this, tr("Overlong Query - Quackle"), QString("<html>%1</html>").arg(tr("Queries, arbitrarily, cannot exceed 40 letters.")));
		return;
	}

	m_wordList = QuackleIO::DictFactory::querier()->query(m_queryEdit->text(), m_buildChecker->isChecked()? Dict::Querier::NoRequireAllLetters : Dict::Querier::None);

	setRemoveSowpods(m_sowpodsChecker->isChecked());
	populateListBox();

	// per Robin's request
	m_queryEdit->deselect();
}

void ListerDialog::chooseFilename()
{
	QString filename = QFileDialog::getSaveFileName(this, tr("Choose file to save list to"), m_filenameEdit->text());
	if (!filename.isEmpty())
		m_filenameEdit->setText(filename);
}

void ListerDialog::filenameChanged(const QString &filename)
{
	m_studyThisButton->setEnabled(!filename.isEmpty());
	m_writeButton->setEnabled(!filename.isEmpty());
	m_writeNormalButton->setEnabled(!filename.isEmpty());

	m_filename = filename;
}

void ListerDialog::clear()
{
	m_wordList.clear();
	populateListBox();
	m_queryEdit->clear();

	// do not remove this fatuous line; otherwise Close button gets
	// mysterious focus?
	m_queryButton->setFocus();

	m_queryEdit->setFocus();
}

void ListerDialog::openFile()
{
	QString filename = QFileDialog::getOpenFileName(this, tr("Choose list to open"));
	if (filename.isEmpty())
		return;

	m_wordList.clear();

	QFile file(filename);
	if (file.open(QIODevice::ReadOnly | QIODevice::Text))
	{
		QTextStream stream(&file);
		SET_QTEXTSTREAM_TO_UTF8(stream);
		QString line;
		while (!stream.atEnd())
		{
			line = stream.readLine();

			qsizetype quoteMarkIndex = line.indexOf("\"");
			if (quoteMarkIndex >= 0)
				line = line.left(quoteMarkIndex).trimmed();

			QStringList words(line.split(" "));
			for (const auto& it : words)
			{
				bool found = false;
				Dict::WordList results(QuackleIO::DictFactory::querier()->query(it));
				for (const auto& resultsIt : results)
				{
					if (resultsIt.word == it)
					{
						m_wordList.append(resultsIt);
						found = true;
						break;
					}
				}

				if (found)
					continue;

				m_wordList += results;
			}
		}

		file.close();
	}

	setWordList(m_wordList);
}

void ListerDialog::showFilter(const QString &filterName)
{
	Filter *newFilter = 0;
	if (filterName == "Playability" || filterName == "Probability")
		newFilter = new PlayabilityFilter(this);
	else if (filterName == "Regex")
		newFilter = new RegexFilter(this);
	else if (filterName == "Num. Anagrams")
		newFilter = new NumAnagramsFilter(this);
	else if (filterName == "Keep British")
		newFilter = new KeepBritishFilter(this);

	if (m_currentFilter)
		saveSettings();
	delete m_currentFilter;

	m_currentFilter = newFilter;

	if (!m_currentFilter)
	{
		m_applyButton->setEnabled(false);
		return;
	}

	m_filtersLayout->addWidget(m_currentFilter);
	loadSettings();
	m_currentFilter->show();

	m_applyButton->setEnabled(true);
	connect(m_applyButton, SIGNAL(clicked()), m_currentFilter, SLOT(apply()));
}

void ListerDialog::showFilter(QListWidgetItem *item)
{
	showFilter(item->text());
}

void ListerDialog::accept()
{
	saveSettings();
	resetFocus();
	reject();
}

QString ListerDialog::run(QWidget *parent, const QString &settingsGroup, const QString &appName, int flags)
{
	ListerDialog dialog(parent, settingsGroup, appName, flags);

	bool accepted = (dialog.exec() == QDialog::Accepted);

	if (accepted)
		return dialog.filename();

	return QString();
}

void ListerDialog::saveSettings()
{
	CustomQSettings settings;
	settings.beginGroup(m_settingsGroup);

	if (m_currentFilter)
		m_currentFilter->saveSettings(&settings);

	settings.setValue("sowpods", m_sowpodsChecker->isChecked());
	settings.setValue("build", m_buildChecker->isChecked());
}

void ListerDialog::loadSettings()
{
	CustomQSettings settings;
	settings.beginGroup(m_settingsGroup);

	if (m_currentFilter)
		m_currentFilter->loadSettings(&settings);

	m_sowpodsChecker->setChecked(settings.value("sowpods", false).toBool());
	m_buildChecker->setChecked(settings.value("build", false).toBool());
}

void ListerDialog::setRemoveSowpods(bool removeSowpods)
{
	if (!removeSowpods)
		return;

	Dict::WordList filteredWords;

	Dict::WordList::Iterator end = m_wordList.end();
	for (Dict::WordList::Iterator it = m_wordList.begin(); it != end; ++it)
	{
		if (!(*it).british)
			filteredWords.append((*it));
	}

	m_wordList = filteredWords;
	populateListBox();
}

void ListerDialog::writeButtonClicked()
{
	// alphagrams
	writeList(true);
}

void ListerDialog::writeNormalButtonClicked()
{
	// not alphagrams
	writeList(false);
}

void ListerDialog::studyButtonClicked()
{
	// alphagrams
	writeList(true);

	saveSettings();
	accept();
}

void ListerDialog::populateListBox()
{
	QStringList words;

	for (const auto& it : m_wordList)
		words.append(it.word + (it.british? "#" : ""));

	m_listBox->clear();
	m_listBox->addItems(words);

	m_numResultsLabel->setText(tr("%1 &results").arg(words.count()));
}

QMap<QString, Dict::WordList> ListerDialog::anagramMap()
{
	QMap<QString, Dict::WordList> anagramSets;

	for (const auto& it : m_wordList)
	{
		QString alpha = QuackleIO::DictFactory::querier()->alphagram(it.word);
		anagramSets[alpha].append(it);
	}

	return anagramSets;
}

QString ListerDialog::writeList(bool alphagrams)
{
	if (m_filename.isEmpty())
		return QString();

	QFile file(m_filename);
	if (!file.open(QIODevice::WriteOnly | QIODevice::Text))
	{
		QMessageBox::critical(this, windowTitleWithAppName(tr("Error writing file")), tr("Could not open %1 for writing.").arg(m_filename));
		return QString();
	}

	QTextStream stream(&file);
	SET_QTEXTSTREAM_TO_UTF8(stream);

	QMap<QString, Dict::WordList> map(anagramMap());

	for (const auto& it : m_wordList)
	{
		if (alphagrams)
		{
			QString alphagram(QuackleIO::DictFactory::querier()->alphagram(it.word));
			if (map.contains(alphagram))
			{
				stream << alphagram << "\n";
				map.remove(alphagram);
			}
		}
		else
			stream << it.word << "\n";
	}
	
	file.close();

	return m_filename;
}

Dict::WordList &ListerDialog::wordList()
{
	return m_wordList;
}

void ListerDialog::setWordList(Dict::WordList list)
{
	m_wordList = list;
	setRemoveSowpods(m_sowpodsChecker->isChecked());
	populateListBox();
}

QSpinBox *ListerDialog::makeSpinBox(int minimum, int maximum, int singleStep)
{
	QSpinBox *ret = new QSpinBox;

	ret->setMinimum(minimum);
	ret->setMaximum(maximum);
	ret->setSingleStep(singleStep);

	return ret;
}

QString ListerDialog::windowTitleWithAppName(const QString &windowTitle)
{
	if (m_appName.isEmpty())
		return windowTitle;
	else
		return windowTitle + QString(" - %1").arg(m_appName);
}

///////////////

Filter::Filter(ListerDialog *dialog)
	: QFrame(dialog), m_dialog(dialog)
{
	m_vbox = new QVBoxLayout(this);
	setFrameStyle(QFrame::Panel | QFrame::Raised);
	setLineWidth(2);
}

void Filter::apply()
{
}

void Filter::saveSettings(QSettings *)
{
}

void Filter::loadSettings(QSettings *)
{
}

///////////////

PlayabilityFilter::PlayabilityFilter(ListerDialog *dialog)
	: Filter(dialog)
{
	QHBoxLayout *minRankLayout = new QHBoxLayout;
	m_vbox->addLayout(minRankLayout);

	QLabel *minRankLabel = new QLabel(tr("&Minimum rank:"));
	minRankLayout->addWidget(minRankLabel);
	m_minRankSpinner = ListerDialog::makeSpinBox(0, 99999, 100);
	m_minRankSpinner->setSpecialValueText(tr("none"));
	minRankLabel->setBuddy(m_minRankSpinner);
	minRankLayout->addWidget(m_minRankSpinner);

	QHBoxLayout *maxRankLayout = new QHBoxLayout;
	m_vbox->addLayout(maxRankLayout);

	QLabel *maxRankLabel = new QLabel(tr("Ma&ximum rank:"));
	maxRankLayout->addWidget(maxRankLabel);
	m_maxRankSpinner = ListerDialog::makeSpinBox(0, 99999, 100);
	m_maxRankSpinner->setSpecialValueText(tr("none"));
	maxRankLabel->setBuddy(m_maxRankSpinner);
	maxRankLayout->addWidget(m_maxRankSpinner);
}

void PlayabilityFilter::apply()
{
	int minimumRank = m_minRankSpinner->value();
	int maximumRank = m_maxRankSpinner->value();
	if (maximumRank == 0)
		maximumRank = INT_MAX;
	
	const bool useProb = m_dialog->flags() & ListerDialog::ProbabilityInsteadOfPlayability;

	QMap<QString, Dict::WordList> map(m_dialog->anagramMap());

	Dict::WordList intermediateList;

	QMap<QString, Dict::WordList>::Iterator end = map.end();
	for (QMap<QString, Dict::WordList>::Iterator it = map.begin(); it != end; ++it)
	{
		Dict::Word newEntry;
		newEntry.word = it.key();

		for (const auto& extIt : it.value())
		{
			if (useProb)
				newEntry.probability += extIt.probability;
			else
				newEntry.playability += extIt.playability;
		}

		intermediateList.append(newEntry);
	}

	int index = 0;

	if (useProb)
		intermediateList.setSortBy(Dict::WordList::Probability);
	else
		intermediateList.setSortBy(Dict::WordList::Playability);
	std::sort(intermediateList.begin(), intermediateList.end());

	Dict::WordList filteredList;

	for (const auto& it : intermediateList)
	{
		++index;

		if (index > maximumRank)
			break;

		if (index < minimumRank)
			continue;

		filteredList += map[it.word];
	}

	m_dialog->setWordList(filteredList);
}

void PlayabilityFilter::saveSettings(QSettings *settings)
{
	settings->setValue("playabilityfilter/minRank", m_minRankSpinner->value());
	settings->setValue("playabilityfilter/maxRank", m_maxRankSpinner->value());
}

void PlayabilityFilter::loadSettings(QSettings *settings)
{
	m_minRankSpinner->setValue(settings->value("playabilityfilter/minRank", 0).toInt());
	m_maxRankSpinner->setValue(settings->value("playabilityfilter/maxRank", 0).toInt());
}

///////////////

RegexFilter::RegexFilter(ListerDialog *dialog)
	: Filter(dialog)
{
	m_lineEdit = new QLineEdit;
	m_vbox->addWidget(m_lineEdit);
	connect(m_lineEdit, SIGNAL(returnPressed()), this, SLOT(apply()));
}

void RegexFilter::apply()
{
	QRegularExpression regexp(m_lineEdit->text(), QRegularExpression::PatternOption::CaseInsensitiveOption);
	
	Dict::WordList filteredList;
	const Dict::WordList &list = m_dialog->wordList();;

	for (const auto& it : list)
		if (regexp.match(it.word).hasMatch())
			filteredList.append(it);

	m_dialog->setWordList(filteredList);
}

////////////////////

NumAnagramsFilter::NumAnagramsFilter(ListerDialog *dialog)
	: Filter(dialog)
{
	QHBoxLayout *twlAnagramsLayout = new QHBoxLayout;
	m_vbox->addLayout(twlAnagramsLayout);

	QLabel *twlAnagramsLabel = new QLabel(tr("Number of &TWL anagrams:"));
	twlAnagramsLayout->addWidget(twlAnagramsLabel);
	m_twlAnagramsSpinner = ListerDialog::makeSpinBox(0, 15, 1);
	twlAnagramsLabel->setBuddy(m_twlAnagramsSpinner);
	twlAnagramsLayout->addWidget(m_twlAnagramsSpinner);

	QHBoxLayout *oswOnlyAnagramsLayout = new QHBoxLayout;
	QLabel *oswOnlyAnagramsLabel = new QLabel(tr("Number of &OSW-only anagrams:"));
	oswOnlyAnagramsLayout->addWidget(oswOnlyAnagramsLabel);
	m_oswOnlyAnagramsSpinner = ListerDialog::makeSpinBox(0, 15, 1);
	oswOnlyAnagramsLabel->setBuddy(m_oswOnlyAnagramsSpinner);
	oswOnlyAnagramsLayout->addWidget(m_oswOnlyAnagramsSpinner);

	if (!(m_dialog->flags() & ListerDialog::IgnoreBritishness))
		m_vbox->addLayout(oswOnlyAnagramsLayout);
}

void NumAnagramsFilter::apply()
{
	int numTwlAnagrams = m_twlAnagramsSpinner->value();
	int numOswOnlyAnagrams = m_oswOnlyAnagramsSpinner->value();

	Dict::WordList filteredList;

	QMap<QString, Dict::WordList> map(m_dialog->anagramMap());

	for (const auto& it : m_dialog->wordList())
	{
		int twl = 0;
		int british = 0;
		QString alphagram(QuackleIO::DictFactory::querier()->alphagram(it.word));

		for (const auto& word : map[alphagram])
		{
			if (word.british)
				british++;
			else
				twl++;
		}

		if ((twl == numTwlAnagrams) && (british == numOswOnlyAnagrams))
			filteredList.append(it);
	}

	m_dialog->setWordList(filteredList);
}

void NumAnagramsFilter::saveSettings(QSettings *settings)
{
	settings->setValue("numanagramsfilter/twlAnagrams", m_twlAnagramsSpinner->value());
	settings->setValue("numanagramsfilter/oswOnlyAnagrams", m_oswOnlyAnagramsSpinner->value());
}

void NumAnagramsFilter::loadSettings(QSettings *settings)
{
	m_twlAnagramsSpinner->setValue(settings->value("numanagramsfilter/twlAnagrams", 1).toInt());
	m_oswOnlyAnagramsSpinner->setValue(settings->value("numanagramsfilter/oswOnlyAnagrams", 0).toInt());
}

////////////////////

KeepBritishFilter::KeepBritishFilter(ListerDialog *dialog)
	: Filter(dialog)
{
}

void KeepBritishFilter::apply()
{
	Dict::WordList filteredList;
	const Dict::WordList &list = m_dialog->wordList();;

	for (const auto& it : list)
		if (it.british)
			filteredList.append(it);

	m_dialog->setWordList(filteredList);
}

