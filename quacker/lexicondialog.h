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

#ifndef QUACKER_LEXICONDIALOG_H
#define QUACKER_LEXICONDIALOG_H

#include <string>
#include "game.h"
#include "lexiconparameters.h"

#include <QWidget>
#include <QDialog>

using namespace Quackle;

class QComboBox;
class QLabel;
class QLineEdit;
class QPushButton;
class DawgFactory;
class FileNameValidator;

class LexiconDialog : public QDialog
{
Q_OBJECT

public:
	LexiconDialog(QWidget *parent = 0, const QString &originalName = QString());
	~LexiconDialog();
	virtual void accept();

	bool itemWasDeleted() { return m_deleted; };
	const QString &lexiconName() { return m_finalLexiconName; };

	void updateLexiconInformation(bool firstTime = false);

protected slots:
	void parametersChanged(const QString &) { updateLexiconInformation(); };
	void deleteLexicon();
	void addWordsFromFile();
	void alphabetChanged(int);
	void loadOriginalDictionary();

protected:
	void addWordsFromDawgFile(const QString &dawgfile);
	void addWordsFromDawgRecursive(const LexiconParameters &lexParams, Quackle::LetterString &word, int index);
	void addWordsFromTextFile(const QString &textFile);

private:
	QLineEdit *m_lexiconName;
	QComboBox *m_alphabetCombo;
	QPushButton *m_addWordsFromFile;
	QPushButton *m_clearAllWords;
	QLabel *m_lexiconInformation;
	FileNameValidator * m_fileNameValidator;
	
	QPushButton *m_saveChanges;
	QPushButton *m_cancel;
	QPushButton *m_deleteLexicon;
	
	QString m_originalName;
	QString m_alphabetFileName;
	QByteArray m_originalHash;
	QString m_finalLexiconName;
	bool m_deleted;

	DawgFactory *m_wordFactory;
};

#endif
