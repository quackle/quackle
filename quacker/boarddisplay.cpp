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

#include <alphabetparameters.h>
#include <board.h>
#include <game.h>
#include <quackleio/util.h>

#include "boarddisplay.h"
#include "geometry.h"

BoardWithQuickEntry::BoardWithQuickEntry(QWidget *parent)
	: View(parent)
{
	m_vlayout = new QVBoxLayout(this);
	Geometry::setupInnerLayout(m_vlayout);

	m_lineEdit = new QLineEditWithShiftReturn;
	connect(m_lineEdit, SIGNAL(returnPressed()), this, SLOT(quickEditReturnPressed()));
	connect(m_lineEdit, SIGNAL(shiftReturnPressed()), this, SLOT(quickEditShiftReturnPressed()));

	QLabel *placeLabel = new QLabel(tr("Move: '<position> <word>' or 'exchange <tiles|number>'"));
	placeLabel->setBuddy(m_lineEdit);
	m_vlayout->addWidget(placeLabel);

	QHBoxLayout *placeEditLayout = new QHBoxLayout;
	Geometry::setupInnerLayout(placeEditLayout);
	m_vlayout->addLayout(placeEditLayout);
	placeEditLayout->addWidget(m_lineEdit);

	QPushButton *placeButton = new QPushButton(tr("Enter move"));
	connect(placeButton, SIGNAL(clicked()), this, SLOT(quickEditReturnPressed()));
	//placeEditLayout->addWidget(placeButton);

	QPushButton *scoreAdditionButton = new QPushButton(tr("+5"));
	connect(scoreAdditionButton, SIGNAL(clicked()), this, SLOT(plusFive()));
	placeEditLayout->addWidget(scoreAdditionButton);

	m_commitButton = new QPushButton(tr("Commit"));
	connect(m_commitButton, SIGNAL(clicked()), this, SLOT(performCommit()));
	placeEditLayout->addWidget(m_commitButton);

	QPushButton *resetButton = new QPushButton(tr("Rese&t"));
	connect(resetButton, SIGNAL(clicked()), this, SLOT(reset()));
	//placeEditLayout->addWidget(resetButton);
}

BoardWithQuickEntry::~BoardWithQuickEntry()
{
}

void BoardWithQuickEntry::positionChanged(const Quackle::GamePosition *position)
{
	View::positionChanged(position);
	setLocalCandidate(&position->moveMade());
}

void BoardWithQuickEntry::setLocalCandidate(const Quackle::Move *candidate)
{
	m_localCandidateMove = *candidate;

	if (candidate->isAMove())
	{
		m_lineEdit->setText(QuackleIO::Util::moveToDetailedString(*candidate));
		m_commitButton->setText(tr("Commit %1").arg(QuackleIO::Util::moveToDetailedString(*candidate)));
	}
	else
	{
		m_lineEdit->clear();
		m_commitButton->setText(tr("Commit"));
	}

	m_commitButton->setEnabled(candidate->isAMove());
}

void BoardWithQuickEntry::quickEditReturnPressed()
{
	processCommand(m_lineEdit->text());

	m_lineEdit->clear();
}

void BoardWithQuickEntry::quickEditShiftReturnPressed()
{
	quickEditReturnPressed();
	performCommit();
	m_lineEdit->setFocus();
}

void BoardWithQuickEntry::plusFive()
{
	m_localCandidateMove.setScoreAddition(m_localCandidateMove.scoreAddition() + 5);
	emit setCandidateMove(&m_localCandidateMove, nullptr);
}

void BoardWithQuickEntry::performCommit()
{
	emit setCandidateMove(&m_localCandidateMove, nullptr);
	emit commit();
}

void BoardWithQuickEntry::reset()
{
	Quackle::Move move = Quackle::Move::createNonmove();
	emit setCandidateMove(&move, nullptr);
}

void BoardWithQuickEntry::provideHelp()
{
	QMessageBox::information(this, tr("Entering Moves - Quackle"), QString("<html>") + tr("To enter a move, click on the board once or twice and start typing. Hold down the Shift key for blanks. To exchange, type something like \"exchange QWUV\" or \"pass\" into the move editor, then press the Enter key or click \"Enter move\".") + "</html>");
}

void BoardWithQuickEntry::processCommand(const QString &command)
{
#if (QT_VERSION >= QT_VERSION_CHECK(5, 14, 0))
	QStringList items(command.split(" ", Qt::SkipEmptyParts));
#else
	QStringList items(command.split(" ", QString::SkipEmptyParts));
#endif
	Quackle::Move move(Quackle::Move::createNonmove());

	if (items.size() <= 0)
	{
		provideHelp();
		return;
	}

	const QString verb(items.first().toLower());

	if (verb.startsWith("pass"))
		move = Quackle::Move::createPassMove();
	else
	{
		if (items.size() != 2)
		{
			provideHelp();
			return;
		}

		if (verb.startsWith(tr("ex")))
		{
			QString letters = items.at(1);
			bool isIntConvertable = false;
			int exchangeLength = letters.toInt(&isIntConvertable);
			bool isPass = false;

			Quackle::LetterString encodedLetters;

			if (isIntConvertable)
			{
				if (exchangeLength == 0)
				{
					isPass = true;
				}
				else
				{
					for (int i = 0; i < exchangeLength; ++i)
						encodedLetters.push_back(QUACKLE_BLANK_MARK);
				}
			}
			else
			{
				encodedLetters = QuackleIO::Util::nonBlankEncode(letters);
			}

			if (isPass)
				move = Quackle::Move::createPassMove();
			else
				move = Quackle::Move::createExchangeMove(encodedLetters, isIntConvertable);
		}
		else
		{
			QString prettyLetters(items.at(1));
			QString letters;

			bool replace = false;
			for (int i = 0; i < prettyLetters.length(); ++i)
			{
				QChar character = prettyLetters.at(i);
				if (character == '(')
					replace = true;
				else if (character == ')')
					replace = false;
				else if (replace)
					letters += ".";
				else
					letters += character;
			}
			move = Quackle::Move::createPlaceMove(QuackleIO::Util::qstringToString(items.first()), QuackleIO::Util::encode(letters));
		}
	}

	if (move.isAMove())
		emit setCandidateMove(&move, nullptr);
}

///////////

TextBoard::TextBoard(QWidget *parent)
	: BoardWithQuickEntry(parent)
{
	m_textEdit = new QTextEdit;
	m_textEdit->setFontPointSize(16);
	m_textEdit->setFontFamily("Courier");
	m_vlayout->addWidget(m_textEdit);

	m_textEdit->setReadOnly(true);
}

void TextBoard::positionChanged(const Quackle::GamePosition *position)
{
	BoardWithQuickEntry::positionChanged(position);
	//m_textEdit->setHtml(QString("<html><font size=\"+4\"><pre>%1</pre></font></html>").arg(QuackleIO::Util::uvStringToQString(position.boardAfterMoveMade().toString())));
	m_textEdit->setPlainText(QString("%1").arg(QuackleIO::Util::uvStringToQString(position->boardAfterMoveMade().toString())));
}

///////////

void QLineEditWithShiftReturn::keyPressEvent(QKeyEvent * e)
{
	if (e->key() == Qt::Key_Return || e->key() == Qt::Key_Enter)
	{
		if (e->modifiers() & Qt::ShiftModifier)
		{
			emit shiftReturnPressed();
			return;
		}
	}
	QLineEdit::keyPressEvent(e);
}

