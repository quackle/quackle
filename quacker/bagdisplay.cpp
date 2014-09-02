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

#include <math.h>

#include <QtGui>

#include <bag.h>
#include <game.h>
#include <quackleio/util.h>

#include "bagdisplay.h"
#include "geometry.h"

BagDisplay::BagDisplay(QWidget *parent)
	: View(parent)
{
	QVBoxLayout *layout = new QVBoxLayout(this);
	Geometry::setupInnerLayout(layout);

	m_textEdit = new QTextEdit;
	m_textEdit->setReadOnly(true);
	m_textEdit->setFontFamily("Courier");
	m_textEdit->setSizePolicy(QSizePolicy::Maximum, QSizePolicy::Maximum);

	m_label = new QLabel;
	m_label->setWordWrap(true);
	m_label->setBuddy(m_textEdit);
	layout->addWidget(m_label);
	layout->addWidget(m_textEdit);

	layout->setStretchFactor(m_textEdit, 10);

	showTiles(Quackle::LongLetterString());
}

BagDisplay::~BagDisplay()
{
}

void BagDisplay::positionChanged(const Quackle::GamePosition &position)
{
	showTiles(position.unseenBag().tiles());

	// Birthday
	const Quackle::PlayerList &players = position.players();
	for (Quackle::PlayerList::const_iterator it = players.begin(); it != players.end(); ++it)
	{
		if ((*it).name() == "zorbonauts")
		{
			m_label->setText(tr("The bag is collapsed in a transparent dead jellyfish-like heap on the table while flies buzz round"));
			break;
		}
	}
}

void BagDisplay::showTiles(const Quackle::LongLetterString &tiles)
{
	if (tiles.empty())
	{
		m_label->setText(tr("&Bag is collapsed in a wrinkled heap on the table"));
		return;
	}

	QMap<Quackle::Letter, int> counts;
	QString text;

	for (Quackle::LongLetterString::const_iterator it = tiles.begin(); it != tiles.end(); ++it)
	{
		if (counts.contains(*it))
			counts[*it] += 1;
		else
			counts.insert(*it, 1);
	}

	QFontMetrics metrics(m_textEdit->currentFont());
	int maxLineWidth = 0;

	for (QMap<Quackle::Letter, int>::iterator it = counts.begin(); it != counts.end(); ++it)
	{
		const int count = it.value();

		QString line;

		const QString qstring = QuackleIO::Util::letterToQString(it.key());
		const QString sanitizedQString = QuackleIO::Util::sanitizeUserVisibleLetterString(qstring);
		const bool separateWithSpaces = qstring != sanitizedQString;
		for (int i = 0; i < count; ++i)
		{
			if (separateWithSpaces && i > 0) line += " ";
			line += sanitizedQString;
		}

		const int lineWidth = metrics.width(line);
		if (lineWidth > maxLineWidth)
			maxLineWidth = lineWidth;

		text += line;
		text += "\n";
	}

	m_label->setText(tr("%1 unseen tiles").arg(tiles.length()));
	m_textEdit->setPlainText(text);

	const int minimumMaxLineWidth = 16;
	if (maxLineWidth < minimumMaxLineWidth)
		maxLineWidth = minimumMaxLineWidth;

	const int maximumWidth = maxLineWidth + m_textEdit->frameWidth() * 2 + (m_textEdit->verticalScrollBar()->isVisible()? m_textEdit->verticalScrollBar()->width() : 0) + 10;
	m_textEdit->setMaximumSize(maximumWidth, 26 * 100);

	m_textEdit->resize(m_textEdit->maximumSize());
}

