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

#include <game.h>
#include <quackleio/util.h>

#include "geometry.h"
#include "noteeditor.h"

NoteEditor::NoteEditor(QWidget *parent)
	: View(parent)
{
	QVBoxLayout *layout = new QVBoxLayout(this);
	Geometry::setupInnerLayout(layout);

	m_textEdit = new QTextEdit;
	m_textEdit->setMinimumSize(60, 20);
	layout->addWidget(m_textEdit);

	showNote(UVString());

	connect(m_textEdit, SIGNAL(textChanged()), this, SLOT(noteEdited()));
}

NoteEditor::~NoteEditor()
{
}

QSize NoteEditor::sizeHint() const
{
	//return QSize(20, 20);
	QSize hint = QFrame::sizeHint();
	return QSize(hint.width(), hint.height() / 2);
}

void NoteEditor::positionChanged(const Quackle::GamePosition *position)
{
	showNote(position->explanatoryNote());
}

void NoteEditor::showNote(const UVString &note)
{
	if (note.empty())
	{
		manuallySetNoteText(tr("Type a note here!"));
		return;
	}

	const QString qstringNote = QuackleIO::Util::uvStringToQString(note);

	manuallySetNoteText(qstringNote);
}

void NoteEditor::manuallySetNoteText(const QString &note)
{
	disconnect(m_textEdit, SIGNAL(textChanged()), this, SLOT(noteEdited()));
	m_textEdit->setPlainText(note);
	connect(m_textEdit, SIGNAL(textChanged()), this, SLOT(noteEdited()));
}

void NoteEditor::noteEdited()
{
	emit setNote(QuackleIO::Util::qstringToString(m_textEdit->toPlainText()));
}
