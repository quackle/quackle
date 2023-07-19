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

#include <game.h>
#include <quackleio/util.h>

#include "geometry.h"
#include "graphicalboard.h"
#include "rackdisplay.h"

QuickEntryRack::QuickEntryRack(QWidget *parent)
	: View(parent)
{
	QHBoxLayout *textLayout = new QHBoxLayout();
	Geometry::setupInnerLayout(textLayout);

	m_lineEdit = new QLineEdit;
	m_lineEdit->setSizePolicy(QSizePolicy::Minimum, QSizePolicy::Minimum);
	connect(m_lineEdit, SIGNAL(returnPressed()), this, SLOT(quickEditReturnPressed()));

	QPushButton *setButton = new QPushButton(tr("Set Rack"));
	connect(setButton, SIGNAL(clicked()), this, SLOT(quickEditReturnPressed()));

	QPushButton *shuffleButton = new QPushButton(tr("Shu&ffle"));
	connect(shuffleButton, SIGNAL(clicked()), this, SLOT(shuffle()));

	m_label = new QLabel(tr("&Rack:"));
	m_label->setBuddy(m_lineEdit);
	textLayout->addWidget(m_label, 1);
	textLayout->addWidget(m_lineEdit, 4);
	textLayout->addWidget(setButton, 1);
	textLayout->addWidget(shuffleButton, 1);

	m_tiles = new GraphicalRack;

	QVBoxLayout *layout = new QVBoxLayout;
	Geometry::setupInnerLayout(layout);
	layout->addWidget(m_tiles);
	layout->addLayout(textLayout);
	setLayout(layout);
}

QuickEntryRack::~QuickEntryRack()
{
}

void QuickEntryRack::positionChanged(const Quackle::GamePosition *position)
{
	if (m_rackTiles == position->currentPlayer().rack().tiles())
		return;

	m_rackTiles = position->currentPlayer().rack().tiles();
	QString tiles = QuackleIO::Util::letterStringToQString(m_rackTiles);
	m_lineEdit->setText(tiles);
	m_tiles->setText(m_rackTiles);
}

void QuickEntryRack::grabFocus()
{
	m_lineEdit->setFocus();
	m_lineEdit->selectAll();
}

void QuickEntryRack::quickEditReturnPressed()
{
	QString text(m_lineEdit->text());
	m_lineEdit->clear();
	processRack(text);
}

void QuickEntryRack::processRack(const QString &rack)
{
	if (rack.isEmpty())
	{
		emit statusMessage(tr("Useless rack."));
		return;
	}
	
	emit setRack(QuackleIO::Util::makeRack(rack));
}

void QuickEntryRack::shuffle()
{
	Quackle::Rack rack(QuackleIO::Util::makeRack(m_lineEdit->text()));
	rack.shuffle();

	emit setRack(rack);
}

GraphicalRack::GraphicalRack(QWidget * parent)
	: QFrame(parent)
{
	m_layout = new QHBoxLayout(this);
	Geometry::setupInnerLayout(m_layout);
	m_layout->addStretch();

	setAcceptDrops(true);
	setMinimumSize(50, 50);
}

void
GraphicalRack::setText(const Quackle::LetterString &text)
{
	// clear old labels
	while(m_layout->count()) {
		QLabel *label = qobject_cast<QLabel*>(m_layout->itemAt(0)->widget());
		if (!label) {
			break;
		}
		m_layout->removeWidget(label);
		label->close();
	}

	PixmapCacher::self()->invalidate();
	for (int i = text.size() - 1; i >= 0 ; --i) {
		QLabel *label = new QLabel;
        label->setAttribute (Qt::WA_DeleteOnClose);

		TileWidget tile;
		Quackle::Board::TileInformation info;
        info.isOnRack = true;
		info.letter = text[i];
		info.tileType = Quackle::Board::LetterTile;
		tile.setDevicePixelRatio(devicePixelRatio());
		tile.setInformation(info);
		tile.setSideLength(50);
		tile.prepare();
	
		label->setPixmap(tile.tilePixmap());

		m_layout->insertWidget(0, label);
	}
}

const QString GraphicalRack::mime_type = "application/x-quackle-tile";

void
GraphicalRack::dragEnterEvent (QDragEnterEvent* event)
{
    if (event->mimeData()->hasFormat(mime_type))
    {
        if (event->source() == this) {
            event->setDropAction (Qt::MoveAction);
            event->accept();
        } else {
            event->acceptProposedAction();
        }
    } else {
        event->ignore();
    }
}

//---------------------------------------------------------------------------
//  dropEvent
//
//! The event handler that receives drop events.
//
//! @param event the drop event
//---------------------------------------------------------------------------
void
GraphicalRack::dropEvent (QDropEvent* event)
{
    if (event->mimeData()->hasFormat (mime_type)) {
        QByteArray itemData = event->mimeData()->data (mime_type);
        QDataStream dataStream(&itemData, QIODevice::ReadOnly);

        QPixmap pixmap;
        QPoint sourcePos;
        QPoint offset;
        dataStream >> pixmap >> sourcePos >> offset;
        pixmap.setDevicePixelRatio(devicePixelRatio());

        QLabel* droppedTile = new QLabel;
        droppedTile->setPixmap(pixmap);
        droppedTile->setAttribute (Qt::WA_DeleteOnClose);

#if (QT_VERSION >= QT_VERSION_CHECK(6, 0, 0))
        QPoint dropPos = event->position().toPoint() - offset;
#else
        QPoint dropPos = event->pos() - offset;
#endif

        // Move the tile an extra half tile width in the direction of the
        // move.  This allows the tile to assume a new spot if it is dragged
        // more than halfway onto the spot.
        int extraMove = (sourcePos.x() < dropPos.x() ? 50 / 2
                                                     : -50 / 2);

        dropPos.setX(dropPos.x() + extraMove);

		for(int i = 0; i < m_layout->count(); ++i) {
			QLabel *label = qobject_cast<QLabel*>(m_layout->itemAt(i)->widget());
			if (!label) { // hit the stretcher
				m_layout->insertWidget(i, droppedTile);
				break;
			}
			if (dropPos.x() > label->pos().x()) {
				continue;
			}
			m_layout->insertWidget(i, droppedTile);
			break;
		}

        if (event->source() == this) {
            event->setDropAction (Qt::MoveAction);
            event->accept();
        } else {
            event->acceptProposedAction();
        }
    }
    else {
        event->ignore();
    }
}

//---------------------------------------------------------------------------
//  mousePressEvent
//
//! The event handler that receives mouse press events.
//
//! @param event the mouse press event
//---------------------------------------------------------------------------
void
GraphicalRack::mousePressEvent (QMouseEvent* event)
{
    QLabel* child = qobject_cast<QLabel*>(childAt (event->pos()));
    if (!child)
        return;

#if (QT_VERSION >= QT_VERSION_CHECK(5, 15, 0))
	QPixmap pixmap = child->pixmap(Qt::ReturnByValue);
#else
    QPixmap pixmap = *(child->pixmap());
#endif

    QByteArray itemData;
    QDataStream dataStream (&itemData, QIODevice::WriteOnly);
    dataStream << pixmap << QPoint (event->pos())
               << QPoint (event->pos() - child->pos());

    QMimeData *mimeData = new QMimeData;
    mimeData->setData (mime_type, itemData);

    QDrag *drag = new QDrag (this);
    drag->setMimeData (mimeData);
    drag->setPixmap (pixmap);
    drag->setHotSpot (event->pos() - child->pos());

    QColor bgColor = palette().color(QPalette::Window);

    QPixmap tempPixmap = pixmap;
    QPainter painter;
    painter.begin (&tempPixmap);
    painter.fillRect(pixmap.rect(), QColor(bgColor.red(), bgColor.green(),
                                            bgColor.blue(), 127));
    painter.end();

    child->setPixmap (tempPixmap);

    if (drag->exec(Qt::CopyAction | Qt::MoveAction) == Qt::MoveAction) {
        child->close();
    }
    else {
        child->show();
        child->setPixmap (pixmap);
    }
}
