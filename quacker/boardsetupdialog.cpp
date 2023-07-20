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

#include <sstream>
#include <QtWidgets>

#include <boardparameters.h>
#include <board.h>
#include <datamanager.h>
#include <quackleio/util.h>

#include "boardsetupdialog.h"
#include "boardsetup.h"
#include "brb.h"
#include "customqsettings.h"
#include "geometry.h"
#include "widgetfactory.h"
#include "settings.h"

using namespace std;

BoardSetupDialog::BoardSetupDialog(QWidget *parent) : QDialog(parent)
{
	resize(700,550);
	setSizeGripEnabled(true);
	
	// construct the board
	BoardSetupFactory factory;
	BRB * brb = new BRB(&factory);
	m_boardFrame = static_cast<BoardSetup *>(brb->getBoardView())->boardFrame();
	initializeBoardName();

	// construct the UI elements
	m_horizontalSymmetry = new QCheckBox(tr("Horizontal"));
	m_verticalSymmetry = new QCheckBox(tr("Vertical"));
	m_diagonalSymmetry = new QCheckBox(tr("Diagonal"));
	m_horizontalSymmetry->setCheckState(Qt::Checked);
	m_verticalSymmetry->setCheckState(Qt::Checked);
	m_diagonalSymmetry->setCheckState(Qt::Checked);

	m_horizontalDimension = constructDimensionComboBox(QUACKLE_BOARD_PARAMETERS->width());
	m_verticalDimension = constructDimensionComboBox(QUACKLE_BOARD_PARAMETERS->height());

	m_boardName = new QLineEdit(QuackleIO::Util::uvStringToQString(QUACKLE_BOARD_PARAMETERS->name()));

	m_saveChanges = new QPushButton(tr("&Save Changes"));
	m_cancel = new QPushButton(tr("&Cancel"));
	m_undoAll = new QPushButton(tr("&Undo All Changes"));
	m_deleteBoard = new QPushButton(tr("&Delete Board"));

	QVBoxLayout * superLayout = new QVBoxLayout;
	Geometry::setupFramedLayout(superLayout);
	QHBoxLayout * mainLayout = new QHBoxLayout;
	Geometry::setupInnerLayout(mainLayout);
	QVBoxLayout * leftSideLayout = new QVBoxLayout;
	Geometry::setupInnerLayout(leftSideLayout);
	QLabel * boardNameLabel = new QLabel(tr("&Board name:"));
	boardNameLabel->setBuddy(m_boardName);
	QGroupBox * dimensionGroup = new QGroupBox(tr("Board dimensions"));
	QHBoxLayout * dimensionRow = new QHBoxLayout(dimensionGroup);
	Geometry::setupFramedLayout(dimensionRow);
	QLabel * dimensionLabel = new QLabel(tr(" by "));
	QGroupBox * symmetryGroup = new QGroupBox(tr("Board symmetry"));
	QVBoxLayout * symmetryCol = new QVBoxLayout(symmetryGroup);
	Geometry::setupFramedLayout(symmetryCol);
	QHBoxLayout * buttonRow = new QHBoxLayout;
	Geometry::setupInnerLayout(buttonRow);

	// build the layout
	dimensionRow->addWidget(m_horizontalDimension);
	dimensionRow->addWidget(dimensionLabel);
	dimensionRow->addWidget(m_verticalDimension);
	dimensionRow->addStretch();

	symmetryCol->addWidget(m_horizontalSymmetry);
	symmetryCol->addWidget(m_verticalSymmetry);
	symmetryCol->addWidget(m_diagonalSymmetry);

	buttonRow->addStretch(1);
	buttonRow->addWidget(m_cancel);
	buttonRow->addWidget(m_saveChanges);

	leftSideLayout->addWidget(boardNameLabel);
	leftSideLayout->addWidget(m_boardName);
	leftSideLayout->addWidget(dimensionGroup);
	leftSideLayout->addWidget(symmetryGroup);
	leftSideLayout->addWidget(m_undoAll);
	if (!m_originalName.isEmpty())
		leftSideLayout->addWidget(m_deleteBoard);
	leftSideLayout->addStretch();

	mainLayout->addLayout(leftSideLayout);
	mainLayout->addWidget(brb);
	mainLayout->setStretchFactor(leftSideLayout, 0);
	mainLayout->setStretchFactor(brb, 10);

	superLayout->addLayout(mainLayout);
	superLayout->addLayout(buttonRow);

	setLayout(superLayout);
	m_saveChanges->setDefault(true);

	// hook up signals and slots
	connect(m_horizontalDimension, SIGNAL(activated(int)), this, SLOT(parametersChanged(int)));
	connect(m_horizontalDimension, SIGNAL(activated(int)), this, SLOT(symmetryChanged()));
	connect(m_verticalDimension, SIGNAL(activated(int)), this, SLOT(parametersChanged(int)));
	connect(m_verticalDimension, SIGNAL(activated(int)), this, SLOT(symmetryChanged()));
	connect(m_boardName, SIGNAL(textEdited(const QString &)), this, SLOT(parametersChanged(const QString &)));
	connect(m_saveChanges, SIGNAL(clicked()), this, SLOT(accept()));
	connect(m_cancel, SIGNAL(clicked()), this, SLOT(reject()));
	connect(m_undoAll, SIGNAL(clicked()), this, SLOT(undoAllChanges()));
	connect(m_deleteBoard, SIGNAL(clicked()), this, SLOT(deleteBoard()));
	connect(m_horizontalSymmetry, SIGNAL(stateChanged(int)), this, SLOT(symmetryChanged()));
	connect(m_verticalSymmetry, SIGNAL(stateChanged(int)), this, SLOT(symmetryChanged()));
	connect(m_diagonalSymmetry, SIGNAL(stateChanged(int)), this, SLOT(symmetryChanged()));
	
	setWindowTitle(tr("Configure Board - Quackle"));

	// sync game board with control states and draw board
	ostringstream boardStream;
	QUACKLE_BOARD_PARAMETERS->Serialize(boardStream);
	m_serializedOriginalBoard = boardStream.str();
	
	parametersChanged(0);
	symmetryChanged();
}

BoardSetupDialog::~BoardSetupDialog()
{
}

QComboBox * BoardSetupDialog::constructDimensionComboBox(int defaultDimension)
{
	QComboBox * returnValue = new QComboBox;

	for (int i = QUACKLE_MINIMUM_BOARD_SIZE; i <= QUACKLE_MAXIMUM_BOARD_SIZE; i++)
	{
		returnValue->addItem(QString::number(i));
		if (i == defaultDimension)
			returnValue->setCurrentIndex(returnValue->count() - 1);
	}

	return returnValue;
}

void BoardSetupDialog::initializeBoardName()
{
	m_originalName = QuackleIO::Util::uvStringToQString(QUACKLE_BOARD_PARAMETERS->name());

	if (m_originalName.isEmpty())
	{
		CustomQSettings settings;
		QString generatedName = "New Board";
		int i = 1;
		settings.beginGroup("quackle/boardparameters");
		while (settings.contains(generatedName))
		{
			generatedName = "New Board ";
			generatedName += QString::number(i++);
		}
		QUACKLE_BOARD_PARAMETERS->setName(QuackleIO::Util::qstringToString(generatedName));
	}
}

void BoardSetupDialog::parametersChanged(const QString& unused)
{
	parametersChanged(0);
}

void BoardSetupDialog::parametersChanged(int unused)
{
	QUACKLE_BOARD_PARAMETERS->setWidth(m_horizontalDimension->currentIndex() + QUACKLE_MINIMUM_BOARD_SIZE);
	QUACKLE_BOARD_PARAMETERS->setHeight(m_verticalDimension->currentIndex() + QUACKLE_MINIMUM_BOARD_SIZE);

	if (QUACKLE_BOARD_PARAMETERS->startColumn() >= QUACKLE_BOARD_PARAMETERS->width())
		QUACKLE_BOARD_PARAMETERS->setStartColumn(QUACKLE_BOARD_PARAMETERS->width() - 1);
	if (QUACKLE_BOARD_PARAMETERS->startRow() >= QUACKLE_BOARD_PARAMETERS->height())
		QUACKLE_BOARD_PARAMETERS->setStartRow(QUACKLE_BOARD_PARAMETERS->height() - 1);
	UVString boardName = QuackleIO::Util::qstringToString(m_boardName->text());
	QUACKLE_BOARD_PARAMETERS->setName(boardName);

	m_game.reset();
	m_game.addPosition();
	m_boardFrame->setBoard(m_game.currentPosition().board());
	m_boardFrame->parametersChanged();
}

void BoardSetupDialog::symmetryChanged()
{
	bool allowDiagonalSymmetry =
		m_horizontalSymmetry->isChecked() && m_verticalSymmetry->isChecked() &&
		(m_horizontalDimension->currentIndex() == m_verticalDimension->currentIndex());
	m_diagonalSymmetry->setEnabled(allowDiagonalSymmetry);
	m_boardFrame->setSymmetry(
				m_horizontalSymmetry->isChecked(),
				m_verticalSymmetry->isChecked(),
				m_diagonalSymmetry->isChecked() && m_diagonalSymmetry->isEnabled());
}

void BoardSetupDialog::accept()
{
	if (m_boardName->text().isEmpty())
	{
		QMessageBox::critical(this, tr("Missing board name"),
			"You must type in a board name before saving this change.",
			QMessageBox::Ok, QMessageBox::NoButton);
		return;
	}
	else if (m_boardName->text() != m_originalName)
	{
		CustomQSettings settings;
		settings.beginGroup("quackle/boardparameters");
		if (settings.contains(m_boardName->text()))
		{
			if (QMessageBox::warning(this, tr("Overwrite existing board?"),
					tr("You've specified a board name which already exists. Do you want to overwrite the existing board?"),
					QMessageBox::Yes, QMessageBox::No) == QMessageBox::No)
				return;
		}
	}

	PixmapCacher::self()->invalidate();
	QDialog::accept();
}

void BoardSetupDialog::reject()
{
	undoAllChanges();
	QDialog::reject();
}
	
void BoardSetupDialog::undoAllChanges()
{
	istringstream boardStream(m_serializedOriginalBoard);
	
	QUACKLE_DATAMANAGER->setBoardParameters(Quackle::BoardParameters::Deserialize(boardStream));
	parametersChanged(0);
}

void BoardSetupDialog::deleteBoard()
{
	QString message = "Do you really want to delete the game board \"";
	message += m_originalName;
	message += "\"?";
	if (QMessageBox::warning(NULL, QString("Confirm Deletion"), message,
			QMessageBox::Yes | QMessageBox::No,
			QMessageBox::No) == QMessageBox::Yes)
	{
		CustomQSettings settings;
		settings.beginGroup("quackle/boardparameters");
		settings.remove(m_originalName);
		QDialog::reject();
	}
}

