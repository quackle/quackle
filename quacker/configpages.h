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

#ifndef QUACKER_CONFIG_PAGES_H
#define QUACKER_CONFIG_PAGES_H

#include <QWidget>

#include "configdialog.h"

class QComboBox;
class QSpinBox;

class LetterboxPage : public ConfigPage
{
public:
	LetterboxPage(QWidget *parent = 0);

	virtual void readConfig();
	virtual void writeConfig();

private:
	QSpinBox *m_baseSpin;
	QSpinBox *m_extraSpin;
};

class InterfacePage : public ConfigPage
{
public:
	InterfacePage(QWidget *parent = 0);

	virtual void readConfig();
	virtual void writeConfig();

private:
	QCheckBox *m_vowelFirstCheck;
	QCheckBox *m_verboseLabelsCheck;
	QCheckBox *m_scoreLabelsCheck;
	QCheckBox *m_octothorpCheck;
	QCheckBox *m_scoreInvalidAsZero;
	QComboBox *m_britishColoringCombo;
};

#endif
