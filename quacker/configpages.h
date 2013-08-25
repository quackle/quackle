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
	QComboBox *m_britishColoringCombo;
};

#endif
