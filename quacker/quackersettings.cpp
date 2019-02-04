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

#include <QtGui>

#include "customqsettings.h"
#include "quackersettings.h"

QuackerSettings *QuackerSettings::m_self = 0;
QuackerSettings *QuackerSettings::self()
{
	return m_self;
}

QuackerSettings::QuackerSettings()
	: britishColoring(TextBritishColoring), verboseLabels(false), scoreLabels(true)
{
	m_self = this;
}

QuackerSettings::~QuackerSettings()
{
}

void QuackerSettings::readSettings()
{
	CustomQSettings settings;
	britishColoring = settings.value("quackle/settings/british-coloring", britishColoring).toInt();
	verboseLabels = settings.value("quackle/settings/verbose-labels", verboseLabels).toBool();
	scoreLabels = settings.value("quackle/settings/score-labels", scoreLabels).toBool();
	QuackleIO::UtilSettings::self()->vowelFirst = settings.value("quackle/settings/vowel-first", QuackleIO::UtilSettings::self()->vowelFirst).toBool();
	QuackleIO::UtilSettings::self()->octothorpBritish = settings.value("quackle/settings/octothorp-british", QuackleIO::UtilSettings::self()->octothorpBritish).toBool();
	QuackleIO::UtilSettings::self()->scoreInvalidAsZero = settings.value("quackle/settings/score-invalid-as-zero", QuackleIO::UtilSettings::self()->scoreInvalidAsZero).toBool();

	m_letterboxSettings.readSettings();
}

void QuackerSettings::writeSettings()
{
	CustomQSettings settings;
	settings.setValue("quackle/settings/british-coloring", britishColoring);
	settings.setValue("quackle/settings/verbose-labels", verboseLabels);
	settings.setValue("quackle/settings/score-labels", scoreLabels);
	settings.setValue("quackle/settings/vowel-first", QuackleIO::UtilSettings::self()->vowelFirst);
	settings.setValue("quackle/settings/octothorp-british", QuackleIO::UtilSettings::self()->octothorpBritish);
	settings.setValue("quackle/settings/score-invalid-as-zero", QuackleIO::UtilSettings::self()->scoreInvalidAsZero);

	m_letterboxSettings.writeSettings();
}

