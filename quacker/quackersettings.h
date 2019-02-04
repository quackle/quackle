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

#ifndef QUACKER_QUACKERSETTINGS_H
#define QUACKER_QUACKERSETTINGS_H

#include <QString>

#include <quackleio/util.h>

#include "letterboxsettings.h"

// This singleton class keeps all Quacker settings in one place.
// It also maintains one instance of a LetterboxSettings and QuackleIO::UtilSettings.

enum BritishColoring { NoBritishColoring = 0, TextBritishColoring = 1, TileBritishColoring = 2};

class QuackerSettings
{
public:
	QuackerSettings();
	~QuackerSettings();

	static QuackerSettings *self();

	// reads and writes Quacker and Letterbox settings
	void readSettings();
	void writeSettings();

	int britishColoring;
	bool verboseLabels;
	bool scoreLabels;

private:
	static QuackerSettings *m_self;

	QuackleIO::UtilSettings m_utilSettings;
	LetterboxSettings m_letterboxSettings;
};

#endif
