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
#include "letterboxsettings.h"

LetterboxSettings *LetterboxSettings::m_self = 0;
LetterboxSettings *LetterboxSettings::self()
{
	return m_self;
}

LetterboxSettings::LetterboxSettings()
	: msecWaitBase(1500), msecWaitExtraPerSolution(1500), backgroundColor("#eeeeee"), foregroundColor("#000000"), sowpodsColor("#ff0000"), lengthOfExtensions(3), autoCompleteLength(0), mathMode(false), numExtensionChars(32), spaceComplete(false), newMissesFile(false)
{
	m_self = this;
}

LetterboxSettings::~LetterboxSettings()
{
}

void LetterboxSettings::readSettings()
{
	CustomQSettings settings;

	msecWaitBase = settings.value("quackle/letterbox/msecWaitBase", msecWaitBase).toInt();
	msecWaitExtraPerSolution = settings.value("quackle/letterbox/msecWaitExtraPerSolution", msecWaitExtraPerSolution).toInt();

	backgroundColor = settings.value("quackle/letterbox/backgroundColor", backgroundColor).toString();
	foregroundColor = settings.value("quackle/letterbox/foregroundColor", foregroundColor).toString();
	sowpodsColor = settings.value("quackle/letterbox/sowpodsColor", sowpodsColor).toString();

	dictFilename = settings.value("quackle/letterbox/dict/filename", dictFilename).toString();
    dictGaddagFilename = settings.value("quackle/letterbox/dict/gaddagfilename", dictGaddagFilename).toString();

	lengthOfExtensions = settings.value("quackle/letterbox/lengthOfExtensions", lengthOfExtensions).toInt();

	mathMode = settings.value("quackle/letterbox/mathMode", mathMode).toBool();

	autoCompleteLength = settings.value("quackle/letterbox/autoCompleteLength", autoCompleteLength).toInt();
	
	numExtensionChars = settings.value("quackle/letterbox/numExtensionChars", numExtensionChars).toInt();
	
	spaceComplete = settings.value("quackle/letterbox/spaceComplete", spaceComplete).toBool();
	newMissesFile = settings.value("quackle/letterbox/newMissesFile", newMissesFile).toBool();
}

void LetterboxSettings::writeSettings()
{
	CustomQSettings settings;

	settings.setValue("quackle/letterbox/msecWaitBase", msecWaitBase);
	settings.setValue("quackle/letterbox/msecWaitExtraPerSolution", msecWaitExtraPerSolution);

	settings.setValue("quackle/letterbox/backgroundColor", backgroundColor);
	settings.setValue("quackle/letterbox/foregroundColor", foregroundColor);
	settings.setValue("quackle/letterbox/sowpodsColor", sowpodsColor);

	settings.setValue("quackle/letterbox/dict/filename", dictFilename);
	settings.setValue("quackle/letterbox/dict/gaddagfilename", dictGaddagFilename);

	settings.setValue("quackle/letterbox/lengthOfExtensions", lengthOfExtensions);

	settings.setValue("quackle/letterbox/mathMode", mathMode);

	settings.setValue("quackle/letterbox/autoCompleteLength", autoCompleteLength);
	settings.setValue("quackle/letterbox/numExtensionChars", numExtensionChars);

	settings.setValue("quackle/letterbox/spaceComplete", spaceComplete);
	settings.setValue("quackle/letterbox/newMissesFile", newMissesFile);
}

