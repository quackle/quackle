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

#include <QtCore>

#include "flexiblealphabet.h"
#include "util.h"

using namespace QuackleIO;
using namespace std;

FlexibleAlphabetParameters::FlexibleAlphabetParameters()
{
}

bool FlexibleAlphabetParameters::load(const QString &filename)
{
	QFile file(filename);

	if (!file.exists())
	{
		UVcout << "alphabet parameters do not exist: " << QuackleIO::Util::qstringToString(filename) << endl;
		return false;
	}

	if (!file.open(QIODevice::ReadOnly | QIODevice::Text))
	{
		UVcout << "Could not open " << QuackleIO::Util::qstringToString(filename) << endl;
		return false;
	}

	QTextStream stream(&file);
	SET_QTEXTSTREAM_TO_UTF8(stream);

	QString line;
	Quackle::Letter letter = QUACKLE_FIRST_LETTER;
	while (!stream.atEnd())
	{
		line = stream.readLine().simplified();
		QStringList strings = line.split(QRegularExpression("\\s+"));

		if (line.startsWith("#"))
			continue;

		if (strings.isEmpty())
			continue;

		QString text = strings.front();
		const UVString textUV = QuackleIO::Util::qstringToString(text);
		strings.pop_front();

		const bool isBlank = text.startsWith("blank", Qt::CaseInsensitive);

		if (isBlank)
		{
			if (strings.size() < 2)
			{
				UVcerr << "Error reading in alphabet: Blank specification does not specify count and score.";
				continue;
			}
		}
		else
		{
			if (strings.size() < 4)
			{
				UVcerr << "Error reading in alphabet: Letter specification does specify blank text, count, score, and vowelness.";
				continue;
			}
		}

		QString blankText;
		if (!isBlank)
		{
			blankText = strings.front();
			strings.pop_front();
		}

		
		bool ok = false;
		int score = strings.takeFirst().toInt(&ok);
		if (!ok)
			UVcerr << "Score of letter " << textUV << " is unparsable.";

		int count = strings.takeFirst().toInt(&ok);
		if (!ok)
			UVcerr << "Count of letter " << textUV << " is unparsable.";

		bool isVowel = false;
		if (!isBlank)
		{
			isVowel = strings.takeFirst().toInt(&ok);
			if (!ok)
				UVcerr << "Vowelness of letter " << textUV << " is unparsable. (Must be 0 or 1.)";
		}

		if (isBlank)
		{
			setCount(QUACKLE_BLANK_MARK, count);
			setScore(QUACKLE_BLANK_MARK, score);
		}
		else
		{
			const UVString blankTextUV = QuackleIO::Util::qstringToString(blankText);

			setLetterParameter(letter, Quackle::LetterParameter(letter, textUV, blankTextUV, score, count, isVowel));
			++letter;

			//UVcout << "New letter " << textUV << " [" << (int)letter << "]" << endl;
		}
	}

	file.close();
	return true;
}

