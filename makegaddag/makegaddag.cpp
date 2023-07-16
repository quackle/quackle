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

#include "alphabetparameters.h"

#include <stdio.h>
#include <string>
#include <fstream>
#include <iostream>
#include <iomanip>
#include <vector>
#include <map>
#include <algorithm>

#include <QtCore>

#include "quackleio/froggetopt.h"
#include "quackleio/gaddagfactory.h"
#include "quackleio/util.h"

using namespace std;



int main(int argc, char **argv)
{
	QCoreApplication a(argc, argv);

	GetOpt opts;
	QString alphabet;
	QString inputFilename;
	QString outputFilename;
	opts.addOption('f', "input", &inputFilename);
	opts.addOption('o', "output", &outputFilename);
	opts.addOption('a', "alphabet", &alphabet);
	if (!opts.parse())
		return 1;

	if (alphabet.isNull())
		alphabet = "english";

	if (inputFilename.isNull())
		inputFilename = "gaddaginput.raw";

	if (outputFilename.isNull())
		outputFilename = "output.gaddag";

	QString alphabetFile = QString("../data/alphabets/%1.quackle_alphabet").arg(alphabet);
	UVcout << "Using alphabet file: " << QuackleIO::Util::qstringToString(alphabetFile) << endl;
	GaddagFactory factory(QuackleIO::Util::qstringToString(alphabetFile));

	QFile file(inputFilename);
	if (!file.exists())
	{
		UVcout << "Input gaddag does not exist: " << QuackleIO::Util::qstringToString(inputFilename) << endl;
		return false;
	}

	if (!file.open(QIODevice::ReadOnly | QIODevice::Text))
	{
		UVcout << "Could not open " << QuackleIO::Util::qstringToString(inputFilename) << endl;
		return false;
	}

	QTextStream stream(&file);
	SET_QTEXTSTREAM_TO_UTF8(stream);

	while (!stream.atEnd())
	{
		QString originalQString;
        stream >> originalQString;

		if (stream.atEnd())
			break;

		if (!factory.pushWord(QuackleIO::Util::qstringToString(originalQString)))
			UVcout << "not encodable without leftover: " << QuackleIO::Util::qstringToString(originalQString) << endl;
	}
	
	UVcout << "Sorting " << factory.wordCount() << " words..." << endl;
	factory.sortWords();

	UVcout << "Generating nodes...";
	factory.generate();

	UVcout << "Writing index...";
	factory.writeIndex(outputFilename.toUtf8().constData());

	UVcout << endl;

	UVcout << "Wrote " << factory.encodableWords() << " words over " << factory.nodeCount() << " nodes to " << QuackleIO::Util::qstringToString(outputFilename) << "." << endl;

	UVcout << "Hash: " << QString(QByteArray(factory.hashBytes(), 16).toHex()).toStdString() << endl;

	if (factory.unencodableWords() > 0)
		UVcout << "There were " << factory.unencodableWords() << " words left out." << endl;

	return 0;
}
