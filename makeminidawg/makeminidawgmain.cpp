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

#include <map>
#include <QtCore>

#include "quackleio/dawgfactory.h"
#include "quackleio/froggetopt.h"
#include "quackleio/util.h"

std::map< QString, bool> smallerMap;
std::map< QString, int> playabilityMap;

int main(int argc, char **argv)
{
	QCoreApplication a(argc, argv);

	GetOpt opts;
	QString alphabet;
	opts.addOption('a', "alphabet", &alphabet);
	if (!opts.parse())
		return 1;

	if (alphabet.isNull())
		alphabet = "english";

	QString alphabetFile = QString("../data/alphabets/%1.quackle_alphabet").arg(alphabet);
	UVcout << "Using alphabet file: " << QuackleIO::Util::qstringToString(alphabetFile) << endl;

	DawgFactory factory(alphabetFile);


	QString smallerDictFilename = "smaller.raw";
	QFile smallerDict(smallerDictFilename);
	if (!smallerDict.exists())
	{
		UVcout << "smaller dictionary does not exist: " << QuackleIO::Util::qstringToString(smallerDictFilename) << endl;
		return false;
	}

	if (!smallerDict.open(QIODevice::ReadOnly | QIODevice::Text))
	{
		UVcout << "Could not open " << QuackleIO::Util::qstringToString(smallerDictFilename) << endl;
		return false;
	}

	QTextStream smallerStream(&smallerDict);
	SET_QTEXTSTREAM_TO_UTF8(smallerStream);
	
	while (!smallerStream.atEnd())
	{
		QString originalQString;
		smallerStream >> originalQString;
		//UVcout << "this word is in the smaller dictionary: " << QuackleIO::Util::qstringToString(originalQString) << endl;
		smallerMap[originalQString] = true;
	}

	QString playabilityFilename = "playabilities.raw";
	QFile playability(playabilityFilename);
	if (!playability.exists())
	{
		UVcout << "playability does not exist: " << QuackleIO::Util::qstringToString(playabilityFilename) << endl;
		return false;
	}

	if (!playability.open(QIODevice::ReadOnly | QIODevice::Text))
	{
		UVcout << "Could not open " << QuackleIO::Util::qstringToString(playabilityFilename) << endl;
		return false;
	}

	QTextStream playabilityStream(&playability);
	SET_QTEXTSTREAM_TO_UTF8(playabilityStream);
	
	while (!playabilityStream.atEnd())
	{
		int pb;
		playabilityStream >> pb;
		QString originalQString;
		playabilityStream >> originalQString;
		//UVcout << "playability: " << QuackleIO::Util::qstringToString(originalQString) << " " << pb << endl;
		playabilityMap[originalQString] = pb;
	}

	QString dawgFilename = "dawginput.raw";
	QFile file(dawgFilename);
	if (!file.exists())
	{
		UVcout << "dawg does not exist: " << QuackleIO::Util::qstringToString(dawgFilename) << endl;
		return false;
	}

	if (!file.open(QIODevice::ReadOnly | QIODevice::Text))
	{
		UVcout << "Could not open " << QuackleIO::Util::qstringToString(dawgFilename) << endl;
		return false;
	}

	QTextStream stream(&file);
	SET_QTEXTSTREAM_TO_UTF8(stream);

	while (!stream.atEnd())
	{
		QString word;
		stream >> word;

		bool inSmaller = smallerMap[word];
		int pb = playabilityMap[word];

		if (stream.atEnd())
			break;

		if (!factory.pushWord(QuackleIO::Util::qstringToString(word), inSmaller, pb))
			UVcout << "not encodable without leftover: " << QuackleIO::Util::qstringToString(word) << endl;
	}

	file.close();

	UVcout << "encodable words: " << factory.encodableWords() << ", unencodable words: " << factory.unencodableWords() << endl;

	UVcout << "nodelist.size(): " << factory.nodeCount() << endl;

	factory.generate();
	UVcout << "Compressed nodelist.size(): " << factory.nodeCount() << endl;

	UVcout << "Hash: " << QString(QByteArray(factory.hashBytes(), 16).toHex()).toStdString() << endl;

	factory.writeIndex("output.dawg");

	return 0;
}

