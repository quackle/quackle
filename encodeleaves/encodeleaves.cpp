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

#include <string>
#include <iostream>
#include <iomanip>
#include <vector>
#include <map>

#include <QtCore>

#include <quackleio/flexiblealphabet.h>
#include <quackleio/froggetopt.h>
#include <quackleio/util.h>

using namespace std;

int main(int argc, char **argv) {
	QCoreApplication a(argc, argv);

	GetOpt opts;
	QString alphabet;
	opts.addOption('a', "alphabet", &alphabet);
	if (!opts.parse())
		return 1;

	if (alphabet.isNull())
		alphabet = "english";

	Quackle::AlphabetParameters *alphas = 0;
	QString alphabetFile = QString("../data/alphabets/%1.quackle_alphabet").arg(alphabet);
	UVcout << "Using alphabet file: " << QuackleIO::Util::qstringToString(alphabetFile) << endl;
	QuackleIO::FlexibleAlphabetParameters *flexure = new QuackleIO::FlexibleAlphabetParameters;
	flexure->load(alphabetFile);
	alphas = flexure;

	QString leavesFilename = "superleaves.raw";
	QFile file(leavesFilename);
	if (!file.exists())
	{
		UVcout << "leaves file does not exist: " << QuackleIO::Util::qstringToString(leavesFilename) << endl;
		return false;
	}

	if (!file.open(QIODevice::ReadOnly | QIODevice::Text))
	{
		UVcout << "Could not open " << QuackleIO::Util::qstringToString(leavesFilename) << endl;
		return false;
	}

	QTextStream stream(&file);
	SET_QTEXTSTREAM_TO_UTF8(stream);

	ofstream out("encoded");

	int encodableLeaves = 0;
	int unencodableLeaves = 0;

  while (!stream.atEnd()) {
		QString leaveQString;
		stream >> leaveQString;
		double value;
		stream >> value;
		//UVcout << "value: " << value << endl;

		UVString leaveString = QuackleIO::Util::qstringToString(leaveQString);

		if (stream.atEnd())
			break;

		//UVcout << "read original string: " << originalString << endl;
		UVString leftover;
    Quackle::LetterString encodedLeave = alphas->encode(leaveString, &leftover);
		if (leftover.empty())
		{
			unsigned char leavelength = encodedLeave.length();
			out.write((char*)(&leavelength), 1);
			out.write(encodedLeave.begin(), encodedLeave.length());
			unsigned short int intvalue = (value + 128) * 256;
			//UVcout << "intvalue: " << intvalue << endl;
			out.write((char*)(&intvalue), 2);
			++encodableLeaves;
		}
		else
		{
			//UVcout << "not encodable without leftover: " << originalString << endl;
			++unencodableLeaves;
		}
    }

	file.close();
	delete alphas;

	UVcout << "encodable leaves: " << encodableLeaves << ", unencodable leaves: " << unencodableLeaves << endl;

}
