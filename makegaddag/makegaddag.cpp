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

#include <gaddag.h>
#include <quackleio/flexiblealphabet.h>
#include <quackleio/froggetopt.h>
#include <quackleio/util.h>

using namespace std;

class Node {
	public:
		Quackle::Letter c;
		bool t;
		vector<Node> children;
		int pointer;
		bool lastchild;
		void pushword(Quackle::LetterString word);
		void print(Quackle::LetterString prefix);
};

vector< Node* > nodelist;

void Node::print(Quackle::LetterString prefix) {
	if (t) {
		//UVcout << QUACKLE_ALPHABET_PARAMETERS->userVisible(prefix)) << endl;
	}

	// UVcout << "prefix: " << QUACKLE_ALPHABET_PARAMETERS->userVisible(prefix) << ", children: " << children.size() << endl;

	if (children.size() > 0) {    
		pointer = nodelist.size();
		children[children.size() - 1].lastchild = true;
	}

	for (int i = 0; i < children.size(); i++) {
		nodelist.push_back(&children[i]);
	}

	for (int i = 0; i < children.size(); i++) {
		children[i].print(prefix + children[i].c);
	}
}


void Node::pushword(Quackle::LetterString word) {
	if (word.length() == 0) {
		t = true;
	}
	else {
		Quackle::Letter first = Quackle::String::front(word);
		Quackle::LetterString rest = Quackle::String::allButFront(word);
		int index = -1;

		// cout << "first: " << first << ", rest: " << rest << endl;

		for (int i = 0; i < children.size(); i++) {
			if (children[i].c == first) {
				index = i;
				i = children.size();
			}
		}

		if (index == -1) {
			Node n;
			n.c = first;
			n.t = false;
			n.pointer = 0;
			n.lastchild = false;
			children.push_back(n);
			index = children.size() - 1;
		}

		children[index].pushword(rest);
	}
}


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

	Quackle::AlphabetParameters *alphas = 0;
	QString alphabetFile = QString("../data/alphabets/%1.quackle_alphabet").arg(alphabet);
	UVcout << "Using alphabet file: " << QuackleIO::Util::qstringToString(alphabetFile) << endl;
	QuackleIO::FlexibleAlphabetParameters *flexure = new QuackleIO::FlexibleAlphabetParameters;
	flexure->load(alphabetFile);
	alphas = flexure;

	// So the separator is sorted to last.
	Quackle::Letter internalSeparatorRepresentation = QUACKLE_FIRST_LETTER + QUACKLE_MAXIMUM_ALPHABET_SIZE;

	Node root;
	root.t = false;
	root.c = QUACKLE_NULL_MARK;  // "_"
	root.pointer = 0;
	root.lastchild = true;

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
	stream.setCodec(QTextCodec::codecForName("UTF-8"));

	int encodableWords = 0;
	int unencodableWords = 0;

	Quackle::WordList gaddagizedWords;

	while (!stream.atEnd())
	{
		QString originalQString;
        stream >> originalQString;

		if (stream.atEnd())
			break;

		UVString originalString = QuackleIO::Util::qstringToString(originalQString);

		UVString leftover;
        Quackle::LetterString encodedWord = alphas->encode(originalString, &leftover);
		if (leftover.empty())
		{
			//for (Quackle::LetterString::iterator it = encodedWord.begin(); it != encodedWord.end(); ++it)
				//UVcout << "got encoded letter: " << (int)(*it) << endl;

			++encodableWords;

			for (int i = 1; i <= encodedWord.length(); i++) {
				Quackle::LetterString newword;

				for (int j = i - 1; j >= 0; j--) {
					newword.push_back(encodedWord[j]);
				}

				if (i < encodedWord.length()) {
					newword.push_back(internalSeparatorRepresentation);  // "^"
					for (int j = i; j < encodedWord.length(); j++) {
						newword.push_back(encodedWord[j]);
					}
				}
				gaddagizedWords.push_back(newword);
			}
		}
		else
		{
			UVcout << "not encodable without leftover: " << originalString << endl;
			++unencodableWords;
		}
	}
	
	UVcout << "Sorting " << gaddagizedWords.size () << " words..." << endl;
	sort(gaddagizedWords.begin(), gaddagizedWords.end());

	UVcout << "Generating nodes...";
	Quackle::WordList::const_iterator wordsEnd = gaddagizedWords.end();
	for (Quackle::WordList::const_iterator wordsIt = gaddagizedWords.begin(); wordsIt != wordsEnd; ++wordsIt)
	{
		root.pushword(*wordsIt);
	}

	UVcout << "Writing index...";

	nodelist.push_back(&root);

	root.print("");    

	ofstream out(QuackleIO::Util::qstringToStdString(outputFilename).c_str(), ios::out | ios::binary);

	for (int i = 0; i < nodelist.size(); i++) {
		// UVcout << nodelist[i]->c << " " << nodelist[i]->pointer << " " << nodelist[i]->t << " " << nodelist[i]->lastchild << endl;

		unsigned int p = (unsigned int)(nodelist[i]->pointer);
		if (p != 0) {
			p -= i; // offset indexing
		}

		char bytes[4];
		unsigned char n1 = (p & 0x00FF0000) >> 16;
		/*
		   UVcout << "byte 1: " << ((p & 0xFF000000) >> 24); 
		   UVcout << ", byte 2: " << ((p & 0x00FF0000) >> 8);
		   UVcout << ", byte 3: " << ((p & 0x0000FF00) >> 8);
		   UVcout << ", byte 4: " << ((p & 0x000000FF) >> 0) << endl;
		   */

		unsigned char n2 = (p & 0x0000FF00) >> 8;
		unsigned char n3 = (p & 0x000000FF) >> 0;
		unsigned char n4; 

		/*
		   UVcout << "p: " << p << ", crap: " << (((unsigned int)(n1) << 24) | 
		   ((unsigned int)(n2) << 16) | 
		   ((unsigned int)(n3) << 8)) << endl;
		   */
		n4 = nodelist[i]->c;
		if (n4 == internalSeparatorRepresentation)
			n4 = QUACKLE_GADDAG_SEPARATOR;

		if (nodelist[i]->t) {
			n4 |= 64;
		}
		if (nodelist[i]->lastchild) {
			n4 |= 128;
		}

		/*
		   UVcout << "p: " << p << endl;;
		   UVcout << "n4:" << (int)(n4) << 
		   ", n1: " << (int)(n1) << 
		   ", n2: " << (int)(n2) <<
		   ", n3: " << (int)(n3) << endl;
		   */

		//bytes[0] = n4; bytes[1] = n1; bytes[2] = n2; bytes[3] = n3;
		bytes[0] = n1; bytes[1] = n2; bytes[2] = n3; bytes[3] = n4;
		//out.write((const char*) &p, 4);
		out.write(bytes, 4);
	}

	UVcout << endl;

	UVcout << "Wrote " << encodableWords << " words over " << nodelist.size() << " nodes to " << QuackleIO::Util::qstringToString(outputFilename) << "." << endl;

	if (unencodableWords > 0)
		UVcout << "There were " << unencodableWords << " words left out." << endl;

	return 0;
}
