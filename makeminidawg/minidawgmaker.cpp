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

#include <string>
#include <iostream>
#include <iomanip>
#include <vector>
#include <map>

#include <QtCore>

#include <quackleio/flexiblealphabet.h>
#include <quackleio/froggetopt.h>
#include <quackleio/util.h>

#include "minidawgmaker.h"

using namespace std;

class Node {
public:
	void pushword(Quackle::LetterString word, bool inSmaller, int pb);
	void print(Quackle::LetterString prefix);

	int depth();
	int subtreesize();
	int lettersum();
	bool equals(Node &n);

	Quackle::Letter c;
	bool t;
	bool insmallerdict;
	int playability;

	vector<Node> children;
	Node* parent;
	int pointer;
	int location;
	int oldpointer;

	bool lastchild;

	bool dexplored;
	bool sexplored;
	bool lexplored;

	int d;
	int s;
	int l;
		
	bool deleted;
	Node* cloneof;
	bool written;
};


vector< Node* > nodelist;
Node root;

vector< int > ofdepth[20][100];

map< QString, bool> smallerMap;
map< QString, int> playabilityMap;

bool Node::equals(Node &n)
{
	if (playability != n.playability)
		return false;
	if (c != n.c)
		return false;
	if (children.size() != n.children.size())
		return false;
	if (t != n.t)
		return false;
	if (insmallerdict != n.insmallerdict)
		return false;
	if (l != n.l)
		return false;
	if (s != n.s)
		return false;
	if (d != n.d)
		return false;

	for (unsigned int i = 0; i < children.size(); i++)
		if (!children[i].equals(n.children[i]))
			return false;
	
	return true;
}

int Node::depth()
{
	if (dexplored)
		return d;
	
	dexplored = true;

	if (children.size() == 0)
	{
		d = 1;
		return d;
	}

	int childmax = 0;

	for (unsigned int i = 0; i < children.size(); i++)
	{	
		int d = children[i].depth();
		if (d > childmax)
			childmax = d;
	}

	d = 1 + childmax;
	return d;
}


int Node::subtreesize()
{	
	if (sexplored)
		return s;
	
	sexplored = true;

	if (children.size() == 0)
	{
		s = 1;
		return s;
	}

	int childsum = 0;

	for (unsigned int i = 0; i < children.size(); i++)
	{	
		int s = children[i].subtreesize();
		childsum += s;
	}

	s = 1 + childsum;
	return s;
}


int Node::lettersum()
{
	if (lexplored)
		return l;
	
	lexplored = true;

	int thisletter = c;

	if (children.size() == 0)
	{
		l = thisletter;
		return l;
	}

	int childsum = 0;

	for (unsigned int i = 0; i < children.size(); i++)
	{	
		int s = children[i].lettersum();
		childsum += s;
	}

	l = thisletter + childsum;
	return l;
}


void Node::print(Quackle::LetterString prefix) {
	
	written = true;
	
    if (t) {
        //cout << prefix << endl;
    }

    //cout << "prefix: " << prefix << ", children: " << children.size() << ", deleted: " << deleted << ", oldpointer: " << oldpointer << ", nthChild: " << nthChild << endl;
	
	if (children.size() == 0)
	{
		//cout << "  no children.  nothing to do." << endl;
		return;
	}

	if (!deleted)
	{
		//cout << "  Setting pointer to " << nodelist.size() << " before I push_back the children." << endl;
		pointer = nodelist.size();
	}
	else
	{
		pointer = cloneof->pointer;
		//cout << "  Setting pointer to clone's (" << pointer << ") and not pushing anything." << endl;
	}

	if (!deleted)
	{
		for (unsigned int i = 0; i < children.size(); i++) {
			children[i].parent = this;
			nodelist.push_back(&children[i]);
    	}

		for (unsigned int i = 0; i < children.size(); i++) {
			if (!children[i].deleted)
    			children[i].print(prefix + children[i].c);
			else if (!children[i].cloneof->written)
				children[i].cloneof->print(prefix + children[i].c);
		}
	}

	if (children.size() > 0)	
		children[children.size() - 1].lastchild = true;
}


void Node::pushword(Quackle::LetterString word, bool inSmaller, int pb) {
    if (word.length() == 0) {
        t = true;
				playability = pb;
				insmallerdict = inSmaller;
    }
    else {
        char first = word[0];
        Quackle::LetterString rest = word.substr(1, word.length() - 1);
        int index = -1;
 
        // cout << "first: " << first << ", rest: " << rest << endl;

        for (unsigned int i = 0; i < children.size(); i++) {
            if (children[i].c == first) {
                index = i;
                i = children.size();
            }
        }
        
        if (index == -1) {
            Node n;
            n.c = first;
            n.t = false;
						n.playability = 0;
						n.insmallerdict = false;
            n.pointer = 0;
						n.oldpointer = 0;
            n.lastchild = false;
            children.push_back(n);
            index = children.size() - 1;
        }

        children[index].pushword(rest, inSmaller, pb);
    }

	dexplored = false;
	sexplored = false;
	lexplored = false;
	deleted = false;
	written = false;
}


void minimize()
{
	nodelist[0]->depth();
	nodelist[0]->subtreesize();
	nodelist[0]->lettersum();

	for (unsigned int i = 0; i < nodelist.size(); i++)
	{
		int d = nodelist[i]->d;
		if ((d >= 1) & (d <= 20))
			ofdepth[d - 1][nodelist[i]->l%100].push_back(i);
	}

	for (int d = 0; d < 20; d++)
	{
		for (int l = 0; l < 100; l++)
		{
			//cout << "l: " << l << endl;
			if (ofdepth[d][l].size() > 0)
				for (vector<int>::iterator it = ofdepth[d][l].begin(); it != ofdepth[d][l].end() - 1; it++)
				{
					if (!nodelist[(*it)]->deleted)
					{	
						for (vector<int>::iterator jt = it + 1; jt != ofdepth[d][l].end(); jt++)
						{
							if (!nodelist[(*jt)]->deleted)
							{
								// cout << "Comparing " << (*it) << " and " << (*jt) << endl;
								if (nodelist[(*it)]->equals(nodelist[(*jt)][0]))
								{
									//cout << "Hey! " << (*it) << " == " << (*jt) << endl;
									// ones[l].erase(jt);
									nodelist[(*jt)]->deleted = true;
									nodelist[(*jt)]->cloneof = nodelist[(*it)];
								}
							}
						}
					}
				}
		}
		
		for (unsigned int i = 0; i < nodelist.size(); i++)
		{
			nodelist[i]->oldpointer = nodelist[i]->pointer;
			nodelist[i]->pointer = 0;
			nodelist[i]->written = false;
		}

	}
	
	nodelist.clear();
    nodelist.push_back(&root);
    root.print("");    
	UVcout << "nodelist.size(): " << nodelist.size() << endl;
}

int MiniDawgMaker::executeFromArguments()
{
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

    root.t = false;
		root.insmallerdict = false;
		root.playability = 0;
    root.c = QUACKLE_BLANK_MARK;
    root.pointer = 0;
    root.lastchild = true;

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
	smallerStream.setCodec(QTextCodec::codecForName("UTF-8"));
	
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
	playabilityStream.setCodec(QTextCodec::codecForName("UTF-8"));
	
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
	stream.setCodec(QTextCodec::codecForName("UTF-8"));

	int encodableWords = 0;
	int unencodableWords = 0;

    while (!stream.atEnd())
	{
		QString originalQString;
    stream >> originalQString;

		bool inSmaller = smallerMap[originalQString];
		int pb = playabilityMap[originalQString];

		if (stream.atEnd())
			break;

		UVString originalString = QuackleIO::Util::qstringToString(originalQString);

		//UVcout << "read original string: " << originalString;
		//if (!inSmaller) UVcout << "#";
		//UVcout << endl;

		UVString leftover;
        Quackle::LetterString encodedWord = alphas->encode(originalString, &leftover);
		if (leftover.empty())
		{
			//for (Quackle::LetterString::iterator it = encodedWord.begin(); it != encodedWord.end(); ++it)
				//UVcout << "got encoded letter: " << (int)(*it) << endl;

			root.pushword(encodedWord, inSmaller, pb);
			++encodableWords;
		}
		else
		{
			UVcout << "not encodable without leftover: " << originalString << endl;
			++unencodableWords;
		}
    }

	file.close();
	delete alphas;

	UVcout << "encodable words: " << encodableWords << ", unencodable words: " << unencodableWords << endl;

    nodelist.push_back(&root);
    root.print("");    
	UVcout << "nodelist.size(): " << nodelist.size() << endl;

	minimize();

	ofstream out("output.dawg", ios::out | ios::binary);

    for (unsigned int i = 0; i < nodelist.size(); i++) {
        //cout << nodelist[i]->c << " " << nodelist[i]->pointer << " " << nodelist[i]->t << " " << nodelist[i]->lastchild << endl;
		Node* n = nodelist[i];
		unsigned int p;
		if (nodelist[i]->deleted)
		{
			p = (unsigned int)(nodelist[i]->cloneof->pointer);
			// n = nodelist[i]->cloneof;
		}
		else
			p = (unsigned int)(nodelist[i]->pointer);
        
        char bytes[7];
        unsigned char n1 = (p & 0x00FF0000) >> 16;
        unsigned char n2 = (p & 0x0000FF00) >>  8;
        unsigned char n3 = (p & 0x000000FF);
        unsigned char n4 = n->c - QUACKLE_FIRST_LETTER;
				
				unsigned int pb = n->playability;
				unsigned char n5 = (pb & 0x00FF0000) >> 16;
				unsigned char n6 = (pb & 0x0000FF00) >>  8;
				unsigned char n7 = (pb & 0x000000FF);

        if (n->t) {
            n4 |= 32;
        }
        if (n->lastchild) {
            n4 |= 64;
        }
				if (n->insmallerdict) {
						n4 |= 128;
				}

        bytes[0] = n1; bytes[1] = n2; bytes[2] = n3; bytes[3] = n4;
				bytes[4] = n5; bytes[5] = n6; bytes[6] = n7;
        out.write(bytes, 7);
    }
}
