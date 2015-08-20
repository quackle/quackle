/*
 *  Quackle -- Crossword game artificial intelligence and analysis tool
 *  Copyright (C) 2005-2014 Jason Katz-Brown and John O'Laughlin.
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


#include <iostream>
#include <QtCore>

#include "dawgfactory.h"
#include "util.h"


DawgFactory::DawgFactory(const QString& alphabetFile)
{
	QuackleIO::FlexibleAlphabetParameters *flexure = new QuackleIO::FlexibleAlphabetParameters;
	flexure->load(alphabetFile);
	alphas = flexure;

	root.t = false;
	root.insmallerdict = false;
	root.playability = 0;
	root.c = QUACKLE_BLANK_MARK;
	root.pointer = 0;
	root.lastchild = true;
}

DawgFactory::~DawgFactory()
{
	delete alphas;
}

bool DawgFactory::pushWord(const QString& word, bool inSmaller, int playability)
{
	UVString originalString = QuackleIO::Util::qstringToString(word);

	UVString leftover;
	Quackle::LetterString encodedWord = alphas->encode(originalString, &leftover);
	if (leftover.empty())
	{
		++m_encodableWords;
		root.pushWord(encodedWord, inSmaller, playability);
		return true;
	}

	++m_unencodableWords;
	return false;
}

void DawgFactory::generate()
{
	const int bucketcount = 2000;
	vector< int > bucket[bucketcount];

	nodelist.clear();
	nodelist.push_back(&root);
	root.print(nodelist);

	nodelist[0]->letterSum();

	for (unsigned int i = 0; i < nodelist.size(); i++)
	{
		bucket[nodelist[i]->sum % bucketcount].push_back(i);
		nodelist[i]->pointer = 0;
		nodelist[i]->written = false;
		nodelist[i]->deleted = false;
		nodelist[i]->cloneof = NULL;
	}

	for (int b = 0; b < bucketcount; b++)
	{
		if (bucket[b].size() == 0)
			continue;
		for (vector<int>::iterator it = bucket[b].begin(); it != bucket[b].end() - 1; it++)
		{
			if (!nodelist[(*it)]->deleted)
			{	
				for (vector<int>::iterator jt = it + 1; jt != bucket[b].end(); jt++)
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
	
	nodelist.clear();
	nodelist.push_back(&root);
	root.print(nodelist);
}

void DawgFactory::writeIndex(const QString& filename)
{
	ofstream out(QuackleIO::Util::qstringToStdString(filename).c_str(), ios::out | ios::binary);

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



void DawgFactory::Node::print(vector< Node* >& nodelist)
{
	written = true;
	
	if (children.size() == 0)
		return;

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
			nodelist.push_back(&children[i]);
		}

		for (unsigned int i = 0; i < children.size(); i++) {
			if (!children[i].deleted)
				children[i].print(nodelist);
			else if (!children[i].cloneof->written)
				children[i].cloneof->print(nodelist);
		}
	}

	if (children.size() > 0)	
		children[children.size() - 1].lastchild = true;
}


void DawgFactory::Node::pushWord(const Quackle::LetterString& word, bool inSmaller, int pb)
{
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
				break;
			}
		}
		
		if (index == -1) {
			Node n;
			n.c = first;
			n.t = false;
			n.playability = 0;
			n.insmallerdict = false;
			n.pointer = 0;
			n.lastchild = false;
			children.push_back(n);
			index = children.size() - 1;
		}

		children[index].pushWord(rest, inSmaller, pb);
	}

	sumexplored = false;
	deleted = false;
	written = false;
}


bool DawgFactory::Node::equals(const Node &n) const
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
	if (sum != n.sum)
		return false;

	for (unsigned int i = 0; i < children.size(); i++)
		if (!children[i].equals(n.children[i]))
			return false;
	
	return true;
}

int DawgFactory::Node::wordCount() const
{
	int wordCount = (t ? 0 : 1);
	for (size_t i = 0; i < children.size(); i++)
		wordCount += children[i].wordCount();
	return wordCount;
}

int DawgFactory::Node::letterSum() const
{
	if (sumexplored)
		return sum;
	
	sumexplored = true;

	// djb2 checksum
	sum = 5381 * 33 + (int) c;

	for (unsigned int i = 0; i < children.size(); i++)
		sum = (sum << 5) + sum + children[i].letterSum();

	return sum;
}
