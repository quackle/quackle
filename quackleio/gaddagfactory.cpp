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

#include "gaddagfactory.h"
#include "util.h"

GaddagFactory::GaddagFactory(const QString& alphabetFile)
{
	QuackleIO::FlexibleAlphabetParameters *flexure = new QuackleIO::FlexibleAlphabetParameters;
	flexure->load(alphabetFile);
	alphas = flexure;

	// So the separator is sorted to last.
	root.t = false;
	root.c = QUACKLE_NULL_MARK;  // "_"
	root.pointer = 0;
	root.lastchild = true;
}

void GaddagFactory::pushWord(const QString& word)
{
	UVString originalString = QuackleIO::Util::qstringToString(word);

	UVString leftover;
    Quackle::LetterString encodedWord = alphas->encode(originalString, &leftover);
	if (leftover.empty())
	{
		++m_encodableWords;

		for (unsigned i = 1; i <= encodedWord.length(); i++)
		{
			Quackle::LetterString newword;

			for (int j = i - 1; j >= 0; j--)
				newword.push_back(encodedWord[j]);

			if (i < encodedWord.length())
			{
				newword.push_back(internalSeparatorRepresentation);  // "^"
				for (unsigned j = i; j < encodedWord.length(); j++)
					newword.push_back(encodedWord[j]);
			}
			gaddagizedWords.push_back(newword);
		}
	}
	else
	{
		UVcout << "not encodable without leftover: " << originalString << endl;
		++m_unencodableWords;
	}
}

void GaddagFactory::generate()
{
	Quackle::WordList::const_iterator wordsEnd = gaddagizedWords.end();
	for (Quackle::WordList::const_iterator wordsIt = gaddagizedWords.begin(); wordsIt != wordsEnd; ++wordsIt)
		root.pushWord(*wordsIt);
	//	for (const auto& words : gaddaggizedWords)
	//		root.pushWord(words);
}

void GaddagFactory::writeIndex(const QString& fname)
{
	nodelist.push_back(&root);

	root.print(nodelist, "");    

	ofstream out(QuackleIO::Util::qstringToStdString(fname).c_str(), ios::out | ios::binary);

	for (size_t i = 0; i < nodelist.size(); i++)
	{
		unsigned int p = (unsigned int)(nodelist[i]->pointer);
		if (p != 0)
			p -= i; // offset indexing

		char bytes[4];
		unsigned char n1 = (p & 0x00FF0000) >> 16;
		unsigned char n2 = (p & 0x0000FF00) >> 8;
		unsigned char n3 = (p & 0x000000FF) >> 0;
		unsigned char n4; 

		n4 = nodelist[i]->c;
		if (n4 == internalSeparatorRepresentation)
			n4 = QUACKLE_NULL_MARK;

		if (nodelist[i]->t)
			n4 |= 64;

		if (nodelist[i]->lastchild)
			n4 |= 128;

		bytes[0] = n1; bytes[1] = n2; bytes[2] = n3; bytes[3] = n4;
		out.write(bytes, 4);
	}
}


void GaddagFactory::Node::print(vector< Node* > nodelist, Quackle::LetterString prefix)
{
	if (children.size() > 0)
	{
		pointer = nodelist.size();
		children[children.size() - 1].lastchild = true;
	}

	for (size_t i = 0; i < children.size(); i++)
		nodelist.push_back(&children[i]);

	for (size_t i = 0; i < children.size(); i++)
		children[i].print(nodelist, prefix + children[i].c);
}


void GaddagFactory::Node::pushWord(Quackle::LetterString word)
{
	if (word.length() == 0)
	{
		t = true;
		return;
	}

	Quackle::Letter first = Quackle::String::front(word);
	Quackle::LetterString rest = Quackle::String::allButFront(word);
	int index = -1;

	for (size_t i = 0; i < children.size(); i++)
	{
		if (children[i].c == first)
		{
			index = i;
			i = children.size();
		}
	}

	if (index == -1)
	{
		Node n;
		n.c = first;
		n.t = false;
		n.pointer = 0;
		n.lastchild = false;
		children.push_back(n);
		index = children.size() - 1;
	}

	children[index].pushWord(rest);
}
