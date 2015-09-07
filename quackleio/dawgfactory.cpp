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
#include <QCryptographicHash>

#include "dawgfactory.h"
#include "util.h"


DawgFactory::DawgFactory(const UVString& alphabetFile)
{
	QuackleIO::FlexibleAlphabetParameters *flexure = new QuackleIO::FlexibleAlphabetParameters;
	flexure->load(QuackleIO::Util::uvStringToQString(alphabetFile));
	m_alphas = flexure;

	m_root.insmallerdict = false;
	m_root.playability = 0;
	m_root.c = QUACKLE_BLANK_MARK;
	m_root.pointer = 0;
	m_root.lastchild = true;

	m_hash.int32ptr[0] = m_hash.int32ptr[1] = m_hash.int32ptr[2] = m_hash.int32ptr[3] = 0;
}

DawgFactory::~DawgFactory()
{
	delete m_alphas;
}

bool DawgFactory::pushWord(const UVString& word, bool inSmaller, int playability)
{
	UVString leftover;
	Quackle::LetterString encodedWord = m_alphas->encode(word, &leftover);
	if (leftover.empty())
	{
		if (m_root.pushWord(encodedWord, inSmaller, playability))
		{
			++m_encodableWords;
			hashWord(encodedWord);
			return true;
		}
		++m_duplicateWords;
		return false;
	}

	++m_unencodableWords;
	return false;
}

void DawgFactory::hashWord(const Quackle::LetterString &word)
{
	QCryptographicHash wordhash(QCryptographicHash::Md5);
	wordhash.addData(word.constData(), word.length());
	QByteArray wordhashbytes = wordhash.result();
	m_hash.int32ptr[0] ^= ((const int32_t*)wordhashbytes.constData())[0];
	m_hash.int32ptr[1] ^= ((const int32_t*)wordhashbytes.constData())[1];
	m_hash.int32ptr[2] ^= ((const int32_t*)wordhashbytes.constData())[2];
	m_hash.int32ptr[3] ^= ((const int32_t*)wordhashbytes.constData())[3];
}

void DawgFactory::generate()
{
	const int bucketcount = 2000;
	vector< int > bucket[bucketcount];

	m_nodelist.clear();
	m_nodelist.push_back(&m_root);
	m_root.print(m_nodelist);

	m_nodelist[0]->letterSum();

	for (unsigned int i = 0; i < m_nodelist.size(); i++)
	{
		bucket[m_nodelist[i]->sum % bucketcount].push_back(i);
		m_nodelist[i]->pointer = 0;
		m_nodelist[i]->written = false;
		m_nodelist[i]->deleted = false;
		m_nodelist[i]->cloneof = NULL;
	}

	for (int b = 0; b < bucketcount; b++)
	{
		if (bucket[b].size() == 0)
			continue;
		for (vector<int>::iterator it = bucket[b].begin(); it != bucket[b].end() - 1; it++)
		{
			if (!m_nodelist[(*it)]->deleted)
			{	
				for (vector<int>::iterator jt = it + 1; jt != bucket[b].end(); jt++)
				{
					if (!m_nodelist[(*jt)]->deleted)
					{
						// cout << "Comparing " << (*it) << " and " << (*jt) << endl;
						if (m_nodelist[(*it)]->equals(m_nodelist[(*jt)][0]))
						{
							//cout << "Hey! " << (*it) << " == " << (*jt) << endl;
							// ones[l].erase(jt);
							m_nodelist[(*jt)]->deleted = true;
							m_nodelist[(*jt)]->cloneof = m_nodelist[(*it)];
						}
					}
				}
			}
		}
	}
	
	m_nodelist.clear();
	m_nodelist.push_back(&m_root);
	m_root.print(m_nodelist);
}

void DawgFactory::writeIndex(const UVString& filename)
{
	ofstream out(filename.c_str(), ios::out | ios::binary);
	unsigned char bytes[7];

	bytes[0] = (m_encodableWords & 0x00FF0000) >> 16;
	bytes[1] = (m_encodableWords & 0x0000FF00) >>  8;
	bytes[2] = (m_encodableWords & 0x000000FF);

	out.put(1); // DAWG format version 1
	out.write(m_hash.charptr, sizeof(m_hash.charptr));
	out.write((char*)bytes, 3);

	for (unsigned int i = 0; i < m_nodelist.size(); i++) {
		//cout << m_nodelist[i]->c << " " << m_nodelist[i]->pointer << " " << m_nodelist[i]->t << " " << m_nodelist[i]->lastchild << endl;
		Node* n = m_nodelist[i];
		unsigned int p;
		if (m_nodelist[i]->deleted)
		{
			p = (unsigned int)(m_nodelist[i]->cloneof->pointer);
			// n = m_nodelist[i]->cloneof;
		}
		else
			p = (unsigned int)(m_nodelist[i]->pointer);

		bytes[0] = (p & 0x00FF0000) >> 16;
		bytes[1] = (p & 0x0000FF00) >>  8;
		bytes[2] = (p & 0x000000FF);
		bytes[3] = n->c - QUACKLE_FIRST_LETTER;
				
		unsigned int pb = n->playability;
		bytes[4] = (pb & 0x00FF0000) >> 16;
		bytes[5] = (pb & 0x0000FF00) >>  8;
		bytes[6] = (pb & 0x000000FF);

		if (n->lastchild) {
			bytes[3] |= 64;
		}
		if (n->insmallerdict) {
			bytes[3] |= 128;
		}

		out.write((char*)bytes, 7);
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


// returns true if the word was actually added...false if it's a duplicate.
bool DawgFactory::Node::pushWord(const Quackle::LetterString& word, bool inSmaller, int pb)
{
	bool added;
	if (word.length() == 0) {
		added = (playability == 0);
		playability = (pb == 0) ? 1 : pb; // word terminators nodes are marked by nonzero playability in the v1 DAWG format
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
			n.playability = 0;
			n.insmallerdict = false;
			n.pointer = 0;
			n.lastchild = false;
			children.push_back(n);
			index = children.size() - 1;
		}

		added = children[index].pushWord(rest, inSmaller, pb);
	}

	sumexplored = false;
	deleted = false;
	written = false;
	return added;
}


bool DawgFactory::Node::equals(const Node &n) const
{
	if (playability != n.playability)
		return false;
	if (c != n.c)
		return false;
	if (children.size() != n.children.size())
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
	int wordCount = ((playability == 0) ? 0 : 1);
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
