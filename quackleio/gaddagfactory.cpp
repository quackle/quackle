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

#include "gaddagfactory.h"
#include "util.h"

GaddagFactory::GaddagFactory(const UVString &alphabetFile)
	: m_encodableWords(0), m_unencodableWords(0), m_alphas(NULL)
{
	if (!alphabetFile.empty())
	{
		QuackleIO::FlexibleAlphabetParameters *flexure = new QuackleIO::FlexibleAlphabetParameters;
		flexure->load(QuackleIO::Util::uvStringToQString(alphabetFile));
		m_alphas = flexure;
		m_scoring = m_alphas->makeScoringAlphabet();
	}
	
	// So the separator is sorted to last.
	m_root.t = false;
	m_root.c = QUACKLE_NULL_MARK;  // "_"
	m_root.pointer = 0;
	m_root.lastchild = true;

	m_hash.int32ptr[0] = m_hash.int32ptr[1] = m_hash.int32ptr[2] = m_hash.int32ptr[3] = 0;
}

GaddagFactory::~GaddagFactory()
{
	delete m_alphas;
}

bool GaddagFactory::addScoringPatterns(const UVString &word) {
	UVString leftover;
	Quackle::LetterString encodedWord = m_alphas->encode(word, &leftover);
	if (leftover.empty()) {
		addScoringPatterns(encodedWord);
		return true;
	}
	return false;
}

void GaddagFactory::gaddagizeScoringPatterns() {
	for (const Quackle::LetterString &pattern : m_scoringPatterns) {
		//UVcout << "pattern: " << m_scoring.userVisible(pattern) << endl;
		for (const Quackle::LetterString &gaddagizedPattern : gaddagizeWord(pattern)) {
			m_gaddagizedScoringPatterns.push_back(gaddagizedPattern);
		}
	}
}

bool GaddagFactory::pushWord(const UVString &word)
{
	UVString leftover;
	Quackle::LetterString encodedWord = m_alphas->encode(word, &leftover);
	if (leftover.empty())
	{
		pushWord(encodedWord);
		return true;
	}

	++m_unencodableWords;
	return false;
}

vector<Quackle::LetterString> GaddagFactory::gaddagizeWord(const Quackle::LetterString& word) {
	vector<Quackle::LetterString> ret;
	for (unsigned i = 1; i <= word.length(); i++) {
		Quackle::LetterString newword;

		for (int j = i - 1; j >= 0; j--)
			newword.push_back(word[j]);

		if (i < word.length()) {
			newword.push_back(internalSeparatorRepresentation);  // "^"
			for (unsigned j = i; j < word.length(); j++)
				newword.push_back(word[j]);
		}
		ret.push_back(newword);
	}
	return ret;	
}

bool GaddagFactory::pushWord(const Quackle::LetterString &word)
{
	++m_encodableWords;
	hashWord(word);
	// FIXME: This hash will fail if duplicate words are passed in.
	// But testing for duplicate words isn't so easy without keeping
	// an entirely separate list.

	for (const Quackle::LetterString& gaddagizedWord : gaddagizeWord(word)) {
		m_gaddagizedWords.push_back(gaddagizedWord);		
	}

	return true;
}

void GaddagFactory::hashWord(const Quackle::LetterString &word)
{
	QCryptographicHash wordhash(QCryptographicHash::Md5);
	wordhash.addData(word.constData(), word.length());
	QByteArray wordhashbytes = wordhash.result();
	m_hash.int32ptr[0] ^= ((const int32_t*)wordhashbytes.constData())[0];
	m_hash.int32ptr[1] ^= ((const int32_t*)wordhashbytes.constData())[1];
	m_hash.int32ptr[2] ^= ((const int32_t*)wordhashbytes.constData())[2];
	m_hash.int32ptr[3] ^= ((const int32_t*)wordhashbytes.constData())[3];
}

void GaddagFactory::generate()
{
	sort(m_gaddagizedWords.begin(), m_gaddagizedWords.end());
	Quackle::WordList::const_iterator wordsEnd = m_gaddagizedWords.end();
	for (Quackle::WordList::const_iterator wordsIt = m_gaddagizedWords.begin(); wordsIt != wordsEnd; ++wordsIt)
		m_root.pushWord(*wordsIt);
	//	for (const auto& words : gaddaggizedWords)
	//		m_root.pushWord(words);
}

void GaddagFactory::writeIndex(const string &fname)
{
	m_nodelist.push_back(&m_root);

	m_root.print(m_nodelist);    

	ofstream out(fname.c_str(), ios::out | ios::binary);

	out.put(1); // GADDAG format version 1
	out.write(m_hash.charptr, sizeof(m_hash.charptr));

	for (size_t i = 0; i < m_nodelist.size(); i++)
	{
		unsigned int p = (unsigned int)(m_nodelist[i]->pointer);
		if (p != 0)
			p -= i; // offset indexing

		char bytes[4];
		unsigned char n1 = (p & 0x00FF0000) >> 16;
		unsigned char n2 = (p & 0x0000FF00) >> 8;
		unsigned char n3 = (p & 0x000000FF) >> 0;
		unsigned char n4; 

		n4 = m_nodelist[i]->c;
		if (n4 == internalSeparatorRepresentation)
			n4 = QUACKLE_NULL_MARK;

		if (m_nodelist[i]->t)
			n4 |= 64;

		if (m_nodelist[i]->lastchild)
			n4 |= 128;

		bytes[0] = n1; bytes[1] = n2; bytes[2] = n3; bytes[3] = n4;
		out.write(bytes, 4);
	}
}


void GaddagFactory::Node::print(vector< Node* >& nodelist)
{
	if (children.size() > 0)
	{
		pointer = nodelist.size();
		children[children.size() - 1].lastchild = true;
	}

	for (size_t i = 0; i < children.size(); i++)
		nodelist.push_back(&children[i]);

	for (size_t i = 0; i < children.size(); i++)
		children[i].print(nodelist);
}

void GaddagFactory::addScoringPatterns(const Quackle::LetterString& word) {
	//UVcout << "addScoringPatterns(" << m_alphas->userVisible(word) << ")..." << endl;
	int numBlanks = m_alphas->count(QUACKLE_BLANK_MARK);
	const Quackle::Letter blank = QUACKLE_BLANK_MARK;
	//UVcout << "numBlanks: " << numBlanks << endl;
	vector<set<Quackle::LetterString>> patterns(numBlanks + 1);
  const Quackle::LetterString wordPattern = m_alphas->toScoreLetters(word);
	//UVcout <<	"wordPattern: " << m_scoring.userVisible(wordPattern) << endl;
	patterns[0].insert(wordPattern);
	for (int i = 1; i <= numBlanks; ++i) {
		for (const Quackle::LetterString& pattern : patterns[i - 1]) {
			//UVcout << "pattern: " << m_scoring.userVisible(pattern) << endl;
			for (unsigned int j = 0; j < pattern.size(); ++j) {
				Quackle::LetterString blankedPattern;
				if (pattern[j] != blank) {
					for (unsigned int k = 0; k < pattern.size(); ++k) {
						if (j == k) {
							blankedPattern.push_back(blank);
						} else {
							blankedPattern.push_back(pattern[k]);
						}
						//UVcout << "blankedPattern: " << m_scoring.userVisible(blankedPattern) << endl;
					}
					//UVcout << "final blankedPattern: " << m_scoring.userVisible(blankedPattern) << endl;
					patterns[i].insert(blankedPattern);
				}
			}
		}
	}
	for (const auto& patternSet : patterns) {
		for (const Quackle::LetterString& pattern : patternSet) {
			//UVcout << m_scoring.userVisible(pattern) << endl;
			m_scoringPatterns.insert(pattern);
		}
	}
}

void GaddagFactory::Node::pushWord(const Quackle::LetterString& word)
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
