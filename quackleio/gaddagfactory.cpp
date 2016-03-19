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


#include <bitset>
#include <iostream>

#include <QByteArray>
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
			newword.push_back(QUACKLE_NULL_MARK);  // delimiter
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

void GaddagFactory::generate() {
	sort(m_gaddagizedWords.begin(), m_gaddagizedWords.end());
	for (const auto& word : m_gaddagizedWords) {
		//UVcout << "pattern: " << m_alphas->userVisible(word) << endl;
		m_root.pushWord(word);
	}
}

void GaddagFactory::writeV1(ofstream* out) const {
	UVcout << "writeV1(...)" << endl;
	for (size_t i = 0; i < m_nodelist.size(); i++) {
		unsigned int p = (unsigned int)(m_nodelist[i]->pointer);
		if (p != 0)
			p -= i; // offset indexing

		char bytes[4];
		unsigned char n1 = (p & 0x00FF0000) >> 16;
		unsigned char n2 = (p & 0x0000FF00) >> 8;
		unsigned char n3 = (p & 0x000000FF) >> 0;
		unsigned char n4; 

		n4 = m_nodelist[i]->c;

		if (m_nodelist[i]->t)
			n4 |= 64;

		if (m_nodelist[i]->lastchild)
			n4 |= 128;

		bytes[0] = n1; bytes[1] = n2; bytes[2] = n3; bytes[3] = n4;
		out->write(bytes, 4);
	}
}

void GaddagFactory::writeV2(int numChildBytes, int numIndexBytes,
														const Node& node, ofstream* out) const {
	//UVcout << "writeV2(...)" << endl;
	QByteArray bytes = node.v2(numChildBytes, numIndexBytes);
	//UVcout << "bytes: " << bytes.toHex().data() << endl;
	out->write(bytes.data(), bytes.size());
	for (const Node& child : node.children) {
		if (child.children.empty()) {
			//UVcout << "no children. nothing to write." << endl;
		} else if (child.duplicate != NULL) {
			//UVcout << "is a duplicate, has already been written. " << endl;
		} else {
			writeV2(numChildBytes, numIndexBytes, child, out);
		}
	}
}

void GaddagFactory::writeV2(ofstream* out) const {
	int alphabetSize = m_alphas->lastLetter() + 1;
	UVcout << "alphabetSize: " << alphabetSize << endl;
	const int numChildBytes = (alphabetSize + 8 - 1) / 8;  // rounding up
	UVcout << "numChildBytes: " << numChildBytes << endl;
	writeV2(numChildBytes, 4, m_root, out);
}

void GaddagFactory::writeIndex(const string &fname, int version) {
	m_nodelist.push_back(&m_root);

	m_root.print(m_nodelist);    

	ofstream out(fname.c_str(), ios::out | ios::binary);

	out.put(version); // GADDAG format version
	out.write(m_hash.charptr, sizeof(m_hash.charptr));

	switch(version) {
	case 1: writeV1(&out); break;
	case 2:
		int bitsets = 0;
		int indices = 0;
		map<int, vector<Node*>> byDepth;
		Node::binByDepth(&m_root, &byDepth);
		for (const auto& pair : byDepth) {
			UVcout << "of depth " << pair.first << ": " << pair.second.size() << endl;
		}
		map<QByteArray, vector<Node*>> byHash;
		Node::binByHash(&m_root, &byHash);
		UVcout << "#unique hash keys: " << byHash.size() << endl;
		Node::markDuplicates(byDepth, byHash);
		UVcout << "Numbering gaddag nodes for v2 format..." << endl;
		m_root.numberV2(&bitsets, &indices);
		UVcout << "Found " << bitsets << " bitsets and "
					 << indices << " indices." << endl;
		writeV2(&out);
		break;
	}
}

void GaddagFactory::Node::binByDepth(Node* node, map<int, vector<Node*>>* byDepth) {
	(*byDepth)[node->depth()].push_back(node);
	for (Node& child : node->children) {
		binByDepth(&child, byDepth);
	}
}

void GaddagFactory::Node::binByHash(Node* node, map<QByteArray, vector<Node*>>* byHash) {
	(*byHash)[node->hash()].push_back(node);
	for (Node& child : node->children) {
		binByHash(&child, byHash);
	}
}

void GaddagFactory::Node::markDuplicates(const map<int, vector<Node*>>& byDepth,
																				 const map<QByteArray, vector<Node*>>& byHash) {
	for (const auto hashPair : byHash) {
		for (unsigned int i = 0; i < hashPair.second.size(); ++i) {
			Node* node_i = hashPair.second[i];
			if (node_i->duplicate != NULL) continue;
			for (unsigned int j = i + 1; j < hashPair.second.size(); ++j) {
				Node* node_j = hashPair.second[j];
				if (node_i->sameAs(*node_j)) {
					node_j->duplicate = node_i;
				} else {
					UVcout << "collision... i: " << i << " j: " << j << endl;
				}
			}
		}
	}
}

bool GaddagFactory::Node::sameAs(const Node& other) const {
	if (children.size() != other.children.size()) return false;
	for (unsigned int i = 0; i < children.size(); ++i) {
		if (children[i].c != other.children[i].c) return false;
		if (children[i].t != other.children[i].t) return false;
	}
	for (unsigned int i = 0; i < children.size(); ++i) {
		if (!children[i].sameAs(other.children[i])) return false;
	}
  return true;
}

void GaddagFactory::Node::print(vector<Node*>& nodelist) {
	if (children.size() > 0) {
		pointer = nodelist.size();
		children[children.size() - 1].lastchild = true;
	}

	for (size_t i = 0; i < children.size(); i++)
		nodelist.push_back(&children[i]);

	for (size_t i = 0; i < children.size(); i++)
		children[i].print(nodelist);
}

int GaddagFactory::Node::depth() {
	if (m_depth >= 0) {
		return m_depth;
	}
	m_depth = 0;
	int maxChildDepth = -1;
	for (Node& node : children) {
		if (node.depth() > maxChildDepth) {
			maxChildDepth = node.depth();
		}
	}
	m_depth += maxChildDepth + 1;
	return m_depth;
}

const QByteArray& GaddagFactory::Node::hash() {
	if (m_hash.isEmpty()) {
		QCryptographicHash h(QCryptographicHash::Md5);
		h.addData("foo");
		for (Node& node : children) {
			QByteArray nodec;
			nodec.append(node.c);
			h.addData(nodec);
			h.addData(node.t ? "." : "-");
			h.addData(node.hash());
		}
		m_hash = h.result();
	}
	return m_hash;
}

void GaddagFactory::Node::numberV2(int* bitsets, int* indices) {
	if (children.empty()) {
		m_bitsets = 0;
		m_indices = 0;
		return;
	}
	m_bitsets = *bitsets;
	m_indices = *indices;
	++(*bitsets);
	(*indices) += children.size();
	for (Node& child : children) {
		if (child.duplicate == NULL) {
			child.numberV2(bitsets, indices);
		}
	}
}

namespace {

	inline void ulongToBytes(unsigned long ulong, int length, char* bytes) {
		for (int i = 0; i < length; ++i) {
			const int shift = i * 8;
			bytes[i] = (ulong >> shift) & 0xFF;
		}
	}
	
}

QByteArray GaddagFactory::Node::v2(int numChildBytes, int numIndexBytes) const {
	QByteArray ret;
	int numChildPointerBytes = children.size() * numIndexBytes;
	char childPointerBytes[numChildPointerBytes];
	bitset<QUACKLE_MAXIMUM_ALPHABET_SIZE> childBits;
	int offset = 0;
	for (const Node& child : children) {
		const Node& childForPointer =
			(child.duplicate == NULL) ? child : *child.duplicate;
		unsigned long childIndex =
			numChildBytes * childForPointer.bitsets() +
			numIndexBytes * childForPointer.indices();
		ulongToBytes(childIndex, numIndexBytes, childPointerBytes + offset);
		if (child.t) {
			// set most significant bit to mark termination;
			childPointerBytes[offset+numIndexBytes-1] |= 0b10000000; 
		}
		offset += numIndexBytes;
		Quackle::Letter letter = child.c;
		childBits.set(letter);
	}
	//UVcout << "childBits: " << childBits.to_string() << endl;
	unsigned long childBitsInt = childBits.to_ulong();
	//UVcout << "childBitsInt: " << childBitsInt << endl;
	char childBytes[numChildBytes];
	ulongToBytes(childBitsInt, numChildBytes, childBytes);
	ret.append(childBytes, numChildBytes);
  ret.append(childPointerBytes, numChildPointerBytes);
	return ret;
}

void GaddagFactory::addScoringPatterns(const Quackle::LetterString& word) {
	//UVcout << "addScoringPatterns(" << m_alphas->userVisible(word) << ")..." << endl;
	int numBlanks = m_alphas->count(QUACKLE_BLANK_MARK);
	numBlanks = 0;
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
		n.duplicate = NULL;
		n.pointer = 0;
		n.lastchild = false;
		children.push_back(n);
		index = children.size() - 1;
	}

	children[index].pushWord(rest);
}
