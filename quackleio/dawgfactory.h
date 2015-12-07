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

#ifndef QUACKLE_DAWGFACTORY_H
#define QUACKLE_DAWGFACTORY_H

#include <cstdint>
#include <string>
#include <vector>
#include "flexiblealphabet.h"


class DawgFactory {
public:

	DawgFactory(const QString &alphabetFile);
	~DawgFactory();

	int wordCount() const { return m_encodableWords; };
	string letterCountString() const;
	int nodeCount() const { return m_nodelist.size(); };
	int encodableWords() const { return m_encodableWords; };
	int unencodableWords() const { return m_unencodableWords; };
	int duplicateWords() const { return m_duplicateWords; };

	bool pushWord(const UVString &word, bool inSmaller, int playability);
	bool pushWord(const Quackle::LetterString &word, bool inSmaller, int playability);
	void hashWord(const Quackle::LetterString &word);
	void generate();
	void writeIndex(const string &filename);

	const char* hashBytes() { return m_hash.charptr; };

private:
	class Node {
	public:
		bool pushWord(const Quackle::LetterString& word, bool inSmaller, int pb);
		void print(vector< Node* > &m_nodelist);

		int letterSum() const;
		bool equals(const Node &n) const;

		Quackle::Letter c;
		bool insmallerdict;
		int playability; // if nonzero, then terminates word

		vector<Node> children;
		int pointer;
		int location;

		bool lastchild;

		mutable bool sumexplored;
		mutable unsigned int sum;
		mutable vector<int> counts;
			
		bool deleted;
		Node* cloneof;
		bool written;
	};

	int m_encodableWords;
	int m_unencodableWords;
	int m_duplicateWords;
	vector< Node* > m_nodelist;
	vector<unsigned int> m_countsByLength;
	Quackle::AlphabetParameters *m_alphas;
	Node m_root;
	union {
		char charptr[16];
		std::int32_t int32ptr[4];
	} m_hash;

	static const char m_versionNumber = 1;
};

#endif


