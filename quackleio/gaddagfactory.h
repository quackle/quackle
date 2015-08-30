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

#ifndef QUACKLE_GADDAGFACTORY_H
#define QUACKLE_GADDAGFACTORY_H

#include "flexiblealphabet.h"


class GaddagFactory {
public:

	static const Quackle::Letter internalSeparatorRepresentation = QUACKLE_FIRST_LETTER + QUACKLE_MAXIMUM_ALPHABET_SIZE;

	GaddagFactory(const QString& alphabetFile);
	~GaddagFactory();

	int wordCount() const { return m_gaddagizedWords.size(); };
	int nodeCount() const { return m_nodelist.size(); };
	int encodableWords() const { return m_encodableWords; };
	int unencodableWords() const { return m_unencodableWords; };

	bool pushWord(const QString& word);
	void hashWord(const Quackle::LetterString &word);
	void sortWords() { sort(m_gaddagizedWords.begin(), m_gaddagizedWords.end()); };
	void generate();
	void writeIndex(const QString& fname);

	const char* hashBytes() { return m_hash.charptr; };


private:
	class Node {
		public:
			Quackle::Letter c;
			bool t;
			vector<Node> children;
			int pointer;
			bool lastchild;
			void pushWord(const Quackle::LetterString& word);
			void print(vector< Node* >& m_nodelist);
	};

	int m_encodableWords;
	int m_unencodableWords;
	Quackle::WordList m_gaddagizedWords;
	vector< Node* > m_nodelist;
	Quackle::AlphabetParameters *m_alphas;
	Node m_root;
	union {
		char charptr[16];
		int32_t int32ptr[4];
	} m_hash;
};

#endif

