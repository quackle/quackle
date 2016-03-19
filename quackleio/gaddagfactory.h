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

#include <set>

#include "flexiblealphabet.h"

// This isn't a strict maximum...you can go higher...but too much higher, and you risk overflowing
// node pointers, which will get you garbage words.  The OSPS dictionary is known to trigger
// such overflows.
const int QUACKLE_MAX_GADDAG_WORDCOUNT = 500000;

class GaddagFactory {
public:

	GaddagFactory(const UVString &alphabetFile);
	~GaddagFactory();

	int wordCount() const { return m_gaddagizedWords.size(); };
	int nodeCount() const { return m_nodelist.size(); };
	int encodableWords() const { return m_encodableWords; };
	int unencodableWords() const { return m_unencodableWords; };

	bool pushWord(const UVString &word);
	bool pushWord(const Quackle::LetterString &word);
	void hashWord(const Quackle::LetterString &word);
	void sortWords() { sort(m_gaddagizedWords.begin(), m_gaddagizedWords.end()); };
	void gaddagizeScoringPatterns();
	int scoringPatternCount() const { return m_scoringPatterns.size(); }
	int gaddagizedScoringPatternCount() const { return m_gaddagizedScoringPatterns.size(); }
	void sortGaddagizedScoringPatterns() { sort (m_gaddagizedScoringPatterns.begin(),
																							 m_gaddagizedScoringPatterns.end()); }
	void generate();
	void writeIndex(const string &fname, int version);
	bool addScoringPatterns(const UVString& word);
	void addScoringPatterns(const Quackle::LetterString& word);
	const char* hashBytes() { return m_hash.charptr; };


private:
	class Node {
		public:
			Quackle::Letter c;
			bool t;
			vector<Node> children;
			int pointer;
			bool lastchild;
      Node* duplicate;
			
			void pushWord(const Quackle::LetterString& word);
			void print(vector< Node* >& m_nodelist);

			int m_bitsets;
			int m_indices;
			int bitsets() const { return m_bitsets; }
			int indices() const { return m_indices; }

			int m_depth = -1;
			int depth();

			QByteArray m_hash;
			const QByteArray& hash();
			
			static void binByDepth(Node* node, map<int, vector<Node*>>* byDepth);
			static void binByHash(Node* node, map<QByteArray, vector<Node*>>* byHash);
			static void markDuplicates(const map<int, vector<Node*>>& byDepth,
																 const map<QByteArray, vector<Node*>>& byHash);
			bool sameAs(const Node& other) const;
			void numberV2(int* bitsets, int* indices);
			QByteArray v2(int numChildBytes, int numIndexBytes) const;
	};

	void writeV1(ofstream* out) const;

	void writeV2(int numChildBytes, int numIndexBytes,
							 const Node& node, ofstream* out) const;
	void writeV2(ofstream* out) const;

	int m_encodableWords;
	int m_unencodableWords;
	Quackle::WordList m_gaddagizedWords;
	vector<Node*> m_nodelist;
	Quackle::AlphabetParameters *m_alphas;
	Quackle::AlphabetParameters m_scoring;
	set<Quackle::LetterString> m_scoringPatterns;
	vector<Quackle::LetterString> m_gaddagizedScoringPatterns;
	static vector<Quackle::LetterString>
		gaddagizeWord(const Quackle::LetterString& word);
	Node m_root;
	union {
		char charptr[16];
		int32_t int32ptr[4];
	} m_hash;
};

#endif

