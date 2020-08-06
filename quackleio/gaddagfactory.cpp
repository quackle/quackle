/*
 *  Quackle -- Crossword game artificial intelligence and analysis tool
 *  Copyright (C) 2005-2019 Jason Katz-Brown, John O'Laughlin, and John Fultz.
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


#include <algorithm>
#include <iostream>
#include <QtCore>
#include <QCryptographicHash>

#include "gaddagfactory.h"
#include "util.h"

using namespace std;

GaddagFactory::GaddagFactory(const UVString &alphabetFile)
	: m_encodableWords(0), m_unencodableWords(0), m_alphas(NULL)
{
	if (!alphabetFile.empty())
	{
		QuackleIO::FlexibleAlphabetParameters *flexure = new QuackleIO::FlexibleAlphabetParameters;
		flexure->load(QuackleIO::Util::uvStringToQString(alphabetFile));
		m_alphas = flexure;
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

bool GaddagFactory::pushWord(const Quackle::LetterString &word)
{
	++m_encodableWords;
	hashWord(word);
	// FIXME: This hash will fail if duplicate words are passed in.
	// But testing for duplicate words isn't so easy without keeping
	// an entirely separate list.

	for (unsigned i = 1; i <= word.length(); i++)
	{
		Quackle::LetterString newword;

		for (int j = i - 1; j >= 0; j--)
			newword.push_back(word[j]);

		if (i < word.length())
		{
			newword.push_back(internalSeparatorRepresentation);  // "^"
			for (unsigned j = i; j < word.length(); j++)
				newword.push_back(word[j]);
		}
		m_gaddagizedWords.push_back(newword);
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

	m_nodelist.push_back(&m_root);

	// Store all the nodes, in a breadth-first fashion (this minimizes
	// the offset sizes). Note that m_nodelist.size() grows all the time.
	for (unsigned int i = 0; i < m_nodelist.size(); i++) {
		m_nodelist[i]->print(m_nodelist);
	}
}

// Similar to the serialized format, but with non-relative pointer value.
void GaddagFactory::createDedupKey(const GaddagFactory::Node *n, char *bytes)
{
	unsigned int p = (unsigned int)(n->pointer);

	bytes[0] = (p & 0xFF000000) >> 24;
	bytes[1] = (p & 0x00FF0000) >> 16;
	bytes[2] = (p & 0x0000FF00) >> 8;
	bytes[3] = (p & 0x000000FF) >> 0;

	unsigned char n4 = n->c;
	if (n4 == internalSeparatorRepresentation)
		n4 = QUACKLE_NULL_MARK;

	if (n->t)
		n4 |= 64;

	// Don't bother with lastchild; it's implicit that the last node
	// in the key has it set, and all the others have it cleared.

	bytes[4] = n4;
}

void GaddagFactory::dedupTails()
{
	size_t initial_nodes = m_nodelist.size();
	size_t deduped;
	do {
		deduped = dedupTailsOnePass();
		UVcout << "Removed " << deduped << " nodes in this pass..." << endl;
	} while (deduped > 0);

	size_t removed_nodes = initial_nodes - m_nodelist.size();
	UVcout << "Removed " << removed_nodes << " of " << initial_nodes << " nodes"
	       << " (" << (100.0 * removed_nodes / initial_nodes) << "%), "
	       << m_nodelist.size() << " remain." << endl;
}

bool GaddagFactory::writeIndex(const string &fname)
{
	for (unsigned int i = 0; i < m_nodelist.size(); i++) {
		unsigned int p = (unsigned int)(m_nodelist[i]->pointer);
		if (p != 0 && p - i > 0xFFFFFF) {
			// Will not fit in our 24-byte offset field, so will give you garbage words.
			return false;
		}
	}

	ofstream out(fname.c_str(), ios::out | ios::binary);

	out.put(1); // GADDAG format version 1
	out.write(m_hash.charptr, sizeof(m_hash.charptr));

	for (unsigned int i = 0; i < m_nodelist.size(); i++)
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
	return true;
}

size_t GaddagFactory::dedupTailsOnePass()
{
	// For each node, find its contents (set of all accepted letters
	// and which child nodes they point to, and flags), and serialize them
	// into keys. (We don't do partial duplication yet; we deduplicate on
	// exact match only.)
	vector<pair<string, unsigned int>> keys;
	string key;
	size_t node_start = 0;
	for (unsigned int i = 0; i < m_nodelist.size(); i++)
	{
		char bytes[5];
		createDedupKey(m_nodelist[i], bytes);
		key.append(bytes, bytes + 5);

		if (m_nodelist[i]->lastchild)
		{
			// We found the end of this node, so store it and begin
			// the key for the next one.
			keys.push_back(make_pair(std::move(key), node_start));
			key.clear();
			node_start = i + 1;
		}
	}
	assert(key.empty());

	// Sort backwards, because when we choose between two identical nodes,
	// we always want to pick the one with the highest ID. (This ensures that
	// children pointers never start pointing backwards, although it can in
	// theory increase the problem of ID overflows making very long word lists
	// uncompilable.)
	sort(keys.begin(), keys.end(), greater<>());

	// Find all duplicated keys, which is easy now that the list is in order.
	// dup_rewrites contains a mapping from old to new ID for each node ID
	// in the list (including those that are not found to be duplicates,
	// and those that are irrelevant -- it keeps the structure simpler).
	unique_ptr<unsigned int[]> dup_rewrites(new unsigned int[m_nodelist.size()]);
	for (unsigned int i = 0; i < m_nodelist.size(); i++)
	{
		dup_rewrites[i] = i;
	}
	const pair<string, unsigned int> *cur_key = nullptr;
	for (const auto &key : keys)
	{
		if (cur_key == nullptr || key.first != cur_key->first)
		{
			// The first (and possibly only) instance of a new key.
			cur_key = &key;
			continue;
		}
		// A duplicate.
		dup_rewrites[key.second] = cur_key->second;
	}

	// Compress away all nodes that are now obsolete, noting their new positions.
	unique_ptr<unsigned int[]> post_compress_pos(new unsigned int[m_nodelist.size()]);
	bool curr_node_is_deduped_away = false;
	unsigned int write_pos = 0;
	for (unsigned int i = 0; i < m_nodelist.size(); i++)
	{
		Node *n = m_nodelist[i];
		if (dup_rewrites[i] != i)
		{
			assert(i == 0 || m_nodelist[i - 1]->lastchild);
			curr_node_is_deduped_away = true;
		}
		if (!curr_node_is_deduped_away)
		{
			post_compress_pos[i] = write_pos;
			m_nodelist[write_pos++] = n;
		}
		if (n->lastchild)
		{
			curr_node_is_deduped_away = false;
		}
	}
	const int new_num_nodes = write_pos;

	// Rewrite all the child pointers to match the new positions.
	for (unsigned int i = 0; i < new_num_nodes; i++)
	{
		Node *n = m_nodelist[i];
		if (n->pointer != 0)
		{
			// Account for deduplication, if appropriate.
			n->pointer = dup_rewrites[n->pointer];

			// Then account for the compression we just did.
			n->pointer = post_compress_pos[n->pointer];
		}
	}

	// Finally, discard all the nodes left over. We've now done one pass;
	// this may have opened up the opportunity for more deduplication,
	// so the caller should call us again as long as we managed to do any
	// work.
	size_t num_removed = m_nodelist.size() - new_num_nodes;
	m_nodelist.resize(new_num_nodes);
	return num_removed;
}

void GaddagFactory::Node::print(vector< Node* >& nodelist)
{
	if (children.size() > 0)
	{
		pointer = (int)nodelist.size();
		children[children.size() - 1].lastchild = true;
	}

	for (size_t i = 0; i < children.size(); i++)
		nodelist.push_back(&children[i]);
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
			index = (int)i;
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
		index = (int)children.size() - 1;
	}

	children[index].pushWord(rest);
}
