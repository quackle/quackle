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

#ifndef QUACKLE_GENERATOR_H
#define QUACKLE_GENERATOR_H

#include <vector>

#include "alphabetparameters.h"
#include "game.h"
#include "move.h"

using namespace std;

namespace Quackle
{

class GaddagNode;

class ExtensionWithInfo
{
public:
	ExtensionWithInfo() : playability(0), probability(0), british(false) {}

	LetterString extensionLetterString;

	int playability;
	double probability;
	bool british;
};

class WordWithInfo
{
public:
	WordWithInfo() : playability(0), probability(0), british(false) {}

	LetterString wordLetterString;
	
	int playability;
	double probability;
	bool british;

	vector<ExtensionWithInfo> frontExtensions;
	vector<ExtensionWithInfo> backExtensions;
};

class Generator
{
public:
	Generator();
	Generator(const Quackle::GamePosition &position);
	~Generator();

	enum KibitzFlags { RegularKibitz = 0x0000, CannotExchange = 0x0001 /*, OtherOption = 0x0002, OtherOption2 = 0x0004 */ };

	// kibitzLength = 1 means kibitz list is of length one, and contains
	// only the best move, and allPossiblePlays() is invalid.
	// kibitzLength <= 1 interpreted as kibitz length of 1
	void kibitz(int kibitzLength = 10, int flags = AnagramRearrange);

	const MoveList &kibitzList();
	const MoveList &allPossiblePlays();

	// set generator to generate on this position
	// (using current player's rack)
	void setPosition(const GamePosition &position);
	const GamePosition &position() const;

	// place a move on the board; if regenerateCrosses is false,
	// you'll need to call allCrosses if you want to make more plays
	// on the board
	void makeMove(const Move &move, bool regenerateCrosses);

	enum AnagramFlags { AnagramRearrange	= 0x0000, 
			    NoRequireAllLetters	= 0x0001, 
			    AddAnyLetters	= 0x0002, 
			    ClearBlanknesses	= 0x0004,
			    SingleMatch		= 0x0008 };
	bool isAcceptableWord(const LetterString &word);
        WordList anagramLetters(const LetterString &letters, 
				int flags = AnagramRearrange);
	void storeWordInfo(WordWithInfo *wordWithInfo);
	void storeExtensions(WordWithInfo *wordWithInfo);
	void allCrosses();

private:
	// only keep track of best move
	void setrecordall(bool b);

	Board &board();
	const Rack &rack() const;

	// passes on to the global evaluator
	double equity(const Move &move) const;

	// i'll make these private very soon
	// no you won't, olaugh :)
	Move generate();
	Move gordongenerate();

	// find all opening plays on an empty board
	Move anagram();

	Move exchange();
	Move findstaticbest(bool canExchange);

	void setupCounts(const LetterString &letters);

	// returned letter is a fancy letter
	void readFromDawg(int index, unsigned int &p, Letter &letter, bool &t, bool &lastchild, bool &british, int &playability) const;

	bool checksuffix(int i, const LetterString &suffix); 
	LetterBitset fitbetween(const LetterString &pre, const LetterString &suf);
	void extendright(const LetterString &partial, int i,  
			int row, int col, int edge, int righttiles, 
			bool horizontal);
	void leftpart(const LetterString &partial, int i, int limit, 
			int row, int col, int edge, bool horizontal);
	void spit(int i, const LetterString &prefix, int flags);
	void wordspit(int i, const LetterString &prefix, int flags);

	LetterBitset gaddagFitbetween(const LetterString &pre, const LetterString &suf);
	void gaddagAnagram(const GaddagNode *node, const LetterString &prefix, int flags);
	void gordongen(int pos, const LetterString &word, const GaddagNode *node);
	void gordongoon(int pos, char L, LetterString word, const GaddagNode *node);

	void filterOutDuplicatePlays();

	// debug stuff
	UVString counts2string();
	UVString cross2string(const LetterBitset &cross);

	Move best;

	// keeps *all* moves
	MoveList m_moveList;

	// sorts and prunes into kibitzed list
	MoveList m_kibitzList;

	GamePosition m_position;

	char m_counts[QUACKLE_FIRST_LETTER + QUACKLE_MAXIMUM_ALPHABET_SIZE];
	int m_laid;
	int m_leftlimit;

	WordList m_spat;
	vector<WordWithInfo> m_wordspat;

	bool m_recordall;
	bool m_gordonhoriz;
	int m_anchorrow, m_anchorcol;
};


inline void Generator::setPosition(const GamePosition &position)
{
	m_position = position;
}

inline const GamePosition &Generator::position() const
{
	return m_position;
}

inline Board &Generator::board()
{
	return m_position.underlyingBoardReference();
}

inline const Rack &Generator::rack() const
{
	return m_position.currentPlayer().rack();
}

inline void Generator::setrecordall(bool b)
{
	m_recordall = b;
}

inline const MoveList &Generator::kibitzList()
{
	return m_kibitzList;
}

inline const MoveList &Generator::allPossiblePlays()
{
	return m_moveList;
}

}

#endif
