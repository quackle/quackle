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

#include <fstream>
#include <iostream>
#include <math.h>

#include "datamanager.h"
#include "evaluator.h"
#include "generator.h"
#include "gaddag.h"
#include "board.h"
#include "boardparameters.h"
#include "gameparameters.h"
#include "lexiconparameters.h"

// #define DEBUG_GENERATOR

using namespace std;
using namespace Quackle;

Generator::Generator()
{
}

Generator::Generator(const GamePosition &position)
	: m_position(position)
{
}

Generator::~Generator()
{
}

void Generator::kibitz(int kibitzLength, int flags)
{
	// don't just record best move, unless kibitz length is one
    setrecordall(kibitzLength > 1);

	// perform actual kibitz
    findstaticbest(!(flags & CannotExchange));

	m_kibitzList.clear();

	if (kibitzLength <= 1)
	{
		m_kibitzList.push_back(best);
		return;
	}

	filterOutDuplicatePlays();

	MoveList::sort(m_moveList, MoveList::Equity);

	int i = 0;
	MoveList::iterator end(m_moveList.end());
    for (MoveList::iterator it = m_moveList.begin(); it != end; ++it, ++i)
	{
		if (i >= kibitzLength)
			break;

		m_kibitzList.push_back(*it);
	}
}

void Generator::filterOutDuplicatePlays()
{
	map<int, bool> oneTilePlayMap;
	for (MoveList::iterator it = m_moveList.begin(); it != m_moveList.end();)
	{
		LetterString usedTiles = (*it).usedTiles();
		if (usedTiles.size() == 1)
		{
			const LetterString &tiles = (*it).tiles();
			int actualTileIndex = 0;
			for (LetterString::const_iterator letterIt = tiles.begin(); letterIt != tiles.end(); ++letterIt, ++actualTileIndex)
				if ((*letterIt) != QUACKLE_PLAYED_THRU_MARK)
					break;

			const int row = (*it).startrow + ((*it).horizontal? 0 : actualTileIndex);
			const int column = (*it).startcol + ((*it).horizontal? actualTileIndex : 0);
			int key = row + QUACKLE_MAXIMUM_BOARD_SIZE * column + (QUACKLE_MAXIMUM_BOARD_SIZE * QUACKLE_MAXIMUM_BOARD_SIZE) * tiles[actualTileIndex];

			if (oneTilePlayMap.find(key) == oneTilePlayMap.end())
			{
				oneTilePlayMap[key] = true;
				++it;
			}
			else
			{
				it = m_moveList.erase(it);
			}
		}
		else
		{
			++it;
		}
	}
}

void Generator::allCrosses()
{
	vector<int> vrows, vcols, hrows, hcols;

	for (int i = 0; i < board().height(); i++) {
		for (int j = 0; j < board().width(); j++) {
			vrows.push_back(i);
			vcols.push_back(j);
			hrows.push_back(i);
			hcols.push_back(j);
		}
	}

	// check the appropriate crosses
	for (unsigned int i = 0; i < vrows.size(); i++) {
		int row = vrows[i];
		int col = vcols[i];

		if (QUACKLE_ALPHABET_PARAMETERS->isSomeLetter(board().letter(row, col))) {
			board().setVCross(row, col, LetterBitset());
		}
		else { 
			LetterString pre; 
			if (row > 0) {
				for (int i = row - 1; i >= 0; i--) {
					if (!QUACKLE_ALPHABET_PARAMETERS->isSomeLetter(board().letter(i, col))) {
						i = -1;
					}
					else {
						LetterString newpre;
						newpre += QUACKLE_ALPHABET_PARAMETERS->clearBlankness(board().letter(i, col));
						newpre += pre;
						pre = newpre;
					}
				}
			}

			LetterString suf;
			if (row < board().height() - 1) {
				for (int i = row + 1; i < board().height(); i++) {
					if (!QUACKLE_ALPHABET_PARAMETERS->isSomeLetter(board().letter(i, col))) {
						i = board().height();
					}
					else {
						suf += QUACKLE_ALPHABET_PARAMETERS->clearBlankness(board().letter(i, col));
					}
				}
			}

#ifdef DEBUG_GENERATOR
			UVcout << QUACKLE_ALPHABET_PARAMETERS->userVisible(pre) << " / " << QUACKLE_ALPHABET_PARAMETERS->userVisible(suf) << endl;
#endif

			if (pre.empty() && suf.empty()) {
				board().setVCross(row, col, LetterBitset().set());
			}
			else {
				board().setVCross(row, col, fitbetween(pre, suf));
			}

#ifdef DEBUG_GENERATOR
			UVcout << "board().vcross[" << row << "][" << col << "] = " << cross2string(board().vcross(row, col)) << endl;
#endif
		}
	}

	for (unsigned int i = 0; i < hrows.size(); i++) {
		int row = hrows[i];
		int col = hcols[i];

		if (QUACKLE_ALPHABET_PARAMETERS->isSomeLetter(board().letter(row, col))) {
			board().setHCross(row, col, LetterBitset());
		}
		else { 
			LetterString pre;
			if (col > 0) {
				for (int i = col - 1; i >= 0; i--) {
					if (!QUACKLE_ALPHABET_PARAMETERS->isSomeLetter(board().letter(row, i))) {
						i = -1;
					}
					else {
						LetterString newpre;
						newpre += QUACKLE_ALPHABET_PARAMETERS->clearBlankness(board().letter(row, i));
						newpre += pre;
						pre = newpre;
					}
				}
			}

			LetterString suf;
			if (col < board().width() - 1) {
				for (int i = col + 1; i < board().width(); i++) {
					if (!QUACKLE_ALPHABET_PARAMETERS->isSomeLetter(board().letter(row, i))) {
						i = board().width();
					}
					else {
						suf += QUACKLE_ALPHABET_PARAMETERS->clearBlankness(board().letter(row, i));
					}
				}
			}
			if (pre.empty() && suf.empty()) {
				board().setHCross(row, col, LetterBitset().set());
			}
			else {
				board().setHCross(row, col, fitbetween(pre, suf));
			}

#ifdef DEBUG_GENERATOR
			UVcout << "board().hcross[" << row << "][" << col << "] = " << cross2string(board().hcross(row, col)) << endl;
#endif
		}
	}
}

void Generator::makeMove(const Move &move, bool regenerateCrosses)
{
	if (move.action != Move::Place)
		return;

	if (!regenerateCrosses)
	{
		board().makeMove(move);
		return;
	}

	// mark which squares need their crosses checked
	vector<int> hrows;
	vector<int> hcols;
	vector<int> vrows;
	vector<int> vcols;
	hrows.reserve(2 * QUACKLE_MAXIMUM_BOARD_SIZE);
	hcols.reserve(2 * QUACKLE_MAXIMUM_BOARD_SIZE);
	vrows.reserve(2 * QUACKLE_MAXIMUM_BOARD_SIZE);
	vcols.reserve(2 * QUACKLE_MAXIMUM_BOARD_SIZE);

	if (move.horizontal) {
		int row = move.startrow;
		int endcol = move.startcol + move.tiles().length() - 1;

		if (move.startcol > 0) {
			hrows.push_back(row);
			hcols.push_back(move.startcol - 1);
		}

		if (endcol < board().width() - 1) {
			hrows.push_back(row);
			hcols.push_back(endcol + 1); 
		}

		for (int col = move.startcol; col <= endcol; col++) {
			if (!QUACKLE_ALPHABET_PARAMETERS->isSomeLetter(board().letter(row, col))) {
				int upempty = -1;
				for (int hookrow = row - 1; hookrow >= 0; hookrow--) {
					if (!QUACKLE_ALPHABET_PARAMETERS->isSomeLetter(board().letter(hookrow, col))) {
						upempty = hookrow;
						hookrow = -1;
					}
				} 
				if (upempty >= 0) {
					vrows.push_back(upempty);
					vcols.push_back(col);
				}

				int downempty = board().height();
				for (int hookrow = row + 1; hookrow < board().height(); hookrow++) {
					if (!QUACKLE_ALPHABET_PARAMETERS->isSomeLetter(board().letter(hookrow, col))) {
						downempty = hookrow;
						hookrow = board().height();
					}
				}
				if (downempty < board().height()) {
					vrows.push_back(downempty);
					vcols.push_back(col);
				}
			}
		}
	}
	else {
		int col = move.startcol;
		int endrow = move.startrow + move.tiles().length() - 1;

		if (move.startrow > 0) {
			vrows.push_back(move.startrow - 1);
			vcols.push_back(col);
		}

		if (endrow < board().height() - 1) {
			vrows.push_back(endrow + 1);
			vcols.push_back(col); 
		}

		for (int row = move.startrow; row <= endrow; row++) {
			if (!QUACKLE_ALPHABET_PARAMETERS->isSomeLetter(board().letter(row, col))) {
				int upempty = -1;
				for (int hookcol = col - 1; hookcol >= 0; hookcol--) {
					if (!QUACKLE_ALPHABET_PARAMETERS->isSomeLetter(board().letter(row, hookcol))) {
						upempty = hookcol;
						hookcol = -1;
					}
				} 
				if (upempty >= 0) {
					hrows.push_back(row);
					hcols.push_back(upempty);
				}

				int downempty = board().width();
				for (int hookcol = col + 1; hookcol < board().width(); hookcol++) {
					if (!QUACKLE_ALPHABET_PARAMETERS->isSomeLetter(board().letter(row, hookcol))) {
						downempty = hookcol;
						hookcol = board().width();
					}
				}
				if (downempty < board().width()) {
					hrows.push_back(row);
					hcols.push_back(downempty);
				}
			}
		}
	}

	board().makeMove(move);

	// check the appropriate crosses
	for (unsigned int i = 0; i < vrows.size(); i++) {
		int row = vrows[i];
		int col = vcols[i];
		if (QUACKLE_ALPHABET_PARAMETERS->isSomeLetter(board().letter(row, col))) {
			board().setVCross(row, col, LetterBitset());
		}
		else { 
			LetterString pre;
			if (row > 0) {
				for (int i = row - 1; i >= 0; i--) {
					if (!QUACKLE_ALPHABET_PARAMETERS->isSomeLetter(board().letter(i, col))) {
						i = -1;
					}
					else {
						LetterString newpre;
						newpre += QUACKLE_ALPHABET_PARAMETERS->clearBlankness(board().letter(i, col));
						newpre += pre;
						pre = newpre;
					}
				}
			}
			LetterString suf;
			if (row < board().height() - 1) {
				for (int i = row + 1; i < board().height(); i++) {
					if (!QUACKLE_ALPHABET_PARAMETERS->isSomeLetter(board().letter(i, col))) {
						i = board().height();
					}
					else {
						suf += QUACKLE_ALPHABET_PARAMETERS->clearBlankness(board().letter(i, col));
					}
				}
			}

#ifdef DEBUG_GENERATOR
			UVcout << QUACKLE_ALPHABET_PARAMETERS->userVisible(pre) << " / " << QUACKLE_ALPHABET_PARAMETERS->userVisible(suf) << endl;
#endif

			if ((pre.empty()) && (suf.empty())) {
				board().setVCross(row, col, LetterBitset().set());
			}
			else {
				board().setVCross(row, col, fitbetween(pre, suf));
			}

#ifdef DEBUG_GENERATOR
			UVcout << "board().vcross[" << row << "][" << col << "] = " << cross2string(board().vcross(row, col)) << endl;
#endif
		}
	}

	for (unsigned int i = 0; i < hrows.size(); i++) {
		int row = hrows[i];
		int col = hcols[i];

		if (QUACKLE_ALPHABET_PARAMETERS->isSomeLetter(board().letter(row, col))) {
			board().setHCross(row, col, LetterBitset());
		}
		else { 
			LetterString pre; 
			if (col > 0) {
				for (int i = col - 1; i >= 0; i--) {
					if (!QUACKLE_ALPHABET_PARAMETERS->isSomeLetter(board().letter(row, i))) {
						i = -1;
					}
					else {
						LetterString newpre;
						newpre += QUACKLE_ALPHABET_PARAMETERS->clearBlankness(board().letter(row, i));
						newpre += pre;
						pre = newpre;
					}
				}
			}
			LetterString suf;
			if (col < board().width() - 1) {
				for (int i = col + 1; i < board().width(); i++) {
					if (!QUACKLE_ALPHABET_PARAMETERS->isSomeLetter(board().letter(row, i))) {
						i = board().width();
					}
					else {
						suf += QUACKLE_ALPHABET_PARAMETERS->clearBlankness(board().letter(row, i));
					}
				}
			}

#ifdef DEBUG_GENERATOR
			UVcout << QUACKLE_ALPHABET_PARAMETERS->userVisible(pre) << " / " << QUACKLE_ALPHABET_PARAMETERS->userVisible(suf) << endl;
#endif

			if ((pre.empty()) && (suf.empty())) {
				board().setHCross(row, col, LetterBitset().set());
			}
			else {
				board().setHCross(row, col, fitbetween(pre, suf));
			}

#ifdef DEBUG_GENERATOR
			UVcout << "board().hcross[" << row << "][" << col << "] = " << cross2string(board().hcross(row, col)) << endl;
#endif
		}
	}
}

void Generator::readFromDawg(int index, unsigned int &p, Letter &letter, bool &t, bool &lastchild, bool &british, int &playability) const
{
	QUACKLE_LEXICON_PARAMETERS->dawgAt(index, p, letter, t, lastchild, british, playability);
}

bool Generator::checksuffix(int i, const LetterString &suffix) {
	unsigned int p;
	Letter c;
	bool t;
	bool lastchild;
	bool british;
	int playability;

	readFromDawg(i, p, c, t, lastchild, british, playability);

	Letter sc = suffix[0];

//#ifdef DEBUG_BOARD
	//UVcout << QUACKLE_ALPHABET_PARAMETERS->userVisible(suffix) << " i: " << i << ", p: " << p << ", c: " << QUACKLE_ALPHABET_PARAMETERS->userVisible(c) << ", sc: " << QUACKLE_ALPHABET_PARAMETERS->userVisible(sc) << endl;
//#endif

	if (c == sc) {
		if (suffix.length() == 1) {
			return t;
		}
		else {
			if (p != 0) {
				return checksuffix(p, String::allButFront(suffix));
			}
			else {
				return false;
			}
		}
	} 
	else if (true) {
	//else if (c < sc) {
		if (lastchild) {
			return false;
		}
		else {
			return checksuffix(i + 1, suffix);
		}
	}
	else {
		return false;
	}
}

LetterBitset Generator::gaddagFitbetween(const LetterString &pre, const LetterString &suf)
{
// 	UVcout << "fit " 
// 		 << QUACKLE_ALPHABET_PARAMETERS->userVisible(pre)
// 		 << "_" 
// 		 << QUACKLE_ALPHABET_PARAMETERS->userVisible(suf) << endl;
	LetterBitset crosses;
	/* process the suffix once */
	const GaddagNode *sufNode = QUACKLE_LEXICON_PARAMETERS->gaddagRoot();
	int sufLen = suf.length();
	for (int i = sufLen - 1; i >= 0; --i) {
		sufNode = sufNode->child(suf[i]);
		if (!sufNode) { // this can only happen if an illegal word is on the board
			return crosses;
		}
	}

	int preLen = pre.length();
	for (const GaddagNode* node = sufNode->firstChild(); node; node = node->nextSibling()) {
	    Letter childLetter = node->letter();
	    if (childLetter == QUACKLE_GADDAG_SEPARATOR) {
			break;
	    }
		const GaddagNode *n = node;
		for (int i = preLen - 1; i >= 0; --i) {
			n = n->child(pre[i]);
			if (!n) {
				break;
			}
		}
		
		if (n && n->isTerminal()) {
			crosses.set(childLetter - QUACKLE_FIRST_LETTER);
		}
	}	
	return crosses;
}

LetterBitset Generator::fitbetween(const LetterString &pre, const LetterString &suf)
{
 	if (QUACKLE_LEXICON_PARAMETERS->hasGaddag()) {
 		return gaddagFitbetween(pre, suf);
	}

	//UVcout << QUACKLE_ALPHABET_PARAMETERS->userVisible(pre) << "_" <<
	//          QUACKLE_ALPHABET_PARAMETERS->userVisible(suf) << endl;

	LetterBitset crosses;

	for (Letter c = QUACKLE_FIRST_LETTER; c <= QUACKLE_ALPHABET_PARAMETERS->lastLetter(); c++) {
/*
		UVcout << "Let's check " <<
		          QUACKLE_ALPHABET_PARAMETERS->userVisible(pre) <<
							QUACKLE_ALPHABET_PARAMETERS->userVisible(c) <<
							QUACKLE_ALPHABET_PARAMETERS->userVisible(suf) << endl;
*/
		if (checksuffix(1, pre + c + suf)) {
			// subtract first letter because crosses hold values starting from zero
			crosses.set(c - QUACKLE_FIRST_LETTER);
			//UVcout << "  that's a word" << endl;
			// UVcout << c;
		}
	}
	
	//UVcout << " crosses: " << crosses << " " << cross2string(crosses) << endl;
	return crosses;
}

UVString Generator::counts2string()
{
	UVString ret;

	for (Letter i = 0; i < QUACKLE_ALPHABET_PARAMETERS->lastLetter(); i++)
		for (int j = 0; j < m_counts[i]; j++)
			ret += QUACKLE_ALPHABET_PARAMETERS->userVisible(i);

	return ret;
}

UVString Generator::cross2string(const LetterBitset &cross)
{
	UVString ret;

	for (int i = 0; i < QUACKLE_ALPHABET_PARAMETERS->length(); i++)
		if (cross.test(i))
			ret += QUACKLE_ALPHABET_PARAMETERS->userVisible(QUACKLE_FIRST_LETTER + i);

	return ret;
}

/*
   Gen(pos, word, rack, arc):   pos = offset from anchor
   IF a letter, L, is already on this square THEN
   GoOn(pos, L, word, rack, NextArc(arc, L), arc)
   ELSE IF letters remain on the rack THEN
   FOR each letter on the rack, L, allowed on this square
   GoOn(pos, L, word, rack - L, NextArc(arc, L), arc)
   IF the rack contains a BLANK THEN
   FOR each letter the BLANK could be, L, allowed...
   GoOn(pos, L, word, rack - BLANK, NextArc(arc, L), arc)

   GoOn(pos, L, word, rack, NewArc, OldArc):
   IF pos <= 0 THEN // moving left
   word <- L + word
   IF L on OldArc & no letter directly left THEN Record
   IF NewArc != 0 THEN
   IF room to the left THEN Gen(pos - 1, word, rack, NewArc)
   NewArc <- NextArc(NewArc, ^)
   IF NewArc != 0, no letter directly left & room to the right THEN
   Gen(1, word, rack, NewArc)
   ELSE IF pos > 0 THEN // moving right
   word <- word + L
   IF L on OldArc & no letter directly right THEN Record
   IF NewArc != & room to the right THEN
   Gen(pos + 1, word, rack, NewArc)
 */

void Generator::gordongoon(int pos, char L, LetterString word, const GaddagNode *node)
{
	//UVcout << "gordongoon(" << pos << ", " << L << ", " << word << ", " << newarc << ", " << oldarc << ")" << 
	//        " horiz: " << m_gordonhoriz << endl;

	if (pos <= 0) {

		int currow = m_anchorrow;
		int curcol = m_anchorcol;

		int leftrow = m_anchorrow;
		int leftcol = m_anchorcol;

		if (m_gordonhoriz) {
			curcol += pos;
			leftcol += pos - 1;
		}          
		else {
			currow += pos;
			leftrow += pos - 1;
		}

		if (QUACKLE_ALPHABET_PARAMETERS->isSomeLetter(board().letter(currow, curcol))) {
			L = QUACKLE_PLAYED_THRU_MARK;
		}

		LetterString newWord;
		newWord += L;
		newWord += word;

		bool emptyleft = true; 
		bool roomtoleft = true;
		bool atboardedge = false;

		if ((leftcol >= 0) && (leftrow >= 0)) {
			if (!QUACKLE_ALPHABET_PARAMETERS->isSomeLetter(board().letter(currow, curcol)) && QUACKLE_ALPHABET_PARAMETERS->isSomeLetter(board().letter(leftrow, leftcol))) {
				roomtoleft = false;
			}

			if (pos < -m_leftlimit) {
				roomtoleft = false;
			}

			if (QUACKLE_ALPHABET_PARAMETERS->isSomeLetter(board().letter(leftrow, leftcol))) {
				emptyleft = false;
			}
		}
		else {
			atboardedge = true;
		}

		if (node->isTerminal() && (roomtoleft) && (m_laid > 0)) {
			// UVcout << "found a word or something " << word << " at " << pos << endl;
			Move move;
			move.action = Move::Place;
			move.setTiles(newWord);
			if (m_gordonhoriz) {
				move.startrow = m_anchorrow;
				move.startcol = m_anchorcol + pos;
			}
			else {
				move.startrow = m_anchorrow + pos;
				move.startcol = m_anchorcol;
			}

			move.horizontal = m_gordonhoriz;
			move.score = board().score(move, &move.isBingo);
			move.equity = equity(move);

			if (m_recordall) {
				m_moveList.push_back(move);
			}

			if (MoveList::equityComparator(best, move)) {
				best = move;
			}
			// UVcout << "found a move: " << move << " score: " << move.score << ", equity: " << move.equity << 
			// " outputted by leftmoving loop" << endl;
		}

        if (roomtoleft && pos != -m_leftlimit && !atboardedge) {
            gordongen(pos - 1, newWord, node);
        }

        // UVcout << "looking for the delimiter" << endl;

        node = node->child(QUACKLE_GADDAG_SEPARATOR);

        // UVcout << "after all that, delimiter's newarc is " << newarc << endl;
        int rightrow = m_anchorrow;
        int rightcol = m_anchorcol;

        if (m_gordonhoriz) {
            rightcol++;
        }          
        else {
            rightrow++;
        }

        bool atrightedge = false;

        if ((rightrow > board().height() - 1) || (rightcol > board().width() - 1)) {
            atrightedge = true;
        }

        if ((node != 0) && emptyleft && !atrightedge) {
            gordongen(1, newWord, node);
        }
	} 
	else {
		// UVcout << "looking to the right" << endl;

		int currow = m_anchorrow;
		int curcol = m_anchorcol;
		int rightrow = m_anchorrow;
		int rightcol = m_anchorcol;

		if (m_gordonhoriz) {
			curcol += pos;
			rightcol += pos + 1;
		}          
		else {
			currow += pos;
			rightrow += pos + 1;
		}

		if (QUACKLE_ALPHABET_PARAMETERS->isSomeLetter(board().letter(currow, curcol))) {
			word += QUACKLE_PLAYED_THRU_MARK;
		}
		else {
			word += L;
		}

		bool roomtoright = true;
		bool atboardedge = false;

		// UVcout << "rightsquare: " << (char)(rightcol + 'A') << rightrow + 1 << endl;

		if ((rightcol <= board().width() - 1) && (rightrow <= board().height() - 1)) {
			if (QUACKLE_ALPHABET_PARAMETERS->isSomeLetter(board().letter(rightrow, rightcol))) {
				roomtoright = false;
				// UVcout << "can't record " << word << " here because of the " << board().letter(rightrow, rightcol) << endl;
			}
			else {
				// UVcout << "yay! " << (char)(rightcol + 'A') << rightrow + 1 << " is empty!" << endl; 
				// UVcout << board() << endl;
			}
		}
		else {
			// UVcout << "at board edge so i can maybe record " << word << endl;
			atboardedge = true;
		}

		if (node->isTerminal() && (roomtoright) && (m_laid > 0)) {
			// UVcout << "found a word or something " << word << " at " << pos << endl;

			Move move;
			move.action = Move::Place;
			move.setTiles(word);

			if (m_gordonhoriz) {
				move.startrow = m_anchorrow;
				move.startcol = m_anchorcol - word.length() + pos + 1;
			}
			else {
				move.startrow = m_anchorrow - word.length() + pos + 1;
				move.startcol = m_anchorcol;
			}

			move.horizontal = m_gordonhoriz;
			move.score = board().score(move, &move.isBingo);
			move.equity = equity(move);

			if (m_recordall) {
				m_moveList.push_back(move);
			}

			if (MoveList::equityComparator(best, move)) {
				best = move;
			}
			// UVcout << "found a move: " << move << " score: " << move.score << ", equity: " << move.equity << 
			//      " outputted by rightmoving loop" << endl;
		}

		// UVcout << "newarc is " << newarc << endl;
        if (!atboardedge) {
            gordongen(pos + 1, word, node);
        }
        else {
            // UVcout << "didn't go ahead because we were at board edge" << endl;
        }
	}
}

void Generator::gordongen(int pos, const LetterString &word, const GaddagNode *node) 
{
	// UVcout << "gordongen(" << pos << ", " << word << ", " << i << ")" << " horiz: " << m_gordonhoriz << endl;

	int currow = m_anchorrow;
	int curcol = m_anchorcol;

	if (m_gordonhoriz) {
		curcol += pos;
	}
	else {
		currow += pos;
	}

	LetterBitset cross;
	if (m_gordonhoriz) {
		cross = board().vcross(currow, curcol);
	}
	else {
		cross = board().hcross(currow, curcol);
	}

	if (QUACKLE_ALPHABET_PARAMETERS->isSomeLetter(board().letter(currow, curcol))) {
		// UVcout << "gordongen sez a letter (" << board().letter(currow, curcol) << ") already on this square" << endl;

		Letter boardc = QUACKLE_ALPHABET_PARAMETERS->clearBlankness(board().letter(currow, curcol));

		const GaddagNode *child = node->child(boardc);
		if (child) {
			gordongoon(pos, board().letter(currow, curcol), word, child);
		}
	}

	else {
		for (const GaddagNode* child = node->firstChild(); child; child = child->nextSibling()) {
			Letter childLetter = child->letter();

			if ((m_counts[childLetter] <= 0) 
					|| !cross.test(childLetter - QUACKLE_FIRST_LETTER)) {
				continue;
			}

			if (childLetter == QUACKLE_GADDAG_SEPARATOR) {
				// UVcout << "ran into a delimiter" << endl;
				break;
			}

			m_counts[childLetter]--;
			m_laid++;
			// UVcout << "    yeah that'll work" << endl;
			gordongoon(pos, childLetter, word, child);
			m_counts[childLetter]++;
			m_laid--;

		}
		if (m_counts[QUACKLE_BLANK_MARK] >= 1) {
			for (const GaddagNode* child = node->firstChild(); child; child = child->nextSibling()) {
				Letter childLetter = child->letter();
				// UVcout << "childLetter is " << (char)(arcc + 'A') << endl;

				if (childLetter == QUACKLE_GADDAG_SEPARATOR) {
					// UVcout << "ran into a delimiter" << endl;
					break;
				}

				if (cross.test(childLetter - QUACKLE_FIRST_LETTER)) {
					m_counts[QUACKLE_BLANK_MARK]--;
					m_laid++;
					// UVcout << "    yeah that'll work" << endl;
					gordongoon(pos, QUACKLE_ALPHABET_PARAMETERS->setBlankness(childLetter), word, child);
					m_counts[QUACKLE_BLANK_MARK]++;
					m_laid--;
				}
			}
		}
	}
}

void Generator::extendright(const LetterString &partial, int i, 
		int row, int col, int edge, int righttiles, bool horizontal)
{
	if (i == 0) {  // is this really correct?
		return;
	}

	unsigned int p;
	Letter c;
	bool t;
	bool lastchild;
	bool british;
	int playability;

	readFromDawg(i, p, c, t, lastchild, british, playability);

	int rowpos = row;
	int rownext = row;
	int colpos = col;
	int colnext = col;
	int dirpos;
	int edgeDirpos;

	if (horizontal) {
		colpos = col + righttiles;
		colnext = col + righttiles + 1;
		dirpos = colpos;
		edgeDirpos = board().width() - 1;
	}
	else {
		rowpos = row + righttiles;
		rownext = row + righttiles + 1;
		dirpos = rowpos;
		edgeDirpos = board().height() - 1;
	}

#ifdef DEBUG_GENERATOR
	UVcout << "extendright(" << QUACKLE_ALPHABET_PARAMETERS->userVisible(partial) << ", " << i << ", " << counts2string() << ", " << rowpos << ", " << colpos << ", " << horizontal <<  ")" << endl;
#endif

	if (!QUACKLE_ALPHABET_PARAMETERS->isSomeLetter(board().letter(rowpos, colpos))) {
		if (m_counts[c] >= 1) {
			LetterBitset cross;
			if (horizontal) {
				cross = board().vcross(rowpos, colpos);
			}
			else {
				cross = board().hcross(rowpos, colpos);
			}
			if (cross.test(c - QUACKLE_FIRST_LETTER))  {
				if (t) {
					bool couldend = true;
					if (dirpos < edgeDirpos) {
						if (QUACKLE_ALPHABET_PARAMETERS->isSomeLetter(board().letter(rownext, colnext))) {
							couldend = false;
						}
					}
					if (couldend) { 
						Move move;
						move.action = Move::Place;
						move.setTiles(partial + c);
						if (horizontal) {
							move.startrow = row;
							move.startcol = col - (partial.length() - righttiles);
						}
						else {
							move.startrow = row - (partial.length() - righttiles);
							move.startcol = col;
						}
						move.horizontal = horizontal;
						move.score = board().score(move, &move.isBingo);
						move.equity = equity(move);

						// i added this because m_laid is wrong and i don't want to break anything by fixing it :)
						// will need to remember to add this bit to the DAGGAD code when we start using it again

						int laid = move.wordTilesWithNoPlayThru().length();
						bool onetilevert = (!move.horizontal) && (laid == 1);
						bool ignore = onetilevert && !board().hcross(row, col).all();
						
						if (1 || !ignore)
						{
							if (m_recordall) {
								m_moveList.push_back(move);
							}

							if (MoveList::equityComparator(best, move)) {
								best = move;
							}

#ifdef DEBUG_GENERATOR
							UVcout << "found a move: " << move << " laid: " << m_laid << ", score: " << move.score << ", equity: " << move.equity << endl;
#endif
						}
					}
				}
				if (dirpos < edgeDirpos) {
					m_counts[c]--;
					m_laid++;
					extendright(partial + c, p, row, col, 
							0, righttiles + 1, horizontal);
					m_counts[c]++;
					m_laid--;
				}
			}
		}       
		if ((m_counts[QUACKLE_BLANK_MARK] >= 1)) {
			LetterBitset cross;
			if (horizontal) {
				cross = board().vcross(rowpos, colpos);
			}
			else {
				cross = board().hcross(rowpos, colpos);
			}
			if (cross.test(c - QUACKLE_FIRST_LETTER)) {
				if (t) {
					bool couldend = true;
					if (dirpos < edgeDirpos) {
						if (QUACKLE_ALPHABET_PARAMETERS->isSomeLetter(board().letter(rownext, colnext))) {
							couldend = false;
						}
					}
					if (couldend) { 
						Move move;
						move.action = Move::Place;
						move.setTiles(partial + QUACKLE_ALPHABET_PARAMETERS->setBlankness(c));
						if (horizontal) {
							move.startrow = row;
							move.startcol = col - (partial.length() - righttiles);
						}
						else {
							move.startrow = row - (partial.length() - righttiles);
							move.startcol = col;
						}
						move.horizontal = horizontal;
						move.score = board().score(move, &move.isBingo);
						move.equity = equity(move);

						int laid = move.wordTilesWithNoPlayThru().length();
						bool onetilevert = (!move.horizontal) && (laid == 1);
						bool ignore = onetilevert && !board().hcross(row, col).all();
																								
						if (1 || !ignore)
						{
							if (m_recordall) { 
								m_moveList.push_back(move);
							}

							if (MoveList::equityComparator(best, move)) {
								best = move;
							}
#ifdef DEBUG_GENERATOR
							UVcout << "found a move: " << move << " laid: " << m_laid << ", score: " << move.score << ", equity: " << move.equity << endl;

#endif
						}
					}
				}
				if (dirpos < edgeDirpos) {
					m_counts[QUACKLE_BLANK_MARK]--;
					m_laid++;
					extendright(partial + QUACKLE_ALPHABET_PARAMETERS->setBlankness(c), p, row, col, 
							0, righttiles + 1, horizontal);
					m_counts[QUACKLE_BLANK_MARK]++;
					m_laid--;
				}
			}
		}

		if (!lastchild) {
			extendright(partial, i + 1, row, col, edge + 1, righttiles, horizontal);
		}
	}
	else {
		Letter boardc = QUACKLE_ALPHABET_PARAMETERS->clearBlankness(board().letter(rowpos, colpos));

		if (c == boardc)
		{
#ifdef DEBUG_GENERATOR
			UVcout << "We're playing with " << QUACKLE_ALPHABET_PARAMETERS->userVisible(partial) << " through a letter (" << boardc << ") or trying to" << endl;
#endif

			bool endofthrough = false;

			if (dirpos < edgeDirpos) {
				extendright(partial + (Letter)QUACKLE_PLAYED_THRU_MARK, p, 
						row, col, 0, righttiles + 1, horizontal);

#ifdef DEBUG_GENERATOR
				UVcout << "  next square is " << board().letter(rownext, colnext) << endl;
#endif

				if (!QUACKLE_ALPHABET_PARAMETERS->isSomeLetter(board().letter(rownext, colnext))) {
					endofthrough = true;
#ifdef DEBUG_GENERATOR
					UVcout << "   woohoo!  next square is empty" << endl;
#endif
				}
			}
			else {
				endofthrough = true;
#ifdef DEBUG_GENERATOR
				UVcout << "    woohoo!  at the edge of the board" << endl;
#endif
			}

			if (!t) {
#ifdef DEBUG_GENERATOR
				UVcout << "    but " << partial + boardc << " isn't a word" << endl;
#endif
			}

			if (endofthrough & t) {
				if (m_laid > 0) {
					Move move;
					move.action = Move::Place;
					move.setTiles(partial + (Letter)QUACKLE_PLAYED_THRU_MARK);
					if (horizontal) {
						move.startrow = row;
						move.startcol = col - (partial.length() - righttiles);
					}
					else {
						move.startrow = row - (partial.length() - righttiles);
						move.startcol = col;
					}
					move.horizontal = horizontal;
					move.score = board().score(move, &move.isBingo);
					move.equity = equity(move);
						
					int laid = move.wordTilesWithNoPlayThru().length();
					bool onetilevert = (!move.horizontal) && (laid == 1);
					bool ignore = onetilevert && !board().hcross(row, col).all();
					
					if (1 || !ignore)
					{
						
						if (m_recordall) {
							m_moveList.push_back(move);
						}

						if (MoveList::equityComparator(best, move)) {
							best = move;
						}

#ifdef DEBUG_GENERATOR
						UVcout << "found a move: " << move << " which has equity " << move.equity << endl;
#endif
					}
				}
			}

		}
		else if (!lastchild)
			// else if ((c < boardc) && (!lastchild))
		{
			extendright(partial, i + 1, row, col, 
					edge + 1, righttiles, horizontal);
		}
	}
}

void Generator::leftpart(const LetterString &partial, int i, int limit, 
		int row, int col, int edge, bool horizontal)
{
#ifdef DEBUG_GENERATOR
	UVcout << "leftpart(" << QUACKLE_ALPHABET_PARAMETERS->userVisible(partial) << ", " << i << ", " << counts2string() << ", " << row << ", " << col << ", " << edge << ")" << endl;
#endif

	if (edge == 0) {
		extendright(partial, i, row, col, 0, 0, horizontal);
	}
	if (limit > 0) {
		if (i == 0) { // is this right at all?
			return;
		}

		unsigned int p;
		Letter c;
		bool t;
		bool lastchild;
		bool british;
		int playability;

		readFromDawg(i, p, c, t, lastchild, british, playability);

		if (m_counts[c] >= 1) {
			m_counts[c]--;
			m_laid++;
			leftpart(partial + c, p, limit - 1, row, col, 0, horizontal);
			m_counts[c]++;
			m_laid--;
		}

		if (m_counts[QUACKLE_BLANK_MARK] >= 1) {
			m_counts[QUACKLE_BLANK_MARK]--;
			m_laid++;
			leftpart(partial + QUACKLE_ALPHABET_PARAMETERS->setBlankness(c), p, limit - 1, row, col, 0, horizontal);
			m_counts[QUACKLE_BLANK_MARK]++;
			m_laid--;
		}

		if (!lastchild) {
			leftpart(partial, i + 1, limit, row, col, edge + 1, horizontal);
		}
	} 
}

void Generator::setupCounts(const LetterString &letters)
{
	String::counts(letters, m_counts);
}

double Generator::equity(const Move &move) const
{
	return QUACKLE_EVALUATOR->equity(m_position, move);
}

Move Generator::generate()
{
#ifdef DEBUG_GENERATOR
	UVcout << "generate called" << endl;
#endif

	for (int row = 0; row < board().height(); row++) {
		for (int col = 0; col < board().width(); col++) {

			// generate horizontal plays

			// what defines an anchor square?

			bool anchor = false;
			if (!QUACKLE_ALPHABET_PARAMETERS->isSomeLetter(board().letter(row, col)) && !board().vcross(row, col).all()) {
				if (col == 0) {
					anchor = true;
				}
				else if (!QUACKLE_ALPHABET_PARAMETERS->isSomeLetter(board().letter(row, col - 1))) {
					anchor = true;
				}
			}
			else if (QUACKLE_ALPHABET_PARAMETERS->isSomeLetter(board().letter(row, col))) {
				if (col == 0) {
					anchor = true;
				}
				else if (!QUACKLE_ALPHABET_PARAMETERS->isSomeLetter(board().letter(row, col - 1))) {
					anchor = true;
				}
			}

			if (anchor) {
				int k = 0;
				for (int i = col - 1; i >= 0; i--)
				{
					// UVcout << "board().vcross[" << row << "][" << i << "] = " << board().vcross(row, i) << endl;
					
					if (!QUACKLE_ALPHABET_PARAMETERS->isSomeLetter(board().letter(row, i)) && board().vcross(row, i).all()) {
						if (i == 0) {
							k++;
						}
						else if (!QUACKLE_ALPHABET_PARAMETERS->isSomeLetter(board().letter(row, i - 1))) {
							k++;
						}
					}
					else {
						i = -1;
					}
				}

#ifdef DEBUG_GENERATOR
				UVcout << "looking horizontally with the " << QUACKLE_ALPHABET_PARAMETERS->userVisible(board().letter(row, col)) << " at " << row + 1 << (char)(col + 'A') << endl;
				UVcout << "Apparently there are " << k << " empty spots to our left" << endl;
#endif

				m_laid = 0;
				leftpart(LetterString(), 1, k, row, col, 0, true);
			}

			// generate vertical plays

			// what defines an anchor square?

			anchor = false;
			if (!QUACKLE_ALPHABET_PARAMETERS->isSomeLetter(board().letter(row, col)) && !board().hcross(row, col).all()) {
				if (row == 0) {
					anchor = true;
				}
				else if (!QUACKLE_ALPHABET_PARAMETERS->isSomeLetter(board().letter(row - 1, col))) {
					anchor = true;
				}
			}
			else if (QUACKLE_ALPHABET_PARAMETERS->isSomeLetter(board().letter(row, col))) {
				if (row == 0) {
					anchor = true;
				}
				else if (!QUACKLE_ALPHABET_PARAMETERS->isSomeLetter(board().letter(row - 1, col))) {
					anchor = true;
				}
			}

			if (anchor) {
				int k = 0;
				for (int i = row - 1; i >= 0; i--) {
					// UVcout << "board().vcross[" << row << "][" << i << "] = " << board().vcross(row, i) << endl;
					if (!QUACKLE_ALPHABET_PARAMETERS->isSomeLetter(board().letter(i, col)) && board().hcross(i, col).all()) {
						if (i == 0) {
							k++;
						}
						else if (!QUACKLE_ALPHABET_PARAMETERS->isSomeLetter(board().letter(i - 1, col))) {
							k++;
						}
					}
					else {
						i = -1;
					}
				}

#ifdef DEBUG_GENERATOR
				UVcout << "looking vertically with the " << board().letter(row, col) << " at " << row + 1 << (char)(col + 'A') << endl;
				UVcout << "Apparently there are " << k << " empty spots to our left" << endl;
#endif

				m_laid = 0;
				leftpart(LetterString(), 1, k, row, col, 0, false);
			}
		}
	}

	return best;
}

// TODO GET RID OF CODE DUPLICATION
Move Generator::gordongenerate()
{
	for (int row = 0; row < board().height(); row++) {
		for (int col = 0; col < board().width(); col++) {

			// generate horizontal plays
			// what defines an anchor square?
			bool anchor = false;
			if (!QUACKLE_ALPHABET_PARAMETERS->isSomeLetter(board().letter(row, col)) && !board().vcross(row, col).all()) {
				if (col == 0) {
					if (!QUACKLE_ALPHABET_PARAMETERS->isSomeLetter(board().letter(row, col + 1))) {
						anchor = true;
					}
				}
				else if (!QUACKLE_ALPHABET_PARAMETERS->isSomeLetter(board().letter(row, col - 1))) {
					if (col == board().width() - 1) {
						anchor = true;
					}
					else {
						if (!QUACKLE_ALPHABET_PARAMETERS->isSomeLetter(board().letter(row, col + 1))) {
							anchor = true;
						}
					}
				}
			}
			else if (QUACKLE_ALPHABET_PARAMETERS->isSomeLetter(board().letter(row, col))) {
				if (col == board().width() - 1) {
					anchor = true;
				}
				else if (!QUACKLE_ALPHABET_PARAMETERS->isSomeLetter(board().letter(row, col + 1))) {
					anchor = true;
				}
			}

			if (anchor) {
				int k = 0;
				for (int i = col; i >= 0; i--) { // skip over filled sqs
					if (QUACKLE_ALPHABET_PARAMETERS->isSomeLetter(board().letter(row, i))) {
						k++;
					} else {
						i = -1;
					}
				}
				for (int i = col - k - 1; i >= 0; i--) {
					// UVcout << "board().vcross[" << row << "][" << i << "] = " << board().vcross(row, i) << endl;
					if (!QUACKLE_ALPHABET_PARAMETERS->isSomeLetter(board().letter(row, i)) && board().vcross(row, i).all()) {
						if (i == 0) {
							k++;
						}
						else if (!QUACKLE_ALPHABET_PARAMETERS->isSomeLetter(board().letter(row, i - 1))) {
							k++;
						}
					}
					else {
						i = -1;
					}
				}
				// UVcout << "Apparently there are " << k << " empty spots to our left" << endl;
				// leftpart("", 1, k, row, col, 0, true);

				// UVcout << "looking horizontally with the " << board().letter(row, col) <<
				//         " at " << row + 1 << (char)(col + 'A') << endl;

				m_anchorrow = row;
				m_anchorcol = col;
				m_gordonhoriz = true;
				m_laid = 0;
				m_leftlimit = k;
				gordongen(0, LetterString(), QUACKLE_LEXICON_PARAMETERS->gaddagRoot());
			}

			// generate vertical plays
			// what defines an anchor square?

			anchor = false;
			if (!QUACKLE_ALPHABET_PARAMETERS->isSomeLetter(board().letter(row, col)) && !board().hcross(row, col).all()) {
				if (row == 0) {
					if (!QUACKLE_ALPHABET_PARAMETERS->isSomeLetter(board().letter(row + 1, col))) {
						anchor = true;
					}
				}
				else if (!QUACKLE_ALPHABET_PARAMETERS->isSomeLetter(board().letter(row - 1, col))) {
					if (row == board().height() - 1) {
						anchor = true;
					}
					else {
						if (!QUACKLE_ALPHABET_PARAMETERS->isSomeLetter(board().letter(row + 1, col))) {
							anchor = true;
						}
					}
				}
			}
			else if (QUACKLE_ALPHABET_PARAMETERS->isSomeLetter(board().letter(row, col))) {
				if (row == board().height() - 1) {
					anchor = true;
				}
				else if (!QUACKLE_ALPHABET_PARAMETERS->isSomeLetter(board().letter(row + 1, col))) {
					anchor = true;
				}
			}

			if (anchor) {
				int k = 0;
				for (int i = row; i >= 0; i--) {
					if (QUACKLE_ALPHABET_PARAMETERS->isSomeLetter(board().letter(i, col))) {
						k++;
					} else {
						i = -1;
					}
				}
				for (int i = row - k - 1; i >= 0; i--) {
					// UVcout << "board().vcross[" << row << "][" << i << "] = " << board().vcross(row, i) << endl;
					if (!QUACKLE_ALPHABET_PARAMETERS->isSomeLetter(board().letter(i, col)) && board().hcross(i, col).all()) {
						if (i == 0) {
							k++;
						}
						else if (!QUACKLE_ALPHABET_PARAMETERS->isSomeLetter(board().letter(i - 1, col))) {
							k++;
						}
					}
					else {
						i = -1;
					}
				}

				// UVcout << "Apparently there are " << k << " empty spots to our left" << endl;
				// leftpart("", 1, k, row, col, 0, false);

				// UVcout << "looking vertically with the " << board().letter(row, col) <<
				//         " at " << row + 1 << (char)(col + 'A') << endl;

				m_anchorrow = row;
				m_anchorcol = col;
				m_gordonhoriz = false;
				m_laid = 0;
				m_leftlimit = k;
				gordongen(0, LetterString(), QUACKLE_LEXICON_PARAMETERS->gaddagRoot());

			}
		}
	}

	return best;
}

void Generator::spit(int i, const LetterString &prefix, int flags)
{
	// UVcout << "spit called... i: " << i << ", prefix: " << prefix << endl;
	
	unsigned int p;
	Letter c;
	bool t;
	bool lastchild;
	bool british;
	int playability;

	readFromDawg(i, p, c, t, lastchild, british, playability);

	if (m_counts[c] >= 1)
	{
		m_counts[c]--;

		if (t)
		{
			// UVcout << QUACKLE_ALPHABET_PARAMETERS->userVisible(prefix) << c << endl;

			bool usedAll = true;
			if (!(flags & NoRequireAllLetters))
			{
				for (int j = 0; j < QUACKLE_FIRST_LETTER + QUACKLE_MAXIMUM_ALPHABET_SIZE; ++j)
				{
					if (m_counts[j] > 0)
					{
						usedAll = false;
						break;
					}
				}
			}

			if (usedAll) {
				m_spat.push_back(prefix + c);
				if (flags & SingleMatch) {
				    return;
				}
			}
		}

		if (p != 0)
		{
			spit(p, prefix + c, flags);
		}

		m_counts[c]++;
	}

	if (!(flags & ClearBlanknesses && m_counts[c] >= 1))
	{
		if (m_counts[QUACKLE_BLANK_MARK] >= 1 || flags & AddAnyLetters)
		{
			m_counts[QUACKLE_BLANK_MARK]--;

			if (t) {
				// UVcout << prefix << QUACKLE_ALPHABET_PARAMETERS->setBlankness(c) << endl;

				bool usedAll = true;
				if (!(flags & NoRequireAllLetters))
					for (int j = 0; j < QUACKLE_FIRST_LETTER + QUACKLE_MAXIMUM_ALPHABET_SIZE; ++j)
						if (m_counts[j] > 0)
							usedAll = false;

				if (usedAll) {
					m_spat.push_back(prefix + (flags & ClearBlanknesses? c : QUACKLE_ALPHABET_PARAMETERS->setBlankness(c)));
					if (flags & SingleMatch) {
					    return;
					}
				}
			}

			if (p != 0) {
				spit(p, prefix + (flags & ClearBlanknesses? c : QUACKLE_ALPHABET_PARAMETERS->setBlankness(c)), flags);
			}

			m_counts[QUACKLE_BLANK_MARK]++;
		}
	}

	if (!lastchild)
	{
		spit(i + 1, prefix, flags);
	}
}

void Generator::wordspit(int i, const LetterString &prefix, int flags)
{
	// UVcout << "spit called... i: " << i << ", prefix: " << prefix << endl;
	
	unsigned int p;
	Letter c;
	bool t;
	bool lastchild;
	bool british;
	int playability;

	//UVcout << "wordspit(" << i << ", " << QUACKLE_ALPHABET_PARAMETERS->userVisible(prefix) << ", " << flags << ")" << endl;

	readFromDawg(i, p, c, t, lastchild, british, playability);

	if (m_counts[c] >= 1)
	{
		m_counts[c]--;

		if (t)
		{
			// UVcout << QUACKLE_ALPHABET_PARAMETERS->userVisible(prefix) << c << endl;

			bool usedAll = true;
			if (!(flags & NoRequireAllLetters))
			{
				for (int j = 0; j < QUACKLE_FIRST_LETTER + QUACKLE_MAXIMUM_ALPHABET_SIZE; ++j)
				{
					if (m_counts[j] > 0)
					{
						usedAll = false;
						break;
					}
				}
			}

			if (usedAll)
			{
				WordWithInfo word;
				word.wordLetterString = prefix + c;
				word.british = british;
				word.playability = playability;
				m_wordspat.push_back(word);
			}
		}

		if (p != 0)
		{
			wordspit(p, prefix + c, flags);
		}

		m_counts[c]++;
	}

	if (!lastchild)
	{
		wordspit(i + 1, prefix, flags);
	}
}


Move Generator::exchange()
{
	map<LetterString, bool> throwmap;

	const int rackSize = rack().tiles().length();
	const int permutations = 1 << rackSize;
	
	for (int i = 1; i < permutations; i++)
	{
		LetterString thrown;
		for (int j = 0; j < rackSize; j++)
			if (i & (1 << j))
				thrown += rack().tiles()[j];

		Move move;
		move.action = Move::Exchange;
		move.setTiles(String::alphabetize(thrown));
		move.score = 0;
		move.equity = equity(move);

		if (throwmap.find(move.tiles()) == throwmap.end())
		{
			if (m_recordall)
				m_moveList.push_back(move);

			if (MoveList::equityComparator(best, move)) 
				best = move;

			throwmap[move.tiles()] = true;
		}
	}

	return best;
}

Move Generator::findstaticbest(bool canExchange)
{
	best = Move::createPassMove();
	m_moveList.clear();

	setupCounts(rack().tiles());

	if (QUACKLE_LEXICON_PARAMETERS->hasSomething())
	{
		if (board().isEmpty())
		{
			anagram();
		}
		else
		{
			// UVcout << rack() << endl;
			// UVcout << board() << endl;

			if (QUACKLE_LEXICON_PARAMETERS->hasGaddag())
				gordongenerate();
			else 
				generate();

			// UVcout << "gaddag says: " << best << " " << best.score << " " << best.equity;
			// UVcout << endl;
			// UVcout << " dawg says: " << best << " " << best.score << " " << best.equity << endl;
		}
	}

	if (canExchange)
		exchange();

	if (m_moveList.empty())
		m_moveList.push_back(best);

	return best;
}

void Generator::gaddagAnagram(const GaddagNode *node, const LetterString &prefix, int flags)
{
	for (const GaddagNode* child = node->firstChild(); child; child = child->nextSibling()) {
	    Letter childLetter = child->letter();

	    if (childLetter == QUACKLE_GADDAG_SEPARATOR) {
			break;
	    }

		if (m_counts[childLetter] <= 0) 
			continue;

	    m_counts[childLetter]--;

		LetterString newPrefix;
		newPrefix += childLetter;
		newPrefix += prefix;

		if (child->isTerminal()) {
			bool usedAll = true;
			if (!(flags & NoRequireAllLetters)) {
				for (int j = 0; j < QUACKLE_FIRST_LETTER + QUACKLE_MAXIMUM_ALPHABET_SIZE; ++j) {
					if (m_counts[j] > 0) {
						usedAll = false;
						break;
					}
				}
			}

			if (usedAll) {
				m_spat.push_back(newPrefix);
				if (flags & SingleMatch) {
				    return;
				}
			}
		}

		if (child->firstChild()) {
			gaddagAnagram(child, newPrefix, flags);
			if (flags & SingleMatch && m_spat.size() > 0) {
			    m_counts[childLetter]++;
			    return;
			}

		}

		m_counts[childLetter]++;
	}

	if (m_counts[QUACKLE_BLANK_MARK] >= 1 || flags & AddAnyLetters) {
		for (const GaddagNode* child = node->firstChild(); child; child = child->nextSibling()) {
			Letter childLetter = child->letter();

			if (childLetter == QUACKLE_GADDAG_SEPARATOR) {
				break;
			}

			if (flags & ClearBlanknesses && m_counts[childLetter] >= 1) {
				continue;
			}

			m_counts[QUACKLE_BLANK_MARK]--;

			LetterString newPrefix;
			newPrefix += flags & ClearBlanknesses ?
				childLetter : QUACKLE_ALPHABET_PARAMETERS->setBlankness(childLetter);
			newPrefix += prefix;

			if (child->isTerminal()) {
				bool usedAll = true;
				if (!(flags & NoRequireAllLetters))
					for (int j = 0; j < QUACKLE_FIRST_LETTER + QUACKLE_MAXIMUM_ALPHABET_SIZE; ++j)
						if (m_counts[j] > 0)
							usedAll = false;

				if (usedAll) {
					m_spat.push_back(newPrefix);
					if (flags & SingleMatch) {
					    return;
					}
				}
			}

			if (child->firstChild()) {
				gaddagAnagram(child, newPrefix, flags);
				if (flags & SingleMatch && m_spat.size() > 0) {
				    m_counts[QUACKLE_BLANK_MARK]++;
				    return;
				}
			}

			m_counts[QUACKLE_BLANK_MARK]++;
		}
	}
}

Move Generator::anagram()
{
	// UVcout << "anagram called" << endl;

	// UVcout << "about to call spit" << endl;

	m_spat.clear();
 	if (QUACKLE_LEXICON_PARAMETERS->hasGaddag()) {
 		gaddagAnagram(QUACKLE_LEXICON_PARAMETERS->gaddagRoot(),
 					  LetterString(), NoRequireAllLetters);
 	} else {
		spit(1, LetterString(), NoRequireAllLetters);
 	}

	// UVcout << "m_spat has " << m_spat.size() << " words in it" << endl;

	WordList::const_iterator end = m_spat.end();
	for (WordList::const_iterator it = m_spat.begin(); it != end; ++it)
	{
		for (unsigned int k = 0; k < (*it).length(); k++)
		{
			Move move;
			move.action = Move::Place;
			move.setTiles(*it);
			move.horizontal = true;
			move.startrow = QUACKLE_BOARD_PARAMETERS->startRow();
			move.startcol = (QUACKLE_BOARD_PARAMETERS->startColumn() - (*it).length() + 1) + k;

			if (move.startcol < 0)
				continue;

			move.score = board().score(move, &move.isBingo);

			move.equity = equity(move);
			// UVcout << move << " has equity " << move.equity << endl;

			if (m_recordall)
				m_moveList.push_back(move);

			if (MoveList::equityComparator(best, move)) 
				best = move;
		}
	}

	return best;
}

bool Generator::isAcceptableWord(const LetterString &word)
{
	WordList results = anagramLetters(word);
	
	WordList::const_iterator end = results.end();
	for (WordList::const_iterator it = results.begin(); it != end; ++it)
		if ((*it) == word)
			return true;
	
	return false;
}

WordList Generator::anagramLetters(const LetterString &letters, int flags)
{
	setupCounts(String::clearBlankness(letters));
	m_spat.clear();

 	if (QUACKLE_LEXICON_PARAMETERS->hasGaddag()) {
 		gaddagAnagram(QUACKLE_LEXICON_PARAMETERS->gaddagRoot(),
 					  LetterString(), flags);
 	} else if (QUACKLE_LEXICON_PARAMETERS->hasSomething()) {
		spit(1, LetterString(), flags);
	}

	return m_spat;
}

void Generator::storeWordInfo(WordWithInfo *wordWithInfo)
{
	if (!QUACKLE_LEXICON_PARAMETERS->hasSomething())
		return;

	setupCounts(String::clearBlankness(wordWithInfo->wordLetterString));

	wordWithInfo->probability = Bag::probabilityOfDrawingFromFullBag(wordWithInfo->wordLetterString);

	m_wordspat.clear();
	wordspit(1, LetterString(), 0);	
	
	vector<WordWithInfo>::const_iterator end = m_wordspat.end();
	for (vector<WordWithInfo>::const_iterator it = m_wordspat.begin(); it != end; ++it)
	{
		if ((*it).wordLetterString == wordWithInfo->wordLetterString)
		{
			wordWithInfo->british = (*it).british;
			wordWithInfo->playability = (*it).playability;
			return;
		}
	}
}

void Generator::storeExtensions(WordWithInfo *wordWithInfo)
{
	// TODO(olaugh)
}

