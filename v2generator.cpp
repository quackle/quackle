#include <time.h>
#include <sys/time.h>

#include "board.h"
#include "boardparameters.h"
#include "datamanager.h"
#include "evaluator.h"
#include "gameparameters.h"
#include "lexiconparameters.h"
#include "strategyparameters.h"
#include "v2gaddag.h"
#include "v2generator.h"

//#define DEBUG_V2GEN

using namespace std;
using namespace Quackle;

V2Generator::V2Generator() {}

V2Generator::V2Generator(const GamePosition &position)
  : m_position(position) {
}

V2Generator::~V2Generator() {}

Move V2Generator::kibitz() {
  return findStaticBest();
}

Move V2Generator::findStaticBest() {
  UVcout << "findStaticBest(): " << endl << m_position << endl;
	m_best = Move::createPassMove();

  m_moveList.clear();

  setUpCounts(rack().tiles());
	m_rackBits = 0;
	for (int letter = QUACKLE_ALPHABET_PARAMETERS->firstLetter();
			 letter <= QUACKLE_ALPHABET_PARAMETERS->lastLetter(); ++letter) {
		if (m_counts[letter] > 0) m_rackBits |= (1 << letter);
	}

	struct timeval start, end;

	// TODO: are enough tiles in the bag?
  // TODO much later: would this end the game?
	gettimeofday(&start, NULL);
	findBestExchange();
	gettimeofday(&end, NULL);
	UVcout << "Time finding exchanges was "
				 << ((end.tv_sec * 1000000 + end.tv_usec)
						 - (start.tv_sec * 1000000 + start.tv_usec)) << " microseconds." << endl;
	UVcout << "best exchange: " << m_best << endl;
	gettimeofday(&start, NULL);
  m_anagrams = QUACKLE_ANAGRAMMAP->lookUp(rack());
	gettimeofday(&end, NULL);
	UVcout << "Time checking anagrammap was "
				 << ((end.tv_sec * 1000000 + end.tv_usec)
						 - (start.tv_sec * 1000000 + start.tv_usec)) << " microseconds." << endl;
	
	if (m_anagrams == NULL) {
		UVcout << "could not find rack anagrams!" << endl;
	} else {
		UVcout << "found rack anagrams!! usesWhatever.thruNone.numPlayed: ";
		UVcout << static_cast<int>(m_anagrams->usesWhatever.thruNone.numPlayed) << endl;
	}
	vector<Spot> spots;
	
	if (board()->isEmpty()) {
		gettimeofday(&start, NULL);
		findEmptyBoardSpots(&spots);
		gettimeofday(&end, NULL);
		UVcout << "Time finding spots on empty board was "
					 << ((end.tv_sec * 1000000 + end.tv_usec)
						 - (start.tv_sec * 1000000 + start.tv_usec)) << " microseconds." << endl;
	} else {
		gettimeofday(&start, NULL);
		computeHooks();
		debugHooks();
		
		findSpots(&spots);
		gettimeofday(&end, NULL);
		UVcout << "Time finding spots on nonempty board was "
					 << ((end.tv_sec * 1000000 + end.tv_usec)
						 - (start.tv_sec * 1000000 + start.tv_usec)) << " microseconds." << endl;

	}
	gettimeofday(&start, NULL);
	std::sort(spots.begin(), spots.end());
	gettimeofday(&end, NULL);
	UVcout << "Time sorting spots was "
				 << ((end.tv_sec * 1000000 + end.tv_usec)
						 - (start.tv_sec * 1000000 + start.tv_usec)) << " microseconds." << endl;

	for (Spot& spot : spots) {
		//#ifdef DEBUG_V2GEN
		UVcout << "Spot: (" << spot.anchorRow << ", " << spot.anchorCol
					 << ", " << "blank: " << spot.canUseBlank << "): "
					 << spot.maxEquity;
		//#endif
		restrictByLength(&spot);
		UVcout << endl;

		if (spot.maxEquity < m_best.equity) {
#ifdef DEBUG_V2GEN
			UVcout << "no need to check this spot!" << endl;
#endif
			continue;
		}
		findMovesAt(&spot);
	}
	UVcout << "best Move: " << m_best << endl;	
	return m_best;
}

void V2Generator::restrictByLength(Spot* spot) {
	int oldLongestViable = spot->longestViable;
	for (int i = 1; i <= oldLongestViable; ++i) {
		if (spot->worthChecking[i].couldBeBest &&
				(spot->worthChecking[i].maxEquity > m_best.equity)) {
			spot->longestViable = i;
		} else {
			spot->worthChecking[i].couldBeBest = false;
		}
	}
#ifdef DEBUG_V2GEN
	for (int i = 1; i <= 7; ++i) {
		if (spot->worthChecking[i].couldBeBest) {
			UVcout << ", [" << i << "]: " << spot->worthChecking[i].maxEquity;
		}
	}
	UVcout << endl;
#endif
}

void V2Generator::findExchanges(const uint64_t* rackPrimes,
																const LetterString& tiles,
																uint64_t product,
																int pos, int numExchanged) {
	if (pos == 7) {
    if (numExchanged == 0) return;
		double leave = product == 1 ? 0 :
			QUACKLE_STRATEGY_PARAMETERS->primeleave(product);
		//UVcout << "leave: " << leave << endl;
		// TODO: heuristics for num exchanged?
		if (leave > m_best.equity) {
			Move move;
			move.action = Move::Exchange;
			LetterString exchanged;
			for (int i = 0; i < numExchanged; ++i) {
				exchanged += m_placed[i];
			}
			move.setTiles(exchanged);
			move.score = 0;
			move.equity = leave;
			m_best = move;
		}
		if (leave > m_bestLeaves[numExchanged]) {
			m_bestLeaves[numExchanged] = leave;
		}
		return;
	}
	findExchanges(rackPrimes, tiles, product * rackPrimes[pos], pos + 1, numExchanged);
	m_placed[numExchanged] = tiles[pos];
	findExchanges(rackPrimes, tiles, product, pos + 1, numExchanged + 1);
}

void V2Generator::findBestExchange() {
	assert(rack().size() == 7);
	for (int i = 0; i <= 7; ++i) {
		m_bestLeaves[i] = -9999;
	}
	double bestEquity = 0;
	uint64_t primes[7];
	int bestMask = 0;
	const LetterString& tiles = rack().tiles();
	for (int i = 0; i < 7; ++i) {
		primes[i] = QUACKLE_PRIMESET->lookUpTile(tiles[i]);
	}
	uint64_t products[127];
	for (int i = 1; i < 127; ++i) products[i] = 1;
	int b = 1;
	for (int i = 0; i < 7; ++i) {
    for (int j = b; j < 127; j += b) {
			for (int k = 0; k < b; ++j, ++k) {
	      products[j] *= primes[i];
			}
		}
		b *= 2;
	}
	for (int mask = 1; mask < 127; mask++) {
#ifdef DEBUG_V2GEN
		{
			uint64_t product = 1;
			int numExchanged = 7;
			for (int i = 0; i < 7; ++i) {
				if (((1 << i) & mask) != 0) {
					--numExchanged;
					product *= primes[i];
				}
			}
			assert(numExchanged == (7 - __builtin_popcount(mask)));
			assert(product == products[mask]);
		}
#endif
		double leave = QUACKLE_STRATEGY_PARAMETERS->primeleave(products[mask]);
		if (leave > bestEquity) {
			bestEquity = leave;
			bestMask = mask;
		}
		int numExchanged = 7 - __builtin_popcount(mask);
		if (leave > m_bestLeaves[numExchanged]) {
			m_bestLeaves[numExchanged] = leave;
		}
	}
	LetterString exchanged;
	for (int i = 0; i < 7; ++i) {
		if (((1 << i) & bestMask) == 0) {
			exchanged += tiles[i];
		}
	}			
	m_best = Move::createExchangeMove(exchanged);
	m_best.equity = bestEquity;
}

bool V2Generator::couldMakeWord(const Spot& spot, int length) const {
	//assert(length > 0);
	//assert(length <= 7);
	if (m_anagrams == NULL) return true;
	const int lengthMask = 1 << length;
	const UsesTiles& usesTiles =
		spot.canUseBlank ? m_anagrams->usesWhatever : m_anagrams->usesNoBlanks;
	const NTileAnagrams& anagrams = usesTiles.thruNone;
	return (anagrams.numPlayed & lengthMask) != 0;
}

void V2Generator::findMovesAt(Spot* spot) {
	m_mainWordScore = spot->throughScore;
	m_hookScore = 0;
	//if (m_counts[QUACKLE_BLANK_MARK] == 0) return;
	//UVcout << "spot->canUseBlank: " << spot->canUseBlank
	//			 << ", m_counts[QUACKLE_BLANK_MARK]: "
	//			 << static_cast<int>(m_counts[QUACKLE_BLANK_MARK]) << endl;
	if (!spot->canUseBlank || m_counts[QUACKLE_BLANK_MARK] == 0) {
		struct timeval start, end;
		gettimeofday(&start, NULL);
		findBlankless(spot, 0, 0, 0, -1, 1,
									QUACKLE_LEXICON_PARAMETERS->v2Gaddag()->root());
		gettimeofday(&end, NULL);
		UVcout << "Time finding moves (without blank) was "
					 << ((end.tv_sec * 1000000 + end.tv_usec)
							 - (start.tv_sec * 1000000 + start.tv_usec))
					 << " microseconds." << endl;
	} else {
		struct timeval start, end;
		gettimeofday(&start, NULL);
		findBlankable(spot, 0, 0, 0, -1, 1,
									QUACKLE_LEXICON_PARAMETERS->v2Gaddag()->root());
		gettimeofday(&end, NULL);
		UVcout << "Time finding moves (with blank) was "
					 << ((end.tv_sec * 1000000 + end.tv_usec)
							 - (start.tv_sec * 1000000 + start.tv_usec))
					 << " microseconds." << endl;		
	}
}

namespace {
	UVString debugLetterMask(uint32_t letters) {
		UVString ret;
		for (Letter i = QUACKLE_ALPHABET_PARAMETERS->firstLetter();
				 i <= QUACKLE_ALPHABET_PARAMETERS->lastLetter(); ++i) {
			if ((letters & (1 << i)) != 0) {
				ret += QUACKLE_ALPHABET_PARAMETERS->userVisible(i);
			}
		}
		return ret;
	}
}

int V2Generator::scorePlay(const Spot& spot, int behind, int ahead) {
	int mainScore = spot.throughScore;
#ifdef DEBUG_V2GEN			
	UVcout << "throughScore: " << spot.throughScore << endl;
#endif
	int wordMultiplier = 1;
	int row = spot.anchorRow;
	int col = spot.anchorCol;
	int pos;
	int anchorPos;
	if (spot.horizontal) {
    anchorPos = col;
		col -= behind;
		pos = col;
	} else {
		anchorPos = row;
		row -= behind;
		pos = row;
	}
	for (; pos < anchorPos; ++pos) {
#ifdef DEBUG_V2GEN
		assert(pos >= 0);
		assert(pos < 15);
		UVcout << "pos: " << pos << endl;
		UVcout << "m_placed[pos]: " << static_cast<int>(m_placed[pos]) << endl;
#endif

#ifdef DEBUG_V2GEN			
		UVcout << "QUACKLE_BOARD_PARAMETERS->wordMultiplier(" << row << ", "
					 << col << "): "
					 << QUACKLE_BOARD_PARAMETERS->wordMultiplier(row, col) << endl;
#endif
		wordMultiplier *= QUACKLE_BOARD_PARAMETERS->wordMultiplier(row, col);
		if (QUACKLE_ALPHABET_PARAMETERS->isPlainLetter(m_placed[pos])) {
			int letterMultiplier = QUACKLE_BOARD_PARAMETERS->letterMultiplier(row, col);
#ifdef DEBUG_V2GEN			
			UVcout << "letterMultiplier: " << letterMultiplier << endl;
#endif			
			mainScore += letterMultiplier *
				QUACKLE_ALPHABET_PARAMETERS->score(m_placed[pos]);
		}
		//UVcout << "col: " << col << ", row: " << row << endl;
		//		if (spot.horizontal) ++col; else ++row;
		col++;
		//UVcout << "(incremented) col: " << col << ", row: " << row << endl;		
	}
	for (int i = 0; i < ahead; ++i) {
		//FIXME: needs to handle playThrough
		//if (m_placed[pos] != QUACKLE_NULL_MARK) {
#ifdef DEBUG_V2GEN			
		UVcout << "QUACKLE_BOARD_PARAMETERS->wordMultiplier(" << row << ", "
					 << col << "): "
					 << QUACKLE_BOARD_PARAMETERS->wordMultiplier(row, col) << endl;
#endif
		wordMultiplier *= QUACKLE_BOARD_PARAMETERS->wordMultiplier(row, col);
		//UVcout << "pos: " << pos << " m_placed[pos]: " << pos << endl;
		//assert(QUACKLE_ALPHABET_PARAMETERS->isPlainLetter(m_placed[pos]));
		if (QUACKLE_ALPHABET_PARAMETERS->isPlainLetter(m_placed[pos])) {
			int letterMultiplier = QUACKLE_BOARD_PARAMETERS->letterMultiplier(row, col);
#ifdef DEBUG_V2GEN			
			UVcout << "letterMultiplier: " << letterMultiplier << endl;
#endif
			mainScore += letterMultiplier *
				QUACKLE_ALPHABET_PARAMETERS->score(m_placed[pos]);
		}
		++pos;
		//UVcout << "col: " << col << ", row: " << row << endl;
		//if (spot.horizontal) ++col; else ++row;
		col++;
		//UVcout << "(incremented) col: " << col << ", row: " << row << endl;		
	}
#ifdef DEBUG_V2GEN			
	UVcout << "wordMultiplier: " << wordMultiplier << endl;
#endif	
	mainScore *= wordMultiplier;
	// TODO: add scores for hooks!
	if (ahead + 1 + behind == QUACKLE_PARAMETERS->rackSize()) {
		mainScore += QUACKLE_PARAMETERS->bingoBonus();
	}
	return mainScore;
}

double V2Generator::getLeave() const {
	uint64_t product = 1;
	for (int i = QUACKLE_BLANK_MARK; i <= QUACKLE_ALPHABET_PARAMETERS->lastLetter(); ++i) {
		for (int j = 0; j < m_counts[i]; ++j) {
			product *= QUACKLE_PRIMESET->lookUpTile(i);
		}
	}
	return QUACKLE_STRATEGY_PARAMETERS->primeleave(product);
}

bool V2Generator::nextLetter(const V2Gaddag& gaddag, const unsigned char* node,
														 uint32_t restriction,
														 Letter minLetter, int* childIndex, Letter* foundLetter,
														 const unsigned char** child) const {
	*child = gaddag.nextRackChild(node, minLetter, restriction, childIndex,
																foundLetter);
#ifdef DEBUG_V2GEN
	if (*child == NULL) {
		UVcout << "child == NULL; no more children on rack." << endl;
	} else {
		UVcout << "foundLetter: " << static_cast<int>(*foundLetter) << endl;
	}
#endif
	return *child != NULL;
}

// updates m_mainWordScore, returns tile score so it can be subtracted later
int V2Generator::scoreLetter(int pos, Letter letter, int letterMultiplier) {
#ifdef DEBUG_V2GEN
	assert(letter >= QUACKLE_ALPHABET_PARAMETERS->firstLetter());
	assert(letter <= QUACKLE_ALPHABET_PARAMETERS->lastLetter());
	UVcout << "placing " << QUACKLE_ALPHABET_PARAMETERS->userVisible(letter)
				 << " at m_placed[" << pos << "]" << endl;
#endif
	m_placed[pos] = letter;
	const int letterScore = QUACKLE_ALPHABET_PARAMETERS->score(letter);
	const int tileMainScore = letterScore * letterMultiplier;
	m_mainWordScore += tileMainScore;
	return tileMainScore;	
}

void V2Generator::debugPlaced(const Spot& spot, int behind, int ahead) const {
	UVcout << "m_rackBits: " << debugLetterMask(m_rackBits) << endl;
	UVcout << "counts: " << counts2string() << endl;
	int startCol = spot.anchorCol - behind;
	UVcout << "startCol: " << startCol << endl;
	LetterString word;
	for (int i = startCol; i < spot.anchorCol + ahead + 1; ++i) {
		assert(i >= 0); assert(i < 15);
		word += m_placed[i];
	}
	UVcout << "m_placed[" << startCol << " to " << spot.anchorCol + ahead
				 << "]: " << QUACKLE_ALPHABET_PARAMETERS->userVisible(word) << endl;
}

void V2Generator::useLetter(Letter letter, uint32_t* foundLetterMask) {
	assert(m_counts[letter] > 0);
	m_counts[letter]--;
	assert(m_counts[letter] >= 0);
	*foundLetterMask = 1 << letter;
	if (m_counts[letter] == 0) m_rackBits &= ~(*foundLetterMask);
}

void V2Generator::unuseLetter(Letter letter, uint32_t foundLetterMask) {
	m_counts[letter]++;
	m_rackBits |= foundLetterMask;	
}

bool V2Generator::maybeRecordMove(const Spot& spot, int wordMultiplier,
																	int behind, int ahead, int numPlaced) {
	// UVcout << "m_mainWordScore: " << m_mainWordScore
	//  			 << ", wordMultiplier: " <<  wordMultiplier
	//  			 << ", m_hookScore: " << m_hookScore << endl;
	int score = m_mainWordScore * wordMultiplier + m_hookScore;
	if (numPlaced == QUACKLE_PARAMETERS->rackSize()) {
		score += QUACKLE_PARAMETERS->bingoBonus();
	}
	//assert(score == scorePlay(spot, behind, ahead));
	double equity = score;
	if (numPlaced < 7) equity += getLeave();
	assert(equity <= spot.maxEquity);
	assert(spot.worthChecking[numPlaced].couldBeBest);
	assert(equity <= spot.worthChecking[numPlaced].maxEquity);
	if (equity > m_best.equity) {
		LetterString word;
		int startRow = spot.anchorRow;
		int startCol = spot.anchorCol;
		int startPos;
		int anchorPos;
		if (spot.horizontal) {
			startCol -= behind;
			startPos = startCol;
			anchorPos = spot.anchorCol;
		} else {
			startRow -= behind;
			startPos = startRow;
			anchorPos = spot.anchorRow;
		}
		for (int pos = startPos; pos < anchorPos + ahead + 1; ++pos) {
			//assert(i >= 0); assert(i < 15);
			//UVcout << "letter: " << static_cast<int>(m_placed[i]) << endl;
			word += m_placed[pos];
		}
		Move move = Move::createPlaceMove(startRow, startCol, spot.horizontal, word);
		move.score = score;
		move.equity = equity;
		m_best = move;
		/*
		UVcout << "spot.worthChecking[" << numPlaced << "].maxEquity: "
					 << spot.worthChecking[numPlaced].maxEquity << endl;
		if (equity > spot.worthChecking[numPlaced].maxEquity) {
			UVcout << "equity > maxEquity, WTF?" << endl;
		}
		UVcout << "new best: " << move << endl;
		*/
		return true;
	}
	return false;
}

// TODO: Make this clever enough to handle all the "through" tiles.
// Should look up board positions of squares relative to anchor+delta
// before calling findBlankless etc
void V2Generator::getSquare(const Spot& spot, int delta,
														int* row, int* col, int* pos) const {
	if (spot.horizontal) {
		*row = spot.anchorRow;
		*col = spot.anchorCol + delta;
		*pos = *col;
	} else {
		*row = spot.anchorRow + delta;
		*col = spot.anchorCol;
		*pos = *row;
	}
#ifdef DEBUG_V2GEN
  UVcout << "row: " << *row << ", col: " << *col << ", pos: " << *pos << endl;
	assert(*col >= 0); assert(*col < 15); assert(*row >= 0); assert(*row < 15);
#endif
}

void V2Generator::findMoreBlankless(Spot* spot, int delta, int ahead,
																		int behind, int velocity, int wordMultiplier,
																		const V2Gaddag& gaddag, const unsigned char* node) {
	if (velocity < 0) {
		if (behind < spot->maxTilesBehind) {
			findBlankless(spot, delta - 1, 0, behind + 1, -1, wordMultiplier, node);
		}
		if (ahead >= spot->maxTilesAhead) return;
#ifdef V2GEN
		UVcout << "changing direction..." << endl;
#endif
		const unsigned char* changeChild = gaddag.changeDirection(node);
		if (changeChild != NULL) {
			node = gaddag.followIndex(changeChild);
			if (node != NULL) { 
				findBlankless(spot, 1, 1, behind, 1, wordMultiplier, node);
			}
		}
	} else if (ahead < spot->maxTilesAhead) {
		findBlankless(spot, delta + 1, ahead + 1, behind, 1, wordMultiplier, node);
	}
}

void V2Generator::findMoreBlankable(Spot* spot, int delta, int ahead,
																		int behind, int velocity, int wordMultiplier,
																		const V2Gaddag& gaddag, const unsigned char* node) {
	if (velocity < 0) {
		if (behind < spot->maxTilesBehind) {
			findBlankable(spot, delta - 1, 0, behind + 1, -1, wordMultiplier, node);
		}
		if (ahead >= spot->maxTilesAhead) return;
#ifdef V2GEN
		UVcout << "changing direction..." << endl;
#endif
		const unsigned char* changeChild = gaddag.changeDirection(node);
		if (changeChild != NULL) {
			node = gaddag.followIndex(changeChild);
			if (node != NULL) { 
				findBlankable(spot, 1, 1, behind, 1, wordMultiplier, node);
			}
		}
	} else if (ahead < spot->maxTilesAhead) {
		findBlankable(spot, delta + 1, ahead + 1, behind, 1, wordMultiplier, node);
	}
}

void V2Generator::findBlankless(Spot* spot, int delta, int ahead, int behind,
																int velocity, int wordMultiplier,
																const unsigned char* node) {
	// TODO: use more specific gaddags based on min/max word length for this spot
	const V2Gaddag& gaddag = *(QUACKLE_LEXICON_PARAMETERS->v2Gaddag());
#ifdef DEBUG_V2GEN
	UVcout << "findBlankless(delta: " << delta<< ", ahead: " << ahead
				 << ", behind: " << behind << ", velocity: " << velocity << ")" << endl;
#endif
	int numPlaced, row, col, pos;
  // I think actually I mean for "ahead" to include the anchor spot
	// (for hooking spots) so ahead and behind maybe ought to initially
	// be 1 and 0 respectively for hook spots (or empty board) in which
	// case numPlaced would just be ahead + behind. Make that change at
	// some point and be sure that everything is still correct. "Through"
	// spots will start at 0 and 0 because there's nothing placed at the
	// anchor.
  numPlaced =	1 + ahead + behind;
	getSquare(*spot, delta, &row, &col, &pos);
	Letter minLetter = QUACKLE_GADDAG_SEPARATOR;
	int childIndex = 0;
	Letter foundLetter;
	uint32_t possibleLetters = m_rackBits;
	const Hook& hook =
		spot->horizontal ? m_vertHooks[row][col] : m_horizHooks[row][col];
	if (hook.touches) {
		possibleLetters &= hook.letters;
		if (possibleLetters == 0) return;
    //UVcout << "hook.score: " << hook.score << ", m_hookScore: "
		//			 << m_hookScore << " -> " << (m_hookScore + hook.score) << endl;
		m_hookScore += hook.score;
	}
	int newWordMultiplier = wordMultiplier *
		QUACKLE_BOARD_PARAMETERS->wordMultiplier(row, col);
	int letterMultiplier = QUACKLE_BOARD_PARAMETERS->letterMultiplier(row, col);
	int hookMultiplier = hookLetterMultiplier(row, col, spot->horizontal);
	const unsigned char* child = NULL;
	while (nextLetter(gaddag, node, possibleLetters, minLetter,
										&childIndex, &foundLetter, &child)) {
		if (child == NULL) UVcout << "child is still NULL!" << endl;
    const int tileMainScore = scoreLetter(pos, foundLetter, letterMultiplier);
		const int tileHookScore =
			hookMultiplier * QUACKLE_ALPHABET_PARAMETERS->score(foundLetter);
		// UVcout << "tileHookScore: " << tileHookScore
		// 			 << ", m_hookScore: " << m_hookScore << " -> "
		// 			 << (m_hookScore + tileHookScore) << endl;
		m_hookScore += tileHookScore;
		uint32_t foundLetterMask; // reused below in unuseLetter
    useLetter(foundLetter, &foundLetterMask);
#ifdef DEBUG_V2GEN
		debugPlaced(*spot, behind, ahead);
#endif		
		if (spot->viableAtLength(numPlaced) &&
				gaddag.completesWord(child) &&
				maybeRecordMove(*spot, newWordMultiplier, behind, ahead, numPlaced)) {
#ifdef DEBUG_V2GEN
			UVcout << "better than " << m_best.equity;
#endif
			restrictByLength(spot);
#ifdef DEBUG_V2GEN
			UVcout << endl;
#endif
		}
		if (numPlaced + 1 <= spot->longestViable) {
			const unsigned char* newNode = gaddag.followIndex(child);
			if (newNode != NULL) {
				findMoreBlankless(spot, delta, ahead, behind, velocity, newWordMultiplier,
													gaddag, newNode);
			}
		}
#ifdef DEBUG_V2GEN
		else {
			UVcout << "don't need to check longer than " << numPlaced << endl;
		}
#endif

		// UVcout << "tileHookScore: " << tileHookScore
		// 			 << ", m_hookScore: " << m_hookScore << " -> "
		// 			 << (m_hookScore - tileHookScore) << endl;
		m_hookScore -= tileHookScore;
		m_mainWordScore -= tileMainScore;
		unuseLetter(foundLetter, foundLetterMask);

		if (numPlaced > spot->longestViable) {
#ifdef DEBUG_V2GEN
			UVcout << "don't need to check any more of length " << numPlaced << endl;
#endif
			break;
		}
		minLetter = foundLetter + 1;
		++childIndex;
	}

	// UVcout << "hook.score: " << hook.score
	// 			 << ", m_hookScore: " << m_hookScore << " -> "
	// 			 << (m_hookScore - hook.score) << endl;
	m_hookScore -= hook.score;
}


void V2Generator::findBlankable(Spot* spot, int delta, int ahead, int behind,
																int velocity, int wordMultiplier,
																const unsigned char* node) {
	// TODO: use more specific gaddags based on min/max word length for this spot
	const V2Gaddag& gaddag = *(QUACKLE_LEXICON_PARAMETERS->v2Gaddag());
#ifdef DEBUG_V2GEN
	UVcout << "findBlankable(delta: " << delta << ", ahead: " << ahead
				 << ", behind: " << behind << ", velocity: " << velocity << ")" << endl;
#endif
	int numPlaced, row, col, pos;
	// Not true for "through" spots!
  numPlaced =	1 + ahead + behind;
	getSquare(*spot, delta, &row, &col, &pos);
	int newWordMultiplier = wordMultiplier *
		QUACKLE_BOARD_PARAMETERS->wordMultiplier(row, col);
	Letter minLetter = QUACKLE_GADDAG_SEPARATOR;
	int childIndex = 0;
	Letter foundLetter;
	assert(blankOnRack());
	uint32_t possibleLetters = m_everyLetter;
	const Hook& hook =
		spot->horizontal ? m_vertHooks[row][col] : m_horizHooks[row][col];
	if (hook.touches) {
		//UVcout << "hook touches at row: " << row << ", col: " << col << endl;
		//UVcout << "hook.letters: " << debugLetterMask(hook.letters) << endl;
		possibleLetters &= hook.letters;
		if (possibleLetters == 0) return;
    //UVcout << "hook.score: " << hook.score << ", m_hookScore: "
		//			 << m_hookScore << " -> " << (m_hookScore + hook.score) << endl;
		m_hookScore += hook.score;
	}
	const unsigned char* child = NULL;
  while (nextLetter(gaddag, node, possibleLetters, minLetter,
										&childIndex, &foundLetter, &child)) {
		assert(child != NULL);
    m_counts[QUACKLE_BLANK_MARK]--;
		
    Letter blankLetter = QUACKLE_ALPHABET_PARAMETERS->setBlankness(foundLetter);
#ifdef DEBUG_V2GEN
		assert(foundLetter >= QUACKLE_ALPHABET_PARAMETERS->firstLetter());
		assert(foundletter <= QUACKLE_ALPHABET_PARAMETERS->lastLetter());
		//UVcout << "placing " << QUACKLE_ALPHABET_PARAMETERS->userVisible(blankLetter)
		//			 << " at m_placed[" << pos << "]" << endl;
#endif
		m_placed[pos] = blankLetter;

		if (spot->viableAtLength(numPlaced) &&
				gaddag.completesWord(child) &&
				maybeRecordMove(*spot, newWordMultiplier, behind, ahead, numPlaced)) {
#ifdef DEBUG_V2GEN
			UVcout << "better than " << m_best.equity;
#endif
			restrictByLength(spot);
#ifdef DEBUG_V2GEN
			UVcout << endl;
#endif
		}
		if (numPlaced + 1 <= spot->longestViable) {
			const unsigned char* newNode = gaddag.followIndex(child);
			if (newNode != NULL) {
				if (blankOnRack()) {
					findMoreBlankable(spot, delta, ahead, behind, velocity,
														newWordMultiplier, gaddag, newNode);

				} else {
					findMoreBlankless(spot, delta, ahead, behind, velocity,
														newWordMultiplier, gaddag, newNode);
				}
			}
		}
#ifdef DEBUG_V2GEN
		else {
			UVcout << "don't need to check longer than " << numPlaced << endl;
		}
#endif

		m_counts[QUACKLE_BLANK_MARK]++;
		minLetter = foundLetter + 1;
		++childIndex;
	}
	if (m_rackBits == 0) {
		m_hookScore -= hook.score;
		return;
	}
	minLetter = QUACKLE_GADDAG_SEPARATOR;
	childIndex = 0;
	child = NULL;
	possibleLetters = m_rackBits;
	if (hook.touches) {
		//UVcout << "hook touches at row: " << row << ", col: " << col << endl;
		//UVcout << "hook.letters: " << debugLetterMask(hook.letters) << endl;
		possibleLetters &= hook.letters;
		//UVcout << "possibleLetters: " << debugLetterMask(possibleLetters) << endl;
		if (possibleLetters == 0) {
			m_hookScore -= hook.score;
			return;
		}
	}
	int letterMultiplier = QUACKLE_BOARD_PARAMETERS->letterMultiplier(row, col);
	int hookMultiplier = hookLetterMultiplier(row, col, spot->horizontal);
	while (nextLetter(gaddag, node, possibleLetters, minLetter,
										&childIndex, &foundLetter, &child)) {
		assert(child != NULL);
    const int tileMainScore = scoreLetter(pos, foundLetter, letterMultiplier);
		const int tileHookScore =
			hookMultiplier * QUACKLE_ALPHABET_PARAMETERS->score(foundLetter);
		//UVcout << "tileHookScore: " << tileHookScore
		//			 << ", m_hookScore: " << m_hookScore << " -> "
		//			 << (m_hookScore + tileHookScore) << endl;
		m_hookScore += tileHookScore;
		uint32_t foundLetterMask; // reused below in unuseLetter
    useLetter(foundLetter, &foundLetterMask);
#ifdef DEBUG_V2GEN
		debugPlaced(*spot, behind, ahead);
#endif		
		// FIXME: all this assumes horizontal
		// Test it by making the empty-board spot vertie!
		if (spot->viableAtLength(numPlaced) &&
				gaddag.completesWord(child) &&
				maybeRecordMove(*spot, newWordMultiplier, behind, ahead, numPlaced)) {
#ifdef DEBUG_V2GEN
			UVcout << "better than " << m_best.equity;
#endif
			restrictByLength(spot);
#ifdef DEBUG_V2GEN
			UVcout << endl;
#endif
		}
		if (numPlaced + 1 <= spot->longestViable) {
			const unsigned char* newNode = gaddag.followIndex(child);
			if (newNode != NULL) {
				if (blankOnRack()) {
					findMoreBlankable(spot, delta, ahead, behind, velocity,
														newWordMultiplier, gaddag, newNode);

				} else {
					findMoreBlankless(spot, delta, ahead, behind, velocity,
														newWordMultiplier, gaddag, newNode);
				}
			}
		}
#ifdef DEBUG_V2GEN
		else {
			UVcout << "don't need to check longer than " << numPlaced << endl;
		}
#endif

		//UVcout << "tileHookScore: " << tileHookScore
		//			 << ", m_hookScore: " << m_hookScore << " -> "
		//			 << (m_hookScore - tileHookScore) << endl;
		m_hookScore -= tileHookScore;
		m_mainWordScore -= tileMainScore;
		unuseLetter(foundLetter, foundLetterMask);

		if (numPlaced > spot->longestViable) {
#ifdef DEBUG_V2GEN
			UVcout << "don't need to check any more of length " << numPlaced << endl;
#endif
			break;
		}
		minLetter = foundLetter + 1;
		++childIndex;
	}
	m_hookScore -= hook.score;
}

float V2Generator::bestLeave(const Spot& spot, int length) const {
	assert(length > 0);
	assert(length < 7);
	// TODO: Handle single letter playedThrough too
  assert(spot.numThrough == 0);
	if (m_anagrams != NULL) {
		const UsesTiles& usesTiles =
			spot.canUseBlank ? m_anagrams->usesWhatever : m_anagrams->usesNoBlanks;
		const NTileAnagrams& anagrams = usesTiles.thruNone;
		int index = length - 1;
		return anagrams.bestLeaves[index] / 256.0f;
	}
	return m_bestLeaves[length];
}

int V2Generator::hookLetterMultiplier(int row, int col, bool horiz) {
	const Hook& hook =
			horiz ? m_vertHooks[row][col] : m_horizHooks[row][col];
	if (hook.touches) {
		return
			QUACKLE_BOARD_PARAMETERS->letterMultiplier(row, col) *
			QUACKLE_BOARD_PARAMETERS->wordMultiplier(row, col);
	}
	return 0;
}

void V2Generator::scoreSpot(Spot* spot) {
	spot->canMakeAnyWord = false;
	const int numTiles = rack().size();
	const LetterString& tiles = rack().tiles();
	int tileScores[QUACKLE_MAXIMUM_BOARD_SIZE];
	for (int i = 0; i < numTiles; ++i) {
		tileScores[i] = QUACKLE_ALPHABET_PARAMETERS->score(tiles[i]);
	}
	std::sort(tileScores, tileScores + numTiles, std::greater<int>());
	int throughScore = 0;
	// This will already be in the Spot, better to sum it as we go through the row
	/*
	for (const Letter letter : spot->playedThrough) {
		if (letter == QUACKLE_NULL_MARK) continue;
		if (QUACKLE_ALPHABET_PARAMETERS->isBlankLetter(letter)) {
			throughScore += QUACKLE_ALPHABET_PARAMETERS->score(QUACKLE_BLANK_MARK);
		} else {
			throughScore += QUACKLE_ALPHABET_PARAMETERS->score(letter);
		}
	}
	*/
	float maxEquity = -9999;
	for (int i = 1; i <= 7; ++i) {
		spot->worthChecking[i].maxEquity = maxEquity;
		spot->worthChecking[i].couldBeBest = false;
	}
	
	int wordMultipliers[QUACKLE_MAXIMUM_BOARD_SIZE];
	int hookLetterMultipliers[QUACKLE_MAXIMUM_BOARD_SIZE];
	int letterMultipliers[QUACKLE_MAXIMUM_BOARD_SIZE];
	int hookScores[QUACKLE_MAXIMUM_BOARD_SIZE];
	int row = spot->anchorRow;
	int col = spot->anchorCol;
	int anchorPos = (spot->horizontal) ? col : row;
	for (int i = 0; i <= spot->maxTilesAhead; ++i) {
		if (board()->isNonempty(row, col)) {
			i--;
		} else {
			wordMultipliers[anchorPos + i] =
				QUACKLE_BOARD_PARAMETERS->wordMultiplier(row, col);
			hookLetterMultipliers[anchorPos + i] =
				hookLetterMultiplier(row, col, spot->horizontal);
			letterMultipliers[anchorPos + i] =
				QUACKLE_BOARD_PARAMETERS->letterMultiplier(row, col);
			const Hook& hook =
				spot->horizontal ? m_vertHooks[row][col] : m_horizHooks[row][col];
			hookScores[anchorPos + i] = hook.score;
		}
		if (spot->horizontal) col++; else row++;
	}
	row = spot->anchorRow;
	col = spot->anchorCol;
	for (int i = 1; i <= spot->maxTilesBehind; ++i) {
		if (spot->horizontal) col--; else row--;
		if (board()->isNonempty(row, col)) {
			i--;
		} else {
			wordMultipliers[anchorPos - i] =
				QUACKLE_BOARD_PARAMETERS->wordMultiplier(row, col);
			hookLetterMultipliers[anchorPos - i] =
				hookLetterMultiplier(row, col, spot->horizontal);
			letterMultipliers[anchorPos - i] =
				QUACKLE_BOARD_PARAMETERS->letterMultiplier(row, col);
			const Hook& hook =
				spot->horizontal ? m_vertHooks[row][col] : m_horizHooks[row][col];
			hookScores[anchorPos - i] = hook.score;
		}
	}
  for (int ahead = spot->minTilesAhead; ahead <= spot->maxTilesAhead; ++ahead) {
		// num tiles played behind anchor + ahead of anchor <= num tiles on rack
		const int maxBehind = std::min(spot->maxTilesBehind, numTiles - ahead);
		for (int behind = 0; behind <= maxBehind; ++behind) {
			//struct timeval start, end;
			//gettimeofday(&start, NULL);

			// This is all wrong except for empty boards
			const int played = ahead + behind;
			if (played < 2) continue; // have to play at least one tile!
			
			if (!couldMakeWord(*spot, played)) {
				//UVcout << "can not make word of length " << played << endl;
				continue;
			} else {
				spot->worthChecking[played].couldBeBest = true;
				//UVcout << "can make word of length " << played << endl;
			}
			spot->canMakeAnyWord = true;
			int wordMultiplier = 1;
			int usedLetterMultipliers[QUACKLE_MAXIMUM_BOARD_SIZE];
			int minPos = anchorPos - behind;
			int hookScore = 0;
			for (int pos = minPos; pos < minPos + played; ++pos) {
				wordMultiplier *= wordMultipliers[pos];
				hookScore += hookScores[pos];
			}
			memcpy(&usedLetterMultipliers, &(letterMultipliers[minPos]),
						 played * sizeof(int));
      for (int i = 0; i < played; ++i) {
				usedLetterMultipliers[i] *= wordMultiplier;
				usedLetterMultipliers[i] += hookLetterMultipliers[i + minPos];
			}
			std::sort(usedLetterMultipliers,
								usedLetterMultipliers + played,
								std::greater<int>());
			int playedScore = 0;
			int maxNonBlankTiles = played;
			//UVcout << "played: " << played << endl;
			// We can do better, but need to make canUseBlank be mustUseBlank
			bool assumeBlankPlayed = blankOnRack() && (played == 7);
			if (assumeBlankPlayed) maxNonBlankTiles--;
			for (int i = 0; i < maxNonBlankTiles; ++i) {
				//UVcout << "playedScore += " << tileScores[i] << " * "
				//			 << usedLetterMultipliers[i] << endl;
				playedScore += usedLetterMultipliers[i] * tileScores[i];
			}
			//UVcout << "playedScore: " << playedScore << endl;
			int score = (throughScore * wordMultiplier) + playedScore + hookScore;
			//UVcout << "score: " << playedScore << " + " << hookScore
			//			 << " = " << score << endl;
			if (played == QUACKLE_PARAMETERS->rackSize()) {
				score += QUACKLE_PARAMETERS->bingoBonus();
				//UVcout << "score += 50 for bingo bonus";
			}
			float optimisticEquity = score;
			if (played < 7) {
				optimisticEquity += bestLeave(*spot, played);
				//UVcout << "optimisticEquity += leave (" << bestLeave(*spot, played)
				//			 << ")" << endl;
				/*
				UVcout << "use?:" << spot->canUseBlank << ", played: " << played
							 << ", minPos: " << minPos << ", score: " << score
							 << ", leave: " << bestLeave(*spot, played)
							 << ", optimisticEquity: " << optimisticEquity << endl;
				*/
			}
			//UVcout << "optimisticEquity: " << optimisticEquity << endl;
			maxEquity = std::max(maxEquity, optimisticEquity);
			spot->worthChecking[played].maxEquity =
				std::max(spot->worthChecking[played].maxEquity, optimisticEquity);
			// gettimeofday(&end, NULL);
			// UVcout << "Time scoring behind: " << behind << " ahead: " << ahead << " "
			// 	 << ((end.tv_sec * 1000000 + end.tv_usec)
			// 			 - (start.tv_sec * 1000000 + start.tv_usec)) << " microseconds." << endl;

		}
	}
	if (!spot->canMakeAnyWord) return;
	spot->maxEquity = maxEquity;
	// UVcout << "Spot: (" << spot->anchorRow << ", " << spot->anchorCol
	// 			 << ", blank: " << spot->canUseBlank << ") "
	// 			 << spot->maxEquity << endl;
}

bool V2Generator::isEmpty(int row, int col) {
	return board()->letter(row, col) == QUACKLE_NULL_MARK;
}

Letter V2Generator::boardLetter(int row, int col) {
	return board()->letter(row, col);
}

const unsigned char* V2Generator::followLetter(const V2Gaddag& gaddag,
																							 int row, int col,
																							 const unsigned char* node) {
	Letter letter = QUACKLE_ALPHABET_PARAMETERS->clearBlankness(boardLetter(row, col));
	if (!gaddag.hasChild(node, letter)) return NULL;
	const unsigned char* child = gaddag.child(node, letter);
	return gaddag.followIndex(child);
}

const unsigned char* V2Generator::vertBeforeNode(int anchorRow, int col,
																								 int numLetters) {
	const V2Gaddag& gaddag = *(QUACKLE_LEXICON_PARAMETERS->v2Gaddag());
	const unsigned char* node = gaddag.root();
	int startRow = anchorRow - numLetters;
	Letter letter = boardLetter(startRow, col);
	letter = QUACKLE_ALPHABET_PARAMETERS->clearBlankness(letter);
	if (!gaddag.hasChild(node, letter)) return NULL;
	const unsigned char* child = gaddag.child(node, letter);
	node = QUACKLE_LEXICON_PARAMETERS->v2Gaddag()->followIndex(child);
	const unsigned char* changeChild = gaddag.changeDirection(node);
  node = QUACKLE_LEXICON_PARAMETERS->v2Gaddag()->followIndex(changeChild);	
	for (int row = startRow + 1; row < anchorRow; ++row) {
		node = followLetter(gaddag, row, col, node);
		if (node == NULL) break;
	}
	return node;
}

const unsigned char* V2Generator::vertAfterNode(int anchorRow, int col,
																								int numLetters) {
	const V2Gaddag& gaddag = *(QUACKLE_LEXICON_PARAMETERS->v2Gaddag());
	const unsigned char* node = gaddag.root();
	int startRow = anchorRow + numLetters;
	Letter letter = boardLetter(startRow, col);
	letter = QUACKLE_ALPHABET_PARAMETERS->clearBlankness(letter);
	if (!gaddag.hasChild(node, letter)) return NULL;
	const unsigned char* child = gaddag.child(node, letter);
	node = QUACKLE_LEXICON_PARAMETERS->v2Gaddag()->followIndex(child);
	for (int row = startRow - 1; row > anchorRow; --row) {
		node = followLetter(gaddag, row, col, node);
		if (node == NULL) break;
	}
	return node;
}

const unsigned char* V2Generator::horizBeforeNode(int row, int anchorCol,
																									int numLetters) {
	const V2Gaddag& gaddag = *(QUACKLE_LEXICON_PARAMETERS->v2Gaddag());
	const unsigned char* node = gaddag.root();
	int startCol = anchorCol - numLetters;
	Letter letter = boardLetter(row, startCol);
	letter = QUACKLE_ALPHABET_PARAMETERS->clearBlankness(letter);
	if (!gaddag.hasChild(node, letter)) return NULL;
	const unsigned char* child = gaddag.child(node, letter);
	node = QUACKLE_LEXICON_PARAMETERS->v2Gaddag()->followIndex(child);
	const unsigned char* changeChild = gaddag.changeDirection(node);
  node = QUACKLE_LEXICON_PARAMETERS->v2Gaddag()->followIndex(changeChild);	
	for (int col = startCol + 1; col < anchorCol; ++col) {
		node = followLetter(gaddag, row, col, node);
		if (node == NULL) break;
	}
	return node;
}

const unsigned char* V2Generator::horizAfterNode(int row, int anchorCol,
																								 int numLetters) {
	const V2Gaddag& gaddag = *(QUACKLE_LEXICON_PARAMETERS->v2Gaddag());
	const unsigned char* node = gaddag.root();
	int startCol = anchorCol + numLetters;
	Letter letter = boardLetter(row, startCol);
	letter = QUACKLE_ALPHABET_PARAMETERS->clearBlankness(letter);
	if (!gaddag.hasChild(node, letter)) return NULL;
	const unsigned char* child = gaddag.child(node, letter);
	node = QUACKLE_LEXICON_PARAMETERS->v2Gaddag()->followIndex(child);
	for (int col = startCol - 1; col > anchorCol; --col) {
		letter = QUACKLE_ALPHABET_PARAMETERS->clearBlankness(boardLetter(row, col));
		if (!gaddag.hasChild(node, letter)) return NULL;
		child = gaddag.child(node, letter);
		node = gaddag.followIndex(child);
		if (node == NULL) break;
	}
	return node;
}

uint32_t V2Generator::wordCompleters(const unsigned char* node) {
	uint32_t completers = 0;
	const V2Gaddag& gaddag = *(QUACKLE_LEXICON_PARAMETERS->v2Gaddag());
	Letter minLetter = QUACKLE_GADDAG_SEPARATOR;
	int childIndex = 0;
	Letter foundLetter;
  const unsigned char* child;
	while (nextLetter(gaddag, node, m_everyLetter, minLetter,
										&childIndex, &foundLetter, &child)) {
		assert(child != NULL);
		if (gaddag.completesWord(child)) completers |= (1 << foundLetter);
		minLetter = foundLetter + 1;
		++childIndex;
	}
	return completers;
}

bool V2Generator::vertCompletesWord(const V2Gaddag& gaddag, int row, int col,
																		const unsigned char* node,
																		int numLettersAfter) {
	for (int i = 0; i < numLettersAfter - 1; ++i) {
		node = followLetter(gaddag, row, col, node);
		if (node == NULL) return false;
		row++;
	}
	Letter letter = QUACKLE_ALPHABET_PARAMETERS->clearBlankness(boardLetter(row, col));
	if (!gaddag.hasChild(node, letter)) return false;
	const unsigned char* child = gaddag.child(node, letter);
	if (child == NULL) return false;
	return gaddag.completesWord(child);
}

bool V2Generator::horizCompletesWord(const V2Gaddag& gaddag, int row, int col,
																		 const unsigned char* node,
																		 int numLettersAfter) {
	for (int i = 0; i < numLettersAfter - 1; ++i) {
		node = followLetter(gaddag, row, col, node);
		if (node == NULL) return false;
		col++;
	}
	Letter letter = QUACKLE_ALPHABET_PARAMETERS->clearBlankness(boardLetter(row, col));
	if (!gaddag.hasChild(node, letter)) return false;
	const unsigned char* child = gaddag.child(node, letter);
	if (child == NULL) return false;
	return gaddag.completesWord(child);
}

uint32_t V2Generator::vertBetween(int row, int col, int numLettersAfter,
																	const unsigned char* beforeNode,
																	const unsigned char* afterNode) {
	const V2Gaddag& gaddag = *(QUACKLE_LEXICON_PARAMETERS->v2Gaddag());
	uint32_t between = 0;
	const uint32_t restriction = gaddag.sharedChildren(beforeNode, afterNode);
	Letter minLetter = QUACKLE_GADDAG_SEPARATOR;
	int childIndex = 0;
	Letter foundLetter;
	const unsigned char* child = NULL;
	while (nextLetter(gaddag, beforeNode, restriction, minLetter,
										&childIndex, &foundLetter, &child)) {
		const unsigned char* node = gaddag.followIndex(child);
		if ((node != NULL) &&
				vertCompletesWord(gaddag, row + 1, col, node, numLettersAfter)) {
			between |= (1 << foundLetter);
		}
		minLetter = foundLetter + 1;
		++childIndex;
	}
	return between;
}

uint32_t V2Generator::horizBetween(int row, int col, int numLettersAfter,
																	const unsigned char* beforeNode,
																	const unsigned char* afterNode) {
	const V2Gaddag& gaddag = *(QUACKLE_LEXICON_PARAMETERS->v2Gaddag());
	uint32_t between = 0;
	uint32_t restriction = gaddag.sharedChildren(beforeNode, afterNode);
	Letter minLetter = QUACKLE_GADDAG_SEPARATOR;
	int childIndex = 0;
	Letter foundLetter;
	const unsigned char* child = NULL;
	while (nextLetter(gaddag, beforeNode, restriction, minLetter,
										&childIndex, &foundLetter, &child)) {
		const unsigned char* node = gaddag.followIndex(child);
		if ((node != NULL) &&
				horizCompletesWord(gaddag, row, col + 1, node, numLettersAfter)) {
			between |= (1 << foundLetter);
		}
		minLetter = foundLetter + 1;
		++childIndex;
	}
	return between;
}

void V2Generator::updateVerticalHooks(int row, int col) {
	//UVcout << "row: " << row << ", col: " << col << endl;
	assert(isEmpty(row, col));
	int numLettersBefore = 0;
	Hook& hook = m_vertHooks[row][col];
	hook.score = 0;
	for (int beforeRow = row - 1; beforeRow >= 0; beforeRow--) {
		if (isEmpty(beforeRow, col)) break;
		hook.score += tileScore(beforeRow, col);
		++numLettersBefore;
	}
	int numLettersAfter = 0;
	for (int afterRow = row + 1; afterRow < 15; afterRow++) {
		if (isEmpty(afterRow, col)) break;
		hook.score += tileScore(afterRow, col);
		++numLettersAfter;
	}
	//UVcout << "numLettersBefore: " << numLettersBefore << endl;
	//UVcout << "numLettersAfter: " << numLettersAfter << endl;
	if (numLettersBefore + numLettersAfter == 0) {
		hook.touches = false;
		return;
	}
	hook.score *= QUACKLE_BOARD_PARAMETERS->wordMultiplier(row, col);
	hook.touches = true;
	hook.letters = 0;
	const unsigned char* beforeNode = NULL;
	if (numLettersBefore > 0) {
		beforeNode = vertBeforeNode(row, col, numLettersBefore);
		if (beforeNode == NULL) return;
	}
	const unsigned char* afterNode = NULL;
	if (numLettersAfter > 0) {
		afterNode = vertAfterNode(row, col, numLettersAfter);
		if (afterNode == NULL) return;
	}
	if (numLettersAfter == 0) {
		//UVcout << "wordCompleters: " << debugLetterMask(wordCompleters(beforeNode))
		//			 << endl;
		hook.letters = wordCompleters(beforeNode);
		return;
	}
	if (numLettersBefore == 0) {
		/*
		UVcout << "wordCompleters: " << debugLetterMask(wordCompleters(afterNode))
					 << endl;
		*/
		hook.letters = wordCompleters(afterNode);
		return;
	}
	if (beforeNode == NULL || afterNode == NULL) return;
	/*
	UVcout << "between: "
				 << debugLetterMask(vertBetween(row, col, numLettersAfter, beforeNode, afterNode))
				 << endl;
	*/
	hook.letters = vertBetween(row, col, numLettersAfter, beforeNode, afterNode);
}

int V2Generator::tileScore(int row, int col) {
	const Letter letter = boardLetter(row, col);
	if (QUACKLE_ALPHABET_PARAMETERS->isPlainLetter(letter)) {
		return QUACKLE_ALPHABET_PARAMETERS->score(letter);
	} else {
		return 0;
	}
}

void V2Generator::updateHorizontalHooks(int row, int col) {
	//UVcout << "row: " << row << ", col: " << col << endl;
	assert(isEmpty(row, col));
	Hook& hook = m_horizHooks[row][col];
	hook.score = 0;
	int numLettersBefore = 0;
	for (int beforeCol = col - 1; beforeCol >= 0; beforeCol--) {
		if (isEmpty(row, beforeCol)) break;
    hook.score += tileScore(row, beforeCol);
		++numLettersBefore;
	}
	int numLettersAfter = 0;
	for (int afterCol = col + 1; afterCol < 15; afterCol++) {
		if (isEmpty(row, afterCol)) break;
		hook.score += tileScore(row, afterCol);
		++numLettersAfter;
	}
	//UVcout << "numLettersBefore: " << numLettersBefore << endl;
	//UVcout << "numLettersAfter: " << numLettersAfter << endl;
	if (numLettersBefore + numLettersAfter == 0) {
		hook.touches = false;
		return;
	}
	hook.score *= QUACKLE_BOARD_PARAMETERS->wordMultiplier(row, col);
	hook.touches = true;
	hook.letters = 0;
	const unsigned char* beforeNode = NULL;
	if (numLettersBefore > 0) {
		beforeNode = horizBeforeNode(row, col, numLettersBefore);
		if (beforeNode == NULL) return;
	}
	const unsigned char* afterNode = NULL;
	if (numLettersAfter > 0) {
		afterNode = horizAfterNode(row, col, numLettersAfter);
		if (afterNode == NULL) return;
	}
	if (numLettersAfter == 0) {
		//UVcout << "wordCompleters: " << debugLetterMask(wordCompleters(beforeNode))
		//			 << endl;
		hook.letters = wordCompleters(beforeNode);
		return;
	}
	if (numLettersBefore == 0) {
		//UVcout << "wordCompleters: " << debugLetterMask(wordCompleters(afterNode))
		//			 << endl;
		hook.letters = wordCompleters(afterNode);
		return;
	}
	if (beforeNode == NULL || afterNode == NULL) return;
	/*
	UVcout << "between: "
				 << debugLetterMask(horizBetween(row, col, numLettersAfter, beforeNode, afterNode))
				 << endl;
	*/
	hook.letters = horizBetween(row, col, numLettersAfter, beforeNode, afterNode);
}

void V2Generator::debugVertHook(int row, int col) {
	const Hook& hook = m_vertHooks[row][col];
	UVcout << "  vert hooks at row: " << row << ", col: " << col << endl;
	if (hook.touches) {
		UVcout << "  touches something... ";
	} else {
		UVcout << "  touches nothing." << endl;
		return;
	}
	UVcout << "hook letters: " << debugLetterMask(hook.letters);
	if (hook.letters != 0) {
		UVcout << ", score: " << hook.score;
	}
	UVcout << endl;
}

void V2Generator::debugHorizHook(int row, int col) {
	const Hook& hook = m_horizHooks[row][col];
	UVcout << "  horiz hooks at row: " << row << ", col: " << col << endl;
	if (hook.touches) {
		UVcout << "  touches something... "; 
	} else {
		UVcout << "  touches nothing." << endl;
		return;
	}
	UVcout << "hook letters: " << debugLetterMask(hook.letters);
	if (hook.letters != 0) {
		UVcout << ", score: " << hook.score;
	}
	UVcout << endl;
}

void V2Generator::debugHooks() {
	for (int row = 0; row < 15; ++row) {
		for (int col = 0; col < 15; ++col) {
			const Hook& hook = m_vertHooks[row][col];
			if (hook.touches) {
				debugVertHook(row, col);
			}
		}
	}
	for (int row = 0; row < 15; ++row) {
		for (int col = 0; col < 15; ++col) {
			const Hook& hook = m_horizHooks[row][col];
			if (hook.touches) {
				debugHorizHook(row, col);
			}
		}
	}
}

bool V2Generator::blankOnRack() const {
	return m_counts[QUACKLE_BLANK_MARK] > 0;
}

uint32_t V2Generator::otherRackBits(uint32_t rackBits, uint32_t rackHooks) const {
	if (__builtin_popcount(rackHooks) == 1) {
		Letter onlyHook = __builtin_ffs(rackHooks) - 1;
		/*		
		UVcout << "rackBits: " << debugLetterMask(rackBits)
					 << ", counts: " << counts2string()
					 << ", rackHooks: " << debugLetterMask(rackHooks)
					 << ", only hook here is "
					 << QUACKLE_ALPHABET_PARAMETERS->userVisible(onlyHook) << endl;
		*/
		if (m_counts[onlyHook] > 1) return rackBits;
		if (m_counts[onlyHook] == 1) {
			if (blankOnRack()) {
				return rackBits;
			} else {
				uint32_t hookLetterMask = 1 << onlyHook;
				return rackBits & ~hookLetterMask;
			}
		} else {
			assert(blankOnRack());
			if (m_counts[QUACKLE_BLANK_MARK] > 1) {
				return rackBits;
			} else {
				return m_rackBits;
			}
		}
	}
	return rackBits;
}

bool V2Generator::restrictSpotUsingHooks(Spot* spot, uint32_t rackBits,
																				 uint32_t rackHooks) const {
	// Assumes no hooks behind. This may change if I try to optimize adjacent hook
	// anchors such that the most restrictive of them (rather than always the
	// first) is given room behind.

	// If a single tile (natural or blank) is required to hook at the anchor
	// square, remove it from the bit mask when checking other hooks.
	rackBits = otherRackBits(rackBits, rackHooks);
	
	int row = spot->anchorRow;
	int col = spot->anchorCol;
	for (int ahead = 1; ahead <= spot->maxTilesAhead; ahead++) {
		if (spot->horizontal) col++; else row++;
		const Hook& hook =
			spot->horizontal ? m_vertHooks[row][col] : m_horizHooks[row][col];
		if (hook.touches && ((hook.letters & rackBits) == 0)) {
			spot->maxTilesAhead = ahead;
			return true;
		}
	}
	return false;
}
																				 
void V2Generator::findHookSpotsInRow(int row, vector<Spot>* spots) {
	const int numTiles = rack().size();
	int startCol = 0;
	for (int col = 0; col < 15; col++) {
		if (isEmpty(row, col)) {
			const Hook& hook = m_vertHooks[row][col];
			if (hook.touches) {
				uint32_t rackBitsOrBlank = blankOnRack() ? m_everyLetter : m_rackBits;
				uint32_t rackHooks = hook.letters & rackBitsOrBlank;
				if (rackHooks != 0) {
					Spot spot;
					spot.anchorRow = row;
					spot.anchorCol = col;
					spot.canUseBlank = true;
					spot.horizontal = true;
					spot.throughScore = 0;
					spot.numThrough = 0;
					spot.maxTilesBehind = std::min(numTiles - 1, col - startCol);
					spot.minTilesAhead = 1;
					spot.maxTilesAhead = std::min(numTiles, 15 - col);
					for (int reachCol = col + 1;
							 reachCol <= std::min(14, col + spot.maxTilesAhead); ++reachCol) {
						if (!isEmpty(row, reachCol)) {
							// 0 23456 8 
							// Q TWIXT Z
							//   ^     ^
							//   col   reachCol
							spot.maxTilesAhead = reachCol - col - 1;
							break;
						}
					}
					if ((spot.maxTilesAhead >= 1) &&
							(spot.maxTilesBehind + spot.maxTilesAhead >= 2)) {
						spot.longestViable = numTiles;
						UVcout << "Spot: (" << spot.anchorRow << ", " << spot.anchorCol << "), "
									 << "maxBehind: " << spot.maxTilesBehind
									 << ", maxAhead: " << spot.maxTilesAhead << endl;
						if (restrictSpotUsingHooks(&spot, rackBitsOrBlank, rackHooks)) {
							UVcout << "restricted Spot: (" << spot.anchorRow << ", "
										 << spot.anchorCol << "), "
										 << "maxBehind: " << spot.maxTilesBehind
										 << ", maxAhead: " << spot.maxTilesAhead << endl;
						}
						scoreSpot(&spot);
						if (spot.canMakeAnyWord) {
							/*
								UVcout << "Spot: (" << spot.anchorRow << ", " << spot.anchorCol
								<< ", blank: " << spot.canUseBlank << ") "
								<< spot.maxEquity << endl;
							*/
							if (blankOnRack()) {
								Spot blankSavingSpot = spot;
								blankSavingSpot.canUseBlank = false;
								scoreSpot(&blankSavingSpot);
								if (blankSavingSpot.canMakeAnyWord) {
									spots->push_back(blankSavingSpot);
									spot.maxEquity -= m_blankSpendingEpsilon;
								}
							}
							spots->push_back(spot);
						}
					}
				}
				startCol = col + 1;
			}
		} else {
			col += 2;
			startCol = col;
		}
	}
}

void V2Generator::findHookSpotsInCol(int col, vector<Spot>* spots) {
	const int numTiles = rack().size();
	int startRow = 0;
	for (int row = 0; row < 15; row++) {
		if (isEmpty(row, col)) {
			const Hook& hook = m_horizHooks[row][col];
			if (hook.touches) {
				uint32_t rackBitsOrBlank = blankOnRack() ? m_everyLetter : m_rackBits;
				uint32_t rackHooks = hook.letters & rackBitsOrBlank;
				if (rackHooks != 0) {
					Spot spot;
					spot.anchorRow = row;
					spot.anchorCol = col;
					spot.canUseBlank = true;
					spot.horizontal = false;
					spot.throughScore = 0;
					spot.numThrough = 0;
					spot.maxTilesBehind = std::min(numTiles - 1, row - startRow);
					spot.minTilesAhead = 1;
					spot.maxTilesAhead = std::min(numTiles, 15 - row);
					for (int reachRow = row + 1;
							 reachRow <= std::min(14, row + spot.maxTilesAhead); ++reachRow) {
						if (!isEmpty(reachRow, col)) {
							// 0 23456 8 
							// Q TWIXT Z
							//   ^     ^
							//   row   reachRow
							spot.maxTilesAhead = reachRow - row - 1;
							break;
						}
					}
					if ((spot.maxTilesAhead >= 1) &&
							(spot.maxTilesBehind + spot.maxTilesAhead >= 2)) {
						spot.longestViable = numTiles;
						UVcout << "Spot: (" << spot.anchorRow << ", "
									 << spot.anchorCol << "), "
									 << "maxBehind: " << spot.maxTilesBehind
									 << ", maxAhead: " << spot.maxTilesAhead << endl;
						if (restrictSpotUsingHooks(&spot, rackBitsOrBlank, rackHooks)) {
							UVcout << "restricted Spot: (" << spot.anchorRow << ", "
										 << spot.anchorCol << "), "
										 << "maxBehind: " << spot.maxTilesBehind
										 << ", maxAhead: " << spot.maxTilesAhead << endl;
						}
						scoreSpot(&spot);
						if (spot.canMakeAnyWord) {
							/*
							UVcout << "Spot: (" << spot.anchorRow << ", " << spot.anchorCol
										 << ", blank: " << spot.canUseBlank << ") "
										 << spot.maxEquity << endl;
							*/
							if (blankOnRack()) {
								Spot blankSavingSpot = spot;
								blankSavingSpot.canUseBlank = false;
								scoreSpot(&blankSavingSpot);
								if (blankSavingSpot.canMakeAnyWord) {
									spots->push_back(blankSavingSpot);
									spot.maxEquity -= m_blankSpendingEpsilon;
								}
							}
							spots->push_back(spot);
						}
					}
				}
				startRow = row + 1;
			}
		} else {
			row += 2;
			startRow = row;
		}
	}
}

void V2Generator::findThroughSpotsInRow(int row, vector<Spot>* spots) {
	//const int numTiles = rack().size();
	for (int col = 0; col < 15; col++) {
	}
}

void V2Generator::findSpots(vector<Spot>* spots) {
	UVcout << "findSpots()..." << endl;
	for (int row = 0; row < 15; ++row) {
		findHookSpotsInRow(row, spots);
		findThroughSpotsInRow(row, spots);
	}
	for (int col = 0; col < 15; ++col) {
		findHookSpotsInCol(col, spots);
	}
}

void V2Generator::findEmptyBoardSpots(vector<Spot>* spots) {
	const int numTiles = rack().size();

	Spot spot;
	spot.anchorRow = QUACKLE_BOARD_PARAMETERS->startRow();
	spot.anchorCol = QUACKLE_BOARD_PARAMETERS->startColumn();
	spot.canUseBlank = true;
	spot.horizontal = false;
	spot.throughScore = 0;
	spot.numThrough = 0;
	spot.maxTilesBehind = numTiles - 1;
	spot.minTilesAhead = 1;
	spot.maxTilesAhead = numTiles;
	spot.longestViable = 7;
	//struct timeval start, end;
	//gettimeofday(&start, NULL);
	scoreSpot(&spot);
	// gettimeofday(&end, NULL);
	// UVcout << "Time scoring spot was "
	// 			 << ((end.tv_sec * 1000000 + end.tv_usec)
	// 					 - (start.tv_sec * 1000000 + start.tv_usec)) << " microseconds." << endl;
	if (spot.canMakeAnyWord) {
		if (blankOnRack()) {
			Spot blankSavingSpot = spot;
			blankSavingSpot.canUseBlank = false;
			scoreSpot(&blankSavingSpot);
			if (blankSavingSpot.canMakeAnyWord) {
				spots->push_back(blankSavingSpot);
				spot.maxEquity -= m_blankSpendingEpsilon;
			}
		} 
		spots->push_back(spot);
	}
}

void V2Generator::computeHooks() {
	for (int row = 0; row < 15; row++) {
		for (int col = 0; col < 15; col++) {
			if (isEmpty(row, col)) {
				updateVerticalHooks(row, col);
				updateHorizontalHooks(row, col);
			}
		}
	}
}

void V2Generator::setUpCounts(const LetterString &letters) {
  String::counts(letters, m_counts);
}

UVString V2Generator::counts2string() const {
	UVString ret;

	for (Letter i = 0; i <= QUACKLE_ALPHABET_PARAMETERS->lastLetter(); i++)
		for (int j = 0; j < m_counts[i]; j++)
			ret += QUACKLE_ALPHABET_PARAMETERS->userVisible(i);

	return ret;
}
