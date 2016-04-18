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

void V2Generator::kibitz() {
  findStaticBest();
}

Move V2Generator::findStaticBest() {
  UVcout << "findStaticBest(): " << endl << m_position << endl;
	m_best = Move::createPassMove();

  m_moveList.clear();

  setUpCounts(rack().tiles());
	struct timeval start, end;
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
		gettimeofday(&start, NULL);
		std::sort(spots.begin(), spots.end());
		gettimeofday(&end, NULL);
		UVcout << "Time sorting spots was "
					 << ((end.tv_sec * 1000000 + end.tv_usec)
						 - (start.tv_sec * 1000000 + start.tv_usec)) << " microseconds." << endl;

		gettimeofday(&start, NULL);
		for (const Spot& spot : spots) {
			//#ifdef DEBUG_V2GEN
			UVcout << "Spot: (" << spot.anchorRow << ", " << spot.anchorCol
						 << ", " << "blank: " << spot.canUseBlank << "): "
						 << spot.maxEquity << endl;
			//#endif
			if (spot.maxEquity < m_best.equity) {
#ifdef DEBUG_V2GEN
				UVcout << "no need to check this spot!" << endl;
#endif
				continue;
			} 
			findMovesAt(spot);
		}
		gettimeofday(&end, NULL);
		UVcout << "Time finding moves was "
					 << ((end.tv_sec * 1000000 + end.tv_usec)
							 - (start.tv_sec * 1000000 + start.tv_usec))
					 << " microseconds." << endl;

	}
	UVcout << "best Move: " << m_best << endl;	
	return m_best;
}

bool V2Generator::couldMakeWord(const Spot& spot, int length) const {
	//assert(length > 0);
	//assert(length <= 7);
	//assert(spot.playedThrough.length() == 0);
	if (m_anagrams == NULL) return true;
	const int lengthMask = 1 << length;
	const UsesTiles& usesTiles =
		spot.canUseBlank ? m_anagrams->usesWhatever : m_anagrams->usesNoBlanks;
	const NTileAnagrams& anagrams = usesTiles.thruNone;
	return (anagrams.numPlayed & lengthMask) != 0;
}

void V2Generator::findMovesAt(const Spot& spot) {
  uint32_t rackBits = 0;
	for (int letter = QUACKLE_ALPHABET_PARAMETERS->firstLetter();
			 letter <= QUACKLE_ALPHABET_PARAMETERS->lastLetter(); ++letter) {
		if (m_counts[letter] > 0) rackBits |= (1 << letter);
	}
	if (!spot.canUseBlank || m_counts[QUACKLE_BLANK_MARK] == 0) {
		findBlankless(spot, 0, 0, 0, -1, rackBits,
									QUACKLE_LEXICON_PARAMETERS->v2Gaddag()->root());
	} else {
		//		findBlankable(spot, spot.anchorRow, spot.anchorCol, 0, 0, /*-1,*/ rackBits,
		//	      NULL);
									// QUACKLE_LEXICON_PARAMETERS->v2Gaddag()->root());
	}
}

namespace {
	UVString debugLetterMask(uint32_t letters) {
		UVString ret;
		for (Letter i = 0; i <= QUACKLE_ALPHABET_PARAMETERS->lastLetter(); ++i) {
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

void V2Generator::findBlankless(const Spot& spot, int delta,
																int ahead, int behind, int velocity,
																uint32_t rackBits, const unsigned char* node) {
	const V2Gaddag& gaddag = *(QUACKLE_LEXICON_PARAMETERS->v2Gaddag());

#ifdef DEBUG_V2GEN
	UVcout << "findBlankless(delta: " << delta
				 << ", ahead: " << ahead << ", behind: " << behind
				 << ", velocity: " << velocity << ")" << endl;
#endif
	// FIXME: this depends on type of anchor, hook vs through.
	const int numPlaced = 1 + ahead + behind;
	int col = spot.anchorCol;
	int row = spot.anchorRow;
	int pos;
	if (spot.horizontal) {
		col += delta;
		pos = col;
	} else {
		row += delta;
		pos = row;
	}
	//assert(col >= 0);
	//assert(col < 15);
	//assert(row >= 0);
	//assert(row < 15);

	Letter minLetter = QUACKLE_GADDAG_SEPARATOR;
	int childIndex = 0;
	for (;;) {
#ifdef DEBUG_V2GEN
		UVcout << "counts: " << counts2string() << endl;
#endif
		Letter foundLetter;
		const unsigned char* child = gaddag.nextRackChild(node,
																											minLetter,
																											rackBits,
																											&childIndex,
																											&foundLetter);
		//UVcout << "child: " << reinterpret_cast<uint64_t>(child) << endl;
		if (child == NULL) return;
		//assert(foundLetter >= QUACKLE_ALPHABET_PARAMETERS->firstLetter());
		//assert(foundLetter <= QUACKLE_ALPHABET_PARAMETERS->lastLetter());
		m_placed[pos] = foundLetter;

#ifdef DEBUG_V2GEN		
		UVcout << "placing " << QUACKLE_ALPHABET_PARAMETERS->userVisible(foundLetter)
					 << " at m_placed[" << pos << "]" << endl;
		int startCol = spot.anchorCol - behind;
		LetterString word;
		for (int i = startCol; i < spot.anchorCol + ahead + 1; ++i) {
			assert(i >= 0);
			assert(i < 15);
			//UVcout << "letter: " << static_cast<int>(m_placed[i]) << endl;
			word += m_placed[i];
		}
    UVcout << "m_placed[" << startCol << " to " << spot.anchorCol + ahead
					 << "]: " << QUACKLE_ALPHABET_PARAMETERS->userVisible(word) << endl;
#endif
		
		//assert(m_counts[foundLetter] > 0);
		m_counts[foundLetter]--;
		//assert(m_counts[foundLetter] >= 0);

		const uint32_t foundLetterMask = 1 << foundLetter;
		if (m_counts[foundLetter] == 0) {
			rackBits &= ~foundLetterMask;
		}		
		// FIXME: all this assumes horizontal
		// Test it by making the empty-board spot vertie!
		if (gaddag.completesWord(child)) {
			int score = scorePlay(spot, behind, ahead);
			LetterString word;
			int startCol = spot.anchorCol - behind;
			for (int i = startCol; i < spot.anchorCol + ahead + 1; ++i) {
				//assert(i >= 0);
				//assert(i < 15);
				//UVcout << "letter: " << static_cast<int>(m_placed[i]) << endl;
				word += m_placed[i];
			}
			Move move = Move::createPlaceMove(7, startCol, spot.horizontal, word);
			move.score = score;
			move.equity = score; // FIXME
			if (move.equity > m_best.equity) {
				m_best = move;
			}
		}
		const unsigned char* newNode = gaddag.followIndex(child);
		//UVcout << "newNode: " << reinterpret_cast<uint64_t>(newNode) << endl;
		if (newNode != NULL) {
			// FIXME: all this assumes horizontal
			if (velocity < 0) {
				findBlankless(spot, delta - 1, 0, behind + 1, -1, rackBits,
											newNode);
#ifdef V2GEN
				UVcout << "changing direction..." << endl;
#endif
				const unsigned char* changeChild = gaddag.changeDirection(newNode);
				//UVcout << "changeChild: " << reinterpret_cast<uint64_t>(changeChild) << endl;
				if (changeChild != NULL) {
					newNode = gaddag.followIndex(changeChild);
					// UVcout << "changeChild's newNode: "
					// 			 << reinterpret_cast<uint64_t>(newNode) << endl;
					if (newNode != NULL) { 
						findBlankless(spot, 1, 1, behind, 1, rackBits, newNode);
					}
				}
			} else {
				findBlankless(spot, delta + 1, ahead + 1, behind, 1, rackBits, newNode);
			}
		}
		
		m_counts[foundLetter]++;
		rackBits |= foundLetterMask;
		minLetter = foundLetter + 1;
		++childIndex;
	}
}

// "behind" should come before "ahead" in the argument list
void V2Generator::findBlanklessOld(const Spot& spot, int row, int col,
																int ahead, int behind, int velocity,
																uint32_t rackBits, const unsigned char* node) {
	const V2Gaddag& gaddag = *(QUACKLE_LEXICON_PARAMETERS->v2Gaddag());

	UVcout << "findBlankless(row: " << row << ", col: " << col
				 << ", ahead: " << ahead << ", behind: " << behind
				 << ", velocity: " << velocity << "...)" << endl;
#ifdef DEBUG_V2GEN	
	// This: only useful for debugging.
	UVcout << "rackBits: " << debugLetterMask(rackBits) << endl;
	const uint32_t rackChildren = gaddag.intersection(node, rackBits);
	UVcout << "rackChildren: " << debugLetterMask(rackChildren) << endl;
#endif

	// Now do it with nextRackChild(...)...
	Letter minLetter = 0;
	int childIndex = 0;
	for (;;) {
		Quackle::Letter foundLetter;
		const unsigned char* child = gaddag.nextRackChild(node,
																											minLetter,
																											rackBits,
																											&childIndex,
																											&foundLetter);
		if (child == NULL) {
#ifdef DEBUG_V2GEN						
			UVcout << "child is NULL" << endl;
#endif			
			return;
		}
#ifdef DEBUG_V2GEN						
		UVcout << "foundLetter: " << static_cast<int>(foundLetter) << endl;
		UVcout << "childIndex: " << childIndex << endl;
#endif
		assert(col >= 0);
		assert(col < 15);
		assert(row >= 0);
		assert(row < 15);
		assert(foundLetter >= QUACKLE_ALPHABET_PARAMETERS->firstLetter());
		assert(foundLetter <= QUACKLE_ALPHABET_PARAMETERS->lastLetter());
		m_placed[spot.horizontal ? col : row] = foundLetter;

		#ifdef DEBUG_V2GEN						
		UVcout << "placing letter "
					 << QUACKLE_ALPHABET_PARAMETERS->userVisible(foundLetter)
		       << " at position " << (spot.horizontal ? col : row) << endl;
		#endif
		assert(m_counts[foundLetter] > 0);
		m_counts[foundLetter]--;
		assert(m_counts[foundLetter] >= 0);
		
		uint32_t foundLetterMask = 1 << foundLetter;
		if (m_counts[foundLetter] == 0) {
			rackBits &= ~foundLetterMask;
		}
		if (gaddag.completesWord(child) &&
				(ahead + 1 >= spot.minTilesAhead)) {
			// process move;
			int score = scorePlay(spot, behind, ahead + 1);
			double leave = getLeave();
			double equity = score + leave;
			if (equity >= m_best.equity) {
				int startCol = spot.anchorCol - behind;
				assert(startCol >= 0);
				LetterString word;
#ifdef DEBUG_V2GEN						
				UVcout << "Found move: 8" << (char)('A' + startCol) << " ";
#endif
				for (int i = startCol; i < spot.anchorCol + ahead + 1; ++i) {
					assert(i >= 0);
					assert(i < 15);
					//UVcout << "letter: " << static_cast<int>(m_placed[i]) << endl;
					word += m_placed[i];
#ifdef DEBUG_V2GEN						
					UVcout << QUACKLE_ALPHABET_PARAMETERS->userVisible(m_placed[i]);
#endif						
				}
				//#ifdef DEBUG_V2GEN						
				UVcout << " score: " << score << " equity: " << equity << endl;
				//#endif
				Move move = Move::createPlaceMove(7, startCol, spot.horizontal, word);
				move.score = score;
        move.equity = equity;
				//#ifdef DEBUG_V2GEN						
				UVcout << "New best Move: " << move << endl;
				//#endif
				m_best = move;
			}
		}
		assert(col >= 0);
		assert(col < 15);
		const unsigned char* newNode = gaddag.followIndex(child);
		if (newNode != NULL) {
			int newRow = row;
			int newCol = col;
			assert(newCol >= 0);
			assert(newCol < 15);
			if (velocity < 0) {
				if (behind < spot.maxTilesBehind) {
					if (spot.horizontal) {
						assert(newCol > 0);
						assert(newCol < 15);
						--newCol;
#ifdef DEBUG_V2GEN											
						UVcout << "still <-- newCol: " << newCol << endl;
#endif
					} else {
						--newRow;
					}
					assert(newCol >= 0);
					assert(newCol < 15);
					/*
					findBlankless(spot, newRow, newCol, ahead, behind + 1, velocity,
												rackBits, newNode);
					*/
				} else {
#ifdef DEBUG_V2GEN											
					UVcout << "Out of room behind..." << endl;
#endif
					const unsigned char* changeChild = gaddag.changeDirection(newNode);
          if (changeChild != NULL) {
						newNode = gaddag.followIndex(changeChild);
						if (newNode != NULL) {
#ifdef DEBUG_V2GEN											
							UVcout << "but can change direction!" << endl;
#endif
							// Also need to consume "playedThrough" letters here...
							if (spot.horizontal) {
								++newCol;
#ifdef DEBUG_V2GEN											
								UVcout << "changing --> newCol: " << newCol << endl;
#endif
							} else {
								++newRow;
							}
							/*
							findBlankless(spot, newRow, newCol, ahead + 1, behind, -velocity,
														rackBits, newNode);
							*/
						}
					}
				}
			} else {
				if (ahead < spot.maxTilesAhead) {
					if (spot.horizontal) {
						++newCol;
#ifdef DEBUG_V2GEN											
								UVcout << "still --> newCol: " << newCol << endl;
#endif
					} else {
						++newRow;
					}
					/*
					findBlankless(spot, newRow, newCol, ahead + 1, behind, velocity,
												rackBits, newNode);
					*/
				} else {
#ifdef DEBUG_V2GEN																
					UVcout << "Out of room ahead..." << endl;
#endif
				}
			}
		}
		// Maybe not needed eventually?
		m_placed[spot.horizontal ? col : row] = QUACKLE_NULL_MARK;
		
		m_counts[foundLetter]++;
		rackBits |= foundLetterMask;
		minLetter = foundLetter + 1;
		++childIndex;
	}
}

void V2Generator::findBlankable(const Spot& spot, int row, int col,
																int ahead, int behind, uint32_t rackBits,
																const unsigned char* node) {
}

float V2Generator::bestLeave(const Spot& spot, int length) const {
	assert(length > 0);
	assert(length < 7);
	assert(spot.playedThrough.length() == 0);
	if (m_anagrams != NULL) {
		const UsesTiles& usesTiles =
			spot.canUseBlank ? m_anagrams->usesWhatever : m_anagrams->usesNoBlanks;
		const NTileAnagrams& anagrams = usesTiles.thruNone;
		int index = length - 1;
		return anagrams.bestLeaves[index] / 256.0f;
	}
	return 9999;
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
	for (const Letter letter : spot->playedThrough) {
		if (letter == QUACKLE_NULL_MARK) continue;
		if (QUACKLE_ALPHABET_PARAMETERS->isBlankLetter(letter)) {
			throughScore += QUACKLE_ALPHABET_PARAMETERS->score(QUACKLE_BLANK_MARK);
		} else {
			throughScore += QUACKLE_ALPHABET_PARAMETERS->score(letter);
		}
	}
	float maxEquity = -9999;
	int wordMultipliers[QUACKLE_MAXIMUM_BOARD_SIZE];
	int letterMultipliers[QUACKLE_MAXIMUM_BOARD_SIZE];
	int row = spot->anchorRow;
	int col = spot->anchorCol;
	int anchorPos = (spot->horizontal) ? col : row;
	for (int i = 0; i <= spot->maxTilesAhead; ++i) {
		if (board()->isNonempty(row, col)) {
			i--;
		} else {
			wordMultipliers[anchorPos + i] =
				QUACKLE_BOARD_PARAMETERS->wordMultiplier(row, col);
			letterMultipliers[anchorPos + i] =
				QUACKLE_BOARD_PARAMETERS->letterMultiplier(row, col);
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
			letterMultipliers[anchorPos - i] =
				QUACKLE_BOARD_PARAMETERS->letterMultiplier(row, col);
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
				//UVcout << "can make word of length " << played << endl;
			}
			spot->canMakeAnyWord = true;
			int wordMultiplier = 1;
			int usedLetterMultipliers[QUACKLE_MAXIMUM_BOARD_SIZE];
			int minPos = anchorPos - behind;
			for (int pos = minPos; pos < minPos + played; ++pos) {
				wordMultiplier *= wordMultipliers[pos];
			}
			memcpy(&usedLetterMultipliers, &(letterMultipliers[minPos]),
						 played * sizeof(int));
			std::sort(usedLetterMultipliers,
								usedLetterMultipliers + played,
								std::greater<int>());
			int playedScore = 0;
			for (int i = 0; i < played; ++i) {
				playedScore += usedLetterMultipliers[i] * tileScores[i];
			}
			int score = (throughScore + playedScore) * wordMultiplier;
			if (played == QUACKLE_PARAMETERS->rackSize()) {
				score += QUACKLE_PARAMETERS->bingoBonus();
			}
			float optimisticEquity = score;
			//if (played < 7) optimisticEquity += bestLeave(*spot, played);
			maxEquity = std::max(maxEquity, optimisticEquity);
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

void V2Generator::findEmptyBoardSpots(vector<Spot>* spots) {
	bool rackHasBlank = false;
	for (unsigned int i = 0; i < rack().size(); ++i) {
		if (rack().tiles()[i] == QUACKLE_BLANK_MARK) {
			rackHasBlank = true;
			break;
		}
	}
	const int numTiles = rack().size();

	const int anchorRow = QUACKLE_BOARD_PARAMETERS->startRow();
	Spot spot;
	spot.anchorRow = anchorRow;
	spot.anchorCol = QUACKLE_BOARD_PARAMETERS->startColumn();
	spot.canUseBlank = true;
	spot.horizontal = true;
	spot.throughScore = 0;
	spot.maxTilesBehind = numTiles - 1;
	spot.minTilesAhead = 1;
	spot.maxTilesAhead = numTiles;
	//struct timeval start, end;
	//gettimeofday(&start, NULL);
	scoreSpot(&spot);
	// gettimeofday(&end, NULL);
	// UVcout << "Time scoring spot was "
	// 			 << ((end.tv_sec * 1000000 + end.tv_usec)
	// 					 - (start.tv_sec * 1000000 + start.tv_usec)) << " microseconds." << endl;
	if (spot.canMakeAnyWord) spots->push_back(spot);
	if (rackHasBlank) {
		spot.canUseBlank = false;
		scoreSpot(&spot);
		if (spot.canMakeAnyWord) spots->push_back(spot);
	}
}

void V2Generator::setUpCounts(const LetterString &letters) {
  String::counts(letters, m_counts);
}

UVString V2Generator::counts2string() {
	UVString ret;

	for (Letter i = 0; i <= QUACKLE_ALPHABET_PARAMETERS->lastLetter(); i++)
		for (int j = 0; j < m_counts[i]; j++)
			ret += QUACKLE_ALPHABET_PARAMETERS->userVisible(i);

	return ret;
}

UVString V2Generator::cross2string(const LetterBitset &cross) {
	UVString ret;

	for (int i = 0; i < QUACKLE_ALPHABET_PARAMETERS->length(); i++)
		if (cross.test(i))
			ret += QUACKLE_ALPHABET_PARAMETERS->userVisible(QUACKLE_FIRST_LETTER + i);

	return ret;
}
