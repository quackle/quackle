#include "board.h"
#include "boardparameters.h"
#include "datamanager.h"
#include "evaluator.h"
#include "gameparameters.h"
#include "lexiconparameters.h"
#include "v2gaddag.h"
#include "v2generator.h"

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
	Move best = Move::createPassMove();

  m_moveList.clear();

  setUpCounts(rack().tiles());

	vector<Spot> spots;
	if (board()->isEmpty()) {
		findEmptyBoardSpots(&spots);
	}
  vector<int> playableRows;
	vector<int> playableCols;
	
	return best;
}

void V2Generator::scoreSpot(Spot* spot) {
	const int numTiles = rack().size();
	vector<int> tileScores;
	const LetterString& tiles = rack().tiles();
	for (int i = 0; i < numTiles; ++i) {
		tileScores.push_back(QUACKLE_ALPHABET_PARAMETERS->score(tiles[i]));
	}
	sort(tileScores.begin(), tileScores.end());
	reverse(tileScores.begin(), tileScores.end());
	int throughScore = 0;
	for (const Letter letter : spot->playedThrough) {
		if (letter == QUACKLE_NULL_MARK) continue;
		if (QUACKLE_ALPHABET_PARAMETERS->isBlankLetter(letter)) {
			throughScore += QUACKLE_ALPHABET_PARAMETERS->score(QUACKLE_BLANK_MARK);
		} else {
			throughScore += QUACKLE_ALPHABET_PARAMETERS->score(letter);
		}
	}
	int maxScore = 0;
  for (int ahead = spot->minTilesAhead; ahead <= spot->maxTilesAhead; ++ahead) {
		// num tiles played behind anchor + ahead of anchor <= num tiles on rack
		const int maxBehind = std::min(spot->maxTilesBehind, numTiles - ahead);
		for (int behind = 0; behind <= maxBehind; ++behind) {
			const int played = ahead + behind;
			if (played == 0) continue; // have to play at least one tile!
			int wordMultiplier = 1;
			vector<int> letterMultipliers;
			int row = spot->anchorRow;
			int col = spot->anchorCol;
			for (int i = 1; i <= ahead; ++i) {
				if (board()->isNonempty(row, col)) {
					i--;
				} else {
					wordMultiplier *= QUACKLE_BOARD_PARAMETERS->wordMultiplier(row, col);
					int letterMultiplier = QUACKLE_BOARD_PARAMETERS->letterMultiplier(row, col);
					letterMultipliers.push_back(letterMultiplier);
				}
				if (spot->horizontal) col++; else row++;
			}
			row = spot->anchorRow;
			col = spot->anchorCol;
			for (int i = 1; i <= behind; ++i) {
				if (spot->horizontal) col--; else row--;
				if (board()->isNonempty(row, col)) {
					i--;
				} else {
					wordMultiplier *= QUACKLE_BOARD_PARAMETERS->wordMultiplier(row, col);
					int letterMultiplier = QUACKLE_BOARD_PARAMETERS->letterMultiplier(row, col);
					letterMultipliers.push_back(letterMultiplier);
				}
			}
			sort(letterMultipliers.begin(), letterMultipliers.end());
			reverse(letterMultipliers.begin(), letterMultipliers.end());
			int playedScore = 0;
			for (unsigned int i = 0; i < letterMultipliers.size(); ++i) {
				playedScore += letterMultipliers[i] * tileScores[i];
			}
			int score = (throughScore + playedScore) * wordMultiplier;
			if (played == QUACKLE_PARAMETERS->rackSize()) {
				score += QUACKLE_PARAMETERS->bingoBonus();
			}
			maxScore = std::max(maxScore, score);
		}
	}
	spot->maxMainScore = maxScore;
	UVcout << "Spot: (" << spot->anchorRow << ", " << spot->anchorCol << "): "
				 << spot->maxMainScore << endl;
}

void V2Generator::findEmptyBoardSpots(vector<Spot>* spots) {
	const int numTiles = rack().size();
	const int anchorRow = QUACKLE_BOARD_PARAMETERS->startRow();
	const int firstCol =
		QUACKLE_BOARD_PARAMETERS->startColumn() - (numTiles - 1);
	const int lastCol = QUACKLE_BOARD_PARAMETERS->startColumn();
	for (int anchorCol = firstCol; anchorCol <= lastCol; ++anchorCol) {
		Spot spot;
		spot.anchorRow = anchorRow;
		spot.anchorCol = anchorCol;
		spot.horizontal = true;
		spot.maxTilesBehind = 0;
		spot.minTilesAhead = 2;
		spot.maxTilesAhead = numTiles;
		scoreSpot(&spot);
		spots->push_back(spot);
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
