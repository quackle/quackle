#ifndef QUACKLE_V2_GENERATOR_H
#define QUACKLE_V2_GENERATOR_H

#include <vector>

#include "alphabetparameters.h"
#include "anagrammap.h"
#include "game.h"
#include "move.h"

using namespace std;

namespace Quackle {

  class V2Generator {

  public:
    V2Generator();
    V2Generator(const Quackle::GamePosition &position);
    ~V2Generator();

    void kibitz();

  private:
    struct Spot {
      int anchorRow;
      int anchorCol;
      bool horizontal;
      bool canUseBlank;
      bool canMakeAnyWord;
      LetterString playedThrough;
      int throughScore;
      float maxEquity;
      int maxTilesBehind;
      int minTilesAhead;
      int maxTilesAhead;

      bool operator<(const Spot& rhs) const {
	if (maxEquity == rhs.maxEquity) {
	  return canUseBlank < rhs.canUseBlank;
	}
	return maxEquity > rhs.maxEquity;
      }
    };
    
    Move findStaticBest();
    int scorePlay(const Spot& spot, int behind, int ahead);
    double getLeave() const;
    void scoreSpot(Spot* spot);
    void findEmptyBoardSpots(vector<Spot>* spots);
    void findMovesAt(const Spot& spot);
    void findBlankless(const Spot& spot, int delta,
		       int ahead, int behind, int velocity, uint32_t rackBits,
		       const unsigned char* node);
    void findBlanklessOld(const Spot& spot, int row, int col,
		       int ahead, int behind, int velocity, uint32_t rackBits,
		       const unsigned char* node);
    void findBlankable(const Spot& spot, int row, int col,
		       int ahead, int behind, uint32_t rackBits,
		       const unsigned char* node);    
    bool couldMakeWord(const Spot& spot, int length) const;
    float bestLeave(const Spot& spot, int length) const;
    Move v2generate();
    void setUpCounts(const LetterString &letters);

    char m_counts[QUACKLE_FIRST_LETTER + QUACKLE_MAXIMUM_ALPHABET_SIZE];
    Letter m_placed[QUACKLE_MAXIMUM_BOARD_SIZE];
    MoveList m_moveList;
    Move m_best;

    // debug stuff
    UVString counts2string();
    UVString cross2string(const LetterBitset &cross);

    GamePosition m_position;
    Board* board() { return &m_position.underlyingBoardReference(); }
    const Rack& rack() const {
      return m_position.currentPlayer().rack();
    }
    const RackAnagrams* m_anagrams;
  };
  
}  // namespace Quackle

#endif

    
