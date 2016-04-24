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

    Move kibitz();
    const unsigned char* vertBeforeNode(int row, int col, int numLetters);
    const unsigned char* vertAfterNode(int row, int col, int numLetters);
    const unsigned char* horizBeforeNode(int row, int col, int numLetters);
    const unsigned char* horizAfterNode(int row, int col, int numLetters);
    uint32_t verticalHooks(int row, int col);
    uint32_t horizontalHooks(int row, int col);

  private:
    struct WorthChecking {
      bool couldBeBest;
      float maxEquity;
    };
    
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
      WorthChecking worthChecking[8];
      int longestViable;

      bool operator<(const Spot& rhs) const {
	if (maxEquity == rhs.maxEquity) {
	  return canUseBlank < rhs.canUseBlank;
	}
	return maxEquity > rhs.maxEquity;
      }

      bool viableAtLength(int length) const {
	return worthChecking[length].couldBeBest;
      }
    };
    
    Move findStaticBest();
    int scorePlay(const Spot& spot, int behind, int ahead);
    inline double getLeave() const;
    void scoreSpot(Spot* spot);
    void addThroughSpots(bool horiz, vector<Spot>* spots, int* row, int* col);
    void maybeAddHookSpot(int row, int col, bool horiz, vector<Spot>* spots);
    void findSpots(vector<Spot>* spots);
    void findEmptyBoardSpots(vector<Spot>* spots);
    void restrictByLength(Spot* spot);
    void findMovesAt(Spot* spot);
    void findBestExchange();
    void findExchanges(const uint64_t* rackPrimes, const LetterString& tiles,
		       uint64_t product, int pos, int numExchanged);
    Letter boardLetter(int row, int col);
    bool isEmpty(int row, int col);

    const unsigned char* followLetter(const V2Gaddag& gaddag, int row, int col,
				      const unsigned char* node);
    uint32_t wordCompleters(const unsigned char* node);
    uint32_t vertBetween(int row, int col, int numLettersAfter,
			 const unsigned char* beforeNode,
			 const unsigned char* afterNode);
    uint32_t horizBetween(int row, int col, int numLettersAfter,
			  const unsigned char* beforeNode,
			  const unsigned char* afterNode);
    bool vertCompletesWord(const V2Gaddag& gaddag, int row, int col,
			   const unsigned char* node,
			   int numLettersAfter);
    bool horizCompletesWord(const V2Gaddag& gaddag, int row, int col,
			    const unsigned char* node,
			    int numLettersAfter);
    // returns true if a letter at or after minLetter alphabetically exists
    // at this node, updates childIndex (because it may skip over children that
    // for which tiles aren't in the rack) foundLetter, child
    inline bool nextLetter(const V2Gaddag& gaddag, const unsigned char* node,
			   Letter minLetter, int* childIndex, Letter* foundLetter,
			   const unsigned char** child) const;

    // restriction is a letter index max, can be used to find hooks
    inline bool nextLetter(const V2Gaddag& gaddag, const unsigned char* node,
			   uint32_t restiction,
			   Letter minLetter, int* childIndex, Letter* foundLetter,
			   const unsigned char** child) const;
    
    inline bool nextBlankLetter(const V2Gaddag& gaddag, const unsigned char* node,
				Letter minLetter, int* childIndex, Letter* foundLetter,
				const unsigned char** child) const;

    inline int scoreLetter(int pos, Letter letter, int letterMultiplier);
    
    void debugPlaced(const Spot& spot, int behind, int ahead) const;
    inline void useLetter(Letter letter, uint32_t* foundLetterMask);
    inline void unuseLetter(Letter letter, uint32_t foundLetterMask);
    inline bool maybeRecordMove(const Spot& spot, int wordMultiplier,
				int behind, int ahead, int numPlaced);
    inline void getSquare(const Spot& spot, int delta,
			  int* row, int* col, int* pos) const;
    inline void findMoreBlankless(Spot* spot, int delta, int ahead,
				  int behind, int velocity, int wordMultiplier,
				  const V2Gaddag& gaddag, const unsigned char* node);
    inline void findMoreBlankable(Spot* spot, int delta, int ahead,
				  int behind, int velocity, int wordMultiplier,
				  const V2Gaddag& gaddag, const unsigned char* node);
    void findBlankless(Spot* spot, int delta, int ahead, int behind, int velocity,
		       int wordMultiplier, const unsigned char* node);
    void findBlankable(Spot* spot, int delta, int ahead, int behind, int velocity,
		       int wordMultiplier, const unsigned char* node);
    bool couldMakeWord(const Spot& spot, int length) const;
    float bestLeave(const Spot& spot, int length) const;
    Move v2generate();
    void setUpCounts(const LetterString &letters);

    int m_mainWordScore;
    char m_counts[QUACKLE_FIRST_LETTER + QUACKLE_MAXIMUM_ALPHABET_SIZE];
    uint32_t m_rackBits;
    Letter m_placed[QUACKLE_MAXIMUM_BOARD_SIZE];
    double m_bestLeaves[8];
    MoveList m_moveList;
    Move m_best;

    double m_blankSpendingEpsilon = 0.01;
    
    // debug stuff
    UVString counts2string() const;

    GamePosition m_position;
    Board* board() { return &m_position.underlyingBoardReference(); }
    const Rack& rack() const {
      return m_position.currentPlayer().rack();
    }
    const RackAnagrams* m_anagrams;
  };
  
}  // namespace Quackle

#endif

    
