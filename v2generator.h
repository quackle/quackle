#ifndef QUACKLE_V2_GENERATOR_H
#define QUACKLE_V2_GENERATOR_H

#include <vector>

#include "alphabetparameters.h"
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
      LetterString playedThrough;
      int maxMainScore;
      int maxTilesBehind;
      int minTilesAhead;
      int maxTilesAhead;
      float maxLeave;
      float maxEquity;
    };
    
    Move findStaticBest();
    void scoreSpot(Spot* spot);
    void findEmptyBoardSpots(vector<Spot>* spots);
    Move v2generate();
    void setUpCounts(const LetterString &letters);

    char m_counts[QUACKLE_FIRST_LETTER + QUACKLE_MAXIMUM_ALPHABET_SIZE];

    MoveList m_moveList;

    // debug stuff
    UVString counts2string();
    UVString cross2string(const LetterBitset &cross);

    GamePosition m_position;
    Board* board() { return &m_position.underlyingBoardReference(); }
    const Rack& rack() const {
      return m_position.currentPlayer().rack();
    } 
  };
  
}  // namespace Quackle

#endif

    
