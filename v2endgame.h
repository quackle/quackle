#ifndef QUACKLE_V2_ENDGAME_H
#define QUACKLE_V2_ENDGAME_H

#include "game.h"

namespace Quackle {

  class V2Endgame {
  public:
    V2Endgame();
    ~V2Endgame();
    
    V2Endgame(const GamePosition& position,
	      const map<Product, vector<LetterString>>& bingos);

    Move solve();

  private:
    GamePosition m_position;
    const map<Product, vector<LetterString>>* m_bingos;
  };

}

#endif

    
