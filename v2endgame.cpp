#include <map>

#include "primeset.h"
#include "v2endgame.h"
#include "v2generator.h"

using namespace Quackle;

V2Endgame::V2Endgame() {}

V2Endgame::~V2Endgame() {}

V2Endgame::V2Endgame(const GamePosition& position,
		     const map<Product, vector<LetterString>>& bingos) {
  m_position = position;
  m_bingos = &bingos;
}

Move V2Endgame::solve() {
  V2Generator gen(m_position, 0, *m_bingos);
  return gen.kibitz();
}
