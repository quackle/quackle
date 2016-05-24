#include <set>

#include <iostream>
#include <math.h>
#include <time.h>
#include <sys/time.h>

#include "datamanager.h"
#include "game.h"
#include "gameparameters.h"
#include "move.h"
#include "strategyparameters.h"
#include "v2generator.h"
#include "v2sim.h"

using namespace Quackle;

V2Simulator::V2Simulator() {
}

V2Simulator::V2Simulator(const Quackle::Game &game) {
  m_originalGame = game;
}

void V2Simulator::getCandidates(unsigned int numPlays) {
  V2Generator gen(m_originalGame.currentPosition());
  MoveList moves = gen.kibitzAll();
  MoveList::sort(moves, MoveList::Equity);
  //const Move& staticTop = moves[0];
  //UVcout << "staticTop: " << staticTop << endl;
  for (unsigned int i = 0; i < numPlays; ++i) {
    if (i >= moves.size()) return;
    m_simmedMoves.push_back(V2SimmedMove(moves[i]));
  }
}

void V2Simulator::sim(Game game, int iteration, V2SimmedMove* move) {
  UVcout << "game.currentPosition(): " << game.currentPosition() << endl;
  //UVcout << "exchangeDividends:";
  // const vector<int>& dividends = game.currentPosition().exchangeDividends();
  // for (const int dividend : dividends) UVcout << " " << dividend;
  // UVcout << endl;
  // UVcout << "exchangeDividendIndex: "
  // 	 << game.currentPosition().exchangeDividendIndex() << endl;
  assert(m_plies >= 2);
  float gameSpread = move->move.score;
  float residual = 0;
  for (int i = 1; i <= m_plies; ++i) {
    if (i == 1) {
      game.currentPosition()
	.setCurrentPlayerRack(m_oppRacks[iteration], false);
    }
    
    // const LongLetterString& order = game.currentPosition().drawingOrder();
    // UVcout << "drawingOrder (" << order.size() << "): "
    // 	   << QUACKLE_ALPHABET_PARAMETERS->userVisible(order) << endl;
    // const vector<int>& dividends =
    //   game.currentPosition().exchangeDividends();
    // UVcout << "exchangeDividends:";
    // for (const int dividend : dividends) UVcout << " " << dividend;
    // UVcout << endl;
    // UVcout << "exchangeDividendIndex: "
    // 	   << game.currentPosition().exchangeDividendIndex() << endl;
    
    V2Generator gen(game.currentPosition(), m_bingos);
    const Move lookAhead = gen.kibitz();
    UVcout << "lookAhead, ply " << i << ": " << lookAhead << endl;
    game.currentPosition().addAndSetMoveMade(lookAhead);
    game.commitMove(lookAhead);
    bool weOnTurn = (i % 2) == 0;
    bool finalPlyForPlayer = (i + 2) > m_plies;
    float turnResidual = lookAhead.equity - lookAhead.score;
    if (weOnTurn) {
      gameSpread += lookAhead.score;
      if (finalPlyForPlayer) residual += turnResidual;
    } else {
      gameSpread -= lookAhead.score;
      if (finalPlyForPlayer) residual -= turnResidual;
    }
  }
  move->gameSpread.incorporateValue(gameSpread);
  move->residual.incorporateValue(residual);
}

void V2Simulator::sim(V2SimmedMove* move, int iterations) {
  UVcout << "simming " << move->move << " for "
	 << iterations << " iterations." << endl;
  for (int i = 0; i < iterations; ++i) {
    Game game = m_originalGame;
    game.currentPosition()
      .setDrawingOrder(m_orders[i]);
    game.currentPosition()
      .setExchangeDividends(m_exchangeDividends[i]);

    // const vector<int>& dividends = game.currentPosition().exchangeDividends();
    // UVcout << "exchangeDividends:";
    // for (const int dividend : dividends) UVcout << " " << dividend;
    // UVcout << endl;
    // UVcout << "exchangeDividendIndex: "
    // 	   << game.currentPosition().exchangeDividendIndex() << endl;

    game.currentPosition().addAndSetMoveMade(move->move);
    game.commitMove(move->move);

    sim(game, i, move);
  }
  UVcout << "equity: " << move->calculateEquity() << endl << endl;
}

void V2Simulator::sim(int iterations) {
  struct timeval start, end;
  gettimeofday(&start, NULL);
  randomizeTiles(iterations);
  gettimeofday(&end, NULL);
  UVcout << "Time randomizing tiles was "
	 << ((end.tv_sec * 1000000 + end.tv_usec)
	     - (start.tv_sec * 1000000 + start.tv_usec))
	 << " microseconds." << endl;

  gettimeofday(&start, NULL);
  set<LetterString> futureRacks;
  for (unsigned int i = 0; i < m_simmedMoves.size(); ++i) {
    for (int j = 0; j < iterations; ++j) {
      Game game = m_originalGame;
      game.currentPosition().setDrawingOrder(m_orders[j]);
      game.currentPosition()
	.addAndSetMoveMade(m_simmedMoves[i].move);
      game.commitMove(m_simmedMoves[i].move);
      //UVcout << "rack: " << game.currentPosition().oppRack() << endl;
      const LetterString rack = game.currentPosition().oppRack().tiles();
      futureRacks.insert(rack);
      m_bingos[QUACKLE_PRIMESET->multiplyTiles(rack)];
    }
  }
  for (int i = 0; i < iterations; ++i) {
    const LetterString& rack = m_oppRacks[i].tiles();
    futureRacks.insert(rack);
    m_bingos[QUACKLE_PRIMESET->multiplyTiles(rack)];
  }
  gettimeofday(&end, NULL);
  UVcout << "Time collecting racks was "
	 << ((end.tv_sec * 1000000 + end.tv_usec)
	     - (start.tv_sec * 1000000 + start.tv_usec))
	 << " microseconds." << endl;
  
  UVcout << "unique future racks: " << futureRacks.size() << endl;
  gettimeofday(&start, NULL);
  V2Generator::findBingos(futureRacks, &m_bingos);
  gettimeofday(&end, NULL);
  UVcout << "Time finding bingos was "
	 << ((end.tv_sec * 1000000 + end.tv_usec)
	     - (start.tv_sec * 1000000 + start.tv_usec))
	 << " microseconds." << endl;
  for (const auto& pair : m_bingos) {
    UVcout << "product: " << pair.first << endl;
    for (const auto& bingo : pair.second) {
      UVcout << "  " << QUACKLE_ALPHABET_PARAMETERS->userVisible(bingo)
	     << endl;
    }
  }
  for (unsigned int i = 0; i < m_simmedMoves.size(); ++i) {
    sim(&(m_simmedMoves[i]), iterations);
  }
}

void V2Simulator::randomizeTiles(int iterations) {
  m_oppRacks.reserve(iterations);
  m_orders.reserve(iterations);
  m_exchangeDividends.resize(iterations);
  for (int i = 0; i < iterations; ++i) {
    Bag bag(m_originalGame.currentPosition().unseenBag());
    Rack rack;
    bag.refill(rack);
    m_oppRacks.push_back(rack);
    m_orders.push_back(bag.shuffledTiles());
    vector<int>* exchangeDividends = &(m_exchangeDividends[i]);
    exchangeDividends->reserve(50);
    for (int j = 0; j < 50; ++j) {
      exchangeDividends->push_back(DataManager::self()->randomNumber());
    }
    // UVcout << "exchangeDividends: ";
    // for (const int dividend : m_exchangeDividends[i]) {
    //   UVcout << " " << dividend;
    // }
    // UVcout << endl;
  }
}
