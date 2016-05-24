#ifndef QUACKLE_V2SIM_H
#define QUACKLE_V2SIM_H

#include <vector>
#include "game.h"

namespace Quackle {

  struct V2AverageValue {
  
    // new zeroed value
    V2AverageValue() : m_valueSum(0), m_squaredValueSum(0),
      m_incorporatedValues(0) {}

    void incorporateValue(double newValue);

    // zero everything
    void clear();
    
    long double valueSum() const;
    long double squaredValueSum() const;
    long int incorporatedValues() const;
    
    // whether or not incorporatedValues is greater than zero
    bool hasValues() const;
    
    // valueSum() / incorporatedValues() or zero
    // if there have been no incorporated values
    double averagedValue() const;

    // sqrt((incorporatedValues() * squaredValueSum() - valueSum()^2) / (incorporatedValues() * (incorporatedValues() - 1)))
    double standardDeviation() const;

  private:
    long double m_valueSum;
    long double m_squaredValueSum;
    long int m_incorporatedValues;
  };

  inline void V2AverageValue::incorporateValue(double newValue) {
    m_valueSum += newValue;
    m_squaredValueSum += newValue * newValue;
    ++m_incorporatedValues;
  }

  inline long double V2AverageValue::valueSum() const {
    return m_valueSum;
  }

  inline long double V2AverageValue::squaredValueSum() const {
    return m_squaredValueSum;
  }

  inline double V2AverageValue::averagedValue() const {
    return m_incorporatedValues == 0? 0 : (m_valueSum / m_incorporatedValues);
  }
  
  inline long int V2AverageValue::incorporatedValues() const {
    return m_incorporatedValues;
  }

  inline bool V2AverageValue::hasValues() const {
    return m_incorporatedValues > 0;
  }

  struct V2SimmedMove {
    V2SimmedMove(const Move &_move) : move(_move) { }

    double calculateEquity() const {
      return gameSpread.averagedValue() + residual.averagedValue();
    }

    Move move;

    V2AverageValue residual;
    V2AverageValue gameSpread;
    V2AverageValue wins;
  };
  
  typedef vector<V2SimmedMove> V2SimmedMoveList;

  class V2Simulator {
  public:
    V2Simulator();
    V2Simulator(const Quackle::Game &game);
    void getCandidates(unsigned int numPlays);
    void sim(Game game, int iteration, V2SimmedMove* move);
    void sim(V2SimmedMove* move, int iterations);
    void sim(int iterations);
    void randomizeTiles(int iterations);
    
  private:
    Game m_originalGame;
    V2SimmedMoveList m_simmedMoves;
    map<Product, vector<LetterString>> m_bingos;
    int m_plies = 2;
    vector<Rack> m_oppRacks;
    vector<LongLetterString> m_orders;
    vector<vector<int>> m_exchangeDividends;
  };
}

#endif
