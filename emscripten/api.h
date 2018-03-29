#ifndef _API_H_
#define _API_H_

#include "computerplayercollection.h"
#include "computerplayer.h"
#include "game.h"
#include "move.h"
#include "datamanager.h"
#include "../test/trademarkedboards.h"
#include "lexiconparameters.h"
#include "strategyparameters.h"


class API
{
    public:
      API();
      void startup();
      string loadGameAndPlayers();
      string kibitzTurn(int playerID, int turnNumber);
      string setupSimulator(int playerID, int turnNumber);
      string simulateIter(int iterationStep, int plies);
      void deleteGame();



    private:
      Quackle::DataManager m_dataManager;
      Quackle::Game *m_game;
      Quackle::GamePosition m_position;
      Quackle::Simulator m_simulator;
      int m_totalIterations;

      Quackle::Rack m_rack;

      string moveToString(const Quackle::Move &move);
      void kibitzPositionAt(int playerID, int turnNumber, int numMoves);
      string serializeMoves(const Quackle::MoveList &moves);
      string letterStringToString(const Quackle::LetterString &ls);
};

#endif