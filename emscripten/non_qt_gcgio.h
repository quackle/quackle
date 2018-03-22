#ifndef QUACKLE_NONQT_GCGIO_H
#define QUACKLE_NONQT_GCGIO_H

#include <string>
#include "game.h"

namespace EmQuackle
{
class GCGIO
{
public:
    GCGIO();
    ~GCGIO(){};

    Quackle::Game *readFile(const std::string &filename);
    Quackle::Game *readFromString(const std::string &contents);

  private:
    int readSignedInt(const std::string &intstring) const;
};
}

#endif