%module quackle
%{
#include "fixedstring.h"
#include "uv.h"
#include "alphabetparameters.h"
#include "move.h"
#include "rack.h"
#include "bag.h"
#include "board.h"
#include "boardparameters.h"
#include "evaluator.h"
#include "catchall.h"
#include "player.h"
#include "game.h"
#include "gameparameters.h"
#include "sim.h"
#include "computerplayer.h"
#include "computerplayercollection.h"
#include "datamanager.h"
#include "endgame.h"
#include "endgameplayer.h"
#include "enumerator.h"
#include "bogowinplayer.h"
#include "clock.h"
#include "generator.h"
#include "gaddag.h"
#include "lexiconparameters.h"
#include "preendgame.h"
#include "reporter.h"
#include "resolvent.h"
#include "strategyparameters.h"

#include <QString>
#include "quackleio/flexiblealphabet.h"
#include "quackleio/util.h"
#include "quackleio/logania.h"
#include "quackleio/gcgio.h"
%}

%include "std_string.i"
%include "std_vector.i"

/*Needed to generate proper iterable types */
%template(MoveVector) std::vector<Quackle::Move>;
%template(PlayerVector) std::vector<Quackle::Player>;
%template(ProbableRackList) std::vector<Quackle::ProbableRack>;
%template(PositionList) std::vector<Quackle::GamePosition>;

%include "fixedstring.h"
%include "uv.h"
%include "alphabetparameters.h"
%include "move.h"
%include "rack.h"
%include "bag.h"
%include "board.h"
%include "boardparameters.h"
%include "evaluator.h"
%include "catchall.h"
%include "player.h"

/* handle output arguments of PlayerList methods using cool SWIG typemaps */
/* what we do here is just to tell SWIG that last bool& argument is an output argument */
%include "typemaps.i"

using namespace std;
namespace Quackle
{
    class PlayerList : public vector<Player>
    {
        public:
        PlayerList();

        const Player &playerForId(int id, bool &OUTPUT) const;
        const Player &playerForName(const UVString &name, bool &OUTPUT) const;
    };
}

%include "game.h"
%include "gameparameters.h"
%include "sim.h"
%include "computerplayer.h"
%include "computerplayercollection.h"
%include "datamanager.h"
%include "endgame.h"
%include "endgameplayer.h"
%include "enumerator.h"
%include "bogowinplayer.h"
%include "clock.h"
%include "generator.h"
%include "gaddag.h"
%include "lexiconparameters.h"
%include "preendgame.h"
%include "reporter.h"
%include "resolvent.h"
%include "strategyparameters.h"

%include <QString>
%include "quackleio/flexiblealphabet.h"
%include "quackleio/util.h"
%include "quackleio/logania.h"
%include "quackleio/gcgio.h"
