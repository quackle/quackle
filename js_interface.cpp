#include "datamanager.h"
#include "computerplayercollection.h"
#include "alphabetparameters.h"
#include "boardparameters.h"
#include "lexiconparameters.h"
#include "strategyparameters.h"

#define QUACKLE_DATAMANAGER Quackle::DataManager::self()

// General flow taken from Python bindings, quackletest.cpp,
// testharness.cpp, etc.
void startup() {
    Quackle::DataManager dataManager;
    dataManager.setAppDataDirectory("data");
    dataManager.setBackupLexicon("twl06");

    dataManager.lexiconParameters()->loadDawg(
        Quackle::LexiconParameters::findDictionaryFile("twl06.dawg"));
    dataManager.lexiconParameters()->loadGaddag(
        Quackle::LexiconParameters::findDictionaryFile("twl06.gaddag"));
    dataManager.strategyParameters()->initialize("twl06");
    dataManager.setBoardParameters(new Quackle::BoardParameters());
    dataManager.setComputerPlayers(
        Quackle::ComputerPlayerCollection::fullCollection());
}

void loadGameAndPlayers() {

    // Set up alphabet -- assume it's english so we probably don't need to do this.
    // see `m_alphabetParameters = new EnglishAlphabetParameters;` in datamanager
    // string abc = Quackle::AlphabetParameters::findAlphabetFile("english");
    // strint abc2 = QuackleIO::Util::stdStringToQString


    // Let's load the GCG the way the Python file does, then create a GCG loader
    // function. MVP for this should be to provide a GCG loader and a speedy
    // player move generator.
    // Later add simming.
}
