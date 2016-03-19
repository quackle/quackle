#include "alphabetparameters.h"
#include "lexiconparameters.h"
#include "gaddag.h"
#include "v2gaddag.h"

#include <QtCore>
#include "quackleio/flexiblealphabet.h"
#include "quackleio/froggetopt.h"
#include "quackleio/util.h"

using namespace std;

void gaddagDump(const Quackle::AlphabetParameters& alphabet,
								const Quackle::GaddagNode *node,
								const Quackle::LetterString &prefix,
								vector<string>* results) {
	UVcout << "gaddagDump..." << endl;
	for (const Quackle::GaddagNode* child = node->firstChild(); child; child = child->nextSibling()) {
		Quackle::Letter letter = child->letter();
		//UVcout << "letter: " << static_cast<int>(letter) << endl;
		if (letter == QUACKLE_GADDAG_SEPARATOR) {
			letter = QUACKLE_PLAYED_THRU_MARK;
		}
		Quackle::LetterString newPrefix = prefix + letter;
		if (child->isTerminal()) {
			//UVcout << "pattern: " << alphabet.userVisible(newPrefix) << endl;
			results->push_back(alphabet.userVisible(newPrefix));
		}
		gaddagDump(alphabet, child, newPrefix, results);
	}
}

void v2gaddagDump(const Quackle::AlphabetParameters& alphabet,
									const Quackle::V2Gaddag& gaddag,
									const Quackle::LetterString& prefix,
									const unsigned char* node,
									vector<string>* results) {
	// UVcout << "v2GaddagDump, prefix: " << alphabet.userVisible(prefix)
	// 			 << " node: " << reinterpret_cast<uint64_t>(node) << endl;
	int minLetter = QUACKLE_NULL_MARK;
	int childIndex = 0;
  for (;;) {
		//UVcout << "minLetter: " << static_cast<int>(minLetter) << endl;
		//UVcout << "childIndex: " << childIndex << endl;
		Quackle::Letter foundLetter;
		const unsigned char* child =
			gaddag.nextChild(node, minLetter, childIndex, &foundLetter);
		if (child == NULL) return;
		//UVcout << "foundLetter: " << static_cast<int>(foundLetter) << endl;
		Quackle::LetterString newPrefix = prefix;
	  if (foundLetter == QUACKLE_NULL_MARK) {
			newPrefix += 2;
		} else {
			newPrefix += foundLetter;
		}
		//UVcout << "newPrefix: " << alphabet.userVisible(newPrefix) << endl;

		if (gaddag.completesWord(child)) {
			//UVcout << "complete word: " << alphabet.userVisible(newPrefix) << endl;
			results->push_back(alphabet.userVisible(newPrefix));
		}
		const unsigned char* newNode = gaddag.followIndex(child);
		//UVcout << "newNode: " << reinterpret_cast<uint64_t>(newNode) << endl;
		if (newNode != NULL) {
			v2gaddagDump(alphabet, gaddag, newPrefix, newNode, results);
		} 
		minLetter = foundLetter + 1;
		++childIndex;
	}
}

int main(int argc, char **argv) {
	QCoreApplication a(argc, argv);
	
	GetOpt opts;
	QString alphabetFilename;
	QString inputFilename;
	QString outputFilename;
	
	opts.addOption('f', "input", &inputFilename);
	opts.addOption('o', "output", &outputFilename);
	opts.addOption('a', "alphabet", &alphabetFilename);
	if (!opts.parse())
		return 1;
	
	if (alphabetFilename.isNull())
		alphabetFilename = "english.quackle_alphabet";

	//const string alphabetFileString = QuackleIO::Util::qstringToString(alphabetFilename);
	QuackleIO::FlexibleAlphabetParameters alphabet;
	alphabet.load(alphabetFilename);
	
	if (inputFilename.isNull())
		inputFilename = "input.gaddag";
	
	if (outputFilename.isNull())
		outputFilename = "gaddagdump.txt";

	const string inputFileString = QuackleIO::Util::qstringToString(inputFilename);
	Quackle::LexiconParameters parameters;
	parameters.loadGaddag(inputFileString);
	vector<string> results;

  //gaddagDump(alphabet, parameters.gaddagRoot(), Quackle::LetterString(), &results);
  v2gaddagDump(alphabet, *parameters.v2Gaddag(), Quackle::LetterString(),
							 parameters.v2Gaddag()->root(), &results);
	
	const string outputFileString = QuackleIO::Util::qstringToString(outputFilename);
	ofstream o(outputFileString.c_str(), ios::out);
	for (const string& pattern : results) {
		o << pattern << endl;
	}
	UVcout << "wrote " << results.size()
				 << " patterns to output file " << outputFileString << endl;
	return 0;
}
