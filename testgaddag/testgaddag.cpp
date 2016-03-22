#include "alphabetparameters.h"
#include "lexiconparameters.h"
#include "gaddag.h"
#include "v2gaddag.h"

#include <QtCore>
#include "quackleio/flexiblealphabet.h"
#include "quackleio/froggetopt.h"
#include "quackleio/util.h"

using namespace std;

void gaddagCount(const Quackle::GaddagNode *node,
								 int* resultCount) {
	for (const Quackle::GaddagNode* child = node->firstChild(); child; child = child->nextSibling()) {
		if (child->isTerminal()) {
			(*resultCount)++;
		}
		gaddagCount(child, resultCount);
	}
}

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

void v2gaddagCount(const Quackle::V2Gaddag& gaddag,
									 const unsigned char* node,
									 int* resultCount) {
	int minLetter = QUACKLE_NULL_MARK;
	int childIndex = 0;
  for (;;) {
		Quackle::Letter foundLetter;
		const unsigned char* child =
			gaddag.nextChild(node, minLetter, childIndex, &foundLetter);
		if (child == NULL) return;
		if (gaddag.completesWord(child)) {
			(*resultCount)++;
		}
		const unsigned char* newNode = gaddag.followIndex(child);
		if (newNode != NULL) {
			v2gaddagCount(gaddag, newNode, resultCount);
		} 
		minLetter = foundLetter + 1;
		++childIndex;
	}
}

void v2gaddagDump(const Quackle::AlphabetParameters& alphabet,
									const Quackle::V2Gaddag& gaddag,
									const Quackle::LetterString& prefix,
									const unsigned char* node,
									vector<string>* results) {
	//UVcout << "v2GaddagDump, prefix: " << alphabet.userVisible(prefix)
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

void anagram(const Quackle::AlphabetParameters& alphabet,
						 int firstLetter, int lastLetter, const Quackle::V2Gaddag& gaddag,
						 const unsigned char* node, int* counts, uint32_t rackBits,
						 Quackle::LetterString* prefix, vector<Quackle::LetterString>* anagrams) {
	//UVcout << "anagram(...)" << endl;
	//UVcout << "prefix: " << alphabet.userVisible(*prefix) << endl;
	//UVcout << "rack (from counts): ";
	//string nodeChildren;
	/*
	for (int letter = firstLetter; letter <= lastLetter; ++letter) {
		//uint32_t letterMask = 1 << letter;
		//const uint32_t& bitset = *(reinterpret_cast<const uint32_t*>(node));
		//if ((bitset & letterMask) != 0) {
		//	nodeChildren += alphabet.userVisible(letter);
		//}
		if (counts[letter] > 0) {
			rackBits |= 1 << letter;
		}
		for (int i = 0; i < counts[letter]; ++i) {
			//UVcout << alphabet.userVisible(letter);
		}
	}
	*/
	//UVcout << endl;
	//UVcout << "nodeChildren: " << nodeChildren << endl;
	//UVcout << "rackBits: " << rackBits << endl;
	if (gaddag.hasAnyChild(node, rackBits)) {
		//UVcout << "node has a child on rack" << endl;
		Quackle::Letter minLetter = 0;
		int childIndex = 0;
		for (;;) {
			//UVcout << "minLetter: " << static_cast<int>(minLetter) << endl;
			Quackle::Letter foundLetter;
			const unsigned char* child = gaddag.nextRackChild(node,
																												minLetter,
																												rackBits,
																												&childIndex,
																												&foundLetter);
			//UVcout << "foundLetter: " << static_cast<int>(foundLetter) << endl;
			if (child == NULL) return;
			prefix->push_back(foundLetter);
			counts[foundLetter]--;
			uint32_t foundLetterMask = 1 << foundLetter;
			if (counts[foundLetter] == 0) {
				rackBits &= ~foundLetterMask;
			}
			if (gaddag.completesWord(child)) {
				//UVcout << "complete word: " << alphabet.userVisible(*prefix) << endl;
				anagrams->push_back(*prefix);
			}
			const unsigned char* newNode = gaddag.followIndex(child);
			if (newNode != NULL) {
				anagram(alphabet, firstLetter, lastLetter, gaddag,
								newNode, counts, rackBits, prefix, anagrams);
			}
			prefix->pop_back();
			counts[foundLetter]++;
			rackBits |= foundLetterMask;
			minLetter = foundLetter + 1;
			++childIndex;
		}
	}
}

void anagram(const Quackle::AlphabetParameters& alphabet,
						 const UVString rackString,
						 const Quackle::V2Gaddag& gaddag,
						 vector<Quackle::LetterString>* anagrams) {
	int counts[alphabet.lastLetter() + 1];
	for (int i = alphabet.firstLetter(); i <= alphabet.lastLetter(); ++i) {
		counts[i] = 0;
	}
	UVString leftover;
	Quackle::LetterString rack = alphabet.encode(rackString, &leftover);
	for (unsigned int i = 0; i < rack.size(); ++i) {
		counts[static_cast<int>(rack[i])]++;
	}
	for (int i = alphabet.firstLetter(); i <= alphabet.lastLetter(); ++i) {
		UVcout << "counts[" << i << "]: " << counts[i] << endl;
	}
		uint32_t rackBits = 0;
	//UVcout << "rack (from counts): ";
	//string nodeChildren;
	for (int letter = alphabet.firstLetter(); letter <= alphabet.lastLetter(); ++letter) {
		//uint32_t letterMask = 1 << letter;
		//const uint32_t& bitset = *(reinterpret_cast<const uint32_t*>(node));
		//if ((bitset & letterMask) != 0) {
		//	nodeChildren += alphabet.userVisible(letter);
		//}
		if (counts[letter] > 0) {
			rackBits |= 1 << letter;
		}
		for (int i = 0; i < counts[letter]; ++i) {
			//UVcout << alphabet.userVisible(letter);
		}
	}
	Quackle::LetterString prefix;
	for (int i = 0; i < 100000; ++i) {
		anagram(alphabet, alphabet.firstLetter(), alphabet.lastLetter(), gaddag,
						gaddag.root(), counts, rackBits, &prefix, anagrams);
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

	//int resultCount = 0;

	/*
	for (int i = 0; i < 100; ++i) {
		gaddagCount(parameters.gaddagRoot(), &resultCount);
	}
	//gaddagDump(alphabet, parameters.gaddagRoot(), Quackle::LetterString(), &results);
	*/

	/*
	for (int i = 0; i < 100; ++i) {
		v2gaddagCount(*parameters.v2Gaddag(),
									parameters.v2Gaddag()->root(),
									&resultCount);
	}
	*/
  //v2gaddagDump(alphabet, *parameters.v2Gaddag(), Quackle::LetterString(),
	//						 parameters.v2Gaddag()->root(), &results);

	vector<Quackle::LetterString> anagrams;
	anagram(alphabet, "ANESTRI", *parameters.v2Gaddag(), &anagrams);
	UVcout << "Found " << anagrams.size() << " anagrams." << endl;
	
	//UVcout << "Found " << resultCount << " patterns." << endl;
	//const string outputFileString = QuackleIO::Util::qstringToString(outputFilename);
	//ofstream o(outputFileString.c_str(), ios::out);
	//for (const string& pattern : results) {
	//	o << pattern << endl;
	//}
	//UVcout << "wrote " << results.size()
	//			 << " patterns to output file " << outputFileString << endl;
	return 0;
}
