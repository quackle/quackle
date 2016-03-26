#include "alphabetparameters.h"
#include "lexiconparameters.h"

#include <set>
#include <QtCore>
#include "quackleio/flexiblealphabet.h"
#include "quackleio/froggetopt.h"
#include "quackleio/util.h"


using namespace std;

void findSubsetsFrom(unsigned int pos,
										 int numUsed,
										 uint64_t product,
										 uint64_t blankPrime,
										 const vector<uint64_t>& primes,
										 set<uint64_t>* subsets) {
	if (numUsed > 7) return;
	if (numUsed > 0) {
		subsets->insert(product);
	}
	if (pos >= primes.size()) return;
	findSubsetsFrom(pos + 1, numUsed, product, blankPrime, primes, subsets);
	findSubsetsFrom(pos + 1, numUsed + 1, product * primes[pos], blankPrime,
									primes, subsets);
	bool hasTwoBlanksAlready = (product % (blankPrime * blankPrime)) == 0;
	if (hasTwoBlanksAlready) return;
	findSubsetsFrom(pos + 1, numUsed + 1, product * blankPrime, blankPrime,
									primes, subsets);
}

void findSubsets(const vector<uint64_t>& primes,
								 uint64_t blankPrime,
								 set<uint64_t>* subsets) {
	findSubsetsFrom(0, 0, 1, blankPrime, primes, subsets);
}

void updateMap(const uint64_t* primes,
							 const Quackle::LetterString& word,
							 map<uint64_t, uint16_t>* primesetMap) {
	set<uint64_t> subsets;
	vector<uint64_t> wordPrimes;
	for (unsigned int i = 0; i < word.length(); ++i) {
		uint64_t prime = primes[static_cast<int>(word[i])];
		wordPrimes.push_back(prime);
	}
	findSubsets(wordPrimes, primes[QUACKLE_BLANK_MARK], &subsets);
	uint16_t lengthMask = 1 << word.length();
	for (const uint64_t subset : subsets) {
		(*primesetMap)[subset] |= lengthMask;
	}
}

bool isPrime(uint64_t n) {
	for (uint64_t d = 2; d * d <= n; ++d) {
		if (n % d == 0) return false;
	}
	return true;
}

uint64_t nextPrime(uint64_t n) {
	if (isPrime(n)) return n;
	return nextPrime(n + 1);
}

void fillPrimes(Quackle::Letter firstLetter, Quackle::Letter lastLetter, uint64_t* primes) {
	uint64_t n = 2;
	uint64_t prime;
	for (int i = firstLetter; i <= lastLetter; ++i) {
		prime = nextPrime(n);
		UVcout << "primes[" << static_cast<int>(i) << "]: " << prime << endl;
		primes[i] = prime;
		n = prime + 1;
	}
	prime = nextPrime(n);
	UVcout << "primes[" << static_cast<int>(QUACKLE_BLANK_MARK) << "]: "
				 << prime << endl;
	primes[QUACKLE_BLANK_MARK] = prime;
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
		inputFilename = "input.txt";
	
	if (outputFilename.isNull())
		outputFilename = "output.primesetmap";

	const string inputFileString = QuackleIO::Util::qstringToString(inputFilename);

	QFile file(inputFilename);
	if (!file.exists()) {
		UVcout << "Input dictionary does not exist: "
					 << QuackleIO::Util::qstringToString(inputFilename) << endl;
		return false;
	}

	if (!file.open(QIODevice::ReadOnly | QIODevice::Text)) {
		UVcout << "Could not open " << QuackleIO::Util::qstringToString(inputFilename)
					 << endl;
		return false;
	}

	QTextStream stream(&file);
	stream.setCodec(QTextCodec::codecForName("UTF-8"));
  uint64_t primes[alphabet.lastLetter() + 1];
	fillPrimes(alphabet.firstLetter(), alphabet.lastLetter(), primes);
	map<uint64_t, uint16_t> primesetMap;
	map<int, vector<Quackle::LetterString>> byLength;
	while (!stream.atEnd()) {
		QString originalQString;
		stream >> originalQString;

		if (stream.atEnd())
			break;

		const UVString word = QuackleIO::Util::qstringToString(originalQString);
		UVString leftover;
		Quackle::LetterString encodedWord = alphabet.encode(word, &leftover);
		if (leftover.empty()) {
			byLength[encodedWord.length()].push_back(encodedWord);
		}
	}
	for (const auto& pair : byLength) {
		UVcout << "updating map with " << pair.first << "-letter words." << endl;
		for (const auto& word : pair.second) {
			updateMap(primes, word, &primesetMap);
		}
		UVcout << "Marked " << primesetMap.size() << " racks." << endl;
	}
  for (const auto& pair : primesetMap) {
		UVcout << "primeset: " << pair.first << " stealMask: " << pair.second << endl;
	}
	return 0;
}
