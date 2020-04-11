#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

#include "datamanager.h"
#include "game.h"
#include "strategyparameters.h"

class CSVRow {
public:
  std::string const &operator[](std::size_t index) const {
    return m_data[index];
  }
  std::size_t size() const { return m_data.size(); }
  void readNextRow(std::istream &str) {
    std::string line;
    std::getline(str, line);

    std::stringstream lineStream(line);
    std::string cell;

    m_data.clear();
    while (std::getline(lineStream, cell, ',')) {
      m_data.push_back(cell);
    }
    // This checks for a trailing comma with no data after it.
    if (!lineStream && cell.empty()) {
      // If there was a trailing comma then add an empty element.
      m_data.push_back("");
    }
  }

private:
  std::vector<std::string> m_data;
};

std::istream &operator>>(std::istream &str, CSVRow &data) {
  data.readNextRow(str);
  return str;
}

std::vector<std::string> getNextLineAndSplitIntoTokens(std::istream &str) {
  std::vector<std::string> result;
  std::string line;
  std::getline(str, line);

  std::stringstream lineStream(line);
  std::string cell;

  while (std::getline(lineStream, cell, ',')) {
    result.push_back(cell);
  }
  // This checks for a trailing comma with no data after it.
  if (!lineStream && cell.empty()) {
    // If there was a trailing comma then add an empty element.
    result.push_back("");
  }
  return result;
}

int main() {
  Quackle::DataManager m_dataManager;
  m_dataManager.setAppDataDirectory("data");
  m_dataManager.setBackupLexicon("default_english");
  m_dataManager.strategyParameters()->initialize("default_english");

  // macondo_leaves is just a list of all possible leaves from macondo
  std::ifstream file("macondo_leaves.csv");
  CSVRow row;

  std::vector<std::string> leaves;

  while (file >> row) {
    leaves.push_back(row[0]);
  }
  file.close();

  for (int i = 0; i < leaves.size(); i++) {
    cout << leaves[i] << ","
         << m_dataManager.strategyParameters()->superleave(
                QUACKLE_ALPHABET_PARAMETERS->encode(MARK_UV(leaves[i])))
         << endl;
  }
}