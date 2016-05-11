#ifndef QUACKLE_V2GADDAG_H
#define QUACKLE_V2GADDAG_H

#include <bitset>

#include "alphabetparameters.h"
#include "datamanager.h"

#define QUACKLE_GADDAG_SEPARATOR QUACKLE_NULL_MARK

namespace Quackle {

class V2Gaddag {
 public:
  V2Gaddag(const unsigned char* data,
	   Letter lastLetter,
	   int bitsetSize,
	   int indexSize);

  inline const unsigned char* nextRackChild(const unsigned char* bitsetData,
					    Letter minLetter,
					    uint32_t rackBits,
					    int* childIndex,
					    Letter* nextLetter) const {
    const uint32_t& bitset = *(reinterpret_cast<const uint32_t*>(bitsetData));
    for (;;) {
      const uint32_t inverseMask = (1 << minLetter) - 1;
      *nextLetter = __builtin_ffs(bitset & (~inverseMask)) - 1;
      if (*nextLetter > m_lastLetter) {
	return NULL;
      }
      if (rackBits & (1 << *nextLetter)) {
	break;
      }
      minLetter = *nextLetter + 1;
      (*childIndex)++;
    }
    return bitsetData + m_bitsetSize + (m_indexSize * (*childIndex));
  }

  uint32_t sharedChildren(const unsigned char* bitsetData1,
			  const unsigned char* bitsetData2) const;
  uint32_t intersection(const unsigned char* bitsetData, uint32_t rackBits) const;
  bool hasAnyChild(const unsigned char* bitsetData, uint32_t rackBits) const;

  inline bool hasChild(const unsigned char* bitsetData, Letter letter) const {
    uint32_t letterMask = 1 << letter;
    const uint32_t& bitset = *(reinterpret_cast<const uint32_t*>(bitsetData));
    return (bitset & letterMask) != 0;
  }

  int numChildren(const unsigned char* bitsetData) const;
  
  inline const unsigned char*
    changeDirection(const unsigned char* bitsetData) const {
    if (hasChild(bitsetData, QUACKLE_GADDAG_SEPARATOR)) {
      return bitsetData + m_bitsetSize;
    } else {
      return NULL;
    }	 
  }
  
  inline const unsigned char*
    child(const unsigned char* bitsetData, Letter letter) const {
    const uint32_t& bitset = *(reinterpret_cast<const uint32_t*>(bitsetData));
    //uint32_t letterMask = 1 << letter;
    //if ((bitset & letterMask) == 0) return NULL;
    uint32_t beforeLetterMask = (1 << letter) - 1;
    int index = __builtin_popcount(bitset & beforeLetterMask);
    return bitsetData + m_bitsetSize + (m_indexSize * index);
  }

  inline bool completesWord(const unsigned char* indexData) const {
    uint32_t data = *(reinterpret_cast<const uint32_t*>(indexData));
    return (data & m_completesWordMask) != 0;
  }

  inline const unsigned char* followIndex(const unsigned char* indexData) const {
    uint32_t data = *(reinterpret_cast<const uint32_t*>(indexData));
    uint32_t index = data & m_indexMask;
    if (index == 0) return NULL;
    return m_data + index;
  }
  
  inline const unsigned char* root() const { return m_data; };
  
 private:
  const unsigned char* m_data;
  const Letter m_lastLetter;
  const int m_bitsetSize;
  const int m_indexSize;
  const uint32_t m_completesWordMask;
  const uint32_t m_indexMask;
};
 
}  // namespace Quackle

#endif
