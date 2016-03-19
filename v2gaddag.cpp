/*
 *  Quackle -- Crossword game artificial intelligence and analysis tool
 *  Copyright (C) 2005-2006 Jason Katz-Brown and John O'Laughlin.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 *  02110-1301  USA
 */

#include "v2gaddag.h"

using namespace Quackle;

V2Gaddag::V2Gaddag(const unsigned char* data,
									 Letter lastLetter,
									 int bitsetSize,
									 int indexSize,
									 int indexUnit) :
	m_data(data),
	m_lastLetter(lastLetter),
	m_bitsetSize(bitsetSize),
	m_indexSize(indexSize),
  m_indexUnit(indexUnit)
{
}
	
const unsigned char* V2Gaddag::nextChild(const unsigned char* bitsetData,
																				 Letter minLetter,
																				 int childIndex,
																				 Letter* nextLetter) const {
	// uint64_t offset = bitsetData - m_data;
	// UVcout << "nextChild(m_data+" << offset << ", " << static_cast<int>(minLetter)
	// 			 << ", " << childIndex << ", *nextLetter)..." << endl;
	std::bitset<32> bits(*(reinterpret_cast<const uint32_t*>(bitsetData)));
	//UVcout << "nextChild bits: " << bits.to_string() << endl;
	for (Letter i = minLetter; i <= m_lastLetter; ++i) {
		//UVcout << "nextChild i: " << static_cast<int>(i) << endl;
		if (bits[i]) {
			//UVcout << "bits[i] is set" << endl;
			*nextLetter = i;
			return bitsetData + m_bitsetSize + (m_indexSize * childIndex);
		} else {
			//UVcout << "bits[i] is unset" << endl;
		}
	}
	return NULL;
}

bool V2Gaddag::hasChild(const unsigned char* bitsetData,
												Letter letter) const {
	std::bitset<32> bits(*(reinterpret_cast<const uint32_t*>(bitsetData)));
	return bits[letter];
}

int V2Gaddag::numChildren(const unsigned char* bitsetData) const {
	//UVcout << "numChildren()..." << endl;
	std::bitset<32> bits(*(reinterpret_cast<const uint32_t*>(bitsetData)));
	return bits.count();
}

const unsigned char* V2Gaddag::child(const unsigned char* bitsetData,
																		 Letter letter) const {
	Letter minLetter = 0;
	int childIndex = 0;
	for (;;) {
		Letter foundLetter;
		const unsigned char* foundChild = nextChild(bitsetData, minLetter,
																								childIndex, &foundLetter);
		if (foundChild == NULL) {
			UVcout << "Could not find letter " << static_cast<int>(letter) << endl;
			return NULL;
		}
		if (foundLetter == letter) {
			return foundChild;
		} else {
			minLetter = foundLetter + 1;
			++childIndex;
		}
	}
	return NULL;
}

bool V2Gaddag::completesWord(const unsigned char* indexData) const {
	//uint64_t offset = indexData - m_data;
	//UVcout << "completesWord(m_data+" << offset << ")..." << endl;
	uint32_t data = *(reinterpret_cast<const uint32_t*>(indexData));
	//UVcout << "data: " << data << endl;
	return (data & 0x80000000) != 0;
}

const unsigned char* V2Gaddag::followIndex(const unsigned char* indexData) const {
	// uint64_t offset = indexData - m_data;
	// UVcout << "followIndex(m_data+" << offset << ")..." << endl;
	uint32_t data = *(reinterpret_cast<const uint32_t*>(indexData));
	//UVcout << "data: " << data << endl;
	uint32_t index = data & 0x7FFFFFFF;
	if (index == 0) return NULL;
	//UVcout << "index: " << index << endl;
	return m_data + index * m_indexUnit;
}
