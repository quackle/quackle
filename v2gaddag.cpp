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
									 int indexSize) :
	m_data(data),
	m_lastLetter(lastLetter),
	m_bitsetSize(bitsetSize),
	m_indexSize(indexSize),
	m_completesWordMask(1 << (indexSize * 8 - 1)),
	m_indexMask(m_completesWordMask - 1)
{
	UVcout << "m_completesWordMask: " << m_completesWordMask << endl;
	UVcout << "m_indexMask: " << m_indexMask << endl;
}

uint32_t V2Gaddag::sharedChildren(const unsigned char* bitsetData1,
																	const unsigned char* bitsetData2) const {
	const uint32_t& bitset1 = *(reinterpret_cast<const uint32_t*>(bitsetData1));
	const uint32_t& bitset2 = *(reinterpret_cast<const uint32_t*>(bitsetData2));
	return bitset1 & bitset2;
}

uint32_t V2Gaddag::intersection(const unsigned char* bitsetData,
													 uint32_t rackBits) const {
	const uint32_t& bitset = *(reinterpret_cast<const uint32_t*>(bitsetData));
	return (bitset & rackBits); 
}

bool V2Gaddag::hasAnyChild(const unsigned char* bitsetData,
													 uint32_t rackBits) const {
	const uint32_t& bitset = *(reinterpret_cast<const uint32_t*>(bitsetData));
	return (bitset & rackBits) != 0; 
}

int V2Gaddag::numChildren(const unsigned char* bitsetData) const {
	//UVcout << "numChildren()..." << endl;
	std::bitset<32> bits(*(reinterpret_cast<const uint32_t*>(bitsetData)));
	return bits.count();
}
