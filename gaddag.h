/*
 *  Quackle -- Crossword game artificial intelligence and analysis tool
 *  Copyright (C) 2005-2014 Jason Katz-Brown and John O'Laughlin.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef QUACKLE_GADDAG_H
#define QUACKLE_GADDAG_H

#include "alphabetparameters.h"

#define QUACKLE_GADDAG_SEPARATOR QUACKLE_NULL_MARK

namespace Quackle
{

class GaddagNode
{
public:
	Letter letter() const;
	bool isTerminal() const;
	const GaddagNode *firstChild() const;
	const GaddagNode *nextSibling() const;
	const GaddagNode *child(Letter l) const;
private:
	unsigned char data[4];
};

inline Letter
GaddagNode::letter() const
{
	return (data[3] & 0x3F /*0b00111111*/);
}

inline bool
GaddagNode::isTerminal() const
{
	return (data[3] & 0x40) != 0 /*0b01000000*/;
}

inline const GaddagNode *
GaddagNode::firstChild() const
{
	unsigned int p = (data[0] << 16) + (data[1] << 8) + (data[2]);
	if (p == 0) {
		return 0;
	} else {
		return this + p;
	}
}

/*
inline const GaddagNode *
GaddagNode::firstChild() const
{
	int p = (data[0] << 16) + (data[1] << 8) + (data[2]);
	if (p == 0) 
	{
		return 0;
	} 
	else 
	{
		if(p & 0x00800000)
			p |= 0xff000000;
		return this + p;
	}
}
*/

inline const GaddagNode *
GaddagNode::nextSibling() const
{
	if (data[3] & 0x80 /*0b10000000*/) {
		return 0;
	} else {
		return this + 1; // assumes packed array of siblings
	}
}
 
inline const GaddagNode *
GaddagNode::child(Letter l) const
{
	for (const GaddagNode *child = firstChild(); child; child = child->nextSibling()) {
		if (child->letter() == l) {
			return child;
		} else if (l != QUACKLE_GADDAG_SEPARATOR && child->letter() > l) {
			return 0;
		}
	}
	return 0;
}

}

#endif
