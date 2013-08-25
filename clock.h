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

#ifndef QUACKLE_CLOCK_H
#define QUACKLE_CLOCK_H

namespace Quackle
{

class Stopwatch
{
public:
	// starts the stopwatch
	Stopwatch();

	// sets the start time to the time now
	void start();

	// returns how much time has passed since start was called
	int elapsed() const;

	// returns true if the elapsed time exceeds the specified
	// number of seconds
	bool exceeded(int seconds) const;

private:
	time_t m_startTime;
};

}

#endif
