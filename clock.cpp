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

#include <time.h>

#include "clock.h"

using namespace Quackle;

Stopwatch::Stopwatch()
	: m_startTime(0)
{
	start();
}

void Stopwatch::start()
{
	m_startTime = time(NULL);
}

int Stopwatch::elapsed() const
{
	time_t now = time(NULL);
	return now - m_startTime;
}

bool Stopwatch::exceeded(int seconds) const
{
	return elapsed() > seconds;
}

