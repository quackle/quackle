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

#ifndef QUACKLE_UV_H
#define QUACKLE_UV_H

#include <fstream>
#include <iostream>
#include <sstream>
#include <string>

using namespace std;

/** Beginning of define options */

// Whether to use wchar for Quackle's user-visible strings.
// Not recommended.
#ifndef QUACKLE_USE_WCHAR_FOR_USER_VISIBLE
#define QUACKLE_USE_WCHAR_FOR_USER_VISIBLE 0
#endif // !QUACKLE_USE_WCHAR_FOR_USER_VISIBLE

/** End of define options */


#if QUACKLE_USE_WCHAR_FOR_USER_VISIBLE
typedef wstring UVString;
typedef wchar_t UVChar;
typedef wstringstream UVStringStream;
typedef wistream UVIStream;
typedef wostream UVOStream;
typedef wostringstream UVOStringStream;
typedef wistringstream UVIStringStream;
typedef wifstream UVIFStream;
typedef wofstream UVOFStream;
#define UVcout wcout
#define UVcerr wcerr
#define MARK_UV(theString)  L ## theString
#else
typedef string UVString;
typedef char UVChar;
typedef stringstream UVStringStream;
typedef istream UVIStream;
typedef ostream UVOStream;
typedef istringstream UVIStringStream;
typedef ostringstream UVOStringStream;
typedef ifstream UVIFStream;
typedef ofstream UVOFStream;
#define UVcout cout
#define UVcerr cerr
#define MARK_UV(theString)  theString
#endif

#endif
