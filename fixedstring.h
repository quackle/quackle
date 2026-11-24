/*
 *  Quackle -- Crossword game artificial intelligence and analysis tool
 *  Copyright (C) 2005-2019 Jason Katz-Brown, John O'Laughlin, and John Fultz.
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

#ifndef QUACKLE_FIXEDSTRING_H
#define QUACKLE_FIXEDSTRING_H

#include <string.h>

#define FIXED_STRING_MAXIMUM_LENGTH 40

namespace Quackle
{

// LetterStrings are built from untrusted input -- GCG racks, word lists, binary
// lexicons -- so the capacity bound is checked in release builds rather than
// asserted, and a violation is fatal instead of silently truncating a rack.
[[noreturn]] void fixedStringViolation(const char *operation, unsigned int length);

class FixedLengthString
{
public:
	typedef char *iterator;
	typedef const char *const_iterator;
	typedef unsigned int size_type;
	typedef char &reference;
	typedef const char &const_reference;

	FixedLengthString();
	FixedLengthString(const char *s, size_type n);
	FixedLengthString(size_type n, char c);
	FixedLengthString(const char *s);
	FixedLengthString(const FixedLengthString &s);
	FixedLengthString(FixedLengthString &&s);

	const_iterator begin() const;
	const_iterator end() const;
	iterator begin();
	iterator end();
	void erase(const iterator i);
	size_type length() const;
	FixedLengthString substr(size_type pos, size_type n) const;
	bool empty() const;
	size_type size() const { return length(); }
	void clear() { m_length = 0; }
	void push_back(char c);
	void pop_back();
	const char *constData() const { return m_data; }

	int compare(const FixedLengthString &s) const;

	FixedLengthString &operator+=(char c);
	FixedLengthString &operator+=(const FixedLengthString &s);

	const_reference operator[](size_type n) const { return m_data[n]; }
	FixedLengthString &operator=(const FixedLengthString &s);

	static const unsigned int maxSize = FIXED_STRING_MAXIMUM_LENGTH;

private:
	char m_data[maxSize];
	size_type m_length;
};

inline FixedLengthString operator+(const FixedLengthString &lhs, const FixedLengthString &rhs)
{
	FixedLengthString str(lhs);
	str += rhs;
	return str;
}

inline FixedLengthString operator+(char lhs, const FixedLengthString &rhs)
{
	FixedLengthString str(1, lhs);
	str += rhs;
	return str;
}

inline FixedLengthString operator+(const FixedLengthString &lhs, char rhs)
{
	FixedLengthString str(lhs);
	str += rhs;
	return str;
}

inline FixedLengthString::FixedLengthString()
	: m_length(0)
{
}

inline FixedLengthString::FixedLengthString(const char *s, size_type n)
{
	if (n > maxSize)
		fixedStringViolation("FixedLengthString(const char *, size_type)", n);
	memcpy(m_data, s, n);
	m_length = n;
}

inline FixedLengthString::FixedLengthString(size_type n, char c)
	: m_length(n)
{
	if (n > maxSize)
		fixedStringViolation("FixedLengthString(size_type, char)", n);
	memset(m_data, c, n);
}

inline FixedLengthString::FixedLengthString(const char *s)
{
	size_t sz = strlen(s);
	if (sz > maxSize)
		fixedStringViolation("FixedLengthString(const char *)", (size_type)sz);
	memcpy(m_data, s, sz);
	m_length = (size_type)sz;
}

inline FixedLengthString::FixedLengthString(const FixedLengthString &s)
	: m_length(s.m_length)
{
	memcpy(m_data, s.m_data, m_length);
}

inline FixedLengthString::FixedLengthString(FixedLengthString &&s)
	: m_length(s.m_length)
{
	memcpy(m_data, s.m_data, m_length);
}

inline FixedLengthString &FixedLengthString::operator=(const FixedLengthString &s)
{
	m_length = s.m_length;
	memcpy(m_data, s.m_data, m_length);
	return *this;
}

inline FixedLengthString::const_iterator FixedLengthString::begin() const
{
	return m_data;
}

inline FixedLengthString::const_iterator FixedLengthString::end() const
{
	return m_data + m_length;
}

inline FixedLengthString::iterator FixedLengthString::begin()
{
	return m_data;
}

inline FixedLengthString::iterator FixedLengthString::end()
{
	return m_data + m_length;
}

inline void FixedLengthString::erase(const iterator i)
{
	if (i < m_data || i >= m_data + m_length)
		fixedStringViolation("erase", size_type(i - m_data) + 1);
	memmove(i, i + 1, (m_data + m_length) - (i + 1));
	--m_length;
}

inline FixedLengthString::size_type FixedLengthString::length() const
{
	return m_length;
}

inline FixedLengthString FixedLengthString::substr(size_type pos, size_type n) const
{
	// Not pos + n <= size(): both are unsigned, and callers reach this with
	// length() - 1 on an empty string, where the sum wraps back into range.
	if (pos > m_length || n > m_length - pos)
		fixedStringViolation("substr", pos + n);
	return FixedLengthString(&m_data[pos], n);
}

inline bool FixedLengthString::empty() const
{
	return m_length == 0;
}

inline FixedLengthString &FixedLengthString::operator+=(char c)
{
	if (m_length >= maxSize)
		fixedStringViolation("operator+=(char)", m_length + 1);
	m_data[m_length++] = c;
	return *this;
}

inline FixedLengthString &FixedLengthString::operator+=(const FixedLengthString &s)
{
	if (s.m_length > maxSize - m_length)
		fixedStringViolation("operator+=(const FixedLengthString &)", m_length + s.m_length);
	memcpy(m_data + m_length, s.m_data, s.m_length);
	m_length += s.m_length;
	return *this;
}

inline void FixedLengthString::push_back(char c)
{
	*this += c;
}

inline void FixedLengthString::pop_back()
{
	if (m_length == 0)
		fixedStringViolation("pop_back", 0);
	--m_length;
}

inline int FixedLengthString::compare(const FixedLengthString &s) const
{
	int size1 = size();
	int size2 = s.size();
	int sz = (size1 < size2) ? size1 : size2;
	for (int i = 0; i < sz; ++i)
	{
		if (m_data[i] < s.m_data[i])
		{
			return -1;
		}
		else if (m_data[i] > s.m_data[i])
		{
			return 1;
		}
	}
	if (size1 > size2)
	{
		return 1;
	}
	else if (size2 > size1)
	{
		return -1;
	}
	return 0;
}

inline bool operator<(const Quackle::FixedLengthString &lhs, const Quackle::FixedLengthString &rhs)
{
	return (lhs.compare(rhs) < 0);
}

} // end namespace

inline bool operator==(const Quackle::FixedLengthString &lhs, const Quackle::FixedLengthString &rhs)
{
	return (lhs.compare(rhs) == 0);
}

inline bool operator!=(const Quackle::FixedLengthString &lhs, const Quackle::FixedLengthString &rhs)
{
	return (lhs.compare(rhs) != 0);
}

#endif
