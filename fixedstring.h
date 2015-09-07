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

#ifndef QUACKLE_FIXEDSTRING_H
#define QUACKLE_FIXEDSTRING_H

#include <cassert>
#include <string>
#include <string.h>

#define FIXED_STRING_MAXIMUM_LENGTH 40

namespace Quackle
{

class FixedLengthString 
{
  public:
    typedef char* iterator;
    typedef const char* const_iterator;
    typedef unsigned int size_type;
    typedef char& reference;
    typedef const char& const_reference;

    FixedLengthString();
    FixedLengthString(const char* s, size_type n);
    FixedLengthString(size_type n, char c);
    FixedLengthString(const char* s);
    FixedLengthString(const FixedLengthString& s);

    const_iterator begin() const;
    const_iterator end() const;
    iterator begin();
    iterator end();
    void erase(const iterator i);
    size_type length() const;
    FixedLengthString substr(size_type pos, size_type n) const;
    bool empty() const;
    size_type size() const { return length(); }
    void clear() { m_end = m_data; }
    void push_back(char c);
    void pop_back();
    const char* constData() const { return m_data; }

    int compare(const FixedLengthString& s) const;

    FixedLengthString& operator+=(char c);
    FixedLengthString& operator+=(const FixedLengthString& s);

    const_reference operator[](size_type n) const { return m_data[n]; }
    FixedLengthString& operator=(const FixedLengthString &s);

    static const unsigned int maxSize = FIXED_STRING_MAXIMUM_LENGTH;

  private:
    static const std::string dummyString; // just to get to traits
    char m_data[maxSize];
    char* m_end; // points at the terminating NULL
};


inline FixedLengthString
operator+(const FixedLengthString &lhs, const FixedLengthString& rhs)
{
    FixedLengthString str(lhs);
    str += rhs;
    return str;
}

inline FixedLengthString
operator+(char lhs, const FixedLengthString& rhs)
{
    FixedLengthString str(1, lhs);
    str += rhs;
    return str;
}

inline FixedLengthString
operator+(const FixedLengthString &lhs, char rhs)
{
    FixedLengthString str(lhs);
    str += rhs;
    return str;
}

inline
FixedLengthString::FixedLengthString()
    : m_end(m_data)
{
}

inline
FixedLengthString::FixedLengthString(const char* s, size_type n)
{
    assert(n < maxSize);
    memcpy(m_data, s, n);
    m_end = m_data + n;
}

inline
FixedLengthString::FixedLengthString(size_type n, char c)
    : m_end(m_data)
{
    assert(n < maxSize);
    for (unsigned int i = 0; i < n; ++i) {
	*m_end++ = c;
    }
}

inline
FixedLengthString::FixedLengthString(const char* s)
{
    unsigned int sz = strlen(s);
    assert(sz < maxSize);
    memcpy(m_data, s, sz);
    m_end = m_data + sz;
}

inline
FixedLengthString::FixedLengthString(const FixedLengthString& s)
{
    int sz = s.size();
    memcpy(m_data, s.m_data, sz);
    m_end = m_data + sz;
}

inline FixedLengthString & 
FixedLengthString::operator=(const FixedLengthString &s)
{
    int sz = s.size();
    memcpy(m_data, s.m_data, sz);
    m_end = m_data + sz;
    return *this;
}

inline FixedLengthString::const_iterator
FixedLengthString::begin() const
{
    return m_data;
}

inline FixedLengthString::const_iterator
FixedLengthString::end() const
{
    return m_end;
}

inline FixedLengthString::iterator
FixedLengthString::begin()
{
    return m_data;
}

inline FixedLengthString::iterator
FixedLengthString::end()
{
    return m_end;
}

inline void
FixedLengthString::erase(const iterator i)
{
    memmove(i, i+1, m_end - i);
    --m_end;
}

inline FixedLengthString::size_type
FixedLengthString::length() const
{
    return m_end - m_data;
}

inline FixedLengthString
FixedLengthString::substr(size_type pos, size_type n) const
{
    assert(pos + n <= size());
    return FixedLengthString(&m_data[pos], n);
}

inline bool
FixedLengthString::empty() const
{
    return length() == 0;
}

inline FixedLengthString & 
FixedLengthString::operator+=(char c)
{
    assert(size() < maxSize - 1);
    *m_end++ = c;
    return *this;
}

inline FixedLengthString & 
FixedLengthString::operator+=(const FixedLengthString& s)
{
    int sz = s.size();
    assert(size() + sz < maxSize);
    memcpy(m_end, s.m_data, sz);
    m_end += sz;
    return *this;
}

inline void
FixedLengthString::push_back(char c)
{
    *this += c;
}

inline void
FixedLengthString::pop_back()
{
    assert(size() > 0);
    m_end--;
}

inline int
FixedLengthString::compare(const FixedLengthString& s) const
{
    int size1 = size();
    int size2 = s.size();
    int sz = (size1 < size2) ? size1 : size2;
    for (int i = 0; i < sz; ++i) {
	if (m_data[i] < s.m_data[i]) {
	    return -1;
	} else if (m_data[i] > s.m_data[i]) {
	    return 1;
	}
    }
    if (size1 > size2) {
	return 1;
    } else if (size2 > size1) {
	return -1;
    }
    return 0;
}

inline bool
operator<(const Quackle::FixedLengthString &lhs, const Quackle::FixedLengthString& rhs)
{
    return (lhs.compare(rhs) < 0);
}


} // end namespace

inline bool
operator==(const Quackle::FixedLengthString &lhs, const Quackle::FixedLengthString& rhs)
{
    return (lhs.compare(rhs) == 0);
}

inline bool
operator!=(const Quackle::FixedLengthString &lhs, const Quackle::FixedLengthString& rhs)
{
    return (lhs.compare(rhs) != 0);
}

#endif
