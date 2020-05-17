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

#include <stdio.h>
#include <string>
#include <fstream>
#include <iostream>
#include <iomanip>
#include <vector>
#include <map>

using namespace std;

unsigned char *dawg;

void spit(int i, string prefix, char counts[29]) {

    int index = i * 4;
    unsigned int p = (dawg[index] << 16) + (dawg[index + 1] << 8) + (dawg[index + 2]);
    char c = dawg[index + 3];     

    bool t = false;
    bool lastchild = false;
    
    if (c & 32) {
        t = true;
    }
    if (c & 64) {
        lastchild = true;
    }

    c = (c & 31) + 'A';
    
    if (c == 27) {
        c = 27 + 'A';
    }

    if ((counts[c - 'A'] >= 1) || (counts[26] >= 1) || (counts[28] >= 1)) {
        bool blankhere = false;
        bool letterhere = false;

        if (counts[c - 'A'] >= 1) {
            counts[c - 'A']--;
            letterhere = true;
        }
        else if (counts[26] >= 1) {
            counts[26]--;
            blankhere = true;
        }
        
        if (t) {
            bool usedall = true;
            for (int j = 0; j < 28; j++) {
                if (counts[j] > 0) {
                    usedall = false;
                }
            }
            if (usedall) {
                cout << prefix << c << endl;
            }
        }

        string neUVString = prefix;
        if (c <= 'Z') {
            neUVString += c;
        }
        else {
            neUVString += '^';
        }
 
        if (p != 0) {
            spit(p, neUVString, counts);
        }
        
        if (letterhere) { 
            counts[c - 'A']++;
        }
        else if (blankhere) {
            counts[26]++;
        }
    }

    if (!lastchild) {
        spit(i + 1, prefix, counts);
    }
}


int main() {
    dawg = new unsigned char[30000000];
    
    ifstream file("twl.gaddag", ios::in | ios::binary);

    int i = 0;
    while (!file.eof()) {
        file.read((char*)(dawg) + i, 4);
        i += 4;
    }

    while (cin) {
        char counts[29];
        for (int j = 0; j < 29; j++) {
            counts[j] = 0;
        }

        string query;
        cin >> query;
        for (int j = 0; j < query.length(); j++) {
            char c = query[j];
            if (isalpha(c)) {
                counts[toupper(c) - 'A']++;
            }
            else if ((c == '?') || (c == '.')) {
                counts[26]++;
            }
            else if (c == '^') {
                counts[27]++;
            }
            else if ((c == '*') || (c == '/')) {
                counts[28]++;
            }
        }

        spit(1, "", counts);
    }
}
