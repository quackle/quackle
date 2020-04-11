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

void spit(int i, string prefix, char counts[28]) {

    int index = i * 7;
    unsigned int p = (dawg[index] << 16) + (dawg[index + 1] << 8) + (dawg[index + 2]);
    char c = dawg[index + 3];     

    bool t = false;
    bool lastchild = false;
		bool insmallerdict = false;
    
    if (c & 32) {
        t = true;
    }
    if (c & 64) {
        lastchild = true;
    }
		if (c & 128) {
				insmallerdict = true;
		}

    c = (c & 31) + 'A';

    if ((counts[c - 'A'] >= 1) || (counts[26] >= 1) || (counts[27] >= 1)) {
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
            for (int j = 0; j < 27; j++) {
                if (counts[j] > 0) {
                    usedall = false;
                }
            }
            if (usedall) {
                cout << prefix << c;
								if (!insmallerdict) cout << "#";
								cout << endl;
            }
        }

        if (p != 0) {
            spit(p, prefix + c, counts);
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
    ifstream file("output.dawg", ios::in | ios::binary);

    dawg = new unsigned char[5000000];

    int i = 0;
    while (!file.eof()) {
        file.read((char*)(dawg) + i, 7);
        i += 7;
    }

    while (cin) {
        char counts[28];
        for (int j = 0; j < 28; j++) {
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
            else if ((c == '*') || (c == '/')) {
                counts[27]++;
            }
        }

        spit(1, "", counts);
    }
}
