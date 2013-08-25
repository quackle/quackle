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

#include <stdio.h>
#include <string>
#include <fstream>
#include <iostream>

using namespace std;

int main() {
    ifstream file("input");

    while(!file.eof()) {
        string word;
        file >> word;
        if (!file.eof()) {
            for (signed int i = 1; i <= word.length(); i++) {
                for (int j = i - 1; j >= 0; j--) {
                    cout << word[j];
                }

                if (i < word.length()) {
                    cout << "^";
                    for (int j = i; j < word.length(); j++) {
                        cout << word[j];
                    }
                }

                cout << endl;
           }
        }
    }
}
               
                
