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

class Node {
public:
    char c;
    bool t;
    vector<Node> children;
    int pointer;
    bool lastchild;
    void pushword(string word);
    void print(string prefix);
};


vector< Node* > nodelist;


void Node::print(string prefix) {
    if (t) {
        // cout << prefix << endl;
    }

    // cout << "prefix: " << prefix << ", children: " << children.size() << endl;

    if (children.size() > 0) {    
        pointer = nodelist.size();
        children[children.size() - 1].lastchild = true;
    }

    for (int i = 0; i < children.size(); i++) {
        nodelist.push_back(&children[i]);
    }

    for (int i = 0; i < children.size(); i++) {
        children[i].print(prefix + children[i].c);
    }
}


void Node::pushword(string word) {
    if (word.length() == 0) {
        t = true;
    }
    else {
        char first = word[0];
        string rest = word.substr(1, word.length() - 1);
        int index = -1;
 
        // cout << "first: " << first << ", rest: " << rest << endl;

        for (int i = 0; i < children.size(); i++) {
            if (children[i].c == first) {
                index = i;
                i = children.size();
            }
        }
        
        if (index == -1) {
            Node n;
            n.c = first;
            n.t = false;
            n.pointer = 0;
            n.lastchild = false;
            children.push_back(n);
            index = children.size() - 1;
        }

        children[index].pushword(rest);
    }
}


int main() {
    ifstream file("dawginput.raw");

    Node root;
    root.t = false;
    root.c = '.';
    root.pointer = 0;
    root.lastchild = true;

    while(!file.eof()) {
        string word;
        file >> word;
        if (!file.eof()) {
            root.pushword(word);
        }
    }

    nodelist.push_back(&root);

    root.print("");    

    cout << "nodelist.size(): " << nodelist.size() << endl;

    ofstream out("output.dawg", ios::out | ios::binary);

    for (int i = 0; i < nodelist.size(); i++) {
        //cout << nodelist[i]->c << " " << nodelist[i]->pointer << " " << nodelist[i]->t << " " << nodelist[i]->lastchild << endl;

        unsigned int p = (unsigned int)(nodelist[i]->pointer);
        
        char bytes[4];
        unsigned char n1 = (p & 0x00FF0000) >> 16;
        unsigned char n2 = (p & 0x0000FF00) >>  8;
        unsigned char n3 = (p & 0x000000FF);
        unsigned char n4 = nodelist[i]->c - 'A';

        if (nodelist[i]->t) {
            n4 |= 32;
        }
        if (nodelist[i]->lastchild) {
            n4 |= 64;
        }

        bytes[0] = n1; bytes[1] = n2; bytes[2] = n3; bytes[3] = n4;

        out.write(bytes, 4);
    }
}
