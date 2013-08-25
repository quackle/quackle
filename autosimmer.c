// NOTICE: this is old, obsolete code.

#include <stdio.h>
#include <stdlib.h>
#include <termios.h>
#include <fstream>
#include <iostream>
#include <iomanip>
#include <vector>
#include <map>

#include "board.h"
#include "bag.h"
#include "util.h"
#include "generator.h"

using namespace std;
using namespace Utility;

Generator G;
Board B;
Bag S;

Rack R[2];

vector<Move> movelist;
vector<double> sums;
vector<int> ns;

map<string, bool> twl;
map<string, int> playability;

int puzzlenumber;

vector<string> spitwords(Move M, Board B) {
    vector<string> words;

    if (M.action == Place) {
        string mainword = "";
        int row = M.startrow;
        int col = M.startcol;
        for (int i = 0; i < M.tiles.length(); i++) {
            if (isalpha(M.tiles[i])) {
                mainword += toupper(M.tiles[i]);
                string hookword = "";
                int hookrow = row;
                int hookcol = col;
                if (M.horizontal) {
                    hookrow--;
                }
                else {
                    hookcol--;
                }
                while((hookrow >= 0) && (hookcol >= 0)) {
                    if (!isalpha(B.letters[hookrow][hookcol])) {
                        break;
                    }
                    string dummy = "";
                    dummy += toupper(B.letters[hookrow][hookcol]);
                    hookword = dummy + hookword;
                    if (M.horizontal) {
                        hookrow--;
                    }
                    else {
                        hookcol--;
                    }
                }
                hookword += toupper(M.tiles[i]);
                hookrow = row;
                hookcol = col;
                if (M.horizontal) {
                    hookrow++;
                }
                else {
                    hookcol++;
                }
                
                while((hookrow < 15) && (hookcol < 15)) {
                    if (!isalpha(B.letters[hookrow][hookcol])) {
                        break;
                    }
                    hookword += toupper(B.letters[hookrow][hookcol]);
                    if (M.horizontal) {
                        hookrow++;
                    }
                    else {
                        hookcol++;
                    }
                }
                if (hookword.length() > 1) {
                    words.push_back(hookword);
                }
            }
            else {
                mainword += toupper(B.letters[row][col]);
            }

            if (M.horizontal) {
                col++;
            }
            else {
                row++;
            }
        }
        
        words.push_back(mainword);
    }

    return words;
}

Move sim(Rack &currR) {
	// cout << B << endl;
	// cout << "Rack: " << currR << endl;

	int num = 20;
	int n = 1000;

	movelist.clear();

	// cout << "G.movelist.size(): " << G.movelist.size() << endl;

    for (int i = 0; i < G.movelist.size(); i++) {
        int insertpos = 0;
        bool insert = false;
        for (int j = num - 1; j >= 0; j--) {
           if (j < movelist.size()) {
               if (G.movelist[i].equity > movelist[j].equity) {
                   insertpos = j;
                   insert = true;
               }
           }
        }

        if (insert) {
            vector<Move>::iterator it = movelist.begin();
            for (int j = 0; j < insertpos; j++) {
                it++;
            }
            movelist.insert(it, 1, G.movelist[i]);
        }
        else {
            movelist.push_back(G.movelist[i]);
        }
    }

	vector<string> mainwords;
	for (int i = 0; i < num; i++)
	{
		string word;
		if (movelist[i].action == Pass)
		{
			word = "pss";
		}
		else if (movelist[i].action == Exchange)
		{
			word = "xch";
		}
		else if (movelist[i].action == Place)
		{
			vector<string> words = spitwords(movelist[i], B);
			word = words[words.size() - 1];
		}

		mainwords.push_back(word); 
	}

    Bag origS;
    for (int i = 0; i < 15; i++) {
        for (int j = 0; j < 15; j++) {
            string c = "";
            if (isalpha(B.letters[i][j])) {
                if (isupper(B.letters[i][j])) {
                    c += B.letters[i][j];
                }
                else {
                    c = "?";
                }
            }
            Rack dummy;
            origS.fill(dummy, c);
        }
    }

	sums.clear();
	ns.clear();
    for (int i = 0; i < num; i++) {
        sums.push_back(0);
        ns.push_back(0);
    }

    for (int k = 0; k < n; k++) 
	{
        for (int i = 0; i < num; i++) 
		{
            Board orig = B;
            G.setboard(B);
            G.allcrosses();
            G.setrecordall(false);
            Move M[3];
            Rack sR[2];
            Bag simBag = origS;

            simBag.fill(sR[0], currR.tiles);
            simBag.refill(sR[1]);

            for (int j = 0; j < 3; j++) {
                if (j > 0) {
                    G.setrack(sR[j % 2]);

                    bool canexch = false;
                    if (simBag.size() >= 7) {
                        canexch = true;
                    }

                    M[j] = G.findstaticbest(canexch);
                }
                else {
                    M[j] = movelist[i];
                }

                if (M[j].action == Place) {
                    G.makemove(M[j]);

                    sR[j % 2] -= M[j];
                    simBag.refill(sR[j % 2]);
                }
                else if (M[j].action == Exchange) {
                    simBag.exch(M[j], sR[j % 2]);
                }
            }

            sums[i] += M[0].score - M[1].equity + M[2].equity;
            ns[i]++;
        }

/*
        if (((k + 1) % 10) == 0) {
            cout << "After " << k + 1 << " iterations:" << endl;
            for (int i = 0; i < num; i++) {
                cout << movelist[i] << " (" << movelist[i].score << ") " << sums[i] / ns[i] << endl;
            }
        }
*/

    }

	bool switching = true;
	while (switching)
	{
		switching = false;
		for (int i = 0; i < num - 1; i++)
		{
			if (sums[i] / ns[i] < sums[i + 1] / ns[i + 1])
			{
				double tempsum = sums[i];
				sums[i] = sums[i + 1];
				sums[i + 1] = tempsum;
				
				int tempn = ns[i];
				ns[i] = ns[i + 1];
				ns[i + 1] = tempn;
	
				switching = true;
			}			
		}
	}

	string hintfile = "hint";
	string answfile = "answ";
	for (int i = 1000; i >= 1; i /= 10)
	{
		int digit = (puzzlenumber % (i * 10)) / i;
		hintfile += (char)(digit + '0');
		answfile += (char)(digit + '0');
	}
 
	ofstream hint(hintfile.c_str());
	ofstream answ(answfile.c_str());

	hint << B << endl;
	hint << currR << endl; 
	hint << endl;
		
	for (int i = 0; i < num; i++)
	{
		if (i + 1 < 10) 
			hint << " ";
		hint << i + 1 << ". ";
		
		if (movelist[i].action == Pass)
			hint << "Pass" << endl;
		else if (movelist[i].action == Exchange)
			hint << "Exchange" << endl;
		else if (movelist[i].action == Place)
		{
			if (twl[mainwords[i]])
				hint << "TWL ";
			else
				hint << "OSW ";

			hint << mainwords[i].length() << "lw, ";
			
			int pb = playability[mainwords[i]];

			if (pb >= 25000)
				hint << "pb: very high";
			else if (pb >= 10000)
				hint << "pb: high";
			else if (pb >= 5000)
				hint << "pb: medium";
			else if (pb >= 2500)
				hint << "pb: low";
			else
				hint << "pb: very low";
			
			hint << endl;
		}
	}
	
	hint.close();
	
	answ << B << endl;
	answ << currR << endl; 
	answ << endl;
		
	for (int i = 0; i < num; i++)
	{
		if (i + 1 < 10) 
			answ << " ";
		answ << i + 1 << ". ";
		
		answ << movelist[i] << " [" << sums[i] / ns[i] << "] ";

		if (movelist[i].action == Place)
		{
			int pb = playability[mainwords[i]];

			answ << "(" << mainwords[i];
			if (!twl[mainwords[i]])
				answ << "#";
			answ << " = ";
			answ << pb << ")";
	
		}
		
		answ << endl;
	}
	
	answ.close();
	
	Move bestMove;
	double bestEquity = -1000;

	for (int i = 0; i < num; i++)
	{
		double equity = sums[i] / ns[i];
		if (equity > bestEquity)
		{
			bestMove = movelist[i];
			bestEquity = equity;
		}
	}

	return bestMove;
}
        
int main() {
    G.loaddawg("sowpods.dawg"); 
    G.loadworths("mavenworths");
    G.loadsyn2("fakesyn2");
   
    cout << "Lexicon loaded." << endl;

	ifstream countfile("sowpods-counts.txt");

    while(!countfile.eof()) {
        string w;
        int c;
        countfile >> c;
        countfile >> w;
        if (!countfile.eof()) {
            playability[w] = c;
        }
    }

	cout << "Playability crap loaded." << endl;

    ifstream twlfile("twl.raw");
                                                                                
    while(!twlfile.eof()) {
        string w;
        twlfile >> w;
        twl[w] = true;
    }

	cout << "TWL loaded." << endl;
                                                                                
    srand(time(NULL));

	puzzlenumber = 1;
  
	for (int i = 0; i < 1000; i++)
	{
		B.init();
		S.init();
		S.refill(R[0]);
		S.refill(R[1]);

		for (int j = 0; j < 10; j++)
		{
			G.setboard(B);
			G.allcrosses();
			G.setrack(R[j % 2]);
			G.setrecordall(true);
			G.findstaticbest(true);

			Move M = sim(R[j % 2]);

			if (M.action == Place)
			{
				B.makemove(M);
				R[j % 2] -= M;
				S.refill(R[j % 2]);
			}
			else if (M.action == Exchange)
			{
				S.exch(M, R[j % 2]);
			}

			puzzlenumber++;
		}
	}			
}
