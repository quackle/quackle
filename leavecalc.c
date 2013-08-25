// old, unused reference code
#include <stdio.h>
#include <string>
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

Board randboard(string leave, int minleft, int maxleft) {

    int leaveatmost = minleft + rand()%(maxleft - minleft);

    Board B;
    B.init();
    Bag S;
    Rack dummy;
    S.fill(dummy, leave);

    Rack R[2];
    S.refill(R[0]);
    S.refill(R[1]);

    G.setboard(B);
 
    for (int i = 0; (i < 25) && !S.empty(); i++) {
        for (int j = 0; j < 2; j++) {

            if (S.size() <= leaveatmost) {
                return G.getboard();
            }

            // cout << S << endl;
            // cout << R << endl;
 
            G.setrack(R[j]);
 
            bool canexch = false;

            if (S.size() >= 7) {
                canexch = true;
            }

            Move M = G.findstaticbest(canexch);
            
            cout << "randboard M: " << R[j] << " " << M << " " << M.score << " " << M.equity << endl;

            if (M.action == Place) {
                G.makemove(M);
                
                R[j] -= M;
                S.refill(R[j]);
            }
            else if (M.action == Exchange) {
                S.exch(M, R[j]);
            }

            // cout << B << endl;

            R[j] -= M;
            S.refill(R[j]);
        }
    }

    return G.getboard();
}


void sim(Board orig, string leave, int n) {
    Bag origS;
    for (int i = 0; i < 15; i++) {
        for (int j = 0; j < 15; j++) {
            string c = "";
            if (isalpha(orig.letters[i][j])) {
                if (isupper(orig.letters[i][j])) {
                    c += orig.letters[i][j];
                }
                else {
                    c = "?";
                }
            }
            Rack dummy;
            origS.fill(dummy, c);
        }
    }

    for (int i = 0; i < n; i++) {
        Board B = orig;
        Rack R[2];
        Bag S = origS;
        // cout << "S: " << S << endl;
        S.fill(R[0], leave);
        // cout << "R[0] before it gets filled: " << R[0] << endl;
        S.refill(R[0]);
        S.refill(R[1]);
        Move M[3];
  
        G.setboard(B);
      
        for (int j = 0; j < 3; j++) {
            G.setrack(R[j % 2]);
            cout << "R[" << j % 2 << "]: " << R[j % 2] << endl;
 
            bool canexch = false;

            if (S.size() >= 7) {
                canexch = true;
            }

            M[j] = G.findstaticbest(canexch);

            if (M[j].action == Place) {
                B.makemove(M[j]);
                G.makemove(M[j]);
                
                R[j % 2] -= M[j];
                S.refill(R[j % 2]);
            }
            else if (M[j].action == Exchange) {
                S.exch(M[j], R[j % 2]);
            }

            cout << "M[" << j << "]: " << M[j] << " " << M[j].score << " " << M[j].equity << endl;
            cout << B << endl;

            R[j % 2] -= M[j];
            S.refill(R[j % 2]);
        }
    }
}   

int main(int argc, char** argv) {
    //G.loaddawg("twl15new.anadawg"); 
    G.loaddawg("ods.dawg");
    G.loadsyn2("syn2");

    cout << "loaded dawg" << endl;

    srand(time(NULL));
    
    string leave;

    if (argc < 2) {
        leave = "";
    }
    else {
        leave = argv[1];
    }

    for (int i = 0; i < leave.length(); i++) {
        leave[i] = toupper(leave[i]);
    }

    cout << "leave: " << leave << endl;

    for (int i = 0; i < 1000; i++) {
        Board B = randboard(leave, 0, 72);
        cout << B << endl;
        sim(B, leave, 10);
    }
}
