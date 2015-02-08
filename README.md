Quackle
=======

[![Build Status](https://secure.travis-ci.org/quackle/quackle.png?branch=master)](http://travis-ci.org/quackle/quackle)

Crossword game artificial intelligence and analysis tool.

See LICENSE in this directory.

In this directory is libquackle. Run qmake and then run make in this directory. Then cd to quackle/quackleio/, run qmake, and then run make.

In quacker/ is a GUI that uses libquackle. It requires Qt 4. Run qmake then make in that directory to build it. Then, from within quackle/quacker/, run ./release/quacker to start the program.

See README.MacOS and README.Windows for platform-specific instructions.

olaughlin@gmail.com
jasonkatzbrown@gmail.edu
jfultz@wolfram.com
matt.liberty@gmail.com

### alkamid's mod

I added a few partisan lines in order for the "test" program in test/ to output playability values. The algorithm is as follows:

1. Generate a list of the best moves (static evaluation, speedy player). Note the value of the move and the tiles played.
2. Go down the list. Add the moves to the "best list" until you see that a different set of tiles is used OR a word composed of the same set of tiles has a lower value.
3. Print the best move(s) and the difference between them and the "next best".

If I understand correctly, this is more or less how John O'Laughlin generated his [playability list](http://pages.cs.wisc.edu/~o-laughl/collins/). Please correct me if I am wrong.