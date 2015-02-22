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

alkamid's mod
=============

I added a few partisan lines in order for the "test" program in test/ to output playability values. The algorithm is as follows:

1. Generate a list of the best moves (static evaluation, speedy player). Note the value of the move and the tiles played.
2. Go down the list. Add the moves to the "best list" until you see that a different set of tiles is used OR a word composed of the same set of tiles has a lower value.
3. Print the best move(s) and the difference between them and the "next best".

If I understand correctly, this is more or less how John O'Laughlin generated his [playability list](http://pages.cs.wisc.edu/~o-laughl/collins/). Please correct me if I am wrong.

### Evaluate rack values

ADUREIL is a much better rack than ZQAAAAE, but how do we quantify it? Quackle uses the following method: play a game of several moves, then set the rack and see what is the best move that we can play. Repeat this ten times or so and average to get an estimated *rack value*. Do the following to calculate the value of all racks: (I'll use Polish as an example)

```shell
./test --mode=enumerate --lexicon=osps --alphabet=polish > racks
```

You'll get a list of all racks and their probabilities:

```
?DĘGHKT 1.34936e-08
?DĘGHKU 8.99575e-09
?DĘGHKW 1.79915e-08
?DĘGHKY 1.79915e-08
?DĘGHKZ 2.24894e-08
```

Delete the first three lines from the file `racks`:

```
Starting up.couldn't open gaddag 
Performance without gaddag won't be quite so awesome.
..
```

Run rack evaluation:

```shell
./test --mode=staticleaves --lexicon=osps --alphabet=polish > leaves.txt
```

This will take a while. You'll end up with a file that looks like this:

```
??AAAFÓ  -AAAFÓ (score = 0, equity = 48.3516, win% = 0)
??AAAFP  8H PArAFkA (score = 80, equity = 78.7943, win% = 0)
```

We are interested in `equity` of each rack. I got a few racks with a negative value — not sure why. I filtered them out from the big list (`python findNegativeLeaves.py`) and run `--mode=staticleaves` on them again.

Now, these equity values are absolute so we want to average them. First filter the list into a "RACK VALUE" format (`python filterLeaves.py`) and take an average of all the racks (for Polish it's something like 30-40) and subtract it from each rack's equity to get a list of valid *rack value* estimations.

The next step is to calculate the value of n<7 letter racks. I'll write a script to do this soon and post it here.