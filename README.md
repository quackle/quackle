Quackle   ![Icon](https://github.com/quackle/quackle/raw/master/IconSmall.png)
=======

[![Travis build status](https://secure.travis-ci.org/quackle/quackle.png?branch=master)](http://travis-ci.org/quackle/quackle)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/ttcu5vruvcdljwel/branch/master?svg=true)](https://ci.appveyor.com/project/jfultz/quackle/branch/master)

Crossword game artificial intelligence and analysis tool.

See LICENSE in this directory.

Building Quackle:
-----------------
Quackle is built and tested with the latest release of Qt 5.9.
See README.MacOS and README.Windows for platform-specific instructions.  Generally:

Clone the repo or download the tarball and untar.  Use qmake to build quackle.pro and quackleio/quackleio.pro:

	qmake quackle.pro && make
	cd quackleio && qmake && make && cd ..

Finally, build the main binary.

	cd quacker && qmake && make

The binary will build as 'Quackle'.  It might be found in the quacker directory or in the release subdirectory.


File organization:
------------------
* quackle/ - libquackle sources.  libquackle is the engine, and can be linked to any convenient interface.  It does not use Qt.
* quackle/quackleio/ - I/O library for Quackle.  Implements stuff for accessing dictionaries, serializing GCG files, etc.  Also, command-line option handling.  This does have some modest dependencies on Qt.
* quackle/quacker/ - code for full Quackle UI.  Written in Qt, and requires libquackleio and libquackle.
* quackle/makeminidawg/ - standalone console program for building Quackle dictionaries.
* quackle/makegaddag/ - standalone console program for building gaddag files.
* quackle/data/ - lexicons, strategy files, and alphabet resources for Quackle.
In this directory is libquackle. Run qmake and then run make in this directory. Then cd to quackle/quackleio/, run qmake, and then run make.


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

Now, these equity values are absolute so we want to average them. First filter the list into a "RACK VALUE" format (`python filterLeaves.py`) and take an average of all the racks (for Polish it's something like 30-40) and subtract it from each rack's equity to get a list of valid *rack value* estimations. (`python calculateAverage.py`)

The next step is to calculate the value of n<7 letter racks. First we need to generate them — for this I recommend changing this line in `testharness.cpp`:

```cpp
E.enumerate(&racks);
```

to

```cpp
E.enumerate(&racks, n);
```

where `n` is the number of letters. Then compile the program and run:

```shell
./test --mode=enumerate --lexicon=osps --alphabet=polish > racksn.txt
```

then strip these lists (`stripRacks.py`) and sort them (`sortRacks.py`). I know, it's a lot of scripts, maybe I'll write a wrapper one day. Finally, we can run `partialRacks.py` and marge the resulting files into one big **leave value** list:

```shell
cat rack2output.txt rack3output.txt rack4output.txt rack5output.txt rack6output.txt rack7output.txt > superleaves.raw
```

Now we copy this file to `encodeleaves` in the main quackle folder, compile it and run:
```shell
qmake
make
./encodeleaves
```

After a minutes or so, we'll have a `superleaves` file that we can copy to `data/strategy/yourlexicon`. From now on, Quackle will take leave value into account for static move evaluation.
