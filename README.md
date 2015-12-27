Quackle   ![Icon](https://github.com/quackle/quackle/raw/master/IconSmall.png)
=======

[![Build Status](https://secure.travis-ci.org/quackle/quackle.png?branch=master)](http://travis-ci.org/quackle/quackle)

Crossword game artificial intelligence and analysis tool.

See LICENSE in this directory.

Building Quackle:
-----------------
Quackle is built and tested with the latest release of Qt 4.8.  It does not presently build against Qt 5.
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
