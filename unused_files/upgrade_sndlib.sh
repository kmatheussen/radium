#!/bin/sh

# If we want to compare the versions, or revert.
rm -fr sndlib_old
mv -f sndlib sndlib_old

rm -fr ../../*sndlib*.o sndlib.tar.gz
wget ftp://ccrma-ftp.stanford.edu/pub/Lisp/sndlib.tar.gz
tar xvzf sndlib.tar.gz

meld sndlib_old sndlib
