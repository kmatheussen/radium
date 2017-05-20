#!/bin/sh

# If we want to compare the versions, or revert.
rm -fr s7_old
mv -f s7 s7_old

rm -fr ../../*s7*.o s7.tar.gz
wget ftp://ccrma-ftp.stanford.edu/pub/Lisp/s7.tar.gz
tar xvzf s7.tar.gz
