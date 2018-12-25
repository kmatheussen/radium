#!/bin/sh

set -e
#set -x

echo "BENCHING VEC"
`pwd`/bin/packages/faust2/compiler/faust -I `pwd`/bin/packages/faust2/architecture -I `pwd`/audio -vec -a `pwd`/bin/packages/faust2/architecture/bench.cpp $@ >gakk.cpp
g++ -I `pwd`/bin/packages/faust2/architecture -Iaudio gakk.cpp -O3 -lpthread -lm
./a.out -c 64 -i 16
./a.out -c 64 -i 32
./a.out -c 64 -i 64
./a.out -c 64 -i 64
./a.out -c 1024 -i 20

echo "BENCHING non-VEC"
`pwd`/bin/packages/faust2/compiler/faust -I `pwd`/bin/packages/faust2/architecture -I `pwd`/audio -Iaudio -a `pwd`/bin/packages/faust2/architecture/bench.cpp $@ >gakk.cpp
g++ -I `pwd`/bin/packages/faust2/architecture -Iaudio gakk.cpp -O3 -lpthread -lm
./a.out -c 64 -i 16
./a.out -c 64 -i 32
./a.out -c 64 -i 64
./a.out -c 64 -i 64
./a.out -c 1024 -i 20
