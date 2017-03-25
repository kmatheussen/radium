#!/bin/sh

set -e

echo "BENCHING VEC"
bin/packages/faust2/compiler/faust -I `pwd`/bin/packages/faust2/architecture -I `pwd`/audio -vec -a bench.cpp $@ >gakk.cpp
g++ -I `pwd`/bin/packages/faust2/architecture -Iaudio gakk.cpp -O3 -lpthread -lm
./a.out -c 64 -i 16
./a.out -c 64 -i 32
./a.out -c 64 -i 64

echo "BENCHING non-VEC"
bin/packages/faust2/compiler/faust -I `pwd`/bin/packages/faust2/architecture -I `pwd`/audio -Iaudio -a bench.cpp $@ >gakk.cpp
g++ -I `pwd`/bin/packages/faust2/architecture -Iaudio gakk.cpp -O3 -lpthread -lm
./a.out -c 64 -i 16
./a.out -c 64 -i 32
./a.out -c 64 -i 64
