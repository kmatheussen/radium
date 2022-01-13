#!/usr/bin/env bash

sed -i s/^float\ faustpower/static\ float\ faustpower/ $2

function fixit () {
    sed -i "s/static float ftbl0$1SIG0\[$3\]/static float \*ftbl0$1SIG0\; __attribute__((constructor)) static void initialize_ftbl0$1SIG0() \{ ftbl0$1SIG0 = (float\*)calloc(sizeof(float), $3)\;\}/" $2
    sed -i "s/static float ftbl1$1SIG1\[$3\]/static float \*ftbl1$1SIG1\; __attribute__((constructor)) static void initialize_ftbl1$1SIG1() \{ ftbl1$1SIG1 = (float\*)calloc(sizeof(float), $3)\;\}/" $2
}

fixit $1 $2 65536
fixit $1 $2 246

