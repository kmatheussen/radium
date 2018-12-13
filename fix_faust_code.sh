#!/bin/sh

sed -i s/^float\ faustpower/static\ float\ faustpower/ $2

sed -i "s/static float ftbl0$1SIG0\[65536\]/static float \*ftbl0$1SIG0\; __attribute__((constructor)) static void initialize_ftbl0$1SIG0() \{ ftbl0$1SIG0 = (float\*)calloc(sizeof(float), 65536)\;\}/" $2
sed -i "s/static float ftbl1$1SIG1\[65536\]/static float \*ftbl1$1SIG1\; __attribute__((constructor)) static void initialize_ftbl1$1SIG1() \{ ftbl1$1SIG1 = (float\*)calloc(sizeof(float), 65536)\;\}/" $2
