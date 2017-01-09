echo "Making radium_wrap.c"

echo "// Radium-spesific parts was automaticly generated from protos.conf by protoconfparser.py." >radium_wrap.c

echo >>radium_wrap.c

cat radium_wrap/radium_wrap_1.c >> radium_wrap.c

$1 ../bin/protoconfparser.py radium_wrap.c >> radium_wrap.c

cat radium_wrap/radium_wrap_3.c >> radium_wrap.c
