#!/bin/sh

UIC=$1
MOC=$2
V=$3

# * Creates the .h and .cpp files for the qt designer ui files.
# * Replace "protected" with "public" in the header files. (to avoid having to make subclasses)
# * Replace QLineEdit with MyQLineEdit so that we can override focusIn

$UIC $3.ui | sed s/protected/public/ | sed s/QLineEdit/MyQLineEdit/ | sed s/QSpinBox/MyQSpinBox/ >Qt_$3.h
$MOC Qt_$3.h >mQt_$3.cpp
$UIC -impl Qt_$3.h $3.ui  | sed s/QLineEdit/MyQLineEdit/ | sed s/QSpinBox/MyQSpinBox/ >Qt_$3.cpp
cat mQt_$3.cpp >>Qt_$3.cpp
