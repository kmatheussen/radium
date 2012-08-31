#!/bin/sh

UIC=$1
MOC=$2
V=$3

# * Creates the .h and .cpp files for the qt designer ui files.
# * Replace "protected" with "public" in the header files. (to avoid having to make subclasses)
# * Replace QLineEdit with MyQLineEdit so that we can override focusIn

UIFILE=qt4_$3.ui
#UIFILE=$3.ui


$UIC $UIFILE | sed s/protected/public/ | sed s/QLineEdit/FocusSnifferQLineEdit/ | sed s/QSpinBox/FocusSnifferQSpinBox/ | sed s:\#include\ \<QtGui/FocusSnifferQLineEdit\>://\ \ qlineedit.h: | sed s:\#include\ \<QtGui/FocusSnifferQSpinBox\>://\ \ qspingox.h: >Qt_$3.h

#$UIC $UIFILE | sed s/protected/public/ | sed s/QSpinBox/FocusSnifferQSpinBox/ | sed s:\#include\ \<QtGui/FocusSnifferQLineEdit\>://\ \ qlineedit.h: | sed s:\#include\ \<QtGui/FocusSnifferQSpinBox\>://\ \ qspingox.h: >Qt_$3.h

$MOC Qt_$3_callbacks.h >mQt_$3_callbacks.h

#echo "class 

#$UIC -impl Qt_$3.h $UIFILE | sed s/QLineEdit/FocusSnifferQLineEdit/ | sed s/QSpinBox/FocusSnifferQSpinBox/ >Qt_$3.cpp
#$MOC Qt_$3.h >>Qt_$3.cpp
