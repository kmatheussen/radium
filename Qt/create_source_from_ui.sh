#!/bin/bash

set -e
#set -x

UIC=$1
MOC=$2
V=$3

LABEL=MyQLabel

CHECKBOX=MyQCheckBox

if [[ $4 == DONT_USE_RADIUM_CHECKBOXES ]]
then
    CHECKBOX=QCheckBox
fi

if [[ $4 == DONT_USE_RADIUM_LABEL ]]
then
    LABEL=QLabel
fi

# * Creates the .h and .cpp files for the qt designer ui files.
# * Replace "protected" with "public" in the header files. (to avoid having to make subclasses)
# * Replace QLineEdit with MyQLineEdit so that we can override focusIn

UIFILE=qt4_$3.ui
#UIFILE=$3.ui

$UIC $UIFILE | sed s/protected/public/ | sed s/QComboBox/FocusSnifferQComboBox/| sed s/QLineEdit/FocusSnifferQLineEdit/ | sed s/QTextEdit/FocusSnifferQTextEdit/ | sed s/QListWidget/FocusSnifferQListWidget/ | sed s/QSpinBox/MyQSpinBox/| sed s/QDoubleSpinBox/FocusSnifferQDoubleSpinBox/ | sed s/QTableWidget/FocusSnifferQTableWidget/ | sed s/\ QSlider/\ MyQSlider/ | sed s/\ QCheckBox/\ $CHECKBOX/ | sed s/\ QLabel/\ $LABEL/ | sed s/\ QToolButton/\ MyQButton/ | sed s:\#include\ \<QtGui/FocusSnifferQLineEdit\>://\ \ qlineedit.h:| sed s:\#include\ \<QtWidgets/FocusSnifferQComboBox\>://\ \ qlineedit.h: | sed s:\#include\ \<QtWidgets/FocusSnifferQLineEdit\>://\ \ qlineedit.h: | sed s:\#include\ \<QtGui/FocusSnifferQTextEdit\>://\ \ qlineedit.h: | sed s:\#include\ \<QtWidgets/FocusSnifferQTextEdit\>://\ \ qlineedit.h: | sed s:\#include\ \<QtGui/FocusSnifferQListWidget\>://\ \ qlineedit.h: | sed s:\#include\ \<QtWidgets/FocusSnifferQListWidget\>://\ \ qlineedit.h: | sed s:\#include\ \<QtGui/MyQSpinBox\>://\ \ qspingox.h:| sed s:\#include\ \<QtWidgets/MyQSpinBox\>://\ \ qspingox.h:| sed s:\#include\ \<QtGui/FocusSnifferQDoubleSpinBox\>://\ \ qspingox.h: | sed s:\#include\ \<QtWidgets/FocusSnifferQDoubleSpinBox\>://\ \ qspingox.h: | sed s:\#include\ \<QtGui/FocusSnifferQTableWidget\>://\ \ qspingox.h: | sed s:\#include\ \<QtWidgets/FocusSnifferQTableWidget\>://\ \ qspingox.h: >Qt_$3.h

#$UIC $UIFILE | sed s/protected/public/ | sed s/QSpinBox/MyQSpinBox/ | sed s:\#include\ \<QtGui/FocusSnifferQLineEdit\>://\ \ qlineedit.h: | sed s:\#include\ \<QtGui/MyQSpinBox\>://\ \ qspingox.h: >Qt_$3.h


if ! [[ -s Qt_$3.h ]] ; then
    echo "Failed creating Qt_$3.h"
    rm Qt_$3.h
    exit -1
fi


$MOC Qt_$3_callbacks.h >mQt_$3_callbacks.h

if ! [[ -s mQt_$3_callbacks.h ]] ; then
    echo "Failed creating mQt_$3_callbacks.h"
    rm mQt_$3_callbacks.h
    exit -2
fi


#echo "class 

#$UIC -impl Qt_$3.h $UIFILE | sed s/QLineEdit/FocusSnifferQLineEdit/ | sed s/QSpinBox/MyQSpinBox/ >Qt_$3.cpp
#$MOC Qt_$3.h >>Qt_$3.cpp
