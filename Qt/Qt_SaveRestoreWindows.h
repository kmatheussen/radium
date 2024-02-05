#pragma once

#include <QWidget>
#include <cstdio>


void saveWindowsState(QWidget * mainWindow);
void restoreWindowsState(QWidget * mainWindow);