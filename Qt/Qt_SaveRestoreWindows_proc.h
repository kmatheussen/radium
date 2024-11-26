#pragma once

class QWidget;

extern bool getDoSaveRestoreWindows(void);
extern void setDoSaveRestoreWindows(const bool doit);

extern void saveWindowsState(const QWidget *mainWindow);
extern void restoreWindowsState(QWidget *mainWindow);
