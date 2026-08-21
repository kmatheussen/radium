#if !defined(NO_PRAGMA_ONCE)
#  pragma once
#endif

#include <string>
#include <functional>
#include <memory>

#include <QApplication>
#include <QWidget>
#include <QString>
#include <QVector>
#include <QPainter>
#include <QMouseEvent>
#include <QCloseEvent>
#include <QDir>
#include <QFile>
#include <QPushButton>
#include <QTimer>
#include <QTime>
#include <QTextStream>
#include <QMap>
#include <memory>
#include <vector>
#include <QString>
#include <QVector>
#include <QWidget>
#include <QPainter>
#include <QDialog>
#include <QMouseEvent>
#include <QThread>
#include <QSet>
#include <QHash>
#include <QFileInfo>

/*
#include <QtWidgets>
#include <QtGui>
#include <QtCore>
*/
#include <QAccessible> // Has dirty use of memset in header. Include here to avoid error in mem_type_assertions.h


#define INCLUDE_SNDFILE_OPEN_FUNCTIONS 1
#define SEQBLOCK_USING_VECTOR 1
#include "../common/nsmtracker.h"

#include "helpers.h"
#include "FocusSniffers.h"

#include "../common/Vector.hpp"

/*
#include "../audio/Peaks.hpp"
#include "../common/TimeData.hpp"
#include "../common/hashmap_proc.h"
*/
