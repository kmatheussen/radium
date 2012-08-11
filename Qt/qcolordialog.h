/****************************************************************************
**
** Definition of QColorDialog class
**
** Created : 990222
**
** Copyright (C) 1992-2008 Trolltech ASA.  All rights reserved.
**
** This file is part of the dialogs module of the Qt GUI Toolkit.
**
** This file may be used under the terms of the GNU General
** Public License versions 2.0 or 3.0 as published by the Free
** Software Foundation and appearing in the files LICENSE.GPL2
** and LICENSE.GPL3 included in the packaging of this file.
** Alternatively you may (at your option) use any later version
** of the GNU General Public License if such license has been
** publicly approved by Trolltech ASA (or its successors, if any)
** and the KDE Free Qt Foundation.
**
** Please review the following information to ensure GNU General
** Public Licensing requirements will be met:
** http://trolltech.com/products/qt/licenses/licensing/opensource/.
** If you are unsure which license is appropriate for your use, please
** review the following information:
** http://trolltech.com/products/qt/licenses/licensing/licensingoverview
** or contact the sales department at sales@trolltech.com.
**
** This file may be used under the terms of the Q Public License as
** defined by Trolltech ASA and appearing in the file LICENSE.QPL
** included in the packaging of this file.  Licensees holding valid Qt
** Commercial licenses may use this file in accordance with the Qt
** Commercial License Agreement provided with the Software.
**
** This file is provided "AS IS" with NO WARRANTY OF ANY KIND,
** INCLUDING THE WARRANTIES OF DESIGN, MERCHANTABILITY AND FITNESS FOR
** A PARTICULAR PURPOSE. Trolltech reserves all rights not granted
** herein.
**
**********************************************************************/

#ifndef QCOLORDIALOG_H
#define QCOLORDIALOG_H

#ifndef QT_H
#include "qdialog.h"
#endif // QT_H

#ifndef QT_NO_COLORDIALOG

class QColorDialogPrivate;

class QColorDialog3 : public QDialog
{
    Q_OBJECT

public:
  static QColor getColor( const QColor& init = Qt::white, QWidget* parent=0, const char* name=0 );
    static QRgb getRgba( QRgb, bool* ok = 0,
			 QWidget* parent=0, const char* name=0 );

    static int customCount();
    static QRgb customColor( int );
    static void setCustomColor( int, QRgb );
    static void setStandardColor( int, QRgb );

private:
    ~QColorDialog3();
    QColorDialog3( QWidget* parent=0, const char* name=0, bool modal=FALSE );

    void setColor( const QColor& );
    QColor color() const;

    bool selectColor( const QColor& );

    void setSelectedAlpha( int );
    int selectedAlpha() const;

    void showCustom( bool=TRUE );

private:	// Disabled copy constructor and operator=
    QColorDialogPrivate *d;
    friend class QColorDialogPrivate;
    friend class QColorShower;

#if defined(Q_DISABLE_COPY)
    QColorDialog3( const QColorDialog3 & );
    QColorDialog3& operator=( const QColorDialog3 & );
#endif
};

#endif

#endif //QCOLORDIALOG_H
