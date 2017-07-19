/* Copyright 2012 Kjetil S. Matheussen

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA. */

// Based on: (although probably not much left of anymore)


/****************************************************************************
**
** Copyright (C) 2012 Nokia Corporation and/or its subsidiary(-ies).
** All rights reserved.
** Contact: Nokia Corporation (qt-info@nokia.com)
**
** This file is part of the demonstration applications of the Qt Toolkit.
**
** $QT_BEGIN_LICENSE:LGPL$
** GNU Lesser General Public License Usage
** This file may be used under the terms of the GNU Lesser General Public
** License version 2.1 as published by the Free Software Foundation and
** appearing in the file LICENSE.LGPL included in the packaging of this
** file. Please review the following information to ensure the GNU Lesser
** General Public License version 2.1 requirements will be met:
** http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html.
**
** In addition, as a special exception, Nokia gives you certain additional
** rights. These rights are described in the Nokia Qt LGPL Exception
** version 1.1, included in the file LGPL_EXCEPTION.txt in this package.
**
** GNU General Public License Usage
** Alternatively, this file may be used under the terms of the GNU General
** Public License version 3.0 as published by the Free Software Foundation
** and appearing in the file LICENSE.GPL included in the packaging of this
** file. Please review the following information to ensure the GNU General
** Public License version 3.0 requirements will be met:
** http://www.gnu.org/copyleft/gpl.html.
**
** Other Usage
** Alternatively, this file may be used in accordance with the terms and
** conditions contained in a signed written agreement between you and Nokia.
**
**
**
**
**
** $QT_END_LICENSE$
**
****************************************************************************/

#ifndef VIEW_H
#define VIEW_H

#include <qapplication.h>
#include <qtoolbutton.h>
#include <qstyle.h>
#include <qscrollbar.h>

//#include <QFrame>
#include <qgraphicsview.h>
#include <qslider.h>
#include <qlabel.h>
#include <qboxlayout.h>

//QT_FORWARD_DECLARE_CLASS(QLabel)
//QT_FORWARD_DECLARE_CLASS(QSlider)
//QT_FORWARD_DECLARE_CLASS(QToolButton)

class View;

class GraphicsView : public QGraphicsView
{
  Q_OBJECT
public:
  GraphicsView(View *v) : QGraphicsView(), view(v) {
    //qRegisterMetaType<GraphicsView>("GraphicsView");
  }

protected:
    void wheelEvent(QWheelEvent *) override;

private:
    View *view;
};

class View : public QWidget
{
    Q_OBJECT
public:
    View(const QString &name, QWidget *parent = 0);

    QGraphicsView *view() const;

public slots:
    void zoomIn(int level = 1);
    void zoomOut(int level = 1);

private slots:
    void resetView();
    void setResetButtonEnabled();
    void setupMatrix();
    //void togglePointerMode();
    //void toggleOpenGL();
    //void toggleAntialiasing();
    //void print();
    void rotateLeft();
    void rotateRight();

private:
    GraphicsView *graphicsView;
    //QLabel *label;
    //QLabel *label2;
    //QToolButton *selectModeButton;
    //QToolButton *dragModeButton;
    //QToolButton *openGlButton;
    //QToolButton *antialiasButton;
    //QToolButton *printButton;
    QToolButton *resetButton;
    QSlider *zoomSlider;
    QSlider *rotateSlider;
};

#endif
