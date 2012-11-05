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


#include <QWidget>
#include <QtGui/QGridLayout>
#include <QtGui/QScrollArea>
#include <QApplication>
#include <QPainter>
#include <stdio.h>

class MixerGui : public QWidget {

public:
  MixerGui(QWidget *parent)
    : QWidget(parent)
  {
    QWidget *mixer = new QWidget(parent);
    QGridLayout *gridLayout = new QGridLayout(mixer);  
    QScrollArea *scrollArea = new QScrollArea(mixer);

    gridLayout->addWidget(scrollArea, 0, 0, 1, 1);

    this->setGeometry(QRect(0, 0, 500, 500));
    scrollArea->setWidget(this);

    mixer->show();

    update();
  }

  void paintEvent( QPaintEvent *e ){
    printf("Got paintevent\n");
    QPainter painter(this);
    painter.drawRect(2,3,50,50);
  }
};


#ifdef TEST_MAIN
/*
  g++ -g -DTEST_MAIN -DUSE_QT_VISUAL -DDEBUG -I../Qt Qt_Mixer.cpp -Wall `pkg-config --libs --cflags QtGui` && ./a.out
  gdb ./a.out
*/

int main(int argc, char **argv){
  QApplication *qapplication=new QApplication(argc,argv);

  MixerGui *mixer = new MixerGui(NULL);
  mixer->show();

  printf("starting exec()-ing\n");
  qapplication->exec();

  return 0;
}
#endif
