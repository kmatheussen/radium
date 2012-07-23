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

#include <math.h>

#include <qfontdialog.h>

#include "../common/nsmtracker.h"
#include "MyWidget.h"

#include "Qt_Fonts_proc.h"


void setFontValues(struct Tracker_Windows *tvisual, const QFont &font){
  QFontMetrics fm(font);

  double width3 = R_MAX(fm.width("D#6"), R_MAX(fm.width("MUL"), fm.width("STP")));
  tvisual->fontwidth=(int)ceil(width3/3.0);
  tvisual->org_fontheight=fm.height();
  tvisual->fontheight=fm.height();
}

//bool GFX_SelectEditFont(struct Tracker_Windows *tvisual){return true;}
char *GFX_SelectEditFont(struct Tracker_Windows *tvisual){
  MyWidget *mywidget=(MyWidget *)tvisual->os_visual.widget;
  mywidget->font = QFontDialog::getFont( 0, mywidget->font ) ;
  mywidget->setFont(mywidget->font);

  setFontValues(tvisual, mywidget->font);
  return talloc_strdup((char*)mywidget->font.toString().ascii());
}

void GFX_IncFontSize(struct Tracker_Windows *tvisual, int pixels){
  MyWidget *mywidget=(MyWidget *)tvisual->os_visual.widget;
  if(false && mywidget->font.pixelSize()!=-1){
    // not used
    mywidget->font.setPixelSize(mywidget->font.pixelSize()+pixels);
  }else{
    float org_size = mywidget->font.pointSize();
    for(int i=1;i<100;i++){
      mywidget->font.setPointSize(org_size+(i*pixels));
      if(mywidget->font.pointSize()!=org_size)
        goto exit;
    }
    for(float i=1;i<100;i++){
      mywidget->font.setPointSize(org_size+(pixels/i));
      if(mywidget->font.pointSize()!=org_size)
        goto exit;
    }
  }
 exit:
  setFontValues(tvisual,mywidget->font);
}


int GFX_get_text_width(struct Tracker_Windows *tvisual, char *text){
  MyWidget *mywidget=(MyWidget *)tvisual->os_visual.widget;
  const QFontMetrics fn = QFontMetrics(mywidget->font);
  return fn.width(text);
}

static bool does_text_fit(const QFontMetrics &fn, const QString &text, int pos, int max_width){
  return fn.width(text, pos) <= max_width;
}

static int average(int min, int max){
  return (1+min+max)/2;
}

// binary search
static int find_text_length(const QFontMetrics &fn, const QString &text, int max_width, int min, int max){
  if(max<=min)
    return min;

  int mid = average(min,max);

  if(does_text_fit(fn, text, mid, max_width))
    return find_text_length(fn, text, max_width, mid, max);
  else
    return find_text_length(fn, text, max_width, min, mid-1);
}

int GFX_get_num_characters(struct Tracker_Windows *tvisual, char *text, int max_width){
  MyWidget *mywidget=(MyWidget *)tvisual->os_visual.widget;
  const QFontMetrics fn = QFontMetrics(mywidget->font);
  int len = strlen(text);
  QString string(text);

  //printf("width: %d / %d / %d\n",fn.width(string,len), fn.width(string,len/2), max_width);

  if(does_text_fit(fn, string, len, max_width))
    return len;
  else
    return find_text_length(fn, string, max_width, 0, len-1);
}

