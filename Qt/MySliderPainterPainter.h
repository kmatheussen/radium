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

#ifndef QT_SLIDERPAINTERPAINTER_H
#define QT_SLIDERPAINTERPAINTER_H

namespace cvs{

  static inline void SLIDERPAINTERPAINTER_paint(MyPainter *p, int x1, int y1, int x2, int y2, bool is_enabled, float val, std::string text, bool alternative_color, bool is_hightlighted = false){
#if 0 //def JUCE_API // Must do this to make the colors look equal. Don't know why, could be that the color mixing functions used for the two API's work differently.
    static MyColor gray(80,80,80);
#else
    static MyColor gray(200,200,200);
#endif
    int height = y2-y1;
    int width = x2-x1;

    MyColor col1;
    MyColor col1b;
    
    if(is_enabled){
      MyColor c(SLIDER1_COLOR_NUM);
      
      col1 = c;
      col1b = MyColor(SLIDER2_COLOR_NUM).lighter(100);
    }else{
      int col1num = SLIDER_DISABLED_COLOR_NUM;
      col1 =  mix_mycolors(MyColor(col1num),              gray, 0.8);
      col1b = mix_mycolors(MyColor(col1num).lighter(110), gray, 0.8);
    }
    
    if(alternative_color==true)
      col1 = MyColor(200,200,200);

    if (is_hightlighted) {
      col1 = col1.lighter(150);
      col1b = col1b.lighter(150);
    }
    
#if 0
    if(alternative_color==false){
      col1.setAlpha(80);
      col1b.setAlpha(100);
    }else{
      col1.setAlpha(120);
      col1b.setAlpha(120);
    }
#endif
    
    if(height > width){ // i.e. vertical
      int pos=scale(val,0,1,0,height);
      p->fillRect(0,pos,width,height,col1);
    }else{

      int pos=scale(val,0,1,0,width);

      p->setGradient(0,0,width,height*3/4,
                     alternative_color==false ? col1.lighter(100) : col1.lighter(150),
                     col1b);

#if 0
      p->setPen(QPen(QColor(Qt::gray).light(50),1));
      p->setBrush(gradient);
      p->drawRect(0   ,0, pos, height);
      p->setBrush(QBrush());
#endif

      p->fillRect(0,0,pos,height,col1);
      p->unsetGradient();

      p->drawRect(0,0,pos,height,gray.lighter(50));

    }


#if 0
    p->setPen(QPen(colors[11].light(110),1));
    p->drawRect(0,0,width,height);
#endif

#ifndef JUCE_API
    p->drawRect(0,0,width,height,get_qcolor(HIGH_BACKGROUND_COLOR_NUM).lighter(110));
#else
    p->drawRect(0,0,width,height,mix_mycolors(MyColor(1,1,1), MyColor(), 0.3));
#endif

    //QRect rect(5,2,width-5,height-2);
    MyRect rect(5,2,width,height);

    if(text.compare("")!=0){
      MyColor c(get_qcolor(SLIDER_TEXT_COLOR_NUM));
#ifndef JUCE_API
      if(is_enabled)
        c.setAlpha(160);
      else
        c.setAlpha(60);
#endif
      p->drawText(rect, text, c);
    }
}
 
} // namespace cvs



#endif // QT_SLIDERPAINTERPAINTER_H
