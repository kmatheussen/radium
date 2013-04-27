/* Copyright 2013 Kjetil S. Matheussen

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


#ifndef MYLINEMYRECT_H
#define MYLINEMYRECT_H


namespace cvs{

struct MyLine{
  int x1,y1,x2,y2;
  MyLine(int x1,int y1, int x2, int y2)
    : x1(x1),y1(y1),x2(x2),y2(y2)
  {}
};

struct MyRect{
  int x1,y1,x2,y2;
  MyRect(int x1,int y1, int x2, int y2)
    : x1(x1),y1(y1),x2(x2),y2(y2)
  {}
  int width() const {return x2-x1;}
  int height() const {return y2-y1;}
};
}

#endif // MYLINEMYRECT_H
