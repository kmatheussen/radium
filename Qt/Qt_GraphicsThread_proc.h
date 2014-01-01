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

#ifndef QT_GRAPHICSTHREAD_PROC_H
#define QT_GRAPHICSTHREAD_PROC_H

#include <QImage>
#include <QPixmap>
#include <QGLFramebufferObject>

//typedef QImage BackBuffer;
//typedef QPixmap BackBuffer;
#if 0
struct BackBuffer : QPixmap {
 BackBuffer(int width, int height, QImage::Format format) 
   : QPixmap(width, height)
    {}
};
#elif 1
struct BackBuffer : QImage {
 BackBuffer(int width, int height, QImage::Format format) 
   : QImage(width, height, format)
    {}
};
#else
struct BackBuffer : QGLFramebufferObject {
 BackBuffer(int width, int height, QImage::Format format) 
   : QGLFramebufferObject(width, height, QGLFramebufferObject::CombinedDepthStencil)
    {
      glViewport (0, 0, width, height);
      glMatrixMode (GL_PROJECTION);     
      glLoadIdentity();
      glOrtho(0, width,0,height,-1,1);
      glMatrixMode (GL_MODELVIEW);
    }
};
#endif

void GTHREAD_init();
void GTHREAD_wakeup();
BackBuffer *GTHREAD_get_image();
void GTHREAD_putback_image(BackBuffer *image);
void GTHREAD_resize(int width, int height);

#endif
