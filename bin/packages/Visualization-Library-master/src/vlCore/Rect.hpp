/**************************************************************************************/
/*                                                                                    */
/*  Visualization Library                                                             */
/*  http://www.visualizationlibrary.org                                               */
/*                                                                                    */
/*  Copyright (c) 2005-2010, Michele Bosi                                             */
/*  All rights reserved.                                                              */
/*                                                                                    */
/*  Redistribution and use in source and binary forms, with or without modification,  */
/*  are permitted provided that the following conditions are met:                     */
/*                                                                                    */
/*  - Redistributions of source code must retain the above copyright notice, this     */
/*  list of conditions and the following disclaimer.                                  */
/*                                                                                    */
/*  - Redistributions in binary form must reproduce the above copyright notice, this  */
/*  list of conditions and the following disclaimer in the documentation and/or       */
/*  other materials provided with the distribution.                                   */
/*                                                                                    */
/*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND   */
/*  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED     */
/*  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE            */
/*  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR  */
/*  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES    */
/*  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;      */
/*  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON    */
/*  ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT           */
/*  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS     */
/*  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                      */
/*                                                                                    */
/**************************************************************************************/

#ifndef Rect_INCLUDE_ONCE
#define Rect_INCLUDE_ONCE

#include <vlCore/Vector2.hpp>

namespace vl
{
  /**
   * Implements the common functions of RectI and RectF.
   */
  template<typename T>
  class Rect
  {
  public:
    Rect(const Rect& other)
    {
      mX = other.x();
      mY = other.y();
      mWidth  = other.width();
      mHeight = other.height();
    }
    Rect()
    {
      // null Rect
      mX = 0;
      mY = 0;
      mWidth  = -1;
      mHeight = -1;
    }
    Rect(T x, T y, T width, T height) 
    { 
      mX = x; 
      mY = y; 
      mWidth = width; 
      mHeight = height; 
    }

    T x() const { return mX; }
    T y() const { return mY; }
    T width() const  { return mWidth; }
    T height() const { return mHeight; }
    T bottom() const { return mY; }
    T left() const   { return mX; }
    T top() const    { return mY+mHeight-1; }
    T right() const  { return mX+mWidth-1;  }

    void setX(T x) { mX = x; }
    void setY(T y) { mY = y; }
    void setWidth(T w)  { mWidth  = w; }
    void setHeight(T h) { mHeight = h; }

    bool isNull()  const { return width() <  0 || height() <  0; }
    bool isPoint() const { return width() == 0 && height() == 0; }

    Rect intersected(const Rect& other) const
    {
      if (isNull() || other.isNull())
        return other;
      T Ax1 = left();
      T Ax2 = right();
      T Ay1 = bottom();
      T Ay2 = top();
      T Bx1 = other.left();
      T Bx2 = other.right();
      T By1 = other.bottom();
      T By2 = other.top();
      if (Ax1 < Bx1) Ax1 = Bx1;
      if (Ay1 < By1) Ay1 = By1;
      if (Ax2 > Bx2) Ax2 = Bx2;
      if (Ay2 > By2) Ay2 = By2;
      return Rect(Ax1,Ay1,Ax2-Ax1+1,Ay2-Ay1+1);
    }

    Rect united(const Rect& other) const
    {
      if (other.isNull())
        return *this;
      if (isNull())
        return other;
      T Ax1 = left();
      T Ax2 = right();
      T Ay1 = bottom();
      T Ay2 = top();
      T Bx1 = other.left();
      T Bx2 = other.right();
      T By1 = other.bottom();
      T By2 = other.top();
      if (Ax1 > Bx1) Ax1 = Bx1;
      if (Ay1 > By1) Ay1 = By1;
      if (Ax2 < Bx2) Ax2 = Bx2;
      if (Ay2 < By2) Ay2 = By2;
      return Rect(Ax1,Ay1,Ax2-Ax1+1,Ay2-Ay1+1);
    }

    /**
     * Defines a sort of lexicographic sorting that make possible the use of the Rect class with STL containers like std::set, std::map etc.
     */
    bool operator<(const Rect& other) const
    {
      if (mX != other.mX)
        return mX < other.mX;
      else
      if (mWidth != other.mWidth)
        return mWidth < other.mWidth;
      else
      if (mHeight != other.mHeight)
        return mHeight < other.mHeight;
      else
      if (mY != other.mY)
        return mY < other.mY;
      else
        return false;
    }

  protected:
    T mX;
    T mY;
    T mWidth;
    T mHeight;
  };

  /**
   * The RectI class represents a 2D rectangular area using \p int precision.
   * This class is mainly used to manipulate rectangles in pixel coordinates (as opposed to real coordinates).
   * The RectI and RectF differ in the following way:
   * - RectF dimensions are implemented using \a float precision, while RectI uses \a int precision.
   * - RectF::right() returns x()+width(), while RectI::right() returns x()+width()-1 
   * - RectF::top() return y()+height(), while RectI::top() return y()+height()-1
   * \sa
   * - RectF
   */
  class RectI: public Rect<int>
  {
  public:
    RectI() {}
    RectI(int x, int y, int width, int height) { mX=x; mY=y; mWidth=width; mHeight=height; }
    RectI(const Rect<int>& other) { *this = other; }
    RectI(const RectI& other): Rect<int>(other) { *this = other; }
    // operator Rect<int>() const { return Rect<int>(x(), y(), width(), height()); }
    RectI& operator=(const Rect<int>& other) { mX=other.x(); mY=other.y(); mWidth=other.width(); mHeight=other.height(); return *this; }
    RectI& operator=(const RectI& other) { mX=other.x(); mY=other.y(); mWidth=other.width(); mHeight=other.height(); return *this; }
    int top() const    { return mY+mHeight-1; }
    int right() const  { return mX+mWidth-1;  }
    ivec2 bottomLeft() const { return ivec2(bottom(),left()); }
    ivec2 topRight() const { return ivec2(top(),right()); }
  };

  /**
   * The RectF class represents a 2D rectangular area using \p float precision.
   * This class is mainly used to manipulate rectangles in real coordinates (as opposed to pixel coordinates).
   * The RectI and RectF differ in the following way:
   * - RectF dimensions are implemented using \a float precision, while RectI uses \a int precision.
   * - RectF::right() returns x()+width(), while RectI::right() returns x()+width()-1 
   * - RectF::top() return y()+height(), while RectI::top() return y()+height()-1
   * \sa
   * - RectI
   */
  class RectF: public Rect<float>
  {
  public:
    RectF() {}
    RectF(float x, float y, float width, float height) { mX=x; mY=y; mWidth=width; mHeight=height; }
    RectF(const RectF& other): Rect<float>(other) { *this = other; }
    RectF(const Rect<float>& other) { *this = other; }
    // operator Rect<float>() const { return Rect<float>(x(), y(), width(), height()); }
    RectF& operator=(const Rect<float>& other) { mX=other.x(); mY=other.y(); mWidth=other.width(); mHeight=other.height(); return *this; }
    RectF& operator=(const RectF& other) { mX=other.x(); mY=other.y(); mWidth=other.width(); mHeight=other.height(); return *this; }
    float top() const    { return mY+mHeight; }
    float right() const  { return mX+mWidth;  }
    fvec2 bottomLeft() const { return fvec2(bottom(),left()); }
    fvec2 topRight() const { return fvec2(top(),right()); }
    void addPoint(const fvec2& p)
    {
      fvec2 tr = topRight();
      fvec2 bl = bottomLeft();
      if (p.x() < bl.x())
        bl.x() = p.x();
      if (p.y() < bl.y())
        bl.y() = p.y();
      if (p.x() > tr.x())
        tr.x() = p.x();
      if (p.y() > tr.y())
        tr.y() = p.y();
      mX = bl.x();
      mY = bl.y();
      mWidth  = tr.x() - bl.x();
      mHeight = tr.y() - bl.y();
    }
  };
}

#endif
