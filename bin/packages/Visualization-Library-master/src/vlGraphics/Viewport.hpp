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

#ifndef Viewport_INCLUDE_ONCE
#define Viewport_INCLUDE_ONCE

#include <vlCore/Object.hpp>
#include <vlCore/Rect.hpp>
#include <vlCore/Vector4.hpp>
#include <vlCore/vlnamespace.hpp>
#include <vlGraphics/link_config.hpp>

namespace vl
{
  //-----------------------------------------------------------------------------
  // Viewport
  //-----------------------------------------------------------------------------
  /**
   * Implements the viewport and clearing settings associated to a Camera.
   * Supports glClearColor, glClearColorIiEXT and glClearColorIuiEXT (see GL_EXT_texture_integer).
   * \sa Camera, Rendering, Renderer
  */
  class VLGRAPHICS_EXPORT Viewport: public Object
  {
    VL_INSTRUMENT_CLASS(vl::Viewport, Object)

  public:
    Viewport();
    Viewport(int x, int y, int w, int h);

    void activate() const;

    bool null() { return mWidth == 0 || mHeight == 0; }

    void set(int x, int y, int w, int h) { mX = x; mY = y; mWidth = w; mHeight = h; }
    void setX(int x) { mX = x; }
    int x() const { return mX; }
    void setY(int y) { mY = y; }
    int y() const { return mY; }
    void setWidth(int width) { mWidth = width; }
    int width() const { return mWidth; }
    void setHeight(int height) { mHeight = height; }
    int height() const { return mHeight; }
    fvec2 center() const { return fvec2(mX + mWidth / 2.0f, mY + mHeight / 2.0f); }

    /**
     * Returns the rectangular area that defines the viewport computed as RectI(x(), y(), x()+width()-1, y()+height()-1).
     */
    RectI rect() const { return RectI(x(),y(),width(),height()); }

    void setClearColor(float r, float g, float b, float a) { mClearColor = fvec4(r,g,b,a); }
    void setClearColor(const fvec4& color) { mClearColor = color; }
    const fvec4& clearColor() const { return mClearColor; }

    void setClearColorInt(int r, int g, int b, int a) { mClearColorInt = ivec4(r,g,b,a); }
    void setClearColorInt(const ivec4& color) { mClearColorInt = color; }
    const ivec4& clearColorInt() const { return mClearColorInt; }

    void setClearColorUInt(unsigned int r, unsigned int g, unsigned int b, unsigned int a) { mClearColorUInt = uvec4(r,g,b,a); }
    void setClearColorUInt(const uvec4& color) { mClearColorUInt = color; }
    const uvec4& clearColorUInt() const { return mClearColorUInt; }

    void setClearStencil(int stencil) { mClearStencil = stencil; }
    int clearStencil() const { return mClearStencil; }

    void setClearDepth(real depth) { mClearDepth = depth; }
    real clearDepth() const { return mClearDepth; }

    /** Usually you want to use rather RendererAbstract::setClearFlags() */
    void setClearFlags(EClearFlags clear_flags) { mClearFlags = clear_flags; }
    EClearFlags clearFlags() const { return mClearFlags; }

    void setClearColorMode(EClearColorMode mode) { mClearColorMode = mode; }
    EClearColorMode clearColorMode() const { return mClearColorMode; }

    /**
     * Returns true if the given point is inside the Viewport
    */
    bool isPointInside(int x, int y, int framebuffer_height) const;

  protected:
    fvec4 mClearColor;
    ivec4 mClearColorInt;
    uvec4 mClearColorUInt;

    real mClearDepth;
    int mClearStencil;
    EClearFlags mClearFlags;
    EClearColorMode mClearColorMode;
    int mX;
    int mY;
    int mWidth;
    int mHeight;
  };
}

#endif
