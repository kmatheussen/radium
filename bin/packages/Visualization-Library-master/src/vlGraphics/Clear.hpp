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

#ifndef Clear_INCLUDE_ONCE
#define Clear_INCLUDE_ONCE

#include <vlCore/Vector4.hpp>
#include <vlGraphics/link_config.hpp>
#include <vlGraphics/Renderable.hpp>

namespace vl
{
  //------------------------------------------------------------------------------
  // Clear
  //------------------------------------------------------------------------------
  /**
   * The Clear class is a Renderable used to clear the whole or a portion of the color, stencil or depth buffer.
   * 
   * The Clear class is a special Renderable that wraps the functionalities of the following OpenGL functions:
   * - glClear() - http://www.opengl.org/sdk/docs/man/xhtml/glClear.xml
   * - glClearColor() - http://www.opengl.org/sdk/docs/man/xhtml/glClearColor.xml
   * - glClearDepth() - http://www.opengl.org/sdk/docs/man/xhtml/glClearDepth.xml
   * - glClearStencil() - http://www.opengl.org/sdk/docs/man/xhtml/glClearStencil.xml
   * - glScissor() - http://www.opengl.org/sdk/docs/man/xhtml/glScissor.xml
   *
   * Usually the color, depth and stencil buffers are cleared at the beginning of a rendering when the Viewport
   * is initialized. To handle less common cases the Clear class allows you to perform a clear operation 
   * also in the middle of a rendering. Just bind it to an an Actor and setup the appropriate clearing options.
   *
   * \note
   *
   * - Binding a Transform to the same Actor will not affect the clearing operation in any way.
   * - You still need to bind an Effect to the same Actor infact the Effect's renderRank() and blending 
   * state enabled/disabled might affect the rendering order of the Actor bound to your Clear object, 
   * i.e. the moment at which your buffers are cleared. In other words the usual rules to determine the 
   * rendering order of an Actor apply also when a Clear object is bound to it. For this reason in order 
   * to define the exact point at which the buffers are cleared you should pay special attention to your 
   * Actor's and Effect's renderRank() and Actor's renderBlock(). 
   * - The following render states also affect the clearing process: ColorMask, StencilMask, DepthMask.
  */
  class VLGRAPHICS_EXPORT Clear: public Renderable
  {
    VL_INSTRUMENT_CLASS(vl::Clear, Renderable)

  public:
    Clear();

    virtual void render_Implementation(const Actor*, const Shader*, const Camera*, OpenGLContext*) const;

    void setClearColorBuffer(bool clear)   { mClearColorBuffer   = clear; }

    void setClearDepthBuffer(bool clear)   { mClearDepthBuffer   = clear; }

    void setClearStencilBuffer(bool clear) { mClearStencilBuffer = clear; }

    void setClearColorValue(const fvec4& clear_val) { mClearColorValue   = clear_val; }
    
    void setClearColorValueInt(const ivec4& clear_val) { mClearColorValueInt   = clear_val; }
    
    void setClearColorValueUInt(const uvec4& clear_val) { mClearColorValueUInt   = clear_val; }
    
    void setClearDepthValue(float clear_val)        { mClearDepthValue   = clear_val; }
    
    void setClearStencilValue(int clear_val)      { mClearStencilValue = clear_val; }

    void setClearColorMode(EClearColorMode mode) { mClearColorMode = mode; }
    
    EClearColorMode clearColorMode() const { return mClearColorMode; }

    //! Defines which portion of the rendering buffers should be cleared.
    //! The parameters 'x', 'y', 'z', 'w' specify a rectangular area in viewport coordinates.
    //! Such area is also clipped against the viewport borders.
    //! If 'w' or 'h' are set to -1 then the whole viewport is cleared.
    void setScissorBox(int x, int y, int w, int h)     { mScissorBox[0] = x; mScissorBox[1] = y; mScissorBox[2] = w; mScissorBox[3] = h; }
    
    void getScissorBox(int& x, int& y, int& w, int& h) { x = mScissorBox[0]; y = mScissorBox[1]; w = mScissorBox[2]; h = mScissorBox[3]; }

    // Renderable interface implementation.

    virtual void updateDirtyBufferObject(EBufferObjectUpdateMode) {}

    virtual void deleteBufferObject() {}

  protected:
    virtual void computeBounds_Implementation() { setBoundingBox(AABB()); setBoundingSphere(Sphere()); }

  protected:
    fvec4 mClearColorValue;
    ivec4 mClearColorValueInt;
    uvec4 mClearColorValueUInt;
    int mScissorBox[4];
    EClearColorMode mClearColorMode;
    float mClearDepthValue;
    int mClearStencilValue;
    bool mClearColorBuffer;
    bool mClearDepthBuffer;
    bool mClearStencilBuffer;
  };
}

#endif
