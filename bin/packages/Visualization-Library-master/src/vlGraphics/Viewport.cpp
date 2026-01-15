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

#include <vlGraphics/Viewport.hpp>
#include <vlGraphics/OpenGL.hpp>
#include <vlCore/Log.hpp>
#include <vlCore/Say.hpp>

using namespace vl;

//-----------------------------------------------------------------------------
// Viewport
//-----------------------------------------------------------------------------
Viewport::Viewport()
{
  VL_DEBUG_SET_OBJECT_NAME()
  mX = 0;
  mY = 0;
  mWidth = 0;
  mHeight = 0;
  mClearColor = fvec4(0,0,0,1);
  mClearDepth = 1.0f;
  mClearStencil = 0;
  mClearFlags = CF_CLEAR_COLOR_DEPTH;
  mClearColorMode = CCM_Float;
}
//-----------------------------------------------------------------------------
Viewport::Viewport(int x, int y, int w, int h)
{
  VL_DEBUG_SET_OBJECT_NAME()
  mX = x;
  mY = y;
  mWidth  = w;
  mHeight = h;
  mClearColor = fvec4(0.8f,0,0.1f,1);
  mClearDepth = 1.0f;
  mClearStencil = 0;
  mClearFlags = CF_CLEAR_COLOR_DEPTH;
  mClearColorMode = CCM_Float;
}
//-----------------------------------------------------------------------------
void Viewport::activate() const
{
  VL_CHECK_OGL()

  // viewport
  int x = mX;
  int y = mY;
  int w = mWidth;
  int h = mHeight;

  if (w < 1) w = 1;
  if (h < 1) h = 1;

  glViewport(x, y, w, h);

  // clear viewport
  if (mClearFlags)
  {
    #ifndef NDEBUG
      if (!Has_GL_EXT_texture_integer)
      {
        switch( clearColorMode() )
        {
        case CCM_Int:
        case CCM_UInt:
          Log::bug("Viewport::activate(): GL_EXT_texture_integer not supported.\n");
          break;
        default:
          break;
        }
      }
    #endif

    // save writemask status to be restored later
    GLboolean color_write_mask[4] = {0,0,0,0};
    glGetBooleanv(GL_COLOR_WRITEMASK, color_write_mask);

    GLboolean depth_write_mask = 0;
    glGetBooleanv(GL_DEPTH_WRITEMASK, &depth_write_mask );

    GLboolean stencil_write_mask = 0;
    glGetBooleanv(GL_STENCIL_WRITEMASK, &stencil_write_mask );

    // make sure we clear everything
    glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE);
    glDepthMask(GL_TRUE);
    glStencilMask(GL_TRUE);

    // setup scissor
    glEnable(GL_SCISSOR_TEST);
    glScissor(x, y, w, h);

    switch( clearColorMode() )
    {
      case CCM_Float: glClearColor(        mClearColor.r(),     mClearColor.g(),     mClearColor.b(),     mClearColor.a()    );  break;
      case CCM_Int:   glClearColorIiEXT(   mClearColorInt.r(),  mClearColorInt.g(),  mClearColorInt.b(),  mClearColorInt.a() );  break;
      case CCM_UInt:  glClearColorIuiEXT(  mClearColorUInt.r(), mClearColorUInt.g(), mClearColorUInt.b(), mClearColorUInt.a() ); break;
    }

    glClearDepth( mClearDepth );

    glClearStencil( mClearStencil );

    glClear(mClearFlags);

    // restore writemasks
    glColorMask(color_write_mask[0], color_write_mask[1], color_write_mask[2], color_write_mask[3]);
    glDepthMask(depth_write_mask);
    glStencilMask(stencil_write_mask);

    VL_CHECK_OGL()
  }
}
//-----------------------------------------------------------------------------
bool Viewport::isPointInside(int x, int y, int framebuffer_height) const
{
  // set x/y relative to the viewport
  x -= this->x();
  y -= framebuffer_height - 1 - (this->y() + height() -1);

  // check that the click is in the viewport

  if (x<0 || y<0 || x>=this->width() || y>=this->height())
    return false;
  else
    return true;
}
//-----------------------------------------------------------------------------
