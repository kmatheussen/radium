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

#include <vlGraphics/Clear.hpp>
#include <vlGraphics/Camera.hpp>
#include <vlCore/Vector4.hpp>
#include <vlCore/Log.hpp>

using namespace vl;

//-----------------------------------------------------------------------------
Clear::Clear(): mClearColorMode(CCM_Float), mClearDepthValue(1.0f), mClearStencilValue(0),
         mClearColorBuffer(false), mClearDepthBuffer(false), mClearStencilBuffer(false)
{
  VL_DEBUG_SET_OBJECT_NAME()
  // no scissor box by default
  mScissorBox[0] = 0;
  mScissorBox[1] = 0;
  mScissorBox[2] = -1;
  mScissorBox[3] = -1;
}
//-----------------------------------------------------------------------------
void Clear::render_Implementation(const Actor*, const Shader*, const Camera* camera, OpenGLContext*) const
{
  // build buffer bit mask
  GLbitfield mask = 0;
  mask = mask | (mClearColorBuffer   ? GL_COLOR_BUFFER_BIT   : 0);
  mask = mask | (mClearDepthBuffer   ? GL_DEPTH_BUFFER_BIT   : 0);
  mask = mask | (mClearStencilBuffer ? GL_STENCIL_BUFFER_BIT : 0);

  // check for integer texture support
  if ( (clearColorMode() == CCM_Int || clearColorMode() == CCM_UInt) && !Has_GL_EXT_texture_integer)
  {
    Log::error("Clear::render(): glClearColorIiEXT and glClearColorIuiEXT not supported.\n");
    return;
  }

  if (mask)
  {
    int viewport[] = { camera->viewport()->x(), camera->viewport()->y(), camera->viewport()->width(), camera->viewport()->height() };

    // save scissor settings
    GLboolean scissor_on = glIsEnabled(GL_SCISSOR_TEST);
    int scissor_box_save[4] = {0,0,-1,-1};
    glGetIntegerv(GL_SCISSOR_BOX, scissor_box_save);

    int scissor_box[4] = {0,0,-1,-1};

    // fit to the viewport
    if (mScissorBox[2] == -1 || mScissorBox[3] == -1)
    {
      scissor_box[0] = viewport[0];
      scissor_box[1] = viewport[1];
      scissor_box[2] = viewport[2];
      scissor_box[3] = viewport[3];
    }
    else
    // scissor box in viewport coords
    {
      scissor_box[0] = mScissorBox[0] + viewport[0];
      scissor_box[1] = mScissorBox[1] + viewport[1];
      scissor_box[2] = mScissorBox[2];
      scissor_box[3] = mScissorBox[3];
    }

    // viewport from x,y,w,h -> x,y,x2,y2
    viewport[2] = viewport[0] + viewport[2] -1;
    viewport[3] = viewport[1] + viewport[3] -1;
    // scissor_box from x,y,w,h -> x,y,x2,y2
    scissor_box[2] = scissor_box[0] + scissor_box[2] -1;
    scissor_box[3] = scissor_box[1] + scissor_box[3] -1;
    // clip scissor box
    if (scissor_box[0] < viewport[0]) scissor_box[0] = viewport[0];
    if (scissor_box[1] < viewport[1]) scissor_box[1] = viewport[1];
    if (scissor_box[2] > viewport[2]) scissor_box[2] = viewport[2];
    if (scissor_box[3] > viewport[3]) scissor_box[3] = viewport[3];
    // culling
    if (scissor_box[0] > scissor_box[2]) 
      return;
    if (scissor_box[1] > scissor_box[3]) 
      return;
    // scissor_box from x,y,x2,y2 -> x,y,w,h 
    scissor_box[2] = scissor_box[2] -scissor_box[0] +1;
    scissor_box[3] = scissor_box[3] -scissor_box[1] +1;

    // enable scissor test
    glEnable(GL_SCISSOR_TEST);
    glScissor(scissor_box[0], scissor_box[1], scissor_box[2], scissor_box[3]); VL_CHECK_OGL()

    // defines the clear values
    if (mClearColorBuffer)
    {
      switch(clearColorMode())
      {
        case CCM_Float: glClearColor(      mClearColorValue.r(),     mClearColorValue.g(),     mClearColorValue.b(),     mClearColorValue.a());     break;
        case CCM_Int:   glClearColorIiEXT( mClearColorValueInt.r(),  mClearColorValueInt.g(),  mClearColorValueInt.b(),  mClearColorValueInt.a());  break;
        case CCM_UInt:  glClearColorIuiEXT(mClearColorValueUInt.r(), mClearColorValueUInt.g(), mClearColorValueUInt.b(), mClearColorValueUInt.a()); break;
      }
    }
    if (mClearDepthBuffer)
      glClearDepth(mClearDepthValue);
    if (mClearStencilBuffer)
      glClearStencil(mClearStencilValue);

    // clear!
    glClear(mask);

    // restore scissor settings
    if (!scissor_on)
      glDisable(GL_SCISSOR_TEST);
    glScissor(scissor_box_save[0], scissor_box_save[1], scissor_box_save[2], scissor_box_save[3]); VL_CHECK_OGL()
  }
}
//-----------------------------------------------------------------------------
