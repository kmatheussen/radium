/**************************************************************************************/
/*                                                                                    */
/*  Visualization Library                                                             */
/*  http://www.visualizationlibrary.org                                               */
/*                                                                                    */
/*  Copyright (c) 2005-2011, Michele Bosi                                             */
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

#include <vlGraphics/Framebuffer.hpp>
#include <vlCore/Log.hpp>
#include <vlCore/Say.hpp>

#include <map>
#include <set>

using namespace vl;

//-----------------------------------------------------------------------------
// Framebuffer
//-----------------------------------------------------------------------------
bool Framebuffer::checkDrawBuffers() const
{
  std::map<int, const char*> fbo_attachments;
  
  fbo_attachments[GL_NONE]        = "GL_NONE"; 
  fbo_attachments[GL_BACK_LEFT]   = "GL_BACK_LEFT";
  fbo_attachments[GL_BACK_RIGHT]  = "GL_BACK_RIGHT";
  fbo_attachments[GL_FRONT_LEFT]  = "GL_FRONT_LEFT";
  fbo_attachments[GL_FRONT_RIGHT] = "GL_FRONT_RIGHT";
  fbo_attachments[GL_AUX0] = "GL_AUX0";
  fbo_attachments[GL_AUX1] = "GL_AUX1";
  fbo_attachments[GL_AUX2] = "GL_AUX2";
  fbo_attachments[GL_AUX3] = "GL_AUX3";
  fbo_attachments[GL_COLOR_ATTACHMENT0]  = "GL_COLOR_ATTACHMENT0"; 
  fbo_attachments[GL_COLOR_ATTACHMENT1]  = "GL_COLOR_ATTACHMENT1"; 
  fbo_attachments[GL_COLOR_ATTACHMENT2]  = "GL_COLOR_ATTACHMENT2"; 
  fbo_attachments[GL_COLOR_ATTACHMENT3]  = "GL_COLOR_ATTACHMENT3"; 
  fbo_attachments[GL_COLOR_ATTACHMENT4]  = "GL_COLOR_ATTACHMENT4"; 
  fbo_attachments[GL_COLOR_ATTACHMENT5]  = "GL_COLOR_ATTACHMENT5"; 
  fbo_attachments[GL_COLOR_ATTACHMENT6]  = "GL_COLOR_ATTACHMENT6"; 
  fbo_attachments[GL_COLOR_ATTACHMENT7]  = "GL_COLOR_ATTACHMENT7"; 
  fbo_attachments[GL_COLOR_ATTACHMENT8]  = "GL_COLOR_ATTACHMENT8"; 
  fbo_attachments[GL_COLOR_ATTACHMENT9]  = "GL_COLOR_ATTACHMENT9"; 
  fbo_attachments[GL_COLOR_ATTACHMENT10] = "GL_COLOR_ATTACHMENT10"; 
  fbo_attachments[GL_COLOR_ATTACHMENT11] = "GL_COLOR_ATTACHMENT11"; 
  fbo_attachments[GL_COLOR_ATTACHMENT12] = "GL_COLOR_ATTACHMENT12"; 
  fbo_attachments[GL_COLOR_ATTACHMENT13] = "GL_COLOR_ATTACHMENT13"; 
  fbo_attachments[GL_COLOR_ATTACHMENT14] = "GL_COLOR_ATTACHMENT14"; 
  fbo_attachments[GL_COLOR_ATTACHMENT15] = "GL_COLOR_ATTACHMENT15"; 

  int fbo = 0;
  if (Has_GL_EXT_framebuffer_object||Has_GL_ARB_framebuffer_object)
    glGetIntegerv(GL_FRAMEBUFFER_BINDING, &fbo);

  if (fbo)
  {
    std::set<GLenum> legal;
    legal.insert(GL_NONE);
    legal.insert(GL_COLOR_ATTACHMENT0);
    legal.insert(GL_COLOR_ATTACHMENT1);
    legal.insert(GL_COLOR_ATTACHMENT2);
    legal.insert(GL_COLOR_ATTACHMENT3);
    legal.insert(GL_COLOR_ATTACHMENT4);
    legal.insert(GL_COLOR_ATTACHMENT5);
    legal.insert(GL_COLOR_ATTACHMENT6);
    legal.insert(GL_COLOR_ATTACHMENT7);
    legal.insert(GL_COLOR_ATTACHMENT8);
    legal.insert(GL_COLOR_ATTACHMENT9);
    legal.insert(GL_COLOR_ATTACHMENT10);
    legal.insert(GL_COLOR_ATTACHMENT11);
    legal.insert(GL_COLOR_ATTACHMENT12);
    legal.insert(GL_COLOR_ATTACHMENT13);
    legal.insert(GL_COLOR_ATTACHMENT14);
    legal.insert(GL_COLOR_ATTACHMENT15);
    for(unsigned i=0; i<mDrawBuffers.size(); ++i)
    {
      if(legal.find(mDrawBuffers[i]) == legal.end())
      {
        Log::error(Say("FBO bound but Framebuffer::setDrawBuffers() called with non FBO compatible draw buffer '%s'.\n") << fbo_attachments[mDrawBuffers[i]]);
        return false;
      }
    }
  }
  else
  {
    std::set<GLenum> legal;
    legal.insert(GL_NONE);
    legal.insert(GL_BACK_LEFT);
    legal.insert(GL_BACK_RIGHT);
    legal.insert(GL_FRONT_LEFT);
    legal.insert(GL_FRONT_RIGHT);
    legal.insert(GL_AUX0);
    legal.insert(GL_AUX1);
    legal.insert(GL_AUX2);
    legal.insert(GL_AUX3);
    for(unsigned i=0; i<mDrawBuffers.size(); ++i)
    {
      if(legal.find(mDrawBuffers[i]) == legal.end())
      {
        Log::error(Say("FBO not bound or not supported but Framebuffer::setDrawBuffers() called with FBO specific draw buffer '%s'.\n") << fbo_attachments[mDrawBuffers[i]]);
        return false;
      }
    }
  }
  return true;
}
//-----------------------------------------------------------------------------
void Framebuffer::bindDrawBuffers() const
{
  VL_CHECK_OGL()

  VL_CHECK(!mDrawBuffers.empty())

  #ifndef NDEBUG
    checkDrawBuffers();
  #endif

  if (mDrawBuffers.size() > 1 && (Has_GL_ARB_draw_buffers||Has_GL_Version_2_0))
  {
    glDrawBuffers( (GLsizei)mDrawBuffers.size(), (const GLenum*)&mDrawBuffers[0] );
    VL_CHECK_OGL() // If you are using RDB_BACK_LEFT/RIGHT make sure sure you have a double buffered gl context.
                   // Otherwise use Framebuffer::setDrawBuffer(RDB_FRONT_LEFT).
  }
  else
  {
    glDrawBuffer( mDrawBuffers[0] );
    VL_CHECK_OGL() // If you are using RDB_BACK_LEFT/RIGHT make sure sure you have a double buffered gl context.
                   // Otherwise use Framebuffer::setDrawBuffer(RDB_FRONT_LEFT).
    if ( mDrawBuffers.size() > 1 )
    {
      Log::error( "Framebuffer::bindDrawBuffers() error:\nglDrawBuffers() not supported by the current OpenGL driver. GL_ARB_draw_buffers or OpenGL 2.0 required.\n" );
    }
  }
}
//-----------------------------------------------------------------------------
void Framebuffer::bindReadBuffer()
{
  VL_CHECK_OGL();

  glReadBuffer( readBuffer() );

  VL_CHECK_OGL();
}
//-----------------------------------------------------------------------------

