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

#include <vlCore/GlobalSettings.hpp>
#include <vlGraphics/OpenGLContext.hpp>
#include <vlGraphics/OpenGL.hpp>
#include <vlGraphics/IVertexAttribSet.hpp>
#include <vlGraphics/Shader.hpp>
#include <vlGraphics/GLSL.hpp>
#include <vlGraphics/Light.hpp>
#include <vlGraphics/ClipPlane.hpp>
#include <vlCore/Log.hpp>
#include <vlCore/Say.hpp>
#include <algorithm>
#include <sstream>
#include <vlGraphics/NaryQuickMap.hpp>

using namespace vl;

//-----------------------------------------------------------------------------
// UIEventListener
//-----------------------------------------------------------------------------
OpenGLContext* UIEventListener::openglContext() { return mOpenGLContext; }
//-----------------------------------------------------------------------------
// OpenGLContext
//-----------------------------------------------------------------------------
OpenGLContext::OpenGLContext(int w, int h)
{
  VL_DEBUG_SET_OBJECT_NAME()
  mLeftFramebuffer  = new Framebuffer(this, w, h, RDB_BACK_LEFT, RDB_BACK_LEFT);
  mRightFramebuffer = new Framebuffer(this, w, h, RDB_BACK_RIGHT, RDB_BACK_RIGHT);

  // set to unknown texture target
  memset( mTexUnitBinding, 0, sizeof(mTexUnitBinding) );

  mCurrentEnableSet = new NaryQuickMap<EEnable, EEnable, EN_EnableCount>;
  mNewEnableSet = new NaryQuickMap<EEnable, EEnable, EN_EnableCount>;

  mCurrentRenderStateSet = new NaryQuickMap<ERenderState, RenderStateSlot, RS_RenderStateCount>;
  mNewRenderStateSet = new NaryQuickMap<ERenderState, RenderStateSlot, RS_RenderStateCount>;

  mIsInitialized = false;
  mHasDoubleBuffer = false;
  mMaxVertexAttrib = 0;
  mTextureSamplerCount = 0;
  mCurVAS = NULL;

  mNormal = fvec3(0,1,0);
  mColor  = fvec4(1,1,1,1);
  mSecondaryColor = fvec3(1,1,1);

  // --- GUI ---

  mMouseVisible = true;
  mContinuousUpdate = true;
  mIgnoreNextMouseMoveEvent = false;
  mFullscreen = false;
}
//-----------------------------------------------------------------------------
OpenGLContext::~OpenGLContext()
{
  if (mFramebufferObject.size() || mEventListeners.size())
    Log::warning("~OpenGLContext(): you should have called dispatchDestroyEvent() before destroying the OpenGLContext!\nNow it's too late to cleanup things!\n");

  // invalidate the left and right framebuffers
  mLeftFramebuffer->mOpenGLContext = NULL;
  mRightFramebuffer->mOpenGLContext = NULL;

  // invalidate FBOs
  for(unsigned i=0; i<mFramebufferObject.size(); ++i)
  {
    // note, we can't destroy the FBOs here because it's too late to call makeCurrent().
    // mFramebufferObject[i]->destroy();
    mFramebufferObject[i]->mOpenGLContext = NULL;
  }

  // remove all the event listeners
  eraseAllEventListeners();
}
//-----------------------------------------------------------------------------
ref<FramebufferObject> OpenGLContext::createFramebufferObject(int width, int height, EReadDrawBuffer draw_buffer, EReadDrawBuffer read_buffer)
{
  makeCurrent();
  mFramebufferObject.push_back(new FramebufferObject(this, width, height, draw_buffer, read_buffer));
  mFramebufferObject.back()->createFBO();
  return mFramebufferObject.back();
}
//-----------------------------------------------------------------------------
void OpenGLContext::destroyFramebufferObject(FramebufferObject* fbort)
{
  makeCurrent();
  for(unsigned i=0; i<mFramebufferObject.size(); ++i)
  {
    if (mFramebufferObject[i] == fbort)
    {
      mFramebufferObject[i]->deleteFBO();
      mFramebufferObject[i]->mOpenGLContext = NULL;
      mFramebufferObject.erase(mFramebufferObject.begin()+i);
      return;
    }
  }
}
//-----------------------------------------------------------------------------
void OpenGLContext::destroyAllFramebufferObjects()
{
  makeCurrent();
  for(unsigned i=0; i<mFramebufferObject.size(); ++i)
  {
    mFramebufferObject[i]->deleteFBO();
    mFramebufferObject[i]->mOpenGLContext = NULL;
  }
  mFramebufferObject.clear();
}
//-----------------------------------------------------------------------------
void OpenGLContext::addEventListener(UIEventListener* el)
{
  VL_CHECK( el );
  VL_CHECK( el->mOpenGLContext == NULL );
  if (el->mOpenGLContext == NULL)
  {
    mEventListeners.push_back(el);
    el->mOpenGLContext = this;
    el->addedListenerEvent(this);
    if (isInitialized())
      el->initEvent();
  }
}
//-----------------------------------------------------------------------------
void OpenGLContext::removeEventListener(UIEventListener* el)
{
  VL_CHECK( el );
  VL_CHECK( el->mOpenGLContext == this || el->mOpenGLContext == NULL );
  if (el->mOpenGLContext == this)
  {
    std::vector< ref<UIEventListener> >::iterator pos = std::find(mEventListeners.begin(), mEventListeners.end(), el);
    if( pos != mEventListeners.end() )
    {
      mEventListeners.erase( pos );
      // any operation here is safe, even adding or removing listeners.
      el->removedListenerEvent(this);
      el->mOpenGLContext = NULL;
    }
  }
}
//-----------------------------------------------------------------------------
void OpenGLContext::eraseAllEventListeners()
{
  // iterate on a temp vector so that any operations inside removedListenerEvent() is safe,
  // even adding or removing listeners.
  std::vector< ref<UIEventListener> > temp = mEventListeners;
  mEventListeners.clear();
  for(size_t i=0; i<temp.size(); ++i)
  {
    VL_CHECK( temp[i]->mOpenGLContext == this );
    temp[i]->removedListenerEvent(this);
    temp[i]->mOpenGLContext = NULL;
  }
}
//-----------------------------------------------------------------------------
void OpenGLContext::setVSyncEnabled(bool enable)
{
#if defined(VL_OPENGL) && defined(VL_PLATFORM_WINDOWS)
  makeCurrent();
  if (Has_GL_EXT_swap_control)
    wglSwapIntervalEXT(enable?1:0);
#else
  // Mac and Linux?
#endif
}
//-----------------------------------------------------------------------------
bool OpenGLContext::vsyncEnabled() const
{
#if defined(VL_OPENGL) && defined(VL_PLATFORM_WINDOWS)
  if (Has_GL_EXT_swap_control)
    return wglGetSwapIntervalEXT() != 0;
  else
    return false;
#else
  return false;
#endif
}
//-----------------------------------------------------------------------------
bool OpenGLContext::initGLContext(bool log)
{
  mIsInitialized = false;

  makeCurrent();

  // init OpenGL extensions
  if (!initializeOpenGL())
  {
    Log::error("Error initializing OpenGL!\n");
    VL_TRAP()
    return false;
  }

  mExtensions = getOpenGLExtensions();

  if (log)
    logOpenGLInfo();

  VL_CHECK_OGL();

  // Find max number of texture units, see http://www.opengl.org/sdk/docs/man/xhtml/glActiveTexture.xml
  mTextureSamplerCount = 1;
  if (Has_GL_ARB_multitexture||Has_GL_Version_1_3||Has_GLES_Version_1_1) // for GL < 2.x
  {
    int max_tmp = 0;
    glGetIntegerv(GL_MAX_TEXTURE_UNITS, &max_tmp); VL_CHECK_OGL(); // deprecated enum
    mTextureSamplerCount = max_tmp > mTextureSamplerCount ? max_tmp : mTextureSamplerCount;
  }
  if (Has_GL_Version_2_0) // for GL == 2.x
  {
    int max_tmp = 0;
    glGetIntegerv(GL_MAX_TEXTURE_COORDS, &max_tmp); VL_CHECK_OGL(); // deprecated enum
    mTextureSamplerCount = max_tmp > mTextureSamplerCount ? max_tmp : mTextureSamplerCount;
  }
  if (Has_GLSL) // for GL >= 2.0
  {
    int max_tmp = 0;
    glGetIntegerv(GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS, &max_tmp); VL_CHECK_OGL();
    mTextureSamplerCount = max_tmp > mTextureSamplerCount ? max_tmp : mTextureSamplerCount;
  }
  mTextureSamplerCount = mTextureSamplerCount < VL_MAX_TEXTURE_UNITS ? mTextureSamplerCount : VL_MAX_TEXTURE_UNITS;

  // find max number of vertex attributes
  mMaxVertexAttrib = 0;
  if(Has_GLSL)
    glGetIntegerv(GL_MAX_VERTEX_ATTRIBS, &mMaxVertexAttrib);
  mMaxVertexAttrib = mMaxVertexAttrib < VL_MAX_GENERIC_VERTEX_ATTRIB ? mMaxVertexAttrib : VL_MAX_GENERIC_VERTEX_ATTRIB;

  VL_CHECK_OGL();

#if defined(VL_OPENGL)
  // test for double buffer availability
  glDrawBuffer(GL_BACK);
  if ( glGetError() )
    mHasDoubleBuffer = false;
  else
    mHasDoubleBuffer = true;
#else
  mHasDoubleBuffer = true;
#endif

  setupDefaultRenderStates();

  return mIsInitialized = true;;
}
//-----------------------------------------------------------------------------
bool OpenGLContext::isExtensionSupported(const char* ext_name)
{
  makeCurrent();
  size_t len = strlen(ext_name);
  const char* ext = mExtensions.c_str();
  const char* ext_end = ext + strlen(ext);

  for( const char* pos = strstr(ext,ext_name); pos && pos < ext_end; pos = strstr(pos,ext_name) )
  {
    if (pos[len] == ' ' || pos[len] == 0)
      return true;
    else
      pos += len;
  }

  return false;
}
//-----------------------------------------------------------------------------
void* OpenGLContext::getProcAddress(const char* function_name)
{
  makeCurrent();
  return getGLProcAddress(function_name);
}
//-----------------------------------------------------------------------------
void OpenGLContext::logOpenGLInfo()
{
  makeCurrent();

  Log::debug(" --- OpenGL Info ---\n");
  Log::debug( Say("OpenGL version: %s\n") << glGetString(GL_VERSION) );
  Log::debug( Say("OpenGL vendor: %s\n") << glGetString(GL_VENDOR) );
  Log::debug( Say("OpenGL renderer: %s\n") << glGetString(GL_RENDERER) );
  Log::debug( Say("OpenGL profile: %s\n") << (Has_Fixed_Function_Pipeline ? "Compatible" : "Core") );

  if (Has_GLSL)
    Log::debug( Say("GLSL version: %s\n") << glGetString(GL_SHADING_LANGUAGE_VERSION) );
    
  int max_val = 0;
  glGetIntegerv(GL_MAX_TEXTURE_SIZE, &max_val);
  Log::debug( Say("Max texture size: %n\n")<<max_val);

  max_val = 1;
  if (Has_GL_ARB_multitexture||Has_GL_Version_1_3||Has_GLES_Version_1_1)
    glGetIntegerv(GL_MAX_TEXTURE_UNITS, &max_val); // deprecated enum
  Log::debug( Say("Texture units (legacy): %n\n") << max_val);

  max_val = 0;
  if (Has_GL_Version_2_0)
  {
    glGetIntegerv(GL_MAX_TEXTURE_COORDS, &max_val); // deprecated enum
    Log::debug( Say("Texture units (client): %n\n") << max_val);
  }
  if (Has_GLSL)
  {
    int tmp = 0;
    glGetIntegerv(GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS, &tmp);
    // max between GL_MAX_TEXTURE_COORDS and GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS
    max_val = tmp > max_val ? tmp : max_val;
    Log::debug( Say("Texture units (combined): %n\n") << max_val);
  }

  max_val = 0;
  if (Has_GLSL)
  {
    glGetIntegerv(GL_MAX_TEXTURE_IMAGE_UNITS, &max_val);
    Log::debug( Say("Texture units (fragment shader): %n\n") << max_val);
  }

  max_val = 0;
  if (Has_GL_EXT_texture_filter_anisotropic)
    glGetIntegerv(GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT, &max_val);
  Log::debug( Say("Anisotropic texture filter: %s, ") << (Has_GL_EXT_texture_filter_anisotropic? "YES" : "NO") );
  Has_GL_EXT_texture_filter_anisotropic ? Log::debug( Say("%nX\n") << max_val) : Log::debug("\n");
  Log::debug( Say("S3 Texture Compression: %s\n") << (Has_GL_EXT_texture_compression_s3tc? "YES" : "NO") );
  Log::debug( Say("Vertex Buffer Object: %s\n") << (Has_BufferObject? "YES" : "NO"));
  Log::debug( Say("Pixel Buffer Object: %s\n") << (Has_PBO ? "YES" : "NO"));
  Log::debug( Say("Framebuffer Object: %s\n") << (Has_FBO? "YES" : "NO"));

  max_val = 0;
  if(Has_GLSL)
  {
    glGetIntegerv(GL_MAX_VERTEX_ATTRIBS, &max_val); VL_CHECK_OGL();
    Log::debug( Say("Max vertex attributes: %n\n")<<max_val);      
  }

  VL_CHECK_OGL();

  max_val = 0; 
  if(Has_GLSL)
  {
    // - opengl 3.2 seem to deprecate both  GL_MAX_VARYING_COMPONENTS and GL_MAX_VARYING_FLOATS 
    //   but does not support explicitly GL_MAX_VARYING_VECTORS, leving the issue ambiguous.
    // - my GTX 460 in opengl 3.2 core allows GL_MAX_VARYING_VECTORS
    // - a user reported that a Quadro FX GL 3.2 compatibility did not support GL_MAX_VARYING_VECTORS
    // - ergo we don't check if we are in GL 3.x non-compatible mode
    if (Has_GLES_Version_2_0||Has_GL_Version_4_1)
    {
      glGetIntegerv(GL_MAX_VARYING_VECTORS, &max_val); VL_CHECK_OGL();
    }
    else
    if (Has_GL_Version_2_0)
    {
      glGetIntegerv(GL_MAX_VARYING_FLOATS, &max_val); VL_CHECK_OGL();
      max_val /= 4;
    }
    Log::debug( Say("Max varying vectors: %n\n")<<max_val);
  }

  max_val = 0;
  if(Has_GLSL)
  {
    if (Has_GLES_Version_2_0)
    {
      glGetIntegerv(GL_MAX_FRAGMENT_UNIFORM_VECTORS, &max_val); VL_CHECK_OGL();
    }
    else
    {
      glGetIntegerv(GL_MAX_FRAGMENT_UNIFORM_COMPONENTS, &max_val); VL_CHECK_OGL();
      max_val /= 4;
    }
      
    Log::debug( Say("Max fragment uniform vectors: %n\n")<<max_val);
  }
    
  max_val = 0;
  if(Has_GLSL)
  {
    if (Has_GLES_Version_2_0)
    {
      glGetIntegerv(GL_MAX_VERTEX_UNIFORM_VECTORS, &max_val); VL_CHECK_OGL();
    }
    else
    {
      glGetIntegerv(GL_MAX_VERTEX_UNIFORM_COMPONENTS, &max_val); VL_CHECK_OGL();
      max_val /= 4;
    }
      
    Log::debug( Say("Max vertex uniform vectors: %n\n")<<max_val);
  }
    
  max_val = 0;
  if(Has_GL_Version_1_2||Has_GL_Version_3_0||Has_GL_Version_4_0)
  {
    glGetIntegerv(GL_MAX_ELEMENTS_VERTICES, &max_val); VL_CHECK_OGL();
    Log::debug( Say("Max elements vertices: %n\n") << max_val );
  }

  max_val = 0;
  if(Has_GL_Version_1_2||Has_GL_Version_3_0||Has_GL_Version_4_0)
  {
    glGetIntegerv(GL_MAX_ELEMENTS_INDICES,  &max_val ); VL_CHECK_OGL();
    Log::debug( Say("Max elements indices: %n\n") << max_val );
  }

  if (Has_Fixed_Function_Pipeline)
  {
    max_val = 0;
    glGetIntegerv(GL_MAX_CLIP_PLANES,  &max_val ); VL_CHECK_OGL();
    Log::debug( Say("Max clipping planes: %n\n") << max_val );
  }
  else
  if (Has_GLSL && !Has_GLES_Version_2_0)
  {
    max_val = 0;
    glGetIntegerv(GL_MAX_CLIP_DISTANCES,  &max_val ); VL_CHECK_OGL();
    Log::debug( Say("Max clip distances: %n\n") << max_val );
  }

  // --- log supported extensions on two columns ---

  Log::debug("\n --- OpenGL Extensions --- \n");

  std::stringstream sstream;
  sstream << extensions();
  std::string ext, line;
  for( int i=0; !sstream.eof(); ++i )
  {
    sstream >> ext;
    if (sstream.eof())
      break;

    if (i && i % 2)
    {
      line.resize(40,' ');
      line += ext;
      Log::debug( Say("%s\n") << line );
      line.clear();
    }
    else
      line = ext;
  }
  if (line.length())
    Log::debug( Say("%s\n") << line );
  Log::debug("\n");

  VL_CHECK_OGL();
}
//------------------------------------------------------------------------------
void OpenGLContext::applyEnables( const EnableSet* new_enables )
{
  VL_CHECK_OGL()

  mNewEnableSet->clear();

  if (new_enables)
  {
    for( size_t i=0; i<new_enables->enables().size(); ++i )
    {
      const EEnable& capability = new_enables->enables()[i];
      mNewEnableSet->append(capability);
      if(!mCurrentEnableSet->hasKey(capability))
      {
        glEnable( Translate_Enable[capability] );
        #ifndef NDEBUG
          if (glGetError() != GL_NO_ERROR)
          {
            Log::error( Say("An unsupported capability has been enabled: %s.\n") << Translate_Enable_String[capability]);
          }
        #endif
      }
    }
  }

  for(EEnable* capability = mCurrentEnableSet->begin(); capability != mCurrentEnableSet->end(); ++capability)
  {
    if (!mNewEnableSet->hasKey(*capability))
    {
      glDisable( Translate_Enable[*capability] );
      #ifndef NDEBUG
        if (glGetError() != GL_NO_ERROR)
        {
          Log::error( Say("An unsupported capability has been disabled: %s.\n") << Translate_Enable_String[*capability]);
        }
      #endif
    }
  }

  std::swap(mNewEnableSet, mCurrentEnableSet);
}
//------------------------------------------------------------------------------
void OpenGLContext::applyRenderStates( const RenderStateSet* new_rs, const Camera* camera) // mic fixme: this camera can also be taken away
{
  VL_CHECK_OGL()

  mNewRenderStateSet->clear();

  if (new_rs)
  {
    for( size_t i=0; i<new_rs->renderStatesCount(); ++i )
    {
      const RenderStateSlot& rs = new_rs->renderStates()[i];
      mNewRenderStateSet->append(rs.type(), rs);
      if (!mCurrentRenderStateSet->hasKey(rs.type()) || rs.mRS.get() != mCurrentRenderStateSet->valueFromKey(rs.type()).mRS.get())
      {
        VL_CHECK(rs.mRS.get());
        rs.apply(camera, this); VL_CHECK_OGL()
      }
    }
  }

  for( RenderStateSlot* rs = mCurrentRenderStateSet->begin(); rs != mCurrentRenderStateSet->end(); ++rs)
  {
    if (!mNewRenderStateSet->hasKey(rs->type()))
    {
      mDefaultRenderStates[rs->type()].apply(NULL, this); VL_CHECK_OGL()
    }
  }

  std::swap(mNewRenderStateSet, mCurrentRenderStateSet);
}
//------------------------------------------------------------------------------
void OpenGLContext::setupDefaultRenderStates()
{
  if ( Has_Fixed_Function_Pipeline )
  {
    mDefaultRenderStates[RS_Color]  = RenderStateSlot(new Color, 0);
    mDefaultRenderStates[RS_SecondaryColor]  = RenderStateSlot(new SecondaryColor, 0);
    mDefaultRenderStates[RS_Normal]  = RenderStateSlot(new Normal, 0);

    mDefaultRenderStates[RS_AlphaFunc]  = RenderStateSlot(new AlphaFunc, 0);
    mDefaultRenderStates[RS_Fog]        = RenderStateSlot(new Fog, 0);
    mDefaultRenderStates[RS_ShadeModel] = RenderStateSlot(new ShadeModel, 0);
    mDefaultRenderStates[RS_LightModel] = RenderStateSlot(new LightModel, 0);
    mDefaultRenderStates[RS_Material]   = RenderStateSlot(new Material, 0);
    if(!Has_GLES_Version_1_1)
    {
      mDefaultRenderStates[RS_PixelTransfer]  = RenderStateSlot(new PixelTransfer, 0);
      mDefaultRenderStates[RS_LineStipple]    = RenderStateSlot(new LineStipple, 0);
      mDefaultRenderStates[RS_PolygonStipple] = RenderStateSlot(new PolygonStipple, 0);
    }

    mDefaultRenderStates[RS_Light0] = RenderStateSlot(new Light, 0); mDefaultRenderStates[RS_Light0].mRS->as<Light>()->setEnabled(false);
    mDefaultRenderStates[RS_Light1] = RenderStateSlot(new Light, 1); mDefaultRenderStates[RS_Light1].mRS->as<Light>()->setEnabled(false);
    mDefaultRenderStates[RS_Light2] = RenderStateSlot(new Light, 2); mDefaultRenderStates[RS_Light2].mRS->as<Light>()->setEnabled(false);
    mDefaultRenderStates[RS_Light3] = RenderStateSlot(new Light, 3); mDefaultRenderStates[RS_Light3].mRS->as<Light>()->setEnabled(false);
    mDefaultRenderStates[RS_Light4] = RenderStateSlot(new Light, 4); mDefaultRenderStates[RS_Light4].mRS->as<Light>()->setEnabled(false);
    mDefaultRenderStates[RS_Light5] = RenderStateSlot(new Light, 5); mDefaultRenderStates[RS_Light5].mRS->as<Light>()->setEnabled(false);
    mDefaultRenderStates[RS_Light6] = RenderStateSlot(new Light, 6); mDefaultRenderStates[RS_Light6].mRS->as<Light>()->setEnabled(false);
    mDefaultRenderStates[RS_Light7] = RenderStateSlot(new Light, 7); mDefaultRenderStates[RS_Light7].mRS->as<Light>()->setEnabled(false);

    mDefaultRenderStates[RS_ClipPlane0] = RenderStateSlot(new ClipPlane, 0); mDefaultRenderStates[RS_ClipPlane0].mRS->as<ClipPlane>()->setEnabled(false);
    mDefaultRenderStates[RS_ClipPlane1] = RenderStateSlot(new ClipPlane, 1); mDefaultRenderStates[RS_ClipPlane1].mRS->as<ClipPlane>()->setEnabled(false);
    mDefaultRenderStates[RS_ClipPlane2] = RenderStateSlot(new ClipPlane, 2); mDefaultRenderStates[RS_ClipPlane2].mRS->as<ClipPlane>()->setEnabled(false);
    mDefaultRenderStates[RS_ClipPlane3] = RenderStateSlot(new ClipPlane, 3); mDefaultRenderStates[RS_ClipPlane3].mRS->as<ClipPlane>()->setEnabled(false);
    mDefaultRenderStates[RS_ClipPlane4] = RenderStateSlot(new ClipPlane, 4); mDefaultRenderStates[RS_ClipPlane4].mRS->as<ClipPlane>()->setEnabled(false);
    mDefaultRenderStates[RS_ClipPlane5] = RenderStateSlot(new ClipPlane, 5); mDefaultRenderStates[RS_ClipPlane5].mRS->as<ClipPlane>()->setEnabled(false);
  }

  if (Has_GL_EXT_blend_color||Has_GL_Version_1_4||Has_GL_Version_3_0||Has_GL_Version_4_0||Has_GLES_Version_2_0)
    mDefaultRenderStates[RS_BlendColor] = RenderStateSlot(new BlendColor, 0);

  if (Has_GL_Version_1_4||Has_GL_Version_3_0||Has_GL_Version_4_0||Has_GL_OES_blend_subtract||Has_GLES_Version_2_0)
    mDefaultRenderStates[RS_BlendEquation] = RenderStateSlot(new BlendEquation, 0);

  if(!Has_GLES)
    mDefaultRenderStates[RS_PolygonMode] = RenderStateSlot(new PolygonMode, 0);

  if(!Has_GLES_Version_2_0)
  {
    mDefaultRenderStates[RS_LogicOp] = RenderStateSlot(new LogicOp, 0);
    mDefaultRenderStates[RS_PointSize] = RenderStateSlot(new PointSize, 0);
  }

  mDefaultRenderStates[RS_PolygonOffset] = RenderStateSlot(new PolygonOffset, 0);
  mDefaultRenderStates[RS_BlendFunc]  = RenderStateSlot(new BlendFunc, 0);
  mDefaultRenderStates[RS_ColorMask]  = RenderStateSlot(new ColorMask, 0);
  mDefaultRenderStates[RS_CullFace]   = RenderStateSlot(new CullFace, 0);
  mDefaultRenderStates[RS_DepthFunc]  = RenderStateSlot(new DepthFunc, 0);
  mDefaultRenderStates[RS_DepthMask]  = RenderStateSlot(new DepthMask, 0);
  mDefaultRenderStates[RS_DepthRange] = RenderStateSlot(new DepthRange, 0);
  mDefaultRenderStates[RS_FrontFace]  = RenderStateSlot(new FrontFace, 0);
  mDefaultRenderStates[RS_Hint]       = RenderStateSlot(new Hint, 0);
  mDefaultRenderStates[RS_LineWidth]  = RenderStateSlot(new LineWidth, 0);
  
  if (Has_GL_ARB_point_parameters||Has_GL_Version_1_4||Has_GL_Version_3_0||Has_GL_Version_4_0||Has_GLES_Version_1_1) // note GLES 2.x is excluded
    mDefaultRenderStates[RS_PointParameter] = RenderStateSlot(new PointParameter, 0);

  if (Has_GL_ARB_multisample||Has_GL_Version_1_3||Has_GL_Version_3_0||Has_GL_Version_4_0||Has_GLES_Version_1_1||Has_GLES_Version_2_0)
    mDefaultRenderStates[RS_SampleCoverage] = RenderStateSlot(new SampleCoverage, 0);
  
  mDefaultRenderStates[RS_StencilFunc] = RenderStateSlot(new StencilFunc, 0);
  mDefaultRenderStates[RS_StencilMask] = RenderStateSlot(new StencilMask, 0);
  mDefaultRenderStates[RS_StencilOp]   = RenderStateSlot(new StencilOp, 0);
  mDefaultRenderStates[RS_GLSLProgram] = RenderStateSlot(new GLSLProgram, 0);

  for(int i=0; i<VL_MAX_TEXTURE_UNITS; ++i)
  {
    if (i < textureUnitCount())
    {
      mDefaultRenderStates[RS_TextureSampler + i] = RenderStateSlot(new TextureSampler, i);
      if( Has_Fixed_Function_Pipeline )
      {
        // TexGen under GLES is supported only if GL_OES_texture_cube_map is present
        if(!Has_GLES_Version_1_1 || Has_GL_OES_texture_cube_map)
          mDefaultRenderStates[RS_TexGen + i] = RenderStateSlot(new TexGen, i);
        mDefaultRenderStates[RS_TexEnv + i] = RenderStateSlot(new TexEnv, i);
        mDefaultRenderStates[RS_TextureMatrix + i] = RenderStateSlot(new TextureMatrix, i);
      }
    }
  }

  VL_CHECK_OGL();

  // applies default render states backwards so we don't need to call VL_glActiveTexture(GL_TEXTURE0) at the end.
  for( int i=RS_RenderStateCount; i--; )
  {
    // the empty ones are the ones that are not supported by the current OpenGL implementation (too old or Core profile)
    if (mDefaultRenderStates[i].mRS)
    {
      mDefaultRenderStates[i].apply(NULL, this); VL_CHECK_OGL();
    }
  }
}
//-----------------------------------------------------------------------------
void OpenGLContext::resetRenderStates()
{
  mCurrentRenderStateSet->clear();
  memset( mTexUnitBinding, 0, sizeof( mTexUnitBinding ) ); // set to unknown texture target
}
//-----------------------------------------------------------------------------
void OpenGLContext::resetEnables()
{
  mCurrentEnableSet->clear();
}
//------------------------------------------------------------------------------
bool OpenGLContext::isCleanState(bool verbose)
{
  VL_CHECK_OGL();
  String error_msg;

  // everything must be disabled except GL_DITHER and GL_MULTISAMPLE
  for( unsigned i=0; i<EN_EnableCount; ++i )
  {
    if (!Is_Enable_Supported[i])
      continue;

    if (i == EN_DITHER || i == EN_MULTISAMPLE)
      continue;

    GLboolean enabled = glIsEnabled( Translate_Enable[i] ); VL_CHECK_OGL();

    if (enabled)
    {
      error_msg += Say(" - Capability %s was enabled!\n") << Translate_Enable_String[i];
      glDisable(Translate_Enable[i]);
    }
  }

  if (Has_Fixed_Function_Pipeline)
  {
    int max_lights = 0;
    glGetIntegerv(GL_MAX_LIGHTS, &max_lights);

    for( int i=0; i<max_lights; ++i)
    {
      if (glIsEnabled(GL_LIGHT0+i))
      {
        error_msg += Say(" - GL_LIGHT%n was enabled!\n") << i;
        glDisable(GL_LIGHT0+i);
      }
    }

    // OpenGL requires 6 planes but GLES 1.x requires only 1.
    int max_planes = 0;
    glGetIntegerv(GL_MAX_CLIP_PLANES, &max_planes);

    for( int i=0; i<max_planes; ++i)
    {
      if (glIsEnabled(GL_CLIP_PLANE0+i))
      {
        error_msg += Say(" - GL_CLIP_PLANE%n was enabled!\n") << i;
        glDisable(GL_CLIP_PLANE0+i);
      }
    }
  }

  if (Has_Primitive_Restart && glIsEnabled(GL_PRIMITIVE_RESTART))
  {
    error_msg += " - GL_PRIMITIVE_RESTART was enabled!\n";
    glDisable(GL_PRIMITIVE_RESTART);
  }

  if(Has_Multitexture)
  {
    int active_tex = -1;
    // mic fixme: PVR emulator bug? returns always 1.
#if !defined(VL_OPENGL_ES2)
    glGetIntegerv(GL_ACTIVE_TEXTURE, &active_tex); VL_CHECK_OGL();
    active_tex -= GL_TEXTURE0;
    if (active_tex != 0)
    {
        error_msg += Say(" - Active texture unit is GL_TEXTURE%n instead of GL_TEXTURE0!\n") << active_tex;
        glActiveTexture(GL_TEXTURE0);
    }
#endif

    if (Has_Fixed_Function_Pipeline)
    {
      active_tex = -1;
      glGetIntegerv(GL_CLIENT_ACTIVE_TEXTURE, &active_tex); VL_CHECK_OGL();
      active_tex -= GL_TEXTURE0;
      if (active_tex != 0)
      {
        error_msg += Say(" - Active client texture unit is GL_TEXTURE%n instead of GL_TEXTURE0!\n") << active_tex;
        glClientActiveTexture(GL_TEXTURE0);
      }
    }
  }

  VL_CHECK_OGL()

  /* We only check the subset of tex-units supported also by glClientActiveTexture() */
  // Find the minimum of the max texture units supported, starting at 16
  int coord_count = 16;
  if (Has_GL_ARB_multitexture||Has_GL_Version_1_3||Has_GLES_Version_1_1)
  {
    int max_tmp = 0;
    glGetIntegerv(GL_MAX_TEXTURE_UNITS, &max_tmp); VL_CHECK_OGL(); // deprecated enum
    coord_count = max_tmp < coord_count ? max_tmp : coord_count;
  }

  VL_CHECK_OGL()

  if (Has_GLSL) // for GL >= 2.0
  {
    int max_tmp = 0;
    glGetIntegerv(GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS, &max_tmp); VL_CHECK_OGL();
    coord_count = max_tmp < coord_count ? max_tmp : coord_count;
  }

  if (Has_GL_Version_2_0) // for GL == 2.x && compatible higher
  {
    int max_tmp = 0;
    glGetIntegerv(GL_MAX_TEXTURE_COORDS, &max_tmp); VL_CHECK_OGL(); // deprecated enum
    coord_count = max_tmp < coord_count ? max_tmp : coord_count;
  }

  VL_CHECK_OGL()

  while(coord_count--)
  {
    VL_CHECK_OGL()
    VL_glActiveTexture(GL_TEXTURE0+coord_count); VL_CHECK_OGL()

    if (Has_Fixed_Function_Pipeline)
    {
	    VL_glClientActiveTexture(GL_TEXTURE0+coord_count); VL_CHECK_OGL()

	    float matrix[16];
	    float imatrix[16];
	    glGetFloatv(GL_TEXTURE_MATRIX, matrix); VL_CHECK_OGL()
	    glMatrixMode(GL_TEXTURE); VL_CHECK_OGL()
	    glLoadIdentity(); VL_CHECK_OGL()
	    glGetFloatv(GL_TEXTURE_MATRIX, imatrix); VL_CHECK_OGL()
	    // glLoadMatrixf(matrix); VL_CHECK_OGL() // we keep the identity
	    if (memcmp(matrix,imatrix,sizeof(matrix)) != 0)
	    {
	      error_msg += Say(" - Texture matrix was not set to identity on texture unit %n!\n") << coord_count;
	    }
	
	    if (glIsEnabled(GL_TEXTURE_COORD_ARRAY))
	    {
          error_msg += Say(" - GL_TEXTURE_COORD_ARRAY was enabled on texture unit %n!\n") << coord_count;
          glDisable(GL_TEXTURE_COORD_ARRAY);
	    }
	
	    // check that all texture targets are disabled and bound to texture #0
	
      if (!Has_GLES)
      {
	      if (glIsEnabled(GL_TEXTURE_1D))
	      {
	        error_msg += Say(" - GL_TEXTURE_1D was enabled on texture unit GL_TEXTURE%n.\n") << coord_count;
          glDisable(GL_TEXTURE_1D);
	      }
	
        GLint bound_tex = 0;
	      glGetIntegerv(GL_TEXTURE_BINDING_1D, &bound_tex); VL_CHECK_OGL()
	      if (bound_tex != 0)
	      {
	        error_msg += Say(" - GL_TEXTURE_BINDING_1D != 0 on texture unit GL_TEXTURE%n.\n") << coord_count;
	      }
      }
	
	    if (glIsEnabled(GL_TEXTURE_2D))
	    {
	      error_msg += Say(" - GL_TEXTURE_2D was enabled on texture unit GL_TEXTURE%n.\n") << coord_count;
        glDisable(GL_TEXTURE_2D);
	    }
    }

    GLint bound_tex = 0;
    glGetIntegerv(GL_TEXTURE_BINDING_2D, &bound_tex); VL_CHECK_OGL()
    if (bound_tex != 0)
    {
      error_msg += Say(" - GL_TEXTURE_BINDING_2D != 0 on texture unit GL_TEXTURE%n.\n") << coord_count;
    }

    if (Has_Texture_Rectangle)
    {
      if (Has_Fixed_Function_Pipeline && glIsEnabled(GL_TEXTURE_RECTANGLE))
      {
        error_msg += Say(" - GL_TEXTURE_RECTANGLE was enabled on texture unit GL_TEXTURE%n.\n") << coord_count;
        glDisable(GL_TEXTURE_RECTANGLE);
      }

      bound_tex = 0;
      glGetIntegerv(GL_TEXTURE_BINDING_RECTANGLE, &bound_tex); VL_CHECK_OGL()
      if (bound_tex != 0)
      {
        error_msg += Say(" - GL_TEXTURE_BINDING_RECTANGLE != 0 on texture unit GL_TEXTURE%n.\n") << coord_count;
      }
    }

    if (Has_Texture_3D)
    {
      if (Has_Fixed_Function_Pipeline && glIsEnabled(GL_TEXTURE_3D))
      {
        error_msg += Say(" - GL_TEXTURE_3D was enabled on texture unit GL_TEXTURE%n.\n") << coord_count;
        glDisable(GL_TEXTURE_3D);
      }

      bound_tex = 0;
      glGetIntegerv(GL_TEXTURE_BINDING_3D, &bound_tex); VL_CHECK_OGL()
      if (bound_tex != 0)
      {
        error_msg += Say(" - GL_TEXTURE_BINDING_3D != 0 on texture unit GL_TEXTURE%n.\n") << coord_count;
      }
    }

    if (Has_Cubemap_Textures)
    {
      if (Has_Fixed_Function_Pipeline && glIsEnabled(GL_TEXTURE_CUBE_MAP))
      {
        error_msg += Say(" - GL_TEXTURE_CUBE_MAP was enabled on texture unit GL_TEXTURE%n.\n") << coord_count;
        glDisable(GL_TEXTURE_CUBE_MAP);
      }

      bound_tex = 0;
      glGetIntegerv(GL_TEXTURE_BINDING_CUBE_MAP, &bound_tex); VL_CHECK_OGL()
      if (bound_tex != 0)
      {
        error_msg += Say(" - GL_TEXTURE_BINDING_CUBE_MAP != 0 on texture unit GL_TEXTURE%n.\n") << coord_count;
      }
    }

    if (Has_Texture_Array)
    {
      bound_tex = 0;
      glGetIntegerv(GL_TEXTURE_BINDING_1D_ARRAY, &bound_tex);
      if (bound_tex != 0)
      {
        error_msg += Say(" - GL_TEXTURE_BINDING_1D_ARRAY != 0 on texture unit GL_TEXTURE%n.\n") << coord_count;
      }

      bound_tex = 0;
      glGetIntegerv(GL_TEXTURE_BINDING_2D_ARRAY, &bound_tex);
      if (bound_tex != 0)
      {
        error_msg += Say(" - GL_TEXTURE_BINDING_2D_ARRAY != 0 on texture unit GL_TEXTURE%n.\n") << coord_count;
      }
    }

    if (Has_Texture_Multisample)
    {
      bound_tex = 0;
      glGetIntegerv(GL_TEXTURE_BINDING_2D_MULTISAMPLE, &bound_tex);
      if (bound_tex != 0)
      {
        error_msg += Say(" - GL_TEXTURE_BINDING_2D_MULTISAMPLE != 0 on texture unit GL_TEXTURE%n.\n") << coord_count;
      }

      bound_tex = 0;
      glGetIntegerv(GL_TEXTURE_BINDING_2D_MULTISAMPLE_ARRAY, &bound_tex);
      if (bound_tex != 0)
      {
        error_msg += Say(" - GL_TEXTURE_BINDING_2D_MULTISAMPLE_ARRAY != 0 on texture unit GL_TEXTURE%n.\n") << coord_count;
      }
    }

    if (Has_Texture_Buffer)
    {
      bound_tex = 0;
      glGetIntegerv(GL_TEXTURE_BINDING_BUFFER, &bound_tex);
      if (bound_tex != 0)
      {
        error_msg += Say(" - GL_TEXTURE_BINDING_BUFFER != 0 on texture unit GL_TEXTURE%n.\n") << coord_count;
      }
    }

    if (Has_Fixed_Function_Pipeline)
    {
#if defined(VL_OPENGL)
	    if (glIsEnabled(GL_TEXTURE_GEN_S))
	    {
	      error_msg += Say(" - GL_TEXTURE_GEN_S was enabled on texture unit GL_TEXTURE%n.\n") << coord_count;
        glDisable(GL_TEXTURE_GEN_S);
	    }
	
	    if (glIsEnabled(GL_TEXTURE_GEN_T))
	    {
	      error_msg += Say(" - GL_TEXTURE_GEN_T was enabled on texture unit GL_TEXTURE%n.\n") << coord_count;
        glDisable(GL_TEXTURE_GEN_T);
	    }
	
	    if (glIsEnabled(GL_TEXTURE_GEN_R))
	    {
	      error_msg += Say(" - GL_TEXTURE_GEN_R was enabled on texture unit GL_TEXTURE%n.\n") << coord_count;
        glDisable(GL_TEXTURE_GEN_R);
	    }
	
	    if (glIsEnabled(GL_TEXTURE_GEN_Q))
	    {
	      error_msg += Say(" - GL_TEXTURE_GEN_Q was enabled on texture unit GL_TEXTURE%n.\n") << coord_count;
        glDisable(GL_TEXTURE_GEN_Q);
	    }
#elif defined(VL_OPENGL_ES1)
	    if (Has_GL_OES_texture_cube_map && glIsEnabled(GL_TEXTURE_GEN_STR_OES))
	    {
	      error_msg += Say(" - GL_TEXTURE_GEN_STR_OES was enabled on texture unit GL_TEXTURE%n.\n") << coord_count;
        glDisable(GL_TEXTURE_GEN_STR_OES);
	    }
#endif
    }
  }

  if (Has_GL_Version_1_1 && glIsEnabled(GL_COLOR_MATERIAL)) // excludes also GLES
  {
    error_msg += " - GL_COLOR_MATERIAL was enabled!\n";
    glDisable(GL_COLOR_MATERIAL);
  }

  if (Has_GL_Version_1_4 || Has_GL_EXT_fog_coord) // excludes also GLES 1.x
  {
    if (glIsEnabled(GL_FOG_COORD_ARRAY))
    {
      error_msg += " - GL_FOG_COORD_ARRAY was enabled!\n";
      glDisable(GL_FOG_COORD_ARRAY);
    }
  }

  if (Has_GL_Version_1_4 || Has_GL_EXT_secondary_color) // excludes also GLES 1.x
  {
    if (glIsEnabled(GL_SECONDARY_COLOR_ARRAY))
    {
      error_msg += " - GL_SECONDARY_COLOR_ARRAY was enabled!\n";
      glDisable(GL_SECONDARY_COLOR_ARRAY);
    }
  }

  if (Has_Fixed_Function_Pipeline && glIsEnabled(GL_COLOR_ARRAY)) // includes GLES 1.x
  {
    error_msg += " - GL_COLOR_ARRAY was enabled!\n";
    glDisable(GL_COLOR_ARRAY);
  }

  if (Has_GL_Version_1_1 && glIsEnabled(GL_EDGE_FLAG_ARRAY)) // excludes GLES 
  {
    error_msg += " - GL_EDGE_FLAG_ARRAY was enabled!\n";
    glDisable(GL_EDGE_FLAG_ARRAY);
  }

  if (Has_GL_Version_1_1 && glIsEnabled(GL_INDEX_ARRAY)) // excludes GLES
  {
    error_msg += " - GL_INDEX_ARRAY was enabled!\n";
    glDisable(GL_INDEX_ARRAY);
  }

  if (Has_Fixed_Function_Pipeline && glIsEnabled(GL_NORMAL_ARRAY)) // includes GLES 1.x
  {
    error_msg += " - GL_NORMAL_ARRAY was enabled!\n";
    glDisable(GL_NORMAL_ARRAY);
  }

  if (Has_Fixed_Function_Pipeline && glIsEnabled(GL_VERTEX_ARRAY)) // includes GLES 1.x
  {
    error_msg += " - GL_VERTEX_ARRAY was enabled!\n";
    glDisable(GL_VERTEX_ARRAY);
  }

  if (glIsEnabled(GL_SCISSOR_TEST))
  {
    error_msg += " - GL_SCISSOR_TEST was enabled!\n";
    glDisable(GL_SCISSOR_TEST);
  }

  GLint max_vert_attribs = 0;
  if (Has_GLSL)
    glGetIntegerv(GL_MAX_VERTEX_ATTRIBS, &max_vert_attribs);
  for(int i=0; i<max_vert_attribs; ++i)
  {
    GLint is_enabled = 0;
    glGetVertexAttribiv(i, GL_VERTEX_ATTRIB_ARRAY_ENABLED, &is_enabled);
    if (is_enabled)
    {
      error_msg += Say(" - GL_VERTEX_ATTRIB_ARRAY #%n is enabled!\n") << i;
    }
  }

  if (Has_GL_ARB_imaging)
  {
    if (glIsEnabled(GL_HISTOGRAM))
    {
      error_msg += " - GL_HISTOGRAM was enabled!\n";
      glDisable(GL_HISTOGRAM);
    }

    if (glIsEnabled(GL_MINMAX))
    {
      error_msg += " - GL_MINMAX was enabled!\n";
      glDisable(GL_MINMAX);
    }
  }

  // we expect these settings for the default blending equation
#if defined(VL_OPENGL_ES2)
  GLint blend_src = 0;
  GLint blend_dst = 0;
  glGetIntegerv( GL_BLEND_SRC_RGB, &blend_src ); VL_CHECK_OGL();
  glGetIntegerv( GL_BLEND_DST_RGB, &blend_dst ); VL_CHECK_OGL();
  if (blend_src != GL_SRC_ALPHA)
  {
    error_msg += " - GL_BLEND_SRC is not GL_SRC_ALPHA!\n";
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  }
  if (blend_dst != GL_ONE_MINUS_SRC_ALPHA)
  {
    error_msg += " - GL_BLEND_DST is not GL_ONE_MINUS_SRC_ALPHA!\n";
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  }
#else
  GLint blend_src = 0;
  GLint blend_dst = 0;
  glGetIntegerv( GL_BLEND_SRC, &blend_src ); VL_CHECK_OGL();
  glGetIntegerv( GL_BLEND_DST, &blend_dst ); VL_CHECK_OGL();
  if (blend_src != GL_SRC_ALPHA)
  {
    error_msg += " - GL_BLEND_SRC is not GL_SRC_ALPHA!\n";
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  }
  if (blend_dst != GL_ONE_MINUS_SRC_ALPHA)
  {
    error_msg += " - GL_BLEND_DST is not GL_ONE_MINUS_SRC_ALPHA!\n";
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  }
#endif

  // buffer object bindings

  GLint buf_bind = 0;
  if (Has_BufferObject)
  {
    glGetIntegerv(GL_ARRAY_BUFFER_BINDING, &buf_bind); VL_CHECK_OGL();
    if (buf_bind != 0)
    {
      error_msg += " - GL_ARRAY_BUFFER_BINDING should be 0!\n";
    }
    buf_bind = 0;
    glGetIntegerv(GL_ELEMENT_ARRAY_BUFFER_BINDING, &buf_bind); VL_CHECK_OGL();
    if (buf_bind != 0)
    {
      error_msg += " - GL_ELEMENT_ARRAY_BUFFER_BINDING should be 0!\n";
    }
  }
  if (Has_GL_Version_2_1)
  {
    buf_bind = 0;
    glGetIntegerv(GL_PIXEL_PACK_BUFFER_BINDING, &buf_bind); VL_CHECK_OGL();
    if (buf_bind != 0)
    {
      error_msg += " - GL_PIXEL_PACK_BUFFER_BINDING should be 0!\n";
    }
    buf_bind = 0;
    glGetIntegerv(GL_PIXEL_UNPACK_BUFFER_BINDING, &buf_bind); VL_CHECK_OGL();
    if (buf_bind != 0)
    {
      error_msg += " - GL_PIXEL_UNPACK_BUFFER_BINDING should be 0!\n";
    }
  }
  if (Has_GL_ARB_uniform_buffer_object)
  {
    buf_bind = 0;
    glGetIntegerv(GL_UNIFORM_BUFFER_BINDING, &buf_bind); VL_CHECK_OGL();
    if (buf_bind != 0)
    {
      error_msg += " - GL_UNIFORM_BUFFER_BINDING should be 0!\n";
    }
  }
  if(Has_Transform_Feedback)
  {
    buf_bind = 0;
    glGetIntegerv(GL_TRANSFORM_FEEDBACK_BUFFER_BINDING, &buf_bind); VL_CHECK_OGL();
    if (buf_bind != 0)
    {
      error_msg += " - GL_TRANSFORM_FEEDBACK_BUFFER_BINDING should be 0!\n";
    }
  }

  #if 0
  // check viewport
  GLint viewport[4] = {0,0,0,0};
  glGetIntegerv(GL_VIEWPORT, viewport);
  if (viewport[2] * viewport[3] == 1)
  {
    error_msg += " - Viewport dimension is 1 pixel!\n"
    "Did you forget to call camera()->viewport()->setWidth()/setHeight() upon window resize event?\n";
  }
  #endif

  GLboolean write_mask[4];
  glGetBooleanv(GL_COLOR_WRITEMASK, write_mask); VL_CHECK_OGL();
  if( !write_mask[0] || !write_mask[1] || !write_mask[2] || !write_mask[3] )
  {
    error_msg += " - Color write-mask should be glColorMask(GL_TRUE ,GL_TRUE, GL_TRUE, GL_TRUE)!\n";
    glColorMask(GL_TRUE ,GL_TRUE, GL_TRUE, GL_TRUE);
  }

  glGetBooleanv(GL_DEPTH_WRITEMASK, write_mask); VL_CHECK_OGL();
  if ( !write_mask[0] )
  {
    error_msg += " - Depth write-mask should be glDepthMask(GL_TRUE)!\n";
    glDepthMask(GL_TRUE);
  }

#if defined(VL_OPENGL)
  GLint poly_mode[2];
  glGetIntegerv(GL_POLYGON_MODE, poly_mode); VL_CHECK_OGL();
  if ( poly_mode[0] != GL_FILL || poly_mode[1] != GL_FILL )
  {
    error_msg += " - Polygon mode should be glPolygonMode(GL_FRONT_AND_BACK, GL_FILL)!\n";
    glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
  }
#endif

  if (!error_msg.empty() && verbose)
  {
    Log::error("Dirty OpenGL context state:\n");
    Log::error(error_msg);
    Log::error("To disable this check use globalSettings()->setCheckOpenGLStates(false);\n");
    Log::error( Say("Driver info: %s, %s, OpenGL %s\n") 
                << glGetString(GL_VENDOR) << glGetString(GL_RENDERER) << glGetString(GL_VERSION) );
  }

  VL_CHECK_OGL(); 

  return error_msg.empty();
}
//-----------------------------------------------------------------------------
bool OpenGLContext::areUniformsColliding(const UniformSet* u1, const UniformSet* u2)
{
  if (!u1 || !u2)
    return false;

  // compile the map
  std::set<std::string> name_set;
  for( size_t i=0; i<u1->uniforms().size(); ++i )
    name_set.insert( u1->uniforms()[i]->name() );

  bool ok = false;
  // check the map
  for( size_t j=0; j<u2->uniforms().size(); ++j )
    if ( name_set.find( u2->uniforms()[j]->name() ) != name_set.end() )
    {
      vl::Log::error( Say("Uniform name collision detected: %s\n") << u2->uniforms()[j]->name() );
      ok = true;
    }

  return ok;
}
//-----------------------------------------------------------------------------
void OpenGLContext::resetContextStates(EResetContextStates start_or_finish)
{
  // Check that the OpenGL state is clear.
  // If this fails use VL_CHECK_OGL to make sure your application does not generate OpenGL errors.
  // See also glGetError() -> http://www.opengl.org/sdk/docs/man/xhtml/glGetError.xml
  VL_CHECK_OGL();

  // perform extra OpenGL environment sanity check
  if (globalSettings()->checkOpenGLStates())
    isCleanState(true);

  VL_glBindFramebuffer(GL_FRAMEBUFFER, 0); VL_CHECK_OGL();

  // not existing under OpenGL ES 1 and 2
#if defined(VL_OPENGL)
  if ( hasDoubleBuffer() )
  {
    glDrawBuffer(GL_BACK); VL_CHECK_OGL();
    glReadBuffer(GL_BACK); VL_CHECK_OGL();
  }
  else
  {
    glDrawBuffer(GL_FRONT); VL_CHECK_OGL();
    glReadBuffer(GL_FRONT); VL_CHECK_OGL();
  }
#endif

  // these need to be cleaned up only when rendering starts

  if (start_or_finish == RCS_RenderingStarted)
  {
    // reset internal VL enables & render states tables
    resetEnables();
    resetRenderStates();

    // reset Vertex Attrib Set tables and also calls "glBindBuffer(GL_ARRAY_BUFFER, 0)"
    bindVAS(NULL, false, true); VL_CHECK_OGL();
  }
}
//-----------------------------------------------------------------------------
void OpenGLContext::bindVAS(const IVertexAttribSet* vas, bool use_bo, bool force)
{
  VL_CHECK_OGL();

  // bring opengl to a known state

  if (vas != mCurVAS || force)
  {

    if (!vas || force)
    {
      mCurVAS = NULL;

      // reset all internal states

      for(int i=0; i<VL_MAX_GENERIC_VERTEX_ATTRIB; ++i)
      {
        mVertexAttrib[i].mEnabled = false; // not used
        mVertexAttrib[i].mPtr = 0;
        mVertexAttrib[i].mBufferObject = 0;
        mVertexAttrib[i].mState = 0;
      }

      for(int i=0; i<VL_MAX_TEXTURE_UNITS; ++i)
      {
        mTexCoordArray[i].mEnabled = false; // not used
        mTexCoordArray[i].mPtr = 0;
        mTexCoordArray[i].mBufferObject = 0;
        mTexCoordArray[i].mState = 0;
      }

      mVertexArray.mEnabled = false;
      mVertexArray.mPtr = 0;
      mVertexArray.mBufferObject = 0;
      mVertexArray.mState = 0; // not used

      mNormalArray.mEnabled = false;
      mNormalArray.mPtr = 0;
      mNormalArray.mBufferObject = 0;
      mNormalArray.mState = 0; // not used

      mColorArray.mEnabled = false;
      mColorArray.mPtr = 0;
      mColorArray.mBufferObject = 0;
      mColorArray.mState = 0; // not used

      mSecondaryColorArray.mEnabled = false;
      mSecondaryColorArray.mPtr = 0;
      mSecondaryColorArray.mBufferObject = 0;
      mSecondaryColorArray.mState = 0; // not used

      mFogArray.mEnabled = false;
      mFogArray.mPtr = 0;
      mFogArray.mBufferObject = 0;
      mFogArray.mState = 0; // not used

      // reset all gl states

      for(int i=0; i<mMaxVertexAttrib; ++i)
        VL_glDisableVertexAttribArray(i); VL_CHECK_OGL();

      // note this one
      VL_glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0); VL_CHECK_OGL();

      VL_glBindBuffer(GL_ARRAY_BUFFER, 0); VL_CHECK_OGL();

      if(Has_Fixed_Function_Pipeline)
      {
        // iterate backwards so the last active is #0
        for ( int i=mTextureSamplerCount; i--; )
        {
          VL_glClientActiveTexture(GL_TEXTURE0 + i); VL_CHECK_OGL();
          glDisableClientState(GL_TEXTURE_COORD_ARRAY); VL_CHECK_OGL();
        }

        glDisableClientState(GL_VERTEX_ARRAY); VL_CHECK_OGL();
        glDisableClientState(GL_NORMAL_ARRAY); VL_CHECK_OGL();
        glDisableClientState(GL_COLOR_ARRAY); VL_CHECK_OGL();

        // not supported under GLES
#if defined(VL_OPENGL)
        glDisableClientState(GL_SECONDARY_COLOR_ARRAY); VL_CHECK_OGL();
        glDisableClientState(GL_FOG_COORD_ARRAY); VL_CHECK_OGL();
#endif
      }
    }

    if (vas)
    {
      int buf_obj = 0;
      const unsigned char* ptr = 0;
      bool enabled = false;

      if(Has_Fixed_Function_Pipeline)
      {

        // ----- vertex array -----

        enabled = vas->vertexArray() != NULL;
        if ( mVertexArray.mEnabled || enabled )
        {
          if (enabled)
          {
            if ( use_bo && vas->vertexArray()->bufferObject()->handle() )
            {
              buf_obj = vas->vertexArray()->bufferObject()->handle();
              ptr = 0;
            }
            else
            {
              buf_obj = 0;
              ptr = vas->vertexArray()->bufferObject()->ptr();
            }
            if ( mVertexArray.mPtr != ptr || mVertexArray.mBufferObject != buf_obj )
            {
              if (!mVertexArray.mEnabled)
              {
                glEnableClientState(GL_VERTEX_ARRAY); VL_CHECK_OGL();
              }
              // mic fixme:
              // Note: for the moment we threat glBindBuffer and glVertexPointer as an atomic operation.
              // In the future we'll want to eliminate all direct calls to glBindBuffer and similar an
              // go through the OpenGLContext that will lazily do everything.
              VL_glBindBuffer(GL_ARRAY_BUFFER, buf_obj); VL_CHECK_OGL();
              glVertexPointer((int)vas->vertexArray()->glSize(), vas->vertexArray()->glType(), /*stride*/0, ptr); VL_CHECK_OGL();
              mVertexArray.mPtr = ptr;
              mVertexArray.mBufferObject = buf_obj;
            }
          }
          else
          {
            glDisableClientState(GL_VERTEX_ARRAY); VL_CHECK_OGL();
            mVertexArray.mPtr = 0;
            mVertexArray.mBufferObject = 0;
          }
          mVertexArray.mEnabled = enabled;
        }

        // ----- normal array -----

        enabled = vas->normalArray() != NULL;
        if ( mNormalArray.mEnabled || enabled )
        {
          if (enabled)
          {
            if ( use_bo && vas->normalArray()->bufferObject()->handle() )
            {
              buf_obj = vas->normalArray()->bufferObject()->handle();
              ptr = 0;
            }
            else
            {
              buf_obj = 0;
              ptr = vas->normalArray()->bufferObject()->ptr();
            }
            if ( mNormalArray.mPtr != ptr || mNormalArray.mBufferObject != buf_obj )
            {
              if (!mNormalArray.mEnabled)
              {
                glEnableClientState(GL_NORMAL_ARRAY); VL_CHECK_OGL();
              }
              VL_glBindBuffer(GL_ARRAY_BUFFER, buf_obj); VL_CHECK_OGL(); 
              glNormalPointer(vas->normalArray()->glType(), /*stride*/0, ptr); VL_CHECK_OGL();
              mNormalArray.mPtr = ptr;
              mNormalArray.mBufferObject = buf_obj;
            }
          }
          else
          {
            glDisableClientState(GL_NORMAL_ARRAY); VL_CHECK_OGL();

            // restore constant normal
            glNormal3f( mNormal.x(), mNormal.y(), mNormal.z() );

            mNormalArray.mPtr = 0;
            mNormalArray.mBufferObject = 0;
          }
          mNormalArray.mEnabled = enabled;
        }

        // ----- color array -----

        enabled = vas->colorArray() != NULL;
        if ( mColorArray.mEnabled || enabled )
        {
          if (enabled)
          {
            if ( use_bo && vas->colorArray()->bufferObject()->handle() )
            {
              buf_obj = vas->colorArray()->bufferObject()->handle();
              ptr = 0;
            }
            else
            {
              buf_obj = 0;
              ptr = vas->colorArray()->bufferObject()->ptr();
            }
            if ( mColorArray.mPtr != ptr || mColorArray.mBufferObject != buf_obj )
            {
              if (!mColorArray.mEnabled)
              {
                glEnableClientState(GL_COLOR_ARRAY); VL_CHECK_OGL();
              }
              VL_glBindBuffer(GL_ARRAY_BUFFER, buf_obj); VL_CHECK_OGL();
              glColorPointer((int)vas->colorArray()->glSize(), vas->colorArray()->glType(), /*stride*/0, ptr); VL_CHECK_OGL();
              mColorArray.mPtr = ptr;
              mColorArray.mBufferObject = buf_obj;
            }
          }
          else
          {
            glDisableClientState(GL_COLOR_ARRAY); VL_CHECK_OGL();

            // restore constant color
            glColor4f( mColor.r(), mColor.g(), mColor.b(), mColor.a() );

            mColorArray.mPtr = 0;
            mColorArray.mBufferObject = 0;
          }
          mColorArray.mEnabled = enabled;
        }

        // ----- secondary color array -----

        enabled = vas->secondaryColorArray() != NULL;
        if ( mSecondaryColorArray.mEnabled || enabled )
        {
          if (enabled)
          {
            if ( use_bo && vas->secondaryColorArray()->bufferObject()->handle() )
            {
              buf_obj = vas->secondaryColorArray()->bufferObject()->handle();
              ptr = 0;
            }
            else
            {
              buf_obj = 0;
              ptr = vas->secondaryColorArray()->bufferObject()->ptr();
            }
            if ( mSecondaryColorArray.mPtr != ptr || mSecondaryColorArray.mBufferObject != buf_obj )
            {
              if (!mSecondaryColorArray.mEnabled)
              {
                glEnableClientState(GL_SECONDARY_COLOR_ARRAY); VL_CHECK_OGL();
              }
              VL_glBindBuffer(GL_ARRAY_BUFFER, buf_obj); VL_CHECK_OGL();
              glSecondaryColorPointer((int)vas->secondaryColorArray()->glSize(), vas->secondaryColorArray()->glType(), /*stride*/0, ptr); VL_CHECK_OGL();
              mSecondaryColorArray.mPtr = ptr;
              mSecondaryColorArray.mBufferObject = buf_obj;
            }
          }
          else
          {
            glDisableClientState(GL_SECONDARY_COLOR_ARRAY); VL_CHECK_OGL();

            // restore constant secondary color
            VL_glSecondaryColor3f( mSecondaryColor.r(), mSecondaryColor.g(), mSecondaryColor.b() );

            mSecondaryColorArray.mPtr = 0;
            mSecondaryColorArray.mBufferObject = 0;
          }
          mSecondaryColorArray.mEnabled = enabled;
        }

        // ----- fog array -----

        enabled = vas->fogCoordArray() != NULL;
        if ( mFogArray.mEnabled || enabled )
        {
          if (enabled)
          {
            if ( use_bo && vas->fogCoordArray()->bufferObject()->handle() )
            {
              buf_obj = vas->fogCoordArray()->bufferObject()->handle();
              ptr = 0;
            }
            else
            {
              buf_obj = 0;
              ptr = vas->fogCoordArray()->bufferObject()->ptr();
            }
            if ( mFogArray.mPtr != ptr || mFogArray.mBufferObject != buf_obj )
            {
              if (!mFogArray.mEnabled)
              {
                glEnableClientState(GL_FOG_COORD_ARRAY); VL_CHECK_OGL();
              }
              VL_glBindBuffer(GL_ARRAY_BUFFER, buf_obj); VL_CHECK_OGL();
              glFogCoordPointer(vas->fogCoordArray()->glType(), /*stride*/0, ptr); VL_CHECK_OGL();
              mFogArray.mPtr = ptr;
              mFogArray.mBufferObject = buf_obj;
            }
          }
          else
          {
            glDisableClientState(GL_FOG_COORD_ARRAY); VL_CHECK_OGL();
            mFogArray.mPtr = 0;
            mFogArray.mBufferObject = 0;
          }
          mFogArray.mEnabled = enabled;
        }

        // ----- texture coords -----

        // (1) enable pass
        for(int i=0; i<vas->texCoordArrayCount(); ++i)
        {
          // texture array info
          const ArrayAbstract* texarr = NULL;
          int tex_unit = 0;
          vas->getTexCoordArrayAt(i, tex_unit, texarr);
          VL_CHECK(tex_unit<VL_MAX_TEXTURE_UNITS);

          mTexCoordArray[tex_unit].mState += 1; // 0 -> 1; 1 -> 2;
          VL_CHECK( mTexCoordArray[tex_unit].mState == 1 || mTexCoordArray[tex_unit].mState == 2 );

          if ( use_bo && texarr->bufferObject()->handle() )
          {
            buf_obj = texarr->bufferObject()->handle();
            ptr = 0;
          }
          else
          {
            buf_obj = 0;
            ptr = texarr->bufferObject()->ptr();
          }
          if ( mTexCoordArray[tex_unit].mPtr != ptr || mTexCoordArray[tex_unit].mBufferObject != buf_obj )
          {
            mTexCoordArray[tex_unit].mPtr = ptr;
            mTexCoordArray[tex_unit].mBufferObject = buf_obj;

            VL_glClientActiveTexture(GL_TEXTURE0 + tex_unit); VL_CHECK_OGL();
            VL_glBindBuffer(GL_ARRAY_BUFFER, buf_obj); VL_CHECK_OGL();
            #if !defined(NDEBUG)
              if ( Has_GLES_Version_1_1 && texarr->glSize() == 1)
              {
                Log::error("OpenGL ES does not allow 1D texture coordinates.\n"); VL_TRAP();
              }
            #endif
            glTexCoordPointer((int)texarr->glSize(), texarr->glType(), 0/*texarr->stride()*/, ptr/*+ texarr->offset()*/); VL_CHECK_OGL();

            // enable if not previously enabled
            if (mTexCoordArray[tex_unit].mState == 1)
            {
              glEnableClientState(GL_TEXTURE_COORD_ARRAY); VL_CHECK_OGL();
            }
            else
            {
              VL_CHECK(glIsEnabled(GL_TEXTURE_COORD_ARRAY));
            }
          }
        }

        // (2) disable pass
        if (mCurVAS)
        {
          for(int i=0; i<mCurVAS->texCoordArrayCount(); ++i)
          {
            // texture array info
            const ArrayAbstract* texarr = NULL;
            int tex_unit = 0;
            mCurVAS->getTexCoordArrayAt(i, tex_unit, texarr);
            VL_CHECK(tex_unit<VL_MAX_TEXTURE_UNITS);

            // disable if not used by new VAS
            if ( mTexCoordArray[tex_unit].mState == 1 )
            {
              VL_glClientActiveTexture(GL_TEXTURE0 + tex_unit); VL_CHECK_OGL();
              glDisableClientState(GL_TEXTURE_COORD_ARRAY); VL_CHECK_OGL();

              mTexCoordArray[tex_unit].mPtr = 0;
              mTexCoordArray[tex_unit].mBufferObject = 0;
            }

            mTexCoordArray[tex_unit].mState >>= 1; // 1 -> 0; 2 -> 1;
          }
        }

      } // Has_Fixed_Function_Pipeline

      // ----- vertex attrib -----

      // (1) enable pass
      for(int i=0; i<vas->vertexAttribArrays()->size(); ++i)
      {
        const VertexAttribInfo* info = vas->vertexAttribArrays()->at(i);
        int idx = info->attribLocation();

        mVertexAttrib[idx].mState += 1; // 0 -> 1; 1 -> 2;
        VL_CHECK( mVertexAttrib[idx].mState == 1 || mVertexAttrib[idx].mState == 2 );

        if ( use_bo && info->data()->bufferObject()->handle() )
        {
          buf_obj = info->data()->bufferObject()->handle();
          ptr = 0;
        }
        else
        {
          buf_obj = 0;
          ptr = info->data()->bufferObject()->ptr();
        }
        if ( mVertexAttrib[idx].mPtr != ptr || mVertexAttrib[idx].mBufferObject != buf_obj )
        {
          mVertexAttrib[idx].mPtr = ptr;
          mVertexAttrib[idx].mBufferObject = buf_obj;
          VL_glBindBuffer(GL_ARRAY_BUFFER, buf_obj); VL_CHECK_OGL();

          if ( info->interpretation() == VAI_NORMAL )
          {
            VL_glVertexAttribPointer( idx, (int)info->data()->glSize(), info->data()->glType(), info->normalize(), /*stride*/0, ptr ); VL_CHECK_OGL();
          }
          else
          if ( info->interpretation() == VAI_INTEGER )
          {
            VL_glVertexAttribIPointer( idx, (int)info->data()->glSize(), info->data()->glType(), /*stride*/0, ptr ); VL_CHECK_OGL();
          }
          else
          if ( info->interpretation() == VAI_DOUBLE )
          {
            VL_glVertexAttribLPointer( idx, (int)info->data()->glSize(), info->data()->glType(), /*stride*/0, ptr ); VL_CHECK_OGL();
          }

          // enable if not previously enabled
          if (mVertexAttrib[idx].mState == 1)
          {
            VL_glEnableVertexAttribArray( idx ); VL_CHECK_OGL();
          }
          else
          {
            // make sure it is actually enabled
            #if !defined(NDEBUG)
              GLint enabled = 0;
              glGetVertexAttribiv( idx, GL_VERTEX_ATTRIB_ARRAY_ENABLED, &enabled); VL_CHECK(enabled);
            #endif
          }
        }
      }

      // (2) disable pass
      if (mCurVAS)
      {
        for(int i=0; i<mCurVAS->vertexAttribArrays()->size(); ++i)
        {
          // vertex array
          const VertexAttribInfo* info = mCurVAS->vertexAttribArrays()->at(i);
          VL_CHECK(info)
          int idx = info->attribLocation();
          // disable if not used by new VAS
          if ( mVertexAttrib[idx].mState == 1 )
          {
            VL_glDisableVertexAttribArray( idx ); VL_CHECK_OGL();

            // restore constant vertex attrib
            glVertexAttrib4fv( idx, mVertexAttribValue[idx].ptr() ); VL_CHECK_OGL();

            mVertexAttrib[idx].mPtr = 0;
            mVertexAttrib[idx].mBufferObject = 0;
          }

          mVertexAttrib[idx].mState >>= 1; // 1 -> 0; 2 -> 1;
        }
      }

      // ----- end -----

      // Note: we don't call "glBindBuffer(GL_ARRAY_BUFFER, 0)" here as it will be called by Renderer::render() just before exiting.
      // VL_glBindBuffer(GL_ARRAY_BUFFER, 0); VL_CHECK_OGL();

    } // if(vas)

  } // if(vas != mCurVAS || force)

  mCurVAS = vas;

  VL_CHECK_OGL();
}
//-----------------------------------------------------------------------------
