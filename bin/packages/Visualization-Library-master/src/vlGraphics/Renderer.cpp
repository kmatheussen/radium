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
#include <vlGraphics/Renderer.hpp>
#include <vlGraphics/OpenGLContext.hpp>
#include <vlGraphics/GLSL.hpp>
#include <vlGraphics/RenderQueue.hpp>
#include <vlCore/Log.hpp>

using namespace vl;

//------------------------------------------------------------------------------
// Renderer
//------------------------------------------------------------------------------
Renderer::Renderer()
{
  VL_DEBUG_SET_OBJECT_NAME()

  mProjViewTransfCallback = new ProjViewTransfCallback;

  mDummyEnables  = new EnableSet;
  mDummyStateSet = new RenderStateSet;
}
//------------------------------------------------------------------------------
namespace
{
  struct GLSLProgState
  {
  public:
    GLSLProgState(): mCamera(NULL), mTransform(NULL), mGLSLProgUniformSet(NULL), mShaderUniformSet(NULL), mActorUniformSet(NULL) {}

    bool operator<(const GLSLProgState& other) const
    {
      if ( mCamera != other.mCamera )
        return mCamera < other.mCamera;
      else
      if ( mTransform != other.mTransform )
        return mTransform < other.mTransform;
      else
      if ( mGLSLProgUniformSet != other.mGLSLProgUniformSet )
        return mGLSLProgUniformSet < other.mGLSLProgUniformSet;
      else
      if ( mShaderUniformSet != other.mShaderUniformSet ) 
        return mShaderUniformSet < other.mShaderUniformSet;
      else
        return mActorUniformSet < other.mActorUniformSet;
    }

    const Camera* mCamera;
    const Transform* mTransform;
    const UniformSet* mGLSLProgUniformSet;
    const UniformSet* mShaderUniformSet;
    const UniformSet* mActorUniformSet;
  };
}
//------------------------------------------------------------------------------
const RenderQueue* Renderer::render(const RenderQueue* render_queue, Camera* camera, real frame_clock)
{
  VL_CHECK_OGL()

  // skip if renderer is disabled

  if (enableMask() == 0)
    return render_queue;

  // enter/exit behavior contract

  class InOutContract 
  {
    Renderer* mRenderer;
    std::vector<RenderStateSlot> mOriginalDefaultRS;
  public:
    InOutContract(Renderer* renderer, Camera* camera): mRenderer(renderer)
    {
      // increment the render tick.
      mRenderer->mRenderTick++;

      // render-target activation.
      // note: an OpenGL context can have multiple rendering targets!
      mRenderer->framebuffer()->activate();

      // viewport setup.
      camera->viewport()->setClearFlags( mRenderer->clearFlags() );
      camera->viewport()->activate();

      OpenGLContext* gl_context = renderer->framebuffer()->openglContext();

      // default render states override
      for(size_t i=0; i<renderer->overriddenDefaultRenderStates().size(); ++i)
      {
        // save overridden default render state to be restored later
        ERenderState type = renderer->overriddenDefaultRenderStates()[i].type();
        mOriginalDefaultRS.push_back(gl_context->defaultRenderState(type));
        // set new default render state
        gl_context->setDefaultRenderState(renderer->overriddenDefaultRenderStates()[i]);
      }

      // dispatch the renderer-started event.
      mRenderer->dispatchOnRendererStarted();

      // check user-generated errors.
      VL_CHECK_OGL()
    }

    ~InOutContract()
    {
      // dispatch the renderer-finished event
      mRenderer->dispatchOnRendererFinished();

      OpenGLContext* gl_context = mRenderer->framebuffer()->openglContext();

      // restore default render states
      for(size_t i=0; i<mOriginalDefaultRS.size(); ++i)
      {
        gl_context->setDefaultRenderState(mOriginalDefaultRS[i]);
      }

      VL_CHECK( !globalSettings()->checkOpenGLStates() || mRenderer->framebuffer()->openglContext()->isCleanState(true) );

      // check user-generated errors.
      VL_CHECK_OGL()

      // note: we don't reset the render target here
    }
  } contract(this, camera);

  // --------------- rendering --------------- 

  std::map<const GLSLProgram*, GLSLProgState> glslprogram_map;

  OpenGLContext* opengl_context = framebuffer()->openglContext();

  // --------------- default scissor ---------------

  // non GLSLProgram state sets
  const RenderStateSet* cur_render_state_set = NULL;
  const EnableSet* cur_enable_set = NULL;
  const Scissor* cur_scissor = NULL;

  // scissor the viewport by default: needed for points and lines since they are not clipped against the viewport
  // this is already setup by the Viewport
  /*
  glEnable(GL_SCISSOR_TEST);
  glScissor(camera->viewport()->x(), camera->viewport()->y(), camera->viewport()->width(), camera->viewport()->height());
  */

  // --------------- rendering ---------------

  for(int itok=0; itok < render_queue->size(); ++itok)
  {
    const RenderToken* tok = render_queue->at(itok); VL_CHECK(tok);
    Actor* actor = tok->mActor; VL_CHECK(actor);

    if ( !isEnabled(actor->enableMask()) )
      continue;

    // --------------- Actor's scissor ---------------

    // mic fixme:this kind of scissor management is not particularly elegant.
    // It is required mainly for convenience for the vector graphics that allow the specification of a clipping rectangular area at any point in the rendering.
    // We must also find a good general solution to support indexed scissoring and viewport.

    const Scissor* scissor = actor->scissor() ? actor->scissor() : tok->mShader->scissor();
    if (cur_scissor != scissor)
    {
      cur_scissor = scissor;
      if (cur_scissor)
      {
        cur_scissor->enable(camera->viewport());
      }
      else
      {
        // scissor the viewport by default: needed for points and lines with size > 1.0 as they are not clipped against the viewport.
        VL_CHECK(glIsEnabled(GL_SCISSOR_TEST))
        glScissor(camera->viewport()->x(), camera->viewport()->y(), camera->viewport()->width(), camera->viewport()->height());
      }
    }

    // multipassing
    for( int ipass=0; tok != NULL; tok = tok->mNextPass, ++ipass )
    {
      VL_CHECK_OGL()

      // --------------- shader setup ---------------

      const Shader* shader = tok->mShader;

      // shader override: select the first that matches

      for( std::map< unsigned int, ref<Shader> >::const_iterator eom_it = mShaderOverrideMask.begin(); 
           eom_it != mShaderOverrideMask.end(); 
           ++eom_it )
      {
        if ( eom_it->first & actor->enableMask() )
        {
          shader = eom_it->second.get();
          break;
        }
      }

      // shader's render states

      if ( cur_render_state_set != shader->getRenderStateSet() )
      {
        opengl_context->applyRenderStates( shader->getRenderStateSet(), camera );
        cur_render_state_set = shader->getRenderStateSet();
      }

      VL_CHECK_OGL()

      // shader's enables

      if ( cur_enable_set != shader->getEnableSet() )
      {
        opengl_context->applyEnables( shader->getEnableSet() );
        cur_enable_set = shader->getEnableSet();
      }

      #ifndef NDEBUG
        if (glGetError() != GL_NO_ERROR)
        {
          Log::error("An unsupported OpenGL glEnable/glDisable capability has been enabled!\n");
          VL_TRAP()
        }
      #endif

      // --------------- Actor pre-render callback ---------------

      // here the user has still the possibility to modify the Actor's uniforms

      actor->dispatchOnActorRenderStarted( frame_clock, camera, tok->mRenderable, shader, ipass );

      VL_CHECK_OGL()

      // --------------- GLSLProgram setup ---------------

      VL_CHECK( !shader->glslProgram() || shader->glslProgram()->linked() );

      VL_CHECK_OGL()

      // current transform
      const Transform*   cur_transform             = actor->transform(); 
      const GLSLProgram* cur_glsl_program          = NULL; // NULL == fixed function pipeline
      const UniformSet*  cur_glsl_prog_uniform_set = NULL;
      const UniformSet*  cur_shader_uniform_set    = NULL;
      const UniformSet*  cur_actor_uniform_set     = NULL;

      // make sure we update these things only if there is a valid GLSLProgram
      if (shader->glslProgram() && shader->glslProgram()->handle())
      {
        cur_glsl_program = shader->glslProgram();

        // consider them NULL if they are empty
        if (cur_glsl_program->getUniformSet() && !cur_glsl_program->getUniformSet()->uniforms().empty())
          cur_glsl_prog_uniform_set = cur_glsl_program->getUniformSet();

        if (shader->getUniformSet() && !shader->getUniformSet()->uniforms().empty())
          cur_shader_uniform_set = shader->getUniformSet();
        
        if (actor->getUniformSet() && !actor->getUniformSet()->uniforms().empty())
          cur_actor_uniform_set = actor->getUniformSet();
      } 

      bool update_cm = false; // update camera
      bool update_tr = false; // update transform
      bool update_pu = false; // update glsl-program uniforms
      bool update_su = false; // update shader uniforms
      bool update_au = false; // update actor uniforms
      GLSLProgState* glsl_state = NULL;

      // retrieve the state of this GLSLProgram (including the NULL one)
      std::map<const GLSLProgram*, GLSLProgState>::iterator glsl_state_it = glslprogram_map.find(cur_glsl_program);
      
      if ( glsl_state_it == glslprogram_map.end() )
      {
        //
        // this is the first time we see this GLSL program so we update everything we can
        //

        // create a new glsl-state entry
        glsl_state = &glslprogram_map[cur_glsl_program];
        update_cm = true;
        update_tr = true;
        update_pu = cur_glsl_prog_uniform_set != NULL;
        update_su = cur_shader_uniform_set    != NULL;
        update_au = cur_actor_uniform_set     != NULL;
      }
      else
      {
        //
        // we already know this GLSLProgram so we update only what has changed since last time
        //

        glsl_state = &glsl_state_it->second;
        // check for differences
        update_cm = glsl_state->mCamera             != camera;
        update_tr = glsl_state->mTransform          != cur_transform;
        update_pu = glsl_state->mGLSLProgUniformSet != cur_glsl_prog_uniform_set && cur_glsl_prog_uniform_set != NULL;
        update_su = glsl_state->mShaderUniformSet   != cur_shader_uniform_set    && cur_shader_uniform_set    != NULL;
        update_au = glsl_state->mActorUniformSet    != cur_actor_uniform_set     && cur_actor_uniform_set     != NULL;
      }

      // update glsl-state structure
      glsl_state->mCamera             = camera;
      glsl_state->mTransform          = cur_transform;
      glsl_state->mGLSLProgUniformSet = cur_glsl_prog_uniform_set;
      glsl_state->mShaderUniformSet   = cur_shader_uniform_set;
      glsl_state->mActorUniformSet    = cur_actor_uniform_set;

      // --- update proj, view and transform matrices ---

      VL_CHECK_OGL()

      if (update_cm || update_tr)
        projViewTransfCallback()->updateMatrices( update_cm, update_tr, cur_glsl_program, camera, cur_transform );

      VL_CHECK_OGL()

      // --- uniforms ---

      // note: the user must not make the glslprogram's, shader's and actor's uniforms collide!
      VL_CHECK( !opengl_context->areUniformsColliding(cur_shader_uniform_set, cur_actor_uniform_set) );
      VL_CHECK( !opengl_context->areUniformsColliding(cur_shader_uniform_set, cur_glsl_prog_uniform_set ) );
      VL_CHECK( !opengl_context->areUniformsColliding(cur_actor_uniform_set, cur_glsl_prog_uniform_set ) );

      VL_CHECK_OGL()

      // glsl program uniform set
      if ( update_pu )
      {
        VL_CHECK( cur_glsl_prog_uniform_set && cur_glsl_prog_uniform_set->uniforms().size() );
        VL_CHECK( shader->getRenderStateSet()->glslProgram() && shader->getRenderStateSet()->glslProgram()->handle() )
        cur_glsl_program->applyUniformSet( cur_glsl_prog_uniform_set );
      }

      VL_CHECK_OGL()

      // shader uniform set
      if ( update_su )
      {
        VL_CHECK( cur_shader_uniform_set && cur_shader_uniform_set->uniforms().size() );
        VL_CHECK( shader->getRenderStateSet()->glslProgram() && shader->getRenderStateSet()->glslProgram()->handle() )
        cur_glsl_program->applyUniformSet( cur_shader_uniform_set );
      }

      VL_CHECK_OGL()

      // actor uniform set
      if ( update_au )
      {
        VL_CHECK( cur_actor_uniform_set && cur_actor_uniform_set->uniforms().size() );
        VL_CHECK( shader->getRenderStateSet()->glslProgram() && shader->getRenderStateSet()->glslProgram()->handle() )
        cur_glsl_program->applyUniformSet( cur_actor_uniform_set );
      }

      VL_CHECK_OGL()

      // --------------- Actor rendering ---------------

      // also compiles display lists and updates BufferObjects if necessary
      tok->mRenderable->render( actor, shader, camera, opengl_context );

      VL_CHECK_OGL()

      // if shader is overridden it does not make sense to perform multipassing so we break the loop here.
      if (shader != tok->mShader)
        break;
    }
  }

  // clear enables
  opengl_context->applyEnables( mDummyEnables.get() ); VL_CHECK_OGL();

  // clear render states
  opengl_context->applyRenderStates( mDummyStateSet.get(), NULL ); VL_CHECK_OGL();

  // enabled texture unit #0
  VL_glActiveTexture( GL_TEXTURE0 ); VL_CHECK_OGL();
  if (Has_Fixed_Function_Pipeline)
    VL_glClientActiveTexture( GL_TEXTURE0 ); VL_CHECK_OGL();

  // disable scissor test
  glDisable(GL_SCISSOR_TEST); VL_CHECK_OGL();

  // disable all vertex arrays, note this also calls "glBindBuffer(GL_ARRAY_BUFFER, 0)"
  opengl_context->bindVAS(NULL, false, false); VL_CHECK_OGL();

  return render_queue;
}
//-----------------------------------------------------------------------------
