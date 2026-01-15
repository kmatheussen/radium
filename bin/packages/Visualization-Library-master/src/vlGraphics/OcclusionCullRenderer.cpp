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

#include <vlGraphics/OcclusionCullRenderer.hpp>
#include <vlGraphics/RenderQueue.hpp>
#include <vlGraphics/OpenGLContext.hpp>
#include <vlCore/Log.hpp>
#include <vlCore/Say.hpp>

using namespace vl;

//-----------------------------------------------------------------------------
OcclusionCullRenderer::OcclusionCullRenderer()
{
  VL_DEBUG_SET_OBJECT_NAME()

  mPrevWrapRenderer = NULL;

  mStatsTotalObjects = 0;
  mStatsOccludedObjects = 0;

  mCulledRenderQueue = new RenderQueue;
  mOcclusionThreshold      = 0;

  // todo: support GL 3.x CORE
  mOcclusionShader = new Shader;
  mOcclusionShader->gocDepthMask()->set(false);
  mOcclusionShader->gocDepthFunc()->set(vl::FU_LEQUAL);
  mOcclusionShader->gocColorMask()->set(false, false, false, false);
  mOcclusionShader->enable(vl::EN_CULL_FACE);
  mOcclusionShader->enable(vl::EN_DEPTH_TEST);
  mOcclusionShader->enable(vl::EN_POLYGON_OFFSET_FILL);
  mOcclusionShader->gocPolygonOffset()->set(-1.0f, -1.0f);

  // for debugging purposes only
  // mOcclusionShader->gocColorMask()->set(true, true, true, true);
  // mOcclusionShader->gocPolygonMode()->set(vl::PM_LINE, vl::PM_LINE);
}
//-----------------------------------------------------------------------------
const RenderQueue* OcclusionCullRenderer::render(const RenderQueue* in_render_queue, Camera* camera, real frame_clock)
{
  // skip if renderer is disabled
  if (enableMask() == 0)
    return in_render_queue;

  // enter/exit behavior contract

  class InOutContract 
  {
    RendererAbstract* mRenderer;

  public:
    InOutContract(RendererAbstract* renderer): mRenderer(renderer)
    {
      // increment the render tick.
      mRenderer->incrementRenderTick();

      // dispatch the renderer-started event.
      mRenderer->dispatchOnRendererStarted();

      // check user-generated errors.
      VL_CHECK_OGL()
    }

    ~InOutContract()
    {
      // dispatch the renderer-finished event
      mRenderer->dispatchOnRendererFinished();

      // check user-generated errors.
      VL_CHECK_OGL()
    }
  } contract(this);

  // --------------- rendering --------------- 

  if (!mWrappedRenderer)
  {
    Log::error("OcclusionCullRenderer::render(): no Renderer is wrapped!\n");
    VL_TRAP();
    return in_render_queue;
  }

  // (1)
  // verify visibility from previous occlusion queries.
  render_pass1( in_render_queue );

  // (2)
  // render only non occluded objects.
  mWrappedRenderer->render( mCulledRenderQueue.get(), camera, frame_clock );
  
  // (3)
  // perform occlusion query on all objects.
  render_pass2( in_render_queue, camera );
  
  // return only the visible, non occluded, objects.
  return mCulledRenderQueue.get();
}
//-----------------------------------------------------------------------------
void OcclusionCullRenderer::setWrappedRenderer(Renderer* renderer) 
{ 
  mWrappedRenderer = renderer; 
}
//-----------------------------------------------------------------------------
const Framebuffer* OcclusionCullRenderer::framebuffer() const
{
  if (mWrappedRenderer)
    return mWrappedRenderer->framebuffer();
  else
    return NULL;
}
//-----------------------------------------------------------------------------
Framebuffer* OcclusionCullRenderer::framebuffer()
{
  if (mWrappedRenderer)
    return mWrappedRenderer->framebuffer();
  else
    return NULL;
}
//-----------------------------------------------------------------------------
void OcclusionCullRenderer::render_pass1(const RenderQueue* in_render_queue )
{
  // reset occluded objects statistics
  mStatsOccludedObjects = 0;
  mStatsTotalObjects    = in_render_queue->size();

  // reset visible objects.
  mCulledRenderQueue->clear();

  // iterate incoming render tokens and output only visible ones
  for( int i=0; i<in_render_queue->size(); ++i)
  {
    const Actor* actor = in_render_queue->at(i)->mActor;

    if ( !mWrappedRenderer->isEnabled(actor->enableMask()) )
      continue;

    bool occluded = false;
    VL_CHECK(Has_Occlusion_Query)

    if ( actor->occlusionQuery() && 
         actor->occlusionQueryTick() == mWrappedRenderer->renderTick() && 
         mPrevWrapRenderer == mWrappedRenderer.get() )
    {
      #if 0
        GLint ready = GL_FALSE;
        glGetQueryObjectiv(actor->occlusionQuery(), GL_QUERY_RESULT_AVAILABLE, &ready); VL_CHECK_OGL();
        if (ready == GL_FALSE)
          vl::Log::error("Occlusion culling query not yet available.\n");
      #endif
      // query the occlusion status: note that this might flush the pipeline
      GLint pixels = 0;
      glGetQueryObjectiv(actor->occlusionQuery(), GL_QUERY_RESULT, &pixels); VL_CHECK_OGL();
      if (pixels <= occlusionThreshold())
        occluded = true;
    }

    if (occluded == false)
    {
      // pass over the incoming render token to the list of visible objects
      RenderToken* tok = mCulledRenderQueue->newToken(false);
      *tok = *in_render_queue->at(i);
    }
    else
      mStatsOccludedObjects++;
  }

  mPrevWrapRenderer = mWrappedRenderer.get();
}
//-----------------------------------------------------------------------------
void OcclusionCullRenderer::render_pass2(const RenderQueue* non_occluded_render_queue, Camera* camera)
{
  // note that we return the occluded render queue
  if (enableMask() == 0)
    return;

#ifndef NDEBUG
  GLint buffer = 0;
  glGetIntegerv(GL_ARRAY_BUFFER_BINDING, &buffer);
  VL_CHECK(buffer == 0);
#endif

  // --------------- render target activation --------------- 

  /* keep the currently active render target */
  // framebuffer()->activate();

  // --------------- viewport activation --------------- 

  /* don't touch the current viewport */
  //camera->viewport()->setClearFlags(vl::CF_DO_NOT_CLEAR);
  //camera->viewport()->activate();

  // --------------- default scissor --------------- 

  // scissor the viewport by default: needed for points and lines since they are not clipped against the viewport.
  #if 1
    glEnable(GL_SCISSOR_TEST);
    glScissor(camera->viewport()->x(), camera->viewport()->y(), camera->viewport()->width(), camera->viewport()->height());
  #else
    glDisable(GL_SCISSOR_TEST);
  #endif

  const Scissor* cur_scissor = NULL;

  // --------------- setup occlusion shader once and for all ---------------

  OpenGLContext* opengl_context = framebuffer()->openglContext();
  GLSLProgram*   glsl_program   = mOcclusionShader->glslProgram();
  Transform*     cur_transform  = NULL;

  opengl_context->resetRenderStates();
  opengl_context->resetEnables();
  opengl_context->applyRenderStates( mOcclusionShader->getRenderStateSet(), camera );
  opengl_context->applyEnables( mOcclusionShader->getEnableSet() );
  projViewTransfCallback()->updateMatrices( true, true, glsl_program, camera, cur_transform );

  // camera/eye position for later usage

  vec3 eye = camera->modelingMatrix().getT();

  // --------------- rendering ---------------

  // iterate over render tokens

  // glColor3f(1.0f, 0.0f, 1.0f); // for debugging only
  glEnableClientState(GL_VERTEX_ARRAY); VL_CHECK_OGL();

  for( int i=0; i<non_occluded_render_queue->size(); ++i)
  {
    const RenderToken* tok = non_occluded_render_queue->at(i);
    Actor* actor = tok->mActor;

    if ( !mWrappedRenderer->isEnabled(actor->enableMask()) )
      continue;

    // --------------- Actor's scissor ---------------

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
        #if 1
          // scissor the viewport by default: needed for points and lines with size > 1.0 as they are not clipped against the viewport.
          VL_CHECK(glIsEnabled(GL_SCISSOR_TEST))
          glScissor(camera->viewport()->x(), camera->viewport()->y(), camera->viewport()->width(), camera->viewport()->height());
        #else
          glDisable(GL_SCISSOR_TEST);
        #endif
      }
    }

    if ( !actor->boundingBox().isInside(eye) )
    {
      VL_CHECK(Has_Occlusion_Query)

      // if occludee -> perform occlusion test to be used for the next frame
      if (actor->isOccludee())
      {

        if (tok->mActor->transform() != cur_transform)
        {
          cur_transform = tok->mActor->transform();
          projViewTransfCallback()->updateMatrices( false, true, glsl_program, camera, cur_transform );
        }

        // register occlusion query tick
        actor->setOcclusionQueryTick( mWrappedRenderer->renderTick() );

        // compute Renderable AABB geometry (we are using the currently active Transform)
        const AABB& aabb = tok->mRenderable->boundingBox();
        const float verts[] = 
        {
          (float)aabb.minCorner().x(), (float)aabb.minCorner().y(), (float)aabb.minCorner().z(),
          (float)aabb.maxCorner().x(), (float)aabb.minCorner().y(), (float)aabb.minCorner().z(),
          (float)aabb.maxCorner().x(), (float)aabb.maxCorner().y(), (float)aabb.minCorner().z(),
          (float)aabb.minCorner().x(), (float)aabb.maxCorner().y(), (float)aabb.minCorner().z(),
          (float)aabb.minCorner().x(), (float)aabb.minCorner().y(), (float)aabb.maxCorner().z(),
          (float)aabb.maxCorner().x(), (float)aabb.minCorner().y(), (float)aabb.maxCorner().z(),
          (float)aabb.maxCorner().x(), (float)aabb.maxCorner().y(), (float)aabb.maxCorner().z(),
          (float)aabb.minCorner().x(), (float)aabb.maxCorner().y(), (float)aabb.maxCorner().z()
        };
        const unsigned quads[] = { 3,2,1,0, 2,6,5,1, 3,7,6,2, 7,3,0,4, 4,0,1,5, 6,7,4,5 };
        glVertexPointer(3, GL_FLOAT, 0, verts); VL_CHECK_OGL();
        actor->createOcclusionQuery(); VL_CHECK_OGL();
        glBeginQuery(GL_SAMPLES_PASSED, actor->occlusionQuery()); VL_CHECK_OGL();
        glDrawElements(GL_QUADS, 6*4, GL_UNSIGNED_INT, quads); VL_CHECK_OGL();
        glEndQuery(GL_SAMPLES_PASSED); VL_CHECK_OGL();
      }
    }
  }

  glDisableClientState(GL_VERTEX_ARRAY); VL_CHECK_OGL();
  glVertexPointer(3, GL_FLOAT, 0, NULL); VL_CHECK_OGL();

  // clear enables
  opengl_context->applyEnables( mDummyEnables.get() );

  // clear render states
  opengl_context->applyRenderStates( mDummyStateSet.get(), camera );

  glDisable(GL_SCISSOR_TEST);
}
//-----------------------------------------------------------------------------
