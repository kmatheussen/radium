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

#include <vlGraphics/Rendering.hpp>
#include <vlGraphics/OpenGLContext.hpp>
#include <vlGraphics/Renderer.hpp>
#include <vlGraphics/SceneManager.hpp>
#include <vlGraphics/RenderQueue.hpp>
#include <vlGraphics/GLSL.hpp>
#include <vlCore/Log.hpp>
#include <vlCore/Say.hpp>

using namespace vl;

//------------------------------------------------------------------------------
Rendering::Rendering():
  mAutomaticResourceInit(true),
  mCullingEnabled(true),
  mEvaluateLOD(true),
  mShaderAnimationEnabled(true),
  mNearFarClippingPlanesOptimized(false)
{
  VL_DEBUG_SET_OBJECT_NAME()
  mRenderQueueSorter  = new RenderQueueSorterStandard;
  mActorQueue         = new ActorCollection;
  mRenderQueue        = new RenderQueue;
  mSceneManagers      = new Collection<SceneManager>;
  mCamera             = new Camera;
  mTransform          = new Transform;
  mRenderers.push_back( new Renderer );
}
//------------------------------------------------------------------------------
Rendering& Rendering::operator=(const Rendering& other)
{
  super::operator=(other);

  mEnableMask               = other.mEnableMask;
  mAutomaticResourceInit    = other.mAutomaticResourceInit;
  mCullingEnabled    = other.mCullingEnabled;
  mEvaluateLOD              = other.mEvaluateLOD;
  mShaderAnimationEnabled   = other.mShaderAnimationEnabled;
  mNearFarClippingPlanesOptimized = other.mNearFarClippingPlanesOptimized;

  mRenderQueueSorter   = other.mRenderQueueSorter;
  /*mActorQueue        = other.mActorQueue;*/
  /*mRenderQueue       = other.mRenderQueue;*/
  *mSceneManagers      = *other.mSceneManagers;
  mRenderers           = other.mRenderers;
  mCamera              = other.mCamera;
  mTransform           = other.mTransform;

  return *this;
}
//------------------------------------------------------------------------------
void Rendering::render()
{
  VL_CHECK(camera());
  VL_CHECK(camera()->viewport());

  // if rendering is disabled skip all.

  if ( enableMask() == 0 )
    return;

  // enter/exit behavior contract

  class InOutContract
  {
    Rendering* mRendering;
    OpenGLContext* mOpenGLContext;

  public:
    InOutContract(Rendering* rendering): mRendering(rendering)
    {
      VL_CHECK(mRendering->renderers().size());
      VL_CHECK(mRendering->renderers()[0]->framebuffer());
      VL_CHECK(mRendering->renderers()[0]->framebuffer()->openglContext());

      // as stated in the documentation all the renderers must target the same OpenGLContext
      mOpenGLContext = mRendering->renderers()[0]->framebuffer()->openglContext();

      // activate OpenGL context
      mOpenGLContext->makeCurrent();
      VL_CHECK_OGL(); // the first check must be done when the context is active!

      // render states ]shield[
      mOpenGLContext->resetContextStates(RCS_RenderingStarted);

      // pre rendering callback
      mRendering->dispatchOnRenderingStarted(); 

      // check user-generated errors.
      VL_CHECK_OGL()
    }

    ~InOutContract()
    {
      // post rendering callback
      mRendering->dispatchOnRenderingFinished();

      // release rendered Actors
      mRendering->actorQueue()->resize(0);

      // check user-generated errors.
      VL_CHECK_OGL()

      // render states ]shield[
      mOpenGLContext->resetContextStates(RCS_RenderingFinished); 
    }
  } contract(this);

  // --------------- rendering --------------- 

  if (renderers().empty())
  {
    vl::Log::error("Rendering::render(): no Renderer specified for this Rendering!\n");
    VL_TRAP();
    return;
  }

  if (!renderers()[0]->framebuffer())
  {
    vl::Log::error("Rendering::render(): no RendererTarget specified for Renderer #0!\n");
    VL_TRAP();
    return;
  }

  if (!renderers()[0]->framebuffer()->openglContext())
  {
    vl::Log::error("Rendering::render(): invalid Framebuffer for Renderer #0, OpenGLContext is NULL!\n");
    VL_TRAP();
    return;
  }

  if (sceneManagers()->empty())
    return;

  if (!camera())
    return;

  if (!camera()->viewport())
    return;

  // transform

  if (transform() != NULL)
    transform()->computeWorldMatrixRecursive( camera() );

  // camera transform update (can be redundant)

  if (camera()->boundTransform())
    camera()->setModelingMatrix( camera()->boundTransform()->worldMatrix() );

  VL_CHECK_OGL()

  // culling & actor queue filling

  camera()->computeFrustumPlanes();

  // if near/far clipping planes optimization is enabled don't perform far-culling
  if (nearFarClippingPlanesOptimized())
  {
    // perform only near culling with plane at distance 0
    camera()->frustum().planes().resize(5);
    camera()->frustum().planes()[4] = Plane( camera()->modelingMatrix().getT(), 
                                             camera()->modelingMatrix().getZ());
  }

  actorQueue()->clear();
  for(int i=0; i<sceneManagers()->size(); ++i)
  {
    if ( isEnabled(sceneManagers()->at(i)->enableMask()) )
    {
      if (cullingEnabled() && sceneManagers()->at(i)->cullingEnabled())
      {
        if (sceneManagers()->at(i)->boundsDirty())
          sceneManagers()->at(i)->computeBounds();
        // try to cull the scene with both bsphere and bbox
        bool visible = !camera()->frustum().cull(sceneManagers()->at(i)->boundingSphere()) && 
                       !camera()->frustum().cull(sceneManagers()->at(i)->boundingBox());
        if ( visible )
          sceneManagers()->at(i)->extractVisibleActors( *actorQueue(), camera() );
      }
      else
        sceneManagers()->at(i)->extractActors( *actorQueue() );
    }
  }

  // collect near/far clipping planes optimization information
  if (nearFarClippingPlanesOptimized())
  {
    Sphere world_bounding_sphere;
    for(int i=0; i<actorQueue()->size(); ++i)
      world_bounding_sphere += actorQueue()->at(i)->boundingSphere();

    // compute the optimized
    camera()->computeNearFarOptimizedProjMatrix(world_bounding_sphere);

    // recompute frustum planes to account for new near/far values
    camera()->computeFrustumPlanes();
  }

  // render queue filling

  renderQueue()->clear();
  fillRenderQueue( actorQueue() );

  // sort the rendering queue according to this renderer sorting algorithm

  if (renderQueueSorter())
    renderQueue()->sort( renderQueueSorter(), camera() );

  // --- RENDER THE QUEUE: loop through the renderers, feeding the output of one as input for the next ---

  const RenderQueue* render_queue = renderQueue();
  for(size_t i=0; i<renderers().size(); ++i)
  {
    if (renderers()[i])
    {
      if (renderers()[i]->framebuffer() == NULL)
      {
        vl::Log::error( Say("Rendering::render(): no RendererTarget specified for Renderer #%n!\n") << i );
        VL_TRAP();
        continue;
      }
      
      if (renderers()[i]->framebuffer()->openglContext() == NULL)
      {
        vl::Log::error( Say("Rendering::render(): invalid Framebuffer for Renderer #%n, OpenGLContext is NULL!\n") << i );
        VL_TRAP();
        continue;
      }

      // loop the rendering
      render_queue = renderers()[i]->render( render_queue, camera(), frameClock() );
    }
  }

  VL_CHECK_OGL()
}
//------------------------------------------------------------------------------
void Rendering::fillRenderQueue( ActorCollection* actor_list )
{
  if (actor_list == NULL)
    return;

  if (actor_list->empty())
    return;

  if (camera() == NULL)
    return;

  if (enableMask() == 0)
    return;

  RenderQueue* list = renderQueue();
  std::set<Shader*> shader_set;

  // iterate actor list

  for(int iactor=0; iactor < actor_list->size(); iactor++)
  {
    Actor* actor = actor_list->at(iactor);

    VL_CHECK(actor->lod(0))

    if ( !isEnabled(actor->enableMask()) )
      continue;

    // update the Actor's bounds
    actor->computeBounds();

    Effect* effect = actor->effect();
    VL_CHECK(effect)

    // effect override: select the first that matches
    
    for( std::map< unsigned int, ref<Effect> >::iterator eom_it = mEffectOverrideMask.begin(); 
         eom_it != mEffectOverrideMask.end(); 
         ++eom_it )
    {
      if (eom_it->first & actor->enableMask())
      {
        effect = eom_it->second.get();
        break;
      }
    }

    if ( !isEnabled(effect->enableMask()) )
      continue;

    // --------------- LOD evaluation ---------------

    int effect_lod = effect->evaluateLOD( actor, camera() );

    int geometry_lod = 0;
    if ( evaluateLOD() )
      geometry_lod = actor->evaluateLOD( camera() );

    // --------------- M U L T I   P A S S I N G ---------------

    RenderToken* prev_pass = NULL;
    const int pass_count = effect->lod(effect_lod)->size();
    for(int ipass=0; ipass<pass_count; ++ipass)
    {
      // setup the shader to be used for this pass

      Shader* shader = effect->lod(effect_lod)->at(ipass);

      // --------------- fill render token ---------------

      // create a render token
      RenderToken* tok = list->newToken(prev_pass != NULL);

      // multipass chain: implemented as a linked list
      if ( prev_pass != NULL )
        prev_pass->mNextPass = tok;
      prev_pass = tok;
      tok->mNextPass = NULL;
      // track the current state
      tok->mActor = actor;
      tok->mRenderable = actor->lod(geometry_lod);
      // set the shader used (multipassing shader or effect->shader())
      tok->mShader = shader;

      if ( shaderAnimationEnabled() )
      {
        VL_CHECK(frameClock() >= 0)
        if( frameClock() >= 0 )
        {
          // note that the condition is != as opposed to <
          if ( shader->lastUpdateTime() != frameClock() && shader->shaderAnimator() && shader->shaderAnimator()->isEnabled() )
          {
            // update
            shader->shaderAnimator()->updateShader( shader, camera(), frameClock() );

            // note that we update this after
            shader->setLastUpdateTime( frameClock() );
          }
        }
      }

      if ( automaticResourceInit() && shader_set.find(shader) == shader_set.end() )
      {
        shader_set.insert(shader);

        // link GLSLProgram
        if (shader->glslProgram() && !shader->glslProgram()->linked())
        {
          shader->glslProgram()->linkProgram();
          VL_CHECK( shader->glslProgram()->linked() );
        }

        // lazy texture creation
        if ( shader->gocRenderStateSet() )
        {
          size_t count = shader->gocRenderStateSet()->renderStatesCount();
          RenderStateSlot* states = shader->gocRenderStateSet()->renderStates();
          for( size_t i=0; i<count; ++i )
          {
            if (states[i].mRS->type() == RS_TextureSampler)
            {
              TextureSampler* tex_unit = static_cast<TextureSampler*>( states[i].mRS.get() );
              VL_CHECK(tex_unit);
              if (tex_unit)
              {
                if (tex_unit->texture() && tex_unit->texture()->setupParams())
                  tex_unit->texture()->createTexture();
              }
            }
          }
          
        }
      }

      tok->mEffectRenderRank = effect->renderRank();
    }
  }
}
//------------------------------------------------------------------------------
