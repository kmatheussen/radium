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

#ifndef Rendering_INCLUDE_ONCE
#define Rendering_INCLUDE_ONCE

#include <vlGraphics/RenderingAbstract.hpp>
#include <vlGraphics/RenderQueueSorter.hpp>
#include <vlGraphics/Actor.hpp>
#include <vlGraphics/RenderQueue.hpp>
#include <vlGraphics/Renderer.hpp>
#include <vlGraphics/Framebuffer.hpp>
#include <vlGraphics/Camera.hpp>
#include <vlGraphics/SceneManager.hpp>
#include <vlCore/Transform.hpp>
#include <vlCore/Collection.hpp>

namespace vl
{
  /** The Rendering class collects all the information to perform the rendering of a scene.
  The Rendering class performs the following steps:
  -# activates the appropriate OpenGLContext
  -# dispatches the onRenderingStarted() event (see RenderEventCallback class).
  -# activates the Framebuffer's framebuffer and draw buffers
  -# recursively computes the world matrix of the installed Transform hierarchy
  -# setups the Camera transform and the Viewport
  -# extracts all the visible Actor[s] from the installed SceneManager[s]
  -# compiles and sorts the RenderQueue using the installed RenderQueueSorter
  -# uses the installed Renderer to perform the rendering of the RenderQueue
  -# dispatches the onRenderingFinished() event (see RenderEventCallback class).

  To be included in the rendering an Actor must have an enableMask() and Effect::enableMask() such that
  \p "(Actor::enableMask() & Rendering::enableMask()) != 0" and \p "(Actor::effect()->enableMask() & Rendering::enableMask()) != 0".

  \sa

  - Renderer
  - Actor
  - Effect
  - Transform */
  class VLGRAPHICS_EXPORT Rendering: public RenderingAbstract
  {
    VL_INSTRUMENT_CLASS(vl::Rendering, RenderingAbstract)

  public:
    /** Constructor. */
    Rendering();
    
    /** Copy constructor. */
    Rendering(const Rendering& other): RenderingAbstract(other) { *this = other; } 
    
    /** Assignment operator. */
    Rendering& operator=(const Rendering& other);

    /** Executes the rendering. */
    virtual void render();

    /** The RenderQueueSorter used to perform the sorting of the objects to be rendered, if NULL no sorting is performed. */
    void setRenderQueueSorter(RenderQueueSorter* render_queue_sorter) { mRenderQueueSorter = render_queue_sorter; }
    
    /** The RenderQueueSorter used to perform the sorting of the objects to be rendered, if NULL no sorting is performed. */
    RenderQueueSorter* renderQueueSorter() { return mRenderQueueSorter.get(); }

    /** The list of Renderers used to perform the rendering. 
      * The output of one Renderer::render() operation will be fed as input for the next Renderer::render() operation. 
      * \note All the renderers must target the same OpenGL context. */
    const std::vector< ref<Renderer> >& renderers() const { return mRenderers; }

    /** The list of Renderers used to perform the rendering. 
      * The output of one Renderer::render() operation will be fed as input for the next Renderer::render() operation.
      * \note All the renderers must target the same OpenGL context. */
    std::vector< ref<Renderer> >& renderers() { return mRenderers; }

    /** Uitlity function: clears the renderers() list and adds the specified one. */
    void setRenderer(Renderer* renderer) 
    {
      renderers().clear();
      renderers().push_back(renderer);
    }

    /** Utility function: returns the first renderer installed or NULL if none is found. */
    const Renderer* renderer() const
    {
      if (renderers().empty())
        return NULL;
      else
        return renderers()[0].get();
    }

    /** Utility function: returns the first renderer installed or NULL if none is found. */
    Renderer* renderer()
    {
      if (renderers().empty())
        return NULL;
      else
        return renderers()[0].get();
    }

    /** The Camera that defines the point of view and viewport to be used when rendering the scene. */
    void setCamera(Camera* camera) { mCamera = camera; }
    
    /** The Camera that defines the point of view and viewport to be used when rendering the scene. */
    const Camera* camera() const { return mCamera.get(); }
    
    /** The Camera that defines the point of view and viewport to be used when rendering the scene. */
    Camera* camera() { return mCamera.get(); }

    /** Returns the list of SceneManager[s] containing the Actor[s] to be rendered. */
    Collection<SceneManager>* sceneManagers() { return mSceneManagers.get(); }
    
    /** Returns the list of SceneManager[s] containing the Actor[s] to be rendered. */
    const Collection<SceneManager>* sceneManagers() const { return mSceneManagers.get(); }

    /** The root of the Transform tree <b>updated at every rendering frame</b>. For more information 
      * about how and when using it see the documentation of Transform. */
    void setTransform(Transform* transform) { mTransform = transform; }
    
    /** The root of the Transform tree <b>updated at every rendering frame</b>. For more information 
      * about how and when using it see the documentation of Transform. */
    const Transform* transform() const { return mTransform.get(); }
    
    /** The root of the Transform tree <b>updated at every rendering frame</b>. For more information 
      * about how and when using it see the documentation of Transform. */
    Transform* transform() { return mTransform.get(); }

    /** Whether the Level-Of-Detail should be evaluated or not. When disabled lod #0 is used. */
    void setEvaluateLOD(bool evaluate_lod) { mEvaluateLOD = evaluate_lod; }

    /** Whether the Level-Of-Detail should be evaluated or not. When disabled lod #0 is used. */
    bool evaluateLOD() const { return mEvaluateLOD; }

    /** Whether Shader::shaderAnimator()->updateShader() should be called or not. 
    \note
    Only Shader[s] belonging to visible Actor[s] are animated. */
    void setShaderAnimationEnabled(bool animate_shaders) { mShaderAnimationEnabled = animate_shaders; }

    /** Whether Shader::shaderAnimator()->updateShader() should be called or not. 
    \note
    Only Shader[s] belonging to visible Actor[s] are animated. */
    bool shaderAnimationEnabled() const { return mShaderAnimationEnabled; }

    /** Whether the installed SceneManager[s] should perform Actor culling or not in order to maximize the rendering performances. */
    void setCullingEnabled(bool enabled) { mCullingEnabled = enabled; }
    
    /** Whether the installed SceneManager[s] should perform Actor culling or not in order to maximize the rendering performances. */
    bool cullingEnabled() const { return mCullingEnabled; }

    /** Whether OpenGL resources such as textures and GLSL programs should be automatically initialized when first used. 
      * Enabling this features forces VL to keep track of which resources are used for each rendering, which might slighly impact the 
      * rendering time, thus to obtain the maximum performances disable this option and manually initialize your textures and GLSL shaders. */
    void setAutomaticResourceInit(bool enable) { mAutomaticResourceInit = enable; }
    
    /** Whether OpenGL resources such as textures and GLSL programs should be automatically initialized before the rendering takes place. */
    bool automaticResourceInit() const { return mAutomaticResourceInit; }

    /** Returns whether near/far planes optimization is enabled. */
    bool nearFarClippingPlanesOptimized() const { return mNearFarClippingPlanesOptimized; }
    
    /** Enabled/disables near/far planes optimization. When enabled, the automatic near/far clipping planes optimization
      * modifies the projection matrix of the current camera to minimize z-fighting artifacts. If later you disable
      * this feature you might want to recompute the original projection matrix of the camera using the method 
      * vl::Camera::setProjectionPerspective(). */
    void setNearFarClippingPlanesOptimized(bool enabled) { mNearFarClippingPlanesOptimized = enabled; }

    /** A bitmask/Effect map used to everride the Effect of those Actors whose enable mask satisfy the following condition: 
       (Actors::enableMask() & bitmask) != 0. Useful when you want to override the Effect of a whole set of Actors.
        If multiple mask/effect pairs match an Actor's enable mask then the effect with the corresponding lowest mask will be used.
        See also vl::Actor::enableMask() and vl::Renderer::shaderOverrideMask(). */
    const std::map<unsigned int, ref<Effect> >& effectOverrideMask() const { return mEffectOverrideMask; }

    /** A bitmask/Effect map used to everride the Effect of those Actors whose enable mask satisfy the following condition: 
       (Actors::enableMask() & bitmask) != 0. Useful when you want to override the Effect of a whole set of Actors.
        If multiple mask/effect pairs match an Actor's enable mask then the effect with the corresponding lowest mask will be used.
        See also vl::Actor::enableMask() and vl::Renderer::shaderOverrideMask(). */
    std::map<unsigned int, ref<Effect> >& effectOverrideMask() { return mEffectOverrideMask; }

  protected:
    // mic fixme: it would be nice to have a mechanism to request the visible actors at will and to 
    // compile and save the render-queue for later renderings to be reused without recomputing the culling.
    // The user could be able to install actor-list or render-queue and use the flags READ|WRITE|TERMINATE
    // to define wether the list should be used for reading, filled, cleaned up after rendering.
    void fillRenderQueue( ActorCollection* actor_list );
    RenderQueue* renderQueue() { return mRenderQueue.get(); }
    ActorCollection* actorQueue() { return mActorQueue.get(); }

  protected:
    ref<RenderQueueSorter> mRenderQueueSorter;
    ref<ActorCollection> mActorQueue;
    ref<RenderQueue> mRenderQueue;
    std::vector< ref<Renderer> > mRenderers;
    ref<Camera> mCamera;
    ref<Transform> mTransform;
    ref<Collection<SceneManager> > mSceneManagers;
    std::map<unsigned int, ref<Effect> > mEffectOverrideMask;

    bool mAutomaticResourceInit;
    bool mCullingEnabled;
    bool mEvaluateLOD;
    bool mShaderAnimationEnabled;
    bool mNearFarClippingPlanesOptimized;
  };
}

#endif
