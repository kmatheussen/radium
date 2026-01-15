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

#ifndef OcclusionCullRenderer_INCLUDE_ONCE
#define OcclusionCullRenderer_INCLUDE_ONCE

#include <vlGraphics/Renderer.hpp>

namespace vl
{
  //------------------------------------------------------------------------------
  // OcclusionCullRenderer
  //------------------------------------------------------------------------------
  /** Wraps a Renderer performing occlusion culling acceleration. 
    * For more information see \ref pagGuideOcclusionCulling */
  class VLGRAPHICS_EXPORT OcclusionCullRenderer: public Renderer
  {
    VL_INSTRUMENT_CLASS(vl::OcclusionCullRenderer, Renderer)

  public:
    /** Constructor. */
    OcclusionCullRenderer();

    /** Renders using the wrapped renderer but also performing occlusion culling. */
    virtual const RenderQueue* render(const RenderQueue* in_render_queue, Camera* camera, real frame_clock);

    /** The renderer to be wrapped by this occlusion culling renderer */
    void setWrappedRenderer(Renderer* renderer);

    /** The renderer to be wrapped by this occlusion culling renderer */
    const Renderer* wrappedRenderer() const { return mWrappedRenderer.get(); }

    /** The renderer to we wrapped by this occlusion culling renderer */
    Renderer* wrappedRenderer() { return mWrappedRenderer.get(); }

    /** The number of pixels visible for an actor to be considered occluded (default = 0) */
    void setOcclusionThreshold(int threshold) { mOcclusionThreshold = threshold; }

    /** The number of pixels visible for an actor to be considered occluded (default = 0) */
    int occlusionThreshold() const { return mOcclusionThreshold; }

    /** Returns the wrapped Renderer's Framebuffer */
    const Framebuffer* framebuffer() const;

    /** Returns the wrapped Renderer's Framebuffer */
    Framebuffer* framebuffer();

    /** Returns the total number or objects candidate for rendering before occlusion culling. */
    int statsTotalObjects() const { return mStatsTotalObjects; }

    /** Returns the number or objects not rendered due to the occlusion culling. */
    int statsOccludedObjects() const { return mStatsOccludedObjects; }

    /** The Shader used to render the bounding boxes during the occlusion culling query. 
      * For example if you have problems with the zbuffer percision you can access the Shader to modify 
      * the polygon offset settings. */
    Shader* occlusionShader() { return mOcclusionShader.get(); }

    /** The Shader used to render the bounding boxes during the occlusion culling query. 
      * For example if you have problems with the zbuffer percision you can access the Shader to modify 
      * the polygon offset settings. */
    const Shader* occlusionShader() const { return mOcclusionShader.get(); }

    /** The Shader used to render the bounding boxes during the occlusion culling query. 
      * For example if you have problems with the zbuffer percision you can access the Shader to modify 
      * the polygon offset settings. */
    void setOcclusionShader(Shader* occ_sh) { mOcclusionShader = occ_sh; }

  protected:
    /** Retrieves the occlusion culling query results from the previous rendering frame. */
    void render_pass1(const RenderQueue* in_render_queue);

    /** Performs a new set of occlusion culling queries to be tested the next frame. */
    void render_pass2(const RenderQueue* in_render_queue, Camera* camera);

  protected:
    vl::ref<Renderer> mWrappedRenderer;
    ref<Shader> mOcclusionShader;
    ref<RenderQueue> mCulledRenderQueue;
    int mOcclusionThreshold;
    Renderer* mPrevWrapRenderer;
    int mStatsTotalObjects;
    int mStatsOccludedObjects;
  };
  //------------------------------------------------------------------------------
}

#endif
