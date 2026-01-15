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

#ifndef Renderer_INCLUDE_ONCE
#define Renderer_INCLUDE_ONCE

#include <vlGraphics/RendererAbstract.hpp>
#include <vlGraphics/ProjViewTransfCallback.hpp>
#include <vlGraphics/Shader.hpp>
#include <map>

namespace vl
{
  //-----------------------------------------------------------------------------
  // Renderer
  //-----------------------------------------------------------------------------
  /** The Renderer class executes the actual rendering on the given RenderQueue.
    * \sa Rendering */
  class VLGRAPHICS_EXPORT Renderer: public RendererAbstract
  {
    VL_INSTRUMENT_CLASS(vl::Renderer, RendererAbstract)

  public:
    Renderer();
    
    virtual ~Renderer() {}
    
    /** Takes as input the render queue to render and returns a possibly filtered render queue for further processing. 
      * Renderer's implementation of this function always returns \p in_render_queue. */
    virtual const RenderQueue* render(const RenderQueue* in_render_queue, Camera* camera, real frame_clock);

    void setProjViewTransfCallback(ProjViewTransfCallback* callback) { mProjViewTransfCallback = callback; }
    
    const ProjViewTransfCallback* projViewTransfCallback() const { return mProjViewTransfCallback.get(); }
    
    ProjViewTransfCallback* projViewTransfCallback() { return mProjViewTransfCallback.get(); }

    /** A bitmask/Shader map used to everride the Shader of those Actors whose enable mask satisfy the following condition: 
        (Actors::enableMask() & bitmask) != 0. Useful when you want to override the Shader of a whole set of Actors.
        If multiple mask/shader pairs match an Actor's enable mask then the shader with the corresponding lowest mask will be used.
        See also vl::Actor::enableMask() and vl::Rendering::effectOverrideMask(). */
    const std::map<unsigned int, ref<Shader> >& shaderOverrideMask() const { return mShaderOverrideMask; }

    /** A bitmask/Shader map used to everride the Shader of those Actors whose enable mask satisfy the following condition: 
        (Actors::enableMask() & bitmask) != 0. Useful when you want to override the Shader of a whole set of Actors.
        If multiple mask/shader pairs match an Actor's enable mask then the shader with the corresponding lowest mask will be used.
        See also vl::Actor::enableMask() and vl::Rendering::effectOverrideMask(). */
    std::map<unsigned int, ref<Shader> >& shaderOverrideMask() { return mShaderOverrideMask; }

    /** Render states that will be used as default by the opengl context by this renderer. 
        Useful for example to setup the default left/right color mask for anaglyph stereo rendering. */
    std::vector<RenderStateSlot>& overriddenDefaultRenderStates() { return mOverriddenDefaultRenderStates; }

    /** Render states that will be used as default by the opengl context by this renderer. 
        Useful for example to setup the default left/right color mask for anaglyph stereo rendering. */
    const std::vector<RenderStateSlot>& overriddenDefaultRenderStates() const { return mOverriddenDefaultRenderStates; }

    bool isEnabled(unsigned int mask) { return (mask & mEnableMask) != 0; }

    /** The Framebuffer on which the rendering is performed. */
    void setFramebuffer(Framebuffer* framebuffer) { mFramebuffer = framebuffer; }

    /** The Framebuffer on which the rendering is performed. */
    const Framebuffer* framebuffer() const { return mFramebuffer.get(); }
    
    /** The Framebuffer on which the rendering is performed. */
    Framebuffer* framebuffer() { return mFramebuffer.get(); }

  protected:
    ref<Framebuffer> mFramebuffer;

    // used to reset the OpenGL states & enables at the end of the rendering.
    vl::ref<EnableSet> mDummyEnables;
    vl::ref<RenderStateSet> mDummyStateSet;

    std::map<unsigned int, ref<Shader> > mShaderOverrideMask;

    std::vector<RenderStateSlot> mOverriddenDefaultRenderStates;

    ref<ProjViewTransfCallback> mProjViewTransfCallback;
  };
  //------------------------------------------------------------------------------
}

#endif
