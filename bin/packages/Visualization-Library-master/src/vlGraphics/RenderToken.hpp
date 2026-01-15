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

#ifndef RenderToken_INCLUDE_ONCE
#define RenderToken_INCLUDE_ONCE

#include <vlGraphics/Actor.hpp>
#include <vlGraphics/Camera.hpp>
#include <vlGraphics/Renderable.hpp>
#include <vlGraphics/Shader.hpp>
#include <vlGraphics/Effect.hpp>

namespace vl
{
  //------------------------------------------------------------------------------
  // RenderToken
  //------------------------------------------------------------------------------
  //! Internally used by the rendering engine
  class RenderToken: public Object
  {
    VL_INSTRUMENT_CLASS(vl::RenderToken, Object)

  public:
    RenderToken(): mNextPass(NULL), mActor(NULL), mShader(NULL), mEffectRenderRank(0), mCameraDistance(0.0)
    {
      VL_DEBUG_SET_OBJECT_NAME()
    }
    const RenderToken* mNextPass;
    
    Actor* mActor; // Actor is non-const as it can be updated by the ActorEventCallback
    Renderable* mRenderable; // Renderable is non-const because Actor is non-const
    const Shader* mShader;
    int mEffectRenderRank;
    // Z distance from the camera. Used for object Z-sorting.
    real mCameraDistance;
  };
  //------------------------------------------------------------------------------
}

#endif
