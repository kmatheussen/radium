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

#ifndef RenderState_INCLUDE_ONCE
#define RenderState_INCLUDE_ONCE

#include <vlCore/Object.hpp>
#include <vlCore/vlnamespace.hpp>

namespace vl
{
  class Camera;
  class OpenGLContext;
  //------------------------------------------------------------------------------
  // RenderState
  //------------------------------------------------------------------------------
  /**
   * Base class for most of the OpenGL render state wrapper classes.
   *
   * \sa Shader, Effect, Actor
  */
  class VLGRAPHICS_EXPORT RenderState: public Object
  {
    VL_INSTRUMENT_ABSTRACT_CLASS(vl::RenderState, Object)

  public:
    RenderState() 
    {
      VL_DEBUG_SET_OBJECT_NAME()
    }
    virtual ERenderState type() const { return RS_NONE; }

    /** The parameter cameara is NULL if we are disabling the state, non-NULL if we are enabling it. */
    virtual void apply(int index, const Camera* camera, OpenGLContext* ctx) const = 0;

    virtual ref<RenderState> clone() const = 0;
  };
  //------------------------------------------------------------------------------
  // RenderStateIndexed
  //------------------------------------------------------------------------------
  /** Base class for those render states which have more than one binding points like lights, clipping planes and texture unit states. */
  class VLGRAPHICS_EXPORT RenderStateIndexed: public RenderState
  {
    VL_INSTRUMENT_ABSTRACT_CLASS(vl::RenderStateIndexed, RenderState)

  public:
    RenderStateIndexed() 
    {
      VL_DEBUG_SET_OBJECT_NAME()
    }
  };
  //------------------------------------------------------------------------------
  // RenderStateNonIndexed
  //------------------------------------------------------------------------------
  /** Base class for those render states which have only one binding point (the vast majority). */
  class VLGRAPHICS_EXPORT RenderStateNonIndexed: public RenderState
  {
    VL_INSTRUMENT_ABSTRACT_CLASS(vl::RenderStateNonIndexed, RenderState)

  public:
    RenderStateNonIndexed() 
    {
      VL_DEBUG_SET_OBJECT_NAME()
    }
  };
  //------------------------------------------------------------------------------
  struct RenderStateSlot
  {
    RenderStateSlot(): mRS(NULL), mIndex(-1) {}
    RenderStateSlot(RenderState* rs, int index): mRS(rs), mIndex(index) {}
    virtual ~RenderStateSlot() {}

    virtual void apply(const Camera* camera, OpenGLContext* ctx) const { mRS->apply( mIndex, camera, ctx ); }

    ERenderState type() const 
    { 
      if (mIndex > 0)
        return (ERenderState)(mRS->type() + mIndex); 
      else
        return mRS->type();
    }

    ref<RenderState> mRS;
    int mIndex;
  };
  //------------------------------------------------------------------------------
}

#endif
