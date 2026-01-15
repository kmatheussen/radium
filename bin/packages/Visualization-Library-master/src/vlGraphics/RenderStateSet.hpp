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

#ifndef RenderStateSet_INCLUDE_ONCE
#define RenderStateSet_INCLUDE_ONCE

#include <vlCore/Object.hpp>
#include <vlCore/vlnamespace.hpp>
#include <vlGraphics/link_config.hpp>
#include <vlGraphics/RenderState.hpp>
#include <vector>

namespace vl
{
  class GLSLProgram;
  class RenderState;

  /** A set of RenderState objects managed by a Shader.
    * \sa Shader, Effect, Actor */
  class VLGRAPHICS_EXPORT RenderStateSet: public Object
  {
    VL_INSTRUMENT_CLASS(vl::RenderStateSet, Object)

  public:
    RenderStateSet(): mGLSLProgram(NULL)
    {
      VL_DEBUG_SET_OBJECT_NAME()
    }

    RenderStateSet& deepCopyFrom(const RenderStateSet& other);

    RenderStateSet& shallowCopyFrom(const RenderStateSet& other) { mRenderStates = other.mRenderStates; mGLSLProgram = other.mGLSLProgram; return *this; }

    // renderstates getters and setters

    void setRenderState(RenderState* renderstate, int index);

    RenderState* renderState( ERenderState type, int index=-1 );

    const RenderState* renderState( ERenderState type, int index=-1 ) const;

    size_t renderStatesCount() const { return mRenderStates.size(); }

    const RenderStateSlot* renderStates() const { if (mRenderStates.empty()) return NULL; else return &mRenderStates[0]; }

    RenderStateSlot* renderStates() { if (mRenderStates.empty()) return NULL; else return &mRenderStates[0]; }

    //! If index == -1 all the renderstates of the given type are removed regardless of their binding index.
    void eraseRenderState(ERenderState type, int index);

    void eraseAllRenderStates() { mRenderStates.clear(); mGLSLProgram = NULL; }
    
    //! Returns the GLSLProgram associated to a RenderStateSet (if any)
    const GLSLProgram* glslProgram() const { return mGLSLProgram; }

    //! Returns the GLSLProgram associated to a RenderStateSet (if any)
    GLSLProgram* glslProgram() { return mGLSLProgram; }

  protected:
    std::vector< RenderStateSlot > mRenderStates;
    GLSLProgram* mGLSLProgram;
  };
}

#endif
