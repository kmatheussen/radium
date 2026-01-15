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

#ifndef PatchParameter_INCLUDE_ONCE
#define PatchParameter_INCLUDE_ONCE

#include <vlCore/Object.hpp>

namespace vl 
{
  //------------------------------------------------------------------------------
  // PatchParameter
  //------------------------------------------------------------------------------
  //! Wrapper of glPatchParameter(), specifies the parameters for patch primitives, used by vl::DrawCall::setPatchParameter().
  //! See also http://www.opengl.org/sdk/docs/man4/xhtml/glPatchParameter.xml
  class PatchParameter: public Object
  {
    VL_INSTRUMENT_CLASS(vl::PatchParameter, Object)

  public:
    //! Constructor
    PatchParameter(): mPatchVertices(0), mPatchDefaultOuterLevel(fvec4(4,4,4,4)), mPatchDefaultInnerLevel(fvec2(4,4)) 
    {
      VL_DEBUG_SET_OBJECT_NAME()
    }

    //! Applies the glPatchParameter values.
    void apply() const
    {
      VL_CHECK(Has_GL_ARB_tessellation_shader||Has_GL_Version_4_0);
      if (Has_GL_ARB_tessellation_shader||Has_GL_Version_4_0)
      {
        glPatchParameteri(GL_PATCH_VERTICES, mPatchVertices); VL_CHECK_OGL();
        glPatchParameterfv(GL_PATCH_DEFAULT_OUTER_LEVEL, mPatchDefaultOuterLevel.ptr()); VL_CHECK_OGL();
        glPatchParameterfv(GL_PATCH_DEFAULT_INNER_LEVEL, mPatchDefaultInnerLevel.ptr()); VL_CHECK_OGL();
      }
    }

    //! Specifies the number of vertices that will be used to make up a single patch primitive. 
    //! Patch primitives are consumed by the tessellation control shader (if present) and subsequently 
    //! used for tessellation. When primitives are specified using glDrawArrays or a similar function, 
    //! each patch will be made from \a vertices control points, each represented by a vertex taken from 
    //! the enabeld vertex arrays. \a vertices must be greater than zero, and less than or equal to the 
    //! value of GL_MAX_PATCH_VERTICES.
    void setPatchVertices(int vertices) { mPatchVertices = vertices; }

    //! Returns the number of vertices that will be used to make up a single patch primitive. 
    int patchVertices() const { return mPatchVertices; }

    //! The four floating-point values corresponding to the four outer tessellation levels 
    //! for each subsequent patch to be used when no tessellation control shader is present.
    void setPatchDefaultOuterLevel(const fvec4& level) { mPatchDefaultOuterLevel = level; }
    
    //! The four floating-point values corresponding to the four outer tessellation levels 
    //! for each subsequent patch to be used when no tessellation control shader is present.
    const fvec4& patchDefaultOuterLevel() const { return mPatchDefaultOuterLevel; }

    //! The two floating-point values corresponding to the tow inner tessellation levels 
    //! for each subsequent patch to be used when no tessellation control shader is present.
    void setPatchDefaultInnerLevel(const fvec2& level) { mPatchDefaultInnerLevel = level; }
    
    //! The two floating-point values corresponding to the tow inner tessellation levels 
    //! for each subsequent patch to be used when no tessellation control shader is present.
    const fvec2& patchDefaultInnerLevel() const { return mPatchDefaultInnerLevel; }

  protected:
    int mPatchVertices;
    fvec4 mPatchDefaultOuterLevel;
    fvec2 mPatchDefaultInnerLevel;
  };
}

#endif
