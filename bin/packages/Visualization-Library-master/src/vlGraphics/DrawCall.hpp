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

#ifndef DrawCommand_INCLUDE_ONCE
#define DrawCommand_INCLUDE_ONCE

#include <vlGraphics/Array.hpp>
#include <vlGraphics/TriangleIterator.hpp>
#include <vlGraphics/IndexIterator.hpp>
#include <vlGraphics/PatchParameter.hpp>

namespace vl 
{
  //------------------------------------------------------------------------------
  // DrawCall
  //------------------------------------------------------------------------------
  /** The base class of DrawArrays, DrawElements, MultiDrawElements and DrawRangeElements. 
   * which are used by Geometry to define a set of primitives to be rendered (see also Geometry::drawCalls()).
   *
   * \par Overview
   *
   * vl::DrawArrays wraps:
   * - glDrawArrays (http://www.opengl.org/sdk/docs/man4/xhtml/glDrawArrays.xml)
   * - glDrawArraysInstanced (http://www.opengl.org/sdk/docs/man4/xhtml/glDrawArraysInstanced.xml)
   *
   * Supports:
   * - <b>Multi instancing</b>: YES
   * - <b>Base vertex</b>: N/A
   * - <b>Primitive restart</b>: N/A
   *
   * vl::DrawElements wraps:
   * - glDrawElements (http://www.opengl.org/sdk/docs/man4/xhtml/glDrawElements.xml)
   * - glDrawElementsInstanced (http://www.opengl.org/sdk/docs/man4/xhtml/glDrawElementsInstanced.xml)
   * - glDrawElementsBaseVertex (http://www.opengl.org/sdk/docs/man4/xhtml/glDrawElementsBaseVertex.xml)
   * - glDrawElementsInstancedBaseVertex (http://www.opengl.org/sdk/docs/man4/xhtml/glDrawElementsInstancedBaseVertex.xml)
   *
   * Supports:
   * - <b>Multi instancing</b>: YES
   * - <b>Base vertex</b>: YES
   * - <b>Primitive restart</b>: YES
   *
   * vl::DrawRangeElements wraps:
   * - glDrawRangeElements (http://www.opengl.org/sdk/docs/man4/xhtml/glDrawRangeElements.xml)
   * - glDrawRangeElementsBaseVertex (http://www.opengl.org/sdk/docs/man4/xhtml/glDrawRangeElementsBaseVertex.xml)
   *
   * Supports:
   * - <b>Multi instancing</b>: NO
   * - <b>Base vertex</b>: YES
   * - <b>Primitive restart</b>: YES
   *
   * vl::MultiDrawElements wraps:
   * - glMultiDrawElements (http://www.opengl.org/sdk/docs/man4/xhtml/glMultiDrawElements.xml)
   * - glMultiDrawElementsBaseVertex (http://www.opengl.org/sdk/docs/man4/xhtml/glMultiDrawElementsBaseVertex.xml)
   *
   * Supports:
   * - <b>Multi instancing</b>: NO
   * - <b>Base vertex</b>: YES
   * - <b>Primitive restart</b>: YES
   *
   * DrawArrays, DrawElements, MultiDrawElements and DrawRangeElements are used by Geometry to define a set of primitives to be rendered.
   * @sa Geometry::drawCalls(), DrawCall, DrawElements, MultiDrawElements, DrawRangeElements, Geometry, Actor */
  class DrawCall: public Object
  {
    VL_INSTRUMENT_ABSTRACT_CLASS(vl::DrawCalls, Object)

  public:
    /** Constructor. */
    DrawCall(): mType(PT_TRIANGLES), mEnabled(true) {}

    /** Assignment operator. */
    DrawCall& operator=(const DrawCall& other)
    {
      mType      = other.mType;
      mEnabled   = other.mEnabled;
      return *this;
    }

    /** Sets the draw call's primitive type. */
    void setPrimitiveType(EPrimitiveType type) { mType = type; }

    /** Returns the draw call's primitive type. */
    EPrimitiveType primitiveType() const { return mType; }

    /** Executes the draw call. */
    virtual void render(bool use_bo = true) const = 0;

    /** Returns a clone of the draw call. */
    virtual ref<DrawCall> clone() const = 0;

    /** Updates the index buffer's BufferObject if marked as dirty. */
    virtual void updateDirtyBufferObject(EBufferObjectUpdateMode) = 0;

    /** Deletes the index buffer's BufferObject. */
    virtual void deleteBufferObject() = 0;

    /** Enables/disables the draw call. */
    void setEnabled(bool enable) { mEnabled = enable; }

    /** True if the draw call is enabled. */
    bool isEnabled() const { return mEnabled; }

    /** 
     * Returns a TriangleIterator used to iterate through the triangles of a DrawCall.
     * Basically the iterator tesselates in triangles any DrawCall of type: PT_TRIANGLES, PT_TRIANGLE_STRIP
     * PT_TRIANGLE_FAN, PT_POLYGON, PT_QUADS, PT_QUAD_STRIP. */
    virtual TriangleIterator triangleIterator() const = 0;

    /** 
     * Returns a IndexIterator used to iterate through the virtual indices of a DrawCall.
     * This \note The returned indices already take into account primitive restart and base vertex. */
    virtual IndexIterator indexIterator() const = 0;

    /** Counts the number of virtual indices of a DrawCall., i.e. the number of indices you would retrieve by iterating over the iterator returned by indexIterator(). */
    u32 countIndices() const
    {
      u32 count = 0;
      for( IndexIterator it = indexIterator(); it.hasNext(); it.next() )
        ++count;
      return count;
    }

    /** Counts the number of virtual triangles of a DrawCall., i.e. the number of triangles you would retrieve by iterating over the iterator returned by triangleIterator(). */
    u32 countTriangles() const
    {
      u32 count = 0;
      for( TriangleIterator it = triangleIterator(); it.hasNext(); it.next() )
        ++count;
      return count;
    }

    /** Returns the number of instances for this set of primitives. */
    virtual int instances() const { return 1; }

    /** Returns whether the primitive-restart functionality is enabled or not. See http://www.opengl.org/registry/specs/NV/primitive_restart.txt */
    virtual bool primitiveRestartEnabled() const { return false; }

    /** Returns the primitive restart index used by the draw call or 0 if primitive restart is not supported. */
    virtual unsigned int primitiveRestartIndex() { return 0; }

    /** Attach a PatchParameter to a DrawCall to be used when using primitive-type PT_PATCHES */
    void setPatchParameter(PatchParameter* patch_param) { mPatchParameter = patch_param; }

    /** The PatchParameter attached to a DrawCall to be used when using primitive-type PT_PATCHES */
    PatchParameter* patchParameter() { return mPatchParameter.get(); }

    /** The PatchParameter attached to a DrawCall to be used when using primitive-type PT_PATCHES */
    const PatchParameter* patchParameter() const { return mPatchParameter.get(); }

  protected:
    void applyPatchParameters() const
    {
      if (mType == PT_PATCHES && mPatchParameter)
        mPatchParameter->apply();

      if (mPatchParameter && mType != PT_PATCHES)
      {
        vl::Log::warning("PatchParameter used with non PT_PATCHES draw call!\n");
      }
      else
      if (!mPatchParameter && mType == PT_PATCHES)
      {
        vl::Log::warning("No PatchParameter supplied while using PT_PATCHES draw call!\n");
      }
    }

  protected:
      ref<PatchParameter> mPatchParameter;
      EPrimitiveType mType;
      bool mEnabled;
  };
}

#endif
