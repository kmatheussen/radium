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

#ifndef DrawArrays_INCLUDE_DEFINE
#define DrawArrays_INCLUDE_DEFINE

#include <vlGraphics/DrawCall.hpp>
#include <vlGraphics/TriangleIterator.hpp>

namespace vl
{
  //------------------------------------------------------------------------------
  // DrawArrays
  //------------------------------------------------------------------------------
  /**
   * Wraps the OpenGL function glDrawArrays(). See vl::DrawCall for an overview of the different draw call methods.
   *
   * This class wraps the following OpenGL functions:
   * - glDrawArrays (http://www.opengl.org/sdk/docs/man4/xhtml/glDrawArrays.xml)
   * - glDrawArraysInstanced (http://www.opengl.org/sdk/docs/man4/xhtml/glDrawArraysInstanced.xml)
   *
   * Supports:
   * - <b>Multi instancing</b>: YES
   * - <b>Base vertex</b>: N/A
   * - <b>Primitive restart</b>: N/A
   *
   * DrawArrays, DrawElements, MultiDrawElements and DrawRangeElements are used by Geometry to define a set of primitives to be rendered.
   * @sa Geometry::drawCalls(), DrawCall, DrawElements, MultiDrawElements, DrawRangeElements, Geometry, Actor */
  class DrawArrays: public DrawCall
  {
    VL_INSTRUMENT_CLASS(vl::DrawArrays, DrawCall)

  public:
    DrawArrays(): mStart(0), mCount(0) 
    { 
      VL_DEBUG_SET_OBJECT_NAME()
      mType      = PT_TRIANGLES;
      mInstances = 1;
    }

    DrawArrays(EPrimitiveType primitive, int start, int count, int instances=1)
      : mStart(start), mCount(count)
    { 
      VL_DEBUG_SET_OBJECT_NAME()
      mInstances = instances;
      mType = primitive;
    }

    DrawArrays& operator=(const DrawArrays& other)
    {
      super::operator=(other);
      mStart     = other.mStart;
      mCount     = other.mCount;
      mInstances = other.mInstances;
      return *this;
    }

    virtual ref<DrawCall> clone() const 
    { 
      return new DrawArrays( primitiveType(), (int)start(), (int)count(), (int)instances() ); 
    }

    virtual void deleteBufferObject() {}
    virtual void updateDirtyBufferObject(EBufferObjectUpdateMode) {}

    virtual void render(bool) const
    {
      // apply patch parameters if any and if using PT_PATCHES
      applyPatchParameters();

      if ( instances() > 1 && (Has_GL_ARB_draw_instanced||Has_GL_EXT_draw_instanced) )
        VL_glDrawArraysInstanced( primitiveType(), (int)start(), (int)count(), (int)instances() );
      else
        glDrawArrays( primitiveType(), (int)start(), (int)count() );

      #ifndef NDEBUG
        unsigned int glerr = glGetError();
        if (glerr != GL_NO_ERROR)
        {
          String msg( getGLErrorString(glerr) );
          Log::error( Say("glGetError() [%s:%n]: %s\n") << __FILE__ << __LINE__ << msg );
          Log::warning( "- If you are using geometry instancing in conjunction with display lists you will have to disable one of them.\n" );
          Log::warning( "- If you are using OpenGL ES you must NOT use GL_QUADS, GL_QUAD_STRIP and GL_POLYGON primitive types.\n" );
          VL_TRAP()
        }
      #endif
    }

    //! sets the starting vertex for the rendering.
    void setStart(int start) { mStart = start; }

    //! returns the starting vertex for the rendering.
    int start() const { return mStart; }

    //! sets the number of vertices to be rendered.
    void setCount(int count) { mCount = count; }

    //! returns the number of vertices to be rendered.
    int count() const { return mCount; }

    //! Sets the number of instances for this set of primitives.
    void setInstances(int instances) { mInstances = instances; }

    //! Returns the number of instances for this set of primitives.
    int instances() const { return mInstances; }

    TriangleIterator triangleIterator() const
    {
      ref<TriangleIteratorDirect> tid = new TriangleIteratorDirect( primitiveType() );
      tid->initialize(mStart, mStart+mCount);
      return TriangleIterator(tid.get());
    }

    IndexIterator indexIterator() const
    {
      ref<IndexIteratorDrawArrays> iida = new IndexIteratorDrawArrays;
      iida->initialize( mStart, mCount );
      IndexIterator iit;
      iit.initialize( iida.get() );
      return iit;
    }

    protected:
      int mStart;
      int mCount;
      int mInstances;
  };

}

#endif

