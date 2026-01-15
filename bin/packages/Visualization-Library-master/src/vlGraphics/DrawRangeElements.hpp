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

#ifndef DrawRangeElements_INCLUDE_ONCE
#define DrawRangeElements_INCLUDE_ONCE

#include <vlGraphics/DrawCall.hpp>
#include <vlGraphics/TriangleIterator.hpp>
#include <vlGraphics/Array.hpp>
#include <vlCore/Log.hpp>
#include <vlCore/Say.hpp>
#include <algorithm>

namespace vl
{

  //------------------------------------------------------------------------------
  // DrawRangeElementsBase
  //------------------------------------------------------------------------------
  /**
   * Base interface for all DrawRangeElements* sub classes.
   * Implements the index-type-independent interface of the class. That is you can cast to DrawRangeElementsBase*
   * and access its members without needing to know whether the actual class is a 
   * vl::DrawRangeElementsUInt, vl::DrawRangeElementsUShort or vl::DrawRangeElementsUByte. */
  class DrawRangeElementsBase: public DrawCall
  {
    VL_INSTRUMENT_ABSTRACT_CLASS(vl::DrawRangeElementsBase, DrawCall)

  public:
    /** Sets the range start. See also http://www.opengl.org/sdk/docs/man3/xhtml/glDrawRangeElements.xml */
    void setRangeStart(int rstart) { mRangeStart = rstart; }

    /** Returns the range start. See also http://www.opengl.org/sdk/docs/man3/xhtml/glDrawRangeElements.xml */
    int rangeStart() const { return mRangeStart; }

    /** Sets the range end. See also http://www.opengl.org/sdk/docs/man3/xhtml/glDrawRangeElements.xml */
    void setRangeEnd(int rend) { mRangeEnd = rend; }

    /** Returns the range end. See also http://www.opengl.org/sdk/docs/man3/xhtml/glDrawRangeElements.xml */
    int rangeEnd() const { return mRangeEnd; }

    /** Returns whether the primitive-restart functionality is enabled or not. See http://www.opengl.org/registry/specs/NV/primitive_restart.txt */
    virtual bool primitiveRestartEnabled() const { return mPrimitiveRestartEnabled; }

    /** Enables the primitive-restart functionality. See http://www.opengl.org/registry/specs/NV/primitive_restart.txt */
    void setPrimitiveRestartEnabled(bool enabled) { mPrimitiveRestartEnabled = enabled; }

    /** If base_vertx is != 0 glDrawRangeElementsBaseVertex/glDrawRangeElementsInstancedBaseVertex will be used instead of their non *BaseVertx counterparts. 
      * Note that using base_vertx != requires OpenGL 3.2 or higher or ARB_draw_elements_base_vertex. 
      * For more information see also http://www.opengl.org/sdk/docs/man3/xhtml/glDrawRangeElementsBaseVertex.xml
      */
    void setBaseVertex(int base_vertex) { mBaseVertex = base_vertex; }

    /** Returns the currently used base vertex.
      * For more information see also http://www.opengl.org/sdk/docs/man3/xhtml/glDrawRangeElementsBaseVertex.xml */
    int  baseVertex() const { return mBaseVertex; }

  protected:
    int mRangeStart;
    int mRangeEnd;
    GLuint mBaseVertex;
    bool mPrimitiveRestartEnabled;
  };
  //------------------------------------------------------------------------------
  // DrawRangeElements
  //------------------------------------------------------------------------------
  /** 
   * Wrapper for the OpenGL function glDrawRangeElements(). See also http://www.opengl.org/sdk/docs/man3/xhtml/glDrawRangeElements.xml for more information.
   *
   * This class wraps the following OpenGL functions:
   * - glDrawRangeElements (http://www.opengl.org/sdk/docs/man4/xhtml/glDrawRangeElements.xml)
   * - glDrawRangeElementsBaseVertex (http://www.opengl.org/sdk/docs/man4/xhtml/glDrawRangeElementsBaseVertex.xml)
   *
   * Supports:
   * - <b>Multi instancing</b>: NO 
   * - <b>Base vertex</b>: YES
   * - <b>Primitive restart</b>: YES
   *
   * Use the functions setPrimitiveRestartIndex() and setPrimitiveRestartEnabled() to use the <b>primitive 
   * restart</b> functionality (requires OpenGL 3.1). For more information see http://www.opengl.org/sdk/docs/man3/xhtml/glPrimitiveRestartIndex.xml
   *
   * Use the function setBaseVertex() to use the <b>base vertex</b> functionality. 
   * Requires OpenGL 3.2 or GL_ARB_draw_elements_base_vertex. For more information see http://www.opengl.org/sdk/docs/man3/xhtml/glDrawRangeElementsBaseVertex.xml
   *
   * DrawElements, MultiDrawElements, DrawRangeElements, DrawArrays are used by Geometry to define a set of primitives to be rendered, see Geometry::drawCalls().
   * The indices are stored in a BufferObject and thus they can be stored locally or on the GPU. 
   * To gain direct access to the BufferObject use the indexBuffer() function.
   *
   * DrawArrays, DrawElements, MultiDrawElements and DrawRangeElements are used by Geometry to define a set of primitives to be rendered.
   * @sa Geometry::drawCalls(), DrawCall, DrawElements, MultiDrawElements, DrawRangeElements, Geometry, Actor */
  template <class arr_type>
  class DrawRangeElements: public DrawRangeElementsBase
  {
    VL_INSTRUMENT_CLASS(vl::DrawRangeElements<arr_type>, DrawRangeElementsBase)

  public:
    typedef typename arr_type::scalar_type index_type;
    //! The special index which identifies a primitive restart. By default it is set to ~0 that is 0xFF, 0xFFFF, 0xFFFFFFFF respectively for GLubyte, GLushort, GLuint index types. */
    static const index_type primitive_restart_index = index_type(~0);
    virtual unsigned int primitiveRestartIndex() { return (unsigned int)primitive_restart_index; }

  private:
    template<typename T>
    class Triangle
    {
    public:
      T ABC[3];
      bool operator<(const Triangle<index_type>& b) const
      {
        if (ABC[0] != b.ABC[0])
          return ABC[0] < b.ABC[0];
        else
        if (ABC[1] != b.ABC[1])
          return ABC[1] < b.ABC[1];
        else
          return ABC[2] < b.ABC[2];
      }
      void rotate()
      {
        if (ABC[0] > ABC[1])
          { T tmp = ABC[0]; ABC[0] = ABC[1]; ABC[1] = ABC[2]; ABC[2] = tmp; }
        if (ABC[0] > ABC[1])
          { T tmp = ABC[0]; ABC[0] = ABC[1]; ABC[1] = ABC[2]; ABC[2] = tmp; }
      }
    };

  public:
    DrawRangeElements(EPrimitiveType primitive = PT_TRIANGLES, int r_start=0, int r_end=primitive_restart_index)
    {
      VL_DEBUG_SET_OBJECT_NAME()
      mType                    = primitive;
      mRangeStart              = r_start;
      mRangeEnd                = r_end;
      mIndexBuffer             = new arr_type;
      mPrimitiveRestartEnabled = false;
      mBaseVertex              = 0;
      mCount                   = -1; // till the end of the indexBuffer()
      mOffset                  = 0; // from the beginning of the indexBuffer()
    }

    DrawRangeElements& operator=(const DrawRangeElements& other)
    {
      super::operator=(other);
      *indexBuffer()               = *other.indexBuffer();
      mRangeStart              = other.mRangeStart;
      mRangeEnd                = other.mRangeEnd;
      mPrimitiveRestartEnabled = other.mPrimitiveRestartEnabled;
      mBaseVertex              = other.mBaseVertex;
      mCount                   = other.mCount;
      mOffset                  = other.mOffset;
      return *this;
    }

    virtual ref<DrawCall> clone() const 
    { 
      ref<DrawRangeElements> de = new DrawRangeElements;
      *de = *this;
      return de;
    }

    //! The number of indices to render, default is -1 which means 'till the end of the indexBuffer() from offset()'.
    void setCount(i32 count) { mCount = count; }

    //! The number of indices to render, default is -1 which means 'till the end of the indexBuffer() from offset()'.
    i32 count() const { return mCount; }

    //! The offset in bytes from which the index buffer will be read.
    void setOffset(u32 offset) { mOffset = offset; }

    //! The offset in bytes from which the index buffer will be read.
    u32 offset() const { return mOffset; }

    //! The BufferObject containing the indices used to render
    void setIndexBuffer(arr_type* index_buffer) { mIndexBuffer = index_buffer; }

    //! The BufferObject containing the indices used to render
    arr_type* indexBuffer() { return mIndexBuffer.get(); }

    //! The BufferObject containing the indices used to render
    const arr_type* indexBuffer() const { return mIndexBuffer.get(); }

    virtual void updateDirtyBufferObject(EBufferObjectUpdateMode mode)
    {
      if (indexBuffer()->isBufferObjectDirty() || (mode & BUF_ForceUpdate))
        indexBuffer()->updateBufferObject(mode);
    }

    virtual void deleteBufferObject()
    {
      indexBuffer()->bufferObject()->deleteBufferObject();
    }

    virtual void render(bool use_bo) const
    {
      VL_CHECK_OGL()
      VL_CHECK(!use_bo || (use_bo && Has_BufferObject))
      use_bo &= Has_BufferObject; // && indexBuffer()->bufferObject()->handle() && indexBuffer()->sizeBufferObject();
      if ( !use_bo && !indexBuffer()->size() )
        return;

      // apply patch parameters if any and if using PT_PATCHES
      applyPatchParameters();

      // primitive restart enable
      if(primitiveRestartEnabled())
      {
        VL_CHECK(Has_Primitive_Restart);
        glEnable(GL_PRIMITIVE_RESTART); VL_CHECK_OGL();
        glPrimitiveRestartIndex(primitive_restart_index); VL_CHECK_OGL();
      }

      // compute base pointer

      const GLvoid* ptr = indexBuffer()->bufferObject()->ptr();
      if (use_bo && indexBuffer()->bufferObject()->handle())
      {
        VL_glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, indexBuffer()->bufferObject()->handle()); VL_CHECK_OGL()
        ptr = 0;
      }
      else
      {
        VL_glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0); VL_CHECK_OGL()
      }

      // compute final pointer and count

      const char*ptr_end = NULL;
      if(mCount < 0)
      {
        // compute the end of the index buffer
        ptr_end = (char*)ptr + sizeof(index_type)*(use_bo ? indexBuffer()->sizeBufferObject() : indexBuffer()->size());

        // offset in the index buffer
        ptr = (char*)ptr + mOffset;
      }
      else
      {
        // offset in the index buffer
        ptr = (char*)ptr + mOffset;

        // compute the end of the indices
        ptr_end = (char*)ptr + sizeof(index_type)*mCount;
      }

      // compute the remaining indices
      const GLsizei count = (GLsizei)((index_type*)ptr_end - (index_type*)ptr);

      if (mBaseVertex == 0)
      {
        glDrawRangeElements( primitiveType(), mRangeStart, mRangeEnd, count, arr_type::gl_type, ptr ); VL_CHECK_OGL()
      }
      else
      {
        VL_CHECK(Has_Base_Vertex)
        VL_glDrawRangeElementsBaseVertex( primitiveType(), mRangeStart, mRangeEnd, count, arr_type::gl_type, ptr, mBaseVertex ); VL_CHECK_OGL()
      }

      // primitive restart disable

      if(primitiveRestartEnabled())
      {
        glDisable(GL_PRIMITIVE_RESTART); VL_CHECK_OGL()
      }
    }

    TriangleIterator triangleIterator() const
    {
      ref< TriangleIteratorIndexed<arr_type> > it = 
        new TriangleIteratorIndexed<arr_type>( mIndexBuffer.get(), primitiveType(), 
            baseVertex(), primitiveRestartEnabled(), primitive_restart_index );
      it->initialize();
      return TriangleIterator(it.get());
    }

    IndexIterator indexIterator() const
    {
      ref< IndexIteratorElements<arr_type> > iie = new IndexIteratorElements<arr_type>;
      iie->initialize( mIndexBuffer.get(), NULL, NULL, mBaseVertex, mPrimitiveRestartEnabled, primitive_restart_index );
      IndexIterator iit;
      iit.initialize( iie.get() );
      return iit;
    }

    void computeRange()
    {
      mRangeStart = primitive_restart_index;
      mRangeEnd   = 0;

      for(IndexIterator it=indexIterator(); it.hasNext(); it.next())
      {
        if (it.index() < mRangeStart)
          mRangeStart = it.index();
        if (it.index() > mRangeEnd)
          mRangeEnd   = it.index();
      }

      if (mRangeEnd < mRangeStart)
      {
        mRangeStart = 0;
        mRangeEnd   = primitive_restart_index;
      }
    }

  protected:
    ref< arr_type > mIndexBuffer;
    i32 mCount;
    u32 mOffset;
  };
  //------------------------------------------------------------------------------
  // typedefs
  //------------------------------------------------------------------------------
  /** See DrawRangeElements. A DrawRangeElements using indices of type \p GLuint. */
  class DrawRangeElementsUInt: public DrawRangeElements<ArrayUInt1>
  {
    VL_INSTRUMENT_CLASS(vl::DrawRangeElementsUInt, DrawRangeElements<ArrayUInt1>)

  public:
    DrawRangeElementsUInt(EPrimitiveType primitive = PT_TRIANGLES, int r_start=0, int r_end=GLuint(~0))
    :DrawRangeElements<ArrayUInt1>(primitive, r_start, r_end)
    {
      VL_DEBUG_SET_OBJECT_NAME();
    }
  };
  //------------------------------------------------------------------------------
  /** See DrawRangeElements. A DrawRangeElements using indices of type \p GLushort. */
  class DrawRangeElementsUShort: public DrawRangeElements<ArrayUShort1>
  {
    VL_INSTRUMENT_CLASS(vl::DrawRangeElementsUShort, DrawRangeElements<ArrayUShort1>)

  public:
    DrawRangeElementsUShort(EPrimitiveType primitive = PT_TRIANGLES, int r_start=0, int r_end=GLushort(~0))
    :DrawRangeElements<ArrayUShort1>(primitive, r_start, r_end)
    {
      VL_DEBUG_SET_OBJECT_NAME();
    }
  };
  //------------------------------------------------------------------------------
  /** See DrawRangeElements. A DrawRangeElements using indices of type \p GLubyte. */
  class DrawRangeElementsUByte: public DrawRangeElements<ArrayUByte1>
  {
    VL_INSTRUMENT_CLASS(vl::DrawRangeElementsUByte, DrawRangeElements<ArrayUByte1>)

  public:
    DrawRangeElementsUByte(EPrimitiveType primitive = PT_TRIANGLES, int r_start=0, int r_end=GLubyte(~0))
    :DrawRangeElements<ArrayUByte1>(primitive, r_start, r_end)
    {
      VL_DEBUG_SET_OBJECT_NAME();
    }
  };
  //------------------------------------------------------------------------------
}

#endif
