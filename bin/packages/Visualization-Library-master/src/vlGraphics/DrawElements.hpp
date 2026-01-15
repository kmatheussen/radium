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

#ifndef DrawElements_INCLUDE_ONCE
#define DrawElements_INCLUDE_ONCE

#include <vlGraphics/DrawCall.hpp>
#include <vlGraphics/TriangleIterator.hpp>
#include <vlGraphics/Array.hpp>
#include <vlCore/Log.hpp>
#include <vlCore/Say.hpp>
#include <algorithm>

namespace vl
{

  //------------------------------------------------------------------------------
  // DrawElementsBase
  //------------------------------------------------------------------------------
  /**
   * Base interface for all DrawElements* sub classes.
   * Implements the index-type-independent interface of the class. That is you can cast to DrawElementsBase*
   * and access its members without needing to know whether the actual class is a 
   * vl::DrawElementsUInt, vl::DrawElementsUShort or vl::DrawElementsUByte. */
  class DrawElementsBase: public DrawCall
  {
    VL_INSTRUMENT_ABSTRACT_CLASS(vl::DrawElementsBase, DrawCall)

  public:
    /** Returns whether the primitive-restart functionality is enabled or not (requires OpenGL 3.1). See http://www.opengl.org/sdk/docs/man3/xhtml/glPrimitiveRestartIndex.xml */
    virtual bool primitiveRestartEnabled() const { return mPrimitiveRestartEnabled; }

    /** Enables the primitive-restart functionality (requires OpenGL 3.1). See http://www.opengl.org/sdk/docs/man3/xhtml/glPrimitiveRestartIndex.xml */
    void setPrimitiveRestartEnabled(bool enabled) { mPrimitiveRestartEnabled = enabled; }

    /** Sets the number of instances for this set of primitives. */
    void setInstances(int instances) { mInstances = instances; }

    /** Returns the number of instances for this set of primitives. */
    int instances() const { return mInstances; }

    /** If base_vertx is != 0 glDrawElementsBaseVertex/glDrawElementsInstancedBaseVertex will be used instead of their non *BaseVertx counterparts. 
      * Note that using base_vertx != requires OpenGL 3.2 or higher or ARB_draw_elements_base_vertex. 
      * For more information see also http://www.opengl.org/sdk/docs/man3/xhtml/glDrawElementsBaseVertex.xml
      */
    void setBaseVertex(int base_vertex) { mBaseVertex = base_vertex; }

    /** Returns the currently used base vertex.
      * For more information see also http://www.opengl.org/sdk/docs/man3/xhtml/glDrawElementsBaseVertex.xml */
    int  baseVertex() const { return mBaseVertex; }

  protected:
    int mInstances;
    bool mPrimitiveRestartEnabled;
    GLuint mBaseVertex;
  };
  //------------------------------------------------------------------------------
  // DrawElements
  //------------------------------------------------------------------------------
  /** 
   * Wrapper for the OpenGL function glDrawElements(). See vl::DrawCall for an overview of the different draw call methods.
   *
   * This class wraps the following OpenGL functions:
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
   * Use the functions setInstances() and instances() to use the <b>Multi instancing</b> functionality.
   * (requires OpenGL 3.1). For more information see http://www.opengl.org/sdk/docs/man3/xhtml/glDrawElementsInstanced.xml
   *
   * Use the functions setPrimitiveRestartIndex() and setPrimitiveRestartEnabled() to use the <b>primitive 
   * restart</b> functionality (requires OpenGL 3.1). For more information see http://www.opengl.org/sdk/docs/man3/xhtml/glPrimitiveRestartIndex.xml
   *
   * Use the function setBaseVertex() with a non zero argument to use the <b>base vertex</b> functionality. 
   * Requires OpenGL 3.2 or GL_ARB_draw_elements_base_vertex. For more information see http://www.opengl.org/sdk/docs/man3/xhtml/glDrawElementsBaseVertex.xml
   *
   * DrawElements, MultiDrawElements, DrawRangeElements, DrawArrays are used by Geometry to define a set of primitives to be rendered, see Geometry::drawCalls().
   * The indices are stored in a BufferObject and thus they can be stored locally or on the GPU. 
   * To gain direct access to the BufferObject use the indexBuffer() function.
   *
   * DrawArrays, DrawElements, MultiDrawElements and DrawRangeElements are used by Geometry to define a set of primitives to be rendered.
   * @sa Geometry::drawCalls(), DrawCall, DrawElements, MultiDrawElements, DrawRangeElements, Geometry, Actor */
  template <class arr_type>
  class DrawElements: public DrawElementsBase
  {
    VL_INSTRUMENT_CLASS(vl::DrawElements<arr_type>, DrawElementsBase)

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
    //! Constructor
    DrawElements(EPrimitiveType primitive = PT_TRIANGLES, int instances = 1)
    {
      VL_DEBUG_SET_OBJECT_NAME()
      mType                    = primitive;
      mInstances               = instances;
      mIndexBuffer             = new arr_type;
      mPrimitiveRestartEnabled = false;
      mBaseVertex              = 0;
      mCount                   = -1; // till the end of the indexBuffer()
      mOffset                  = 0; // from the beginning of the indexBuffer()
    }

    //! Assign operator
    DrawElements& operator=(const DrawElements& other)
    {
      super::operator=(other);
      *indexBuffer() = *other.indexBuffer();
      mInstances = other.mInstances;
      mPrimitiveRestartEnabled = other.mPrimitiveRestartEnabled;
      mBaseVertex              = other.mBaseVertex;
      mCount                   = other.mCount;
      mOffset                  = other.mOffset;
      return *this;
    }

    //! Returns a clone of this DrawCall
    virtual ref<DrawCall> clone() const 
    { 
      ref<DrawElements> de = new DrawElements;
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

#if !defined(NDEBUG) && (defined(VL_OPENGL_ES1) || defined(GL_OPENGL_ES2))
      bool error = true;
      // check primitive type
      switch(primitiveType())
      {
      case PT_QUADS:      Log::error("vl::DrawElements does not support PT_QUADS under OpenGL ES!\n"); break;
      case PT_QUAD_STRIP: Log::error("vl::DrawElements does not support PT_QUAD_STRIP under OpenGL ES!\n"); break;
      case PT_POLYGON:    Log::error("vl::DrawElements does not support PT_POLYGON under OpenGL ES!\n"); break;
      case PT_LINES_ADJACENCY:      Log::error("vl::DrawElements does not support PT_LINES_ADJACENCY under OpenGL ES!\n"); break;
      case PT_LINE_STRIP_ADJACENCY: Log::error("vl::DrawElements does not support PT_LINE_STRIP_ADJACENCY under OpenGL ES!\n"); break;
      case PT_TRIANGLES_ADJACENCY:  Log::error("vl::DrawElements does not support PT_TRIANGLES_ADJACENCY under OpenGL ES!\n"); break;
      case PT_TRIANGLE_STRIP_ADJACENCY: Log::error("vl::DrawElements does not support PT_TRIANGLE_STRIP_ADJACENCY under OpenGL ES!\n"); break;
      case PT_PATCHES:    Log::error("vl::DrawElements does not support PT_PATCHES under OpenGL ES!\n"); break;
      default:
        error = false;
        break;
      }
      // check index type
      if (indexBuffer()->glType() != GL_UNSIGNED_BYTE && indexBuffer()->glType() != GL_UNSIGNED_SHORT)
      {
        Log::error("vl::DrawElements only supports GL_UNSIGNED_BYTE and GL_UNSIGNED_SHORT under OpenGL ES!\n");
        error = true;
      }
      VL_CHECK(!error)
#endif

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
        if ( instances() == 1 )
        {
          glDrawElements( primitiveType(), count, arr_type::gl_type, ptr ); VL_CHECK_OGL()
        }
        else
        {
          VL_CHECK(Has_Primitive_Instancing)
          VL_glDrawElementsInstanced( primitiveType(), count, arr_type::gl_type, ptr, instances() ); VL_CHECK_OGL()
        }
      }
      else
      {
        VL_CHECK(Has_Base_Vertex)
        if ( instances() == 1 )
        {
          VL_glDrawElementsBaseVertex( primitiveType(), count, arr_type::gl_type, ptr, mBaseVertex ); VL_CHECK_OGL()
        }
        else
        {
          VL_CHECK(Has_Primitive_Instancing)
          VL_glDrawElementsInstancedBaseVertex( primitiveType(), count, arr_type::gl_type, ptr, instances(), mBaseVertex ); VL_CHECK_OGL()
        }
      }

      // primitive restart disable

      if(primitiveRestartEnabled())
      {
        glDisable(GL_PRIMITIVE_RESTART); VL_CHECK_OGL()
      }

      #ifndef NDEBUG
        unsigned int glerr = glGetError();
        if (glerr != GL_NO_ERROR)
        {
          String msg( getGLErrorString(glerr) );
          Log::error( Say("glGetError() [%s:%n]: %s\n") << __FILE__ << __LINE__ << msg );
          Log::warning( "- If you are using geometry instancing in conjunction with display lists you will have to disable one of them.\n" );
          Log::warning( "- If you are using OpenGL ES you must not use GL_QUADS, GL_QUAD_STRIP and GL_POLYGON primitive types.\n" );
          VL_TRAP()
        }
      #endif
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

  protected:
    ref< arr_type > mIndexBuffer;
    i32 mCount;
    u32 mOffset;
  };
  //------------------------------------------------------------------------------
  // typedefs
  //------------------------------------------------------------------------------
  /** See DrawElements. A DrawElements using indices of type \p GLuint. */
  class DrawElementsUInt: public DrawElements<ArrayUInt1>
  {
    VL_INSTRUMENT_CLASS(vl::DrawElementsUInt, DrawElements<ArrayUInt1>)

  public:
    DrawElementsUInt(EPrimitiveType primitive = PT_TRIANGLES, int instances = 1)
    :DrawElements<ArrayUInt1>(primitive, instances) 
    {
      VL_DEBUG_SET_OBJECT_NAME();
    }
  };
  //------------------------------------------------------------------------------
  /** See DrawElements. A DrawElements using indices of type \p GLushort. */
  class DrawElementsUShort: public DrawElements<ArrayUShort1>
  {
    VL_INSTRUMENT_CLASS(vl::DrawElementsUShort, DrawElements<ArrayUShort1>)

  public:
    DrawElementsUShort(EPrimitiveType primitive = PT_TRIANGLES, int instances = 1)
    :DrawElements<ArrayUShort1>(primitive, instances)
    {
      VL_DEBUG_SET_OBJECT_NAME();
    }
  };
  //------------------------------------------------------------------------------
  /** See DrawElements. A DrawElements using indices of type \p GLubyte. */
  class DrawElementsUByte: public DrawElements<ArrayUByte1>
  {
    VL_INSTRUMENT_CLASS(vl::DrawElementsUByte, DrawElements<ArrayUByte1>)

  public:
    DrawElementsUByte(EPrimitiveType primitive = PT_TRIANGLES, int instances = 1)
    :DrawElements<ArrayUByte1>(primitive, instances)
    {
      VL_DEBUG_SET_OBJECT_NAME();
    }
  };
   //------------------------------------------------------------------------------
}

#endif
