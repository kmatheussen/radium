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

#ifndef MultiDrawElements_INCLUDE_ONCE
#define MultiDrawElements_INCLUDE_ONCE

#include <vlGraphics/DrawCall.hpp>
#include <vlGraphics/Array.hpp>
#include <vlGraphics/TriangleIterator.hpp>
#include <vlCore/Log.hpp>
#include <vlCore/Say.hpp>
#include <algorithm>

namespace vl
{
  //------------------------------------------------------------------------------
  // MultiDrawElementsBase
  //------------------------------------------------------------------------------
  /**
   * Base interface for all MultiDrawElements* sub classes.
   * Implements the index-type-independent interface of the class. That is you can cast to MultiDrawElementsBase*
   * and access its members without needing to know whether the actual class is a 
   * vl::MultiDrawElementsUInt, vl::MultiDrawElementsUShort or vl::MultiDrawElementsUByte. */
  class MultiDrawElementsBase: public DrawCall
  {
    VL_INSTRUMENT_ABSTRACT_CLASS(vl::MultiDrawElementsBase, DrawCall)

  public:
    /** Returns whether the primitive-restart functionality is enabled or not. See http://www.opengl.org/registry/specs/NV/primitive_restart.txt */
    virtual bool primitiveRestartEnabled() const { return mPrimitiveRestartEnabled; }
    
    /** Enables the primitive-restart functionality. See http://www.opengl.org/registry/specs/NV/primitive_restart.txt */
    void setPrimitiveRestartEnabled(bool enabled) { mPrimitiveRestartEnabled = enabled; }

    /** Calls computePointerVector(), computeBufferObjectPointerVector() and resizes the base vertex array to fit the count vector.
      * @note Must be called after the index buffer has been filled. */
    void finalizeSetup()
    {
      // update pointers
      computePointerVector();
      computeBufferObjectPointerVector();
      // set default base vertices to 0
      if (mBaseVertices.size() != mCountVector.size())
        mBaseVertices.resize(mCountVector.size());
    }

    /** Sets the vector defining the length of each primitive and automatically computes the pointer vectors used to exectue glMultiDrawElements(). 
      * @note Must be called after the index buffer has been filled. */
    void setCountVector(const std::vector<GLsizei>& vcount)
    {
      mCountVector = vcount;
      finalizeSetup();
    }

    /** Sets the vector defining the length of each primitive and automatically computes the pointer vectors used to exectue glMultiDrawElements().
      * @note Must be called after the index buffer has been filled. */
    void setCountVector(const GLsizei* vcount, size_t size)
    {
      mCountVector.resize(size);
      for(size_t i=0; i<size; ++i)
        mCountVector[i] = vcount[i];
      finalizeSetup();
    }

    /** The count vector used as 'count' parameter of glMultiDrawElements. */
    const std::vector<GLsizei>& countVector() const { return mCountVector; }

    /** The count vector used as 'count' parameter of glMultiDrawElements. */
    std::vector<GLsizei>& countVector() { return mCountVector; }

    /** Returns the list of base vertices, one for each primitive. This will enable the use 
      * of glMultiDrawElementsBaseVertex() to render a set of primitives. 
      * See also http://www.opengl.org/sdk/docs/man3/xhtml/glMultiDrawElementsBaseVertex.xml */
    void setBaseVertices(const std::vector<GLint>& base_verts) { mBaseVertices = base_verts; }

    /** Returns the list of base vertices, one for each primitive. */
    const std::vector<GLint>& baseVertices() const { return mBaseVertices; }

    /** Returns the list of base vertices, one for each primitive. */
    std::vector<GLint>& baseVertices() { return mBaseVertices; }

    /** Computes the pointer vector to be used when BufferObjects are DISABLED. Call this function after having updated the count vector and the index buffer indexBuffer().
     * @note Normally you don't need to call this function as setCountVector() already calls it. */
    virtual void computePointerVector() = 0;

    /** Computes the pointer vector to be used when BufferObjects are ENABLED. Call this function after having updated the count vector and the index buffer indexBuffer().
     * @note Normally you don't need to call this function as setCountVector() already calls it. */
    virtual void computeBufferObjectPointerVector() = 0;

  protected:
    bool mPrimitiveRestartEnabled;
    std::vector<GLsizei> mCountVector;
    std::vector<GLint>   mBaseVertices;
  };
  //------------------------------------------------------------------------------
  // MultiDrawElements
  //------------------------------------------------------------------------------
  /** 
   * Wrapper for the OpenGL function glMultiDrawElements(). See vl::DrawCall for an overview of the different draw call methods.
   *
   * This class wraps the following OpenGL functions:
   * - glMultiDrawElements (http://www.opengl.org/sdk/docs/man4/xhtml/glMultiDrawElements.xml)
   * - glMultiDrawElementsBaseVertex (http://www.opengl.org/sdk/docs/man4/xhtml/glMultiDrawElementsBaseVertex.xml)
   *
   * Supports: 
   * - <b>Multi instancing</b>: NO 
   * - <b>Base vertex</b>: YES
   * - <b>Primitive restart</b>: YES
   *
   * Use the functions setPrimitiveRestartIndex() and setPrimitiveRestartEnabled() to use the <b>primitive 
   * restart</b> functionality (requires OpenGL 3.1). For more information see http://www.opengl.org/sdk/docs/man3/xhtml/glPrimitiveRestartIndex.xml
   *
   * Use the function setBaseVertices() to use the <b>base vertex</b> functionality. 
   * Requires OpenGL 3.2 or GL_ARB_draw_elements_base_vertex. For more information see http://www.opengl.org/sdk/docs/man3/xhtml/glMultiDrawElementsBaseVertex.xml
   *
   * DrawElements, MultiDrawElements, DrawRangeElements, DrawArrays are used by Geometry to define a set of primitives to be rendered, see Geometry::drawCalls().
   * The indices are stored in a BufferObject and thus they can be stored locally or on the GPU. 
   * To gain direct access to the BufferObject use the indexBuffer() function.
   *
   * DrawArrays, DrawElements, MultiDrawElements and DrawRangeElements are used by Geometry to define a set of primitives to be rendered.
   * @sa Geometry::drawCalls(), DrawCall, DrawElements, MultiDrawElements, DrawRangeElements, Geometry, Actor */
  template <class arr_type>
  class MultiDrawElements: public MultiDrawElementsBase
  {
    VL_INSTRUMENT_CLASS(vl::MultiDrawElements<arr_type>, MultiDrawElementsBase)

  public:
    typedef typename arr_type::scalar_type index_type;
    //! The special index which identifies a primitive restart. By default it is set to ~0 that is 0xFF, 0xFFFF, 0xFFFFFFFF respectively for GLubyte, GLushort, GLuint index types. */
    static const index_type primitive_restart_index = index_type(~0);
    virtual unsigned int primitiveRestartIndex() { return (unsigned int)primitive_restart_index; }

  public:
    MultiDrawElements(EPrimitiveType primitive = PT_TRIANGLES)
    {
      VL_DEBUG_SET_OBJECT_NAME()
      mType                    = primitive;
      mIndexBuffer             = new arr_type;
      mPrimitiveRestartEnabled = false;
    }

    MultiDrawElements& operator=(const MultiDrawElements& other)
    {
      super::operator=(other);
      *indexBuffer() = *other.indexBuffer();
      mPrimitiveRestartEnabled = other.mPrimitiveRestartEnabled;
      setCountVector(other.mCountVector);
      return *this;
    }

    virtual ref<DrawCall> clone() const 
    { 
      ref<MultiDrawElements> de = new MultiDrawElements;
      *de = *this;
      return de;
    }

    void setIndexBuffer(arr_type* index_buffer) { mIndexBuffer = index_buffer; }

    arr_type* indexBuffer() { return mIndexBuffer.get(); }

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
      VL_CHECK(Has_GL_EXT_multi_draw_arrays||Has_GL_Version_1_4||Has_GL_Version_3_0||Has_GL_Version_4_0);
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

      const GLvoid **indices_ptr = NULL;
      if (use_bo && indexBuffer()->bufferObject()->handle())
      {
        VL_glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, indexBuffer()->bufferObject()->handle()); VL_CHECK_OGL()
        VL_CHECK(!mBufferObjectPointerVector.empty())
        indices_ptr = (const GLvoid**)&mBufferObjectPointerVector[0];
      }
      else
      {
        VL_glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
        VL_CHECK(!mPointerVector.empty())
        indices_ptr = (const GLvoid**)&mPointerVector[0];
      }

      if (baseVertices().size())
      {
        VL_CHECK( baseVertices().size() == pointerVector().size() )
        VL_CHECK( baseVertices().size() == countVector().size() )
        if (Has_GL_ARB_draw_elements_base_vertex||Has_GL_Version_3_1||Has_GL_Version_4_0)
        {
          glMultiDrawElementsBaseVertex( primitiveType(), (GLsizei*)&mCountVector[0], indexBuffer()->glType(), indices_ptr, (GLsizei)mCountVector.size(), (GLint*)&mBaseVertices[0] ); VL_CHECK_OGL()
        }
        else
        {
          vl::Log::error("MultiDrawElements::render(): glMultiDrawElementsBaseVertex() not supported!\n"
            "OpenGL 3.1 or GL_ARB_draw_elements_base_vertex extension required.\n"
          );
        }
      }
      else
      {
        glMultiDrawElements( primitiveType(), (GLsizei*)&mCountVector[0], indexBuffer()->glType(), (const GLvoid**)indices_ptr, (GLsizei)mCountVector.size() ); VL_CHECK_OGL()
      }

      // primitive restart disable
      if(primitiveRestartEnabled())
      {
        glDisable(GL_PRIMITIVE_RESTART); VL_CHECK_OGL()
      }
    }

    TriangleIterator triangleIterator() const;

    IndexIterator indexIterator() const
    {
      ref< IndexIteratorElements<arr_type> > iie = new IndexIteratorElements<arr_type>;
      iie->initialize( mIndexBuffer.get(), &mBaseVertices, &mCountVector, 0, mPrimitiveRestartEnabled, primitive_restart_index );
      IndexIterator iit;
      iit.initialize( iie.get() );
      return iit;
    }

    /** The pointer vector used as 'indices' parameter of glMultiDrawElements when NOT using BufferObjects. 
     * Automatically computed when calling setCountVector(). If you need to modify this manually then you also have to modify the bufferObjectPointerVector. */
    const std::vector<const index_type*>& pointerVector() const { return mPointerVector; }

    /** The pointer vector used as 'indices' parameter of glMultiDrawElements when NOT using BufferObjects. */
    std::vector<const index_type*>& pointerVector() { return mPointerVector; }

    /** The pointer vector used as 'indices' parameter of glMultiDrawElements when using BufferObjects. */
    const std::vector<const index_type*>& bufferObjectPointerVector() const { return mBufferObjectPointerVector; }

    /** The pointer vector used as 'indices' parameter of glMultiDrawElements when using BufferObjects. */
    std::vector<const index_type*>& bufferObjectPointerVector() { return mBufferObjectPointerVector; }

    /** Computes pointerVector() based on the values contained on countVector(). */
    void computePointerVector()
    {
      VL_CHECK( indexBuffer() && indexBuffer()->size() )
      mPointerVector.resize( mCountVector.size() );
      const index_type* ptr = (const index_type*)indexBuffer()->bufferObject()->ptr();
      for(size_t i=0; i<mCountVector.size(); ++i)
      {
        mPointerVector[i] = ptr;
        ptr += mCountVector[i];
      }
    }

    //! Computes bufferObjectPointerVector() based on the values contained in pointerVector().
    void computeBufferObjectPointerVector()
    {
      VL_CHECK( indexBuffer() && indexBuffer()->size() )
      mBufferObjectPointerVector.resize( mPointerVector.size() );
      const index_type* base_ptr = (const index_type*)indexBuffer()->ptr();
      VL_CHECK(base_ptr)
      for(size_t i=0; i<mPointerVector.size(); ++i)
      {
        size_t offset = mPointerVector[i] - base_ptr;
        mBufferObjectPointerVector[i] = (const index_type*)0 + offset;
      }
    }

  protected:
    ref< arr_type > mIndexBuffer;
    std::vector<const index_type*> mPointerVector;
    std::vector<const index_type*> mBufferObjectPointerVector;
  };
  //------------------------------------------------------------------------------
  // typedefs
  //------------------------------------------------------------------------------
  /** See MultiDrawElements. A MultiDrawElements using indices of type \p GLuint. */
  class MultiDrawElementsUInt: public MultiDrawElements<ArrayUInt1>
  {
    VL_INSTRUMENT_CLASS(vl::MultiDrawElementsUInt, MultiDrawElements< ArrayUInt1>)

  public:
    MultiDrawElementsUInt(EPrimitiveType primitive = PT_TRIANGLES)
    :MultiDrawElements<ArrayUInt1>(primitive)
    {
      VL_DEBUG_SET_OBJECT_NAME();
    }
  };
  //------------------------------------------------------------------------------
  /** See MultiDrawElements. A MultiDrawElements using indices of type \p GLushort. */
  class MultiDrawElementsUShort: public MultiDrawElements<ArrayUShort1>
  {
    VL_INSTRUMENT_CLASS(vl::MultiDrawElementsUShort, MultiDrawElements< ArrayUShort1>)

  public:
    MultiDrawElementsUShort(EPrimitiveType primitive = PT_TRIANGLES)
    :MultiDrawElements<ArrayUShort1>(primitive)
    {
      VL_DEBUG_SET_OBJECT_NAME();
    }
  };
  //------------------------------------------------------------------------------
  /** See MultiDrawElements. A MultiDrawElements using indices of type \p GLubyte. */
  class MultiDrawElementsUByte: public MultiDrawElements<ArrayUByte1>
  {
    VL_INSTRUMENT_CLASS(vl::MultiDrawElementsUByte, MultiDrawElements<ArrayUByte1>)

  public:
    MultiDrawElementsUByte(EPrimitiveType primitive = PT_TRIANGLES)
    :MultiDrawElements<ArrayUByte1>(primitive)
    {
      VL_DEBUG_SET_OBJECT_NAME();
    }
  };
//-----------------------------------------------------------------------------
  template <class arr_type>
  TriangleIterator MultiDrawElements<arr_type>::triangleIterator() const
  {
    ref< TriangleIteratorMulti<arr_type> > it = 
      new TriangleIteratorMulti<arr_type>( &mBaseVertices, &mCountVector, mIndexBuffer.get(), primitiveType(), 
          primitiveRestartEnabled(), primitive_restart_index );
    it->initialize();
    return TriangleIterator(it.get());
  }
//-----------------------------------------------------------------------------
}

#endif
