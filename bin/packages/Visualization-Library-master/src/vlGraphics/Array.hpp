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

#ifndef Array_INCLUDE_ONCE
#define Array_INCLUDE_ONCE

#include <vlGraphics/BufferObject.hpp>
#include <vlCore/half.hpp>
#include <vector>

namespace vl
{
//-----------------------------------------------------------------------------
// ArrayAbstract
//-----------------------------------------------------------------------------
  /**
   * The ArrayAbstract class defines an abstract interface to conveniently manipulate data stored in a BufferObject.
   * \sa
   *
   * - vl::Array
   * - vl::ArrayFloat1, vl::ArrayFloat2, vl::ArrayFloat3, vl::ArrayFloat4
   * - vl::ArrayDouble1, vl::ArrayDouble2, vl::ArrayDouble3, vl::ArrayDouble4
   * - vl::ArrayInt1, vl::ArrayInt2, vl::ArrayInt3,  vl::ArrayInt4
   * - vl::ArrayUInt1, vl::ArrayUInt2, vl::ArrayUInt3, vl::ArrayUInt4
   * - vl::ArrayByte1, vl::ArrayByte2, vl::ArrayByte3, vl::ArrayByte4
   * - vl::ArrayUByte1, vl::ArrayUByte2, vl::ArrayUByte3, vl::ArrayUByte4
   * - vl::ArrayShort1, vl::ArrayShort2, vl::ArrayShort3, vl::ArrayShort4
   * - vl::ArrayUShort1, vl::ArrayUShort2, vl::ArrayUShort3, vl::ArrayUShort4
   */
  class ArrayAbstract: public Object
  {
    VL_INSTRUMENT_ABSTRACT_CLASS(vl::ArrayAbstract, Object)

  public:
    //! Default constructor.
    ArrayAbstract()
    { 
      VL_DEBUG_SET_OBJECT_NAME()
      mBufferObject = new BufferObject;
      mBufferObjectDirty = true;
      mBufferObjectUsage = vl::BU_STATIC_DRAW;
    }

    //! Copies only the local data and not the BufferObject related fields
    ArrayAbstract(const ArrayAbstract& other): Object(other) 
    {
      VL_DEBUG_SET_OBJECT_NAME()
      mBufferObject = new BufferObject;
      mBufferObjectDirty = true;
      mBufferObjectUsage = vl::BU_STATIC_DRAW;
      operator=(other);
    }

    //! Copies only the local data and not the BufferObject related fields
    void operator=(const ArrayAbstract& other) 
    {
      bufferObject()->resize( other.bufferObject()->bytesUsed() );
      memcpy( ptr(), other.ptr(), bytesUsed() );
    }

    virtual ref<ArrayAbstract> clone() const = 0;

    const BufferObject* bufferObject() const { return mBufferObject.get(); }
    BufferObject* bufferObject() { return mBufferObject.get(); }

    void clear() { if (bufferObject()) bufferObject()->clear(); }

    //! Returns the pointer to the first element of the local buffer. Equivalent to bufferObject()->ptr()
    const unsigned char* ptr() const { return bufferObject() ? bufferObject()->ptr() : NULL; }

    //! Returns the pointer to the first element of the local buffer. Equivalent to bufferObject()->ptr()
    unsigned char* ptr() { return bufferObject() ? bufferObject()->ptr() : NULL; }

    //! Returns the amount of memory in bytes used by an array. Equivalent to bufferObject()->bytesUsed().
    virtual size_t bytesUsed() const { return bufferObject() ? bufferObject()->bytesUsed() : 0; }

    //! Returns the number of scalar components for the array, ie 3 for ArrayFloat3, 1 for ArrayUInt1 etc.
    virtual size_t glSize() const = 0;

    //! Returns the OpenGL type for the array, ie GL_FLOAT for ArrayFloat3, GL_UNSIGNED_INT for ArrayUInt1 etc.
    virtual GLenum glType() const = 0;

    //! Returns the number of elements of an array
    virtual size_t size() const = 0;

    //! Computes the bounding sphere enclosing the vectors contained in the buffer.
    virtual Sphere computeBoundingSphere() const = 0;

    //! Computes the axis aligned bounding box enclosing the vectors contained in the buffer.
    virtual AABB computeBoundingBox() const = 0;

    //! Transforms the vectors contained in the buffer
    virtual void transform(const mat4& m) = 0;

    //! Normalizes the vectors contained in the buffer
    virtual void normalize() = 0;

    //! Returns a vector from the buffer as a \p vec4 value.
    virtual vec4 getAsVec4(size_t vector_index) const = 0;

    //! Returns a vector from the buffer as a \p vec3 value.
    virtual vec3 getAsVec3(size_t vector_index) const = 0;

    //! Returns a vector from the buffer as a \p vec2 value.
    virtual vec2 getAsVec2(size_t vector_index) const = 0;

    //! Compares two vectors
    virtual int compare(int a, int b) const = 0;

    //! Wether the BufferObject should be updated or not using the local storage. Initially set to true.
    bool isBufferObjectDirty() const { return mBufferObjectDirty; }

    //! Wether the BufferObject should be updated or not using the local storage. Initially set to true.
    void setBufferObjectDirty(bool dirty=true) { mBufferObjectDirty = dirty; }

    //! BU_STATIC_DRAW by default
    EBufferObjectUsage usage() const { return mBufferObjectUsage; }

    //! BU_STATIC_DRAW by default
    void setUsage(EBufferObjectUsage usage) { mBufferObjectUsage = usage; }

    //! Updates the BufferObject. 
    //! @param mode Only the BUF_DiscardRamBuffer flag is checked as the BUF_ForceUpdate flag is considered always set for this function. By default mode is set to BUM_KeepRamBuffer.
    void updateBufferObject(EBufferObjectUpdateMode mode = BUM_KeepRamBuffer)
    {
      bufferObject()->setBufferData(usage(), (mode & BUF_DiscardRamBuffer) !=  0);
      setBufferObjectDirty(false);
    }

  protected:
    ref<BufferObject> mBufferObject;
    EBufferObjectUsage mBufferObjectUsage;
    bool mBufferObjectDirty;
  };
//-----------------------------------------------------------------------------
// Array
//-----------------------------------------------------------------------------
  /**
   * The Array class is a template array used to conveniently manipulate data stored in a BufferObject.
   * \sa
   *
   * - ArrayAbstract
   * - vl::ArrayFloat1, vl::ArrayFloat2, vl::ArrayFloat3, vl::ArrayFloat4
   * - vl::ArrayDouble1, vl::ArrayDouble2, vl::ArrayDouble3, vl::ArrayDouble4
   * - vl::ArrayInt1, vl::ArrayInt2, vl::ArrayInt3,  vl::ArrayInt4
   * - vl::ArrayUInt1, vl::ArrayUInt2, vl::ArrayUInt3, vl::ArrayUInt4
   * - vl::ArrayByte1, vl::ArrayByte2, vl::ArrayByte3, vl::ArrayByte4
   * - vl::ArrayUByte1, vl::ArrayUByte2, vl::ArrayUByte3, vl::ArrayUByte4
   * - vl::ArrayShort1, vl::ArrayShort2, vl::ArrayShort3, vl::ArrayShort4
   * - vl::ArrayUShort1, vl::ArrayUShort2, vl::ArrayUShort3, vl::ArrayUShort4
   * - vl::ArrayHFloat1, vl::ArrayHFloat2, vl::ArrayHFloat3, vl::ArrayHFloat4
   * - vl::ArrayFixed1, vl::ArrayFixed2, vl::ArrayFixed3, vl::ArrayFixed4
   * - vl::ArrayInt_2_10_10_10_REV1, ArrayInt_2_10_10_10_REV2, ArrayInt_2_10_10_10_REV3, ArrayInt_2_10_10_10_REV4
   * - vl::ArrayUInt_2_10_10_10_REV1, ArrayUInt_2_10_10_10_REV2, ArrayUInt_2_10_10_10_REV3, ArrayUInt_2_10_10_10_REV4
  */
  template <typename T_VectorType, typename T_Scalar, size_t T_GL_Size, GLenum T_GL_Type>
  class Array: public ArrayAbstract
  {
    VL_INSTRUMENT_ABSTRACT_CLASS(vl::Array, ArrayAbstract)

  public:
    typedef T_Scalar scalar_type;
    typedef T_VectorType vector_type;
    static const size_t gl_size = T_GL_Size;
    static const GLenum gl_type = T_GL_Type;

    virtual size_t glSize() const { return T_GL_Size; }

    virtual GLenum glType() const { return T_GL_Type; }

    virtual size_t bytesPerVector() const { return sizeof(T_VectorType); }

    // ---

    void clear() { resize(0); bufferObject()->deleteBufferObject(); }
    
    void resize(size_t dim) { bufferObject()->resize(dim*bytesPerVector()); }
    
    size_t size() const { return bytesUsed() / bytesPerVector(); }
    
    size_t sizeBufferObject() const { return bufferObject() ? bufferObject()->byteCountBufferObject() / bytesPerVector() : 0; }
    
    size_t scalarCount() const { return size() * T_GL_Size; }
    
    size_t scalarCountBufferObject() const { return sizeBufferObject() * T_GL_Size; }

    // ---

    const T_VectorType* begin() const { return reinterpret_cast<const T_VectorType*>(ptr()); }

    T_VectorType* begin() { return reinterpret_cast<T_VectorType*>(ptr()); }
    
    const T_VectorType* end() const { return (reinterpret_cast<const T_VectorType*>(ptr()))+size(); }

    T_VectorType* end() { return (reinterpret_cast<T_VectorType*>(ptr()))+size(); }

    // ---

    T_VectorType& at(size_t i) { VL_CHECK(i<size()); return *(reinterpret_cast<T_VectorType*>(ptr())+i); }

    const T_VectorType& at(size_t i) const { VL_CHECK(i<size()); return *(reinterpret_cast<const T_VectorType*>(ptr())+i); }

    T_VectorType& operator[](size_t i) { return at(i); }

    const T_VectorType& operator[](size_t i) const { return at(i); }

    // ---

    virtual ref<ArrayAbstract> createArray() const { return new Array; }

    virtual ref<ArrayAbstract> clone() const
    {
      ref<Array> arr = createArray()->template as<Array>(); VL_CHECK(arr);
      if (size())
      {
        arr->resize(size());
        memcpy(arr->ptr(), ptr(), bytesUsed());
      }
      return arr;
    }

    // ---

    Sphere computeBoundingSphere() const
    {
      AABB aabb;
      const int count = T_GL_Size == 4 ? 3 : T_GL_Size;
      for(size_t i=0; i<size(); ++i)
      {
        vec3 v;
        const T_Scalar* pv = reinterpret_cast<const T_Scalar*>(&at(i));
        for( int j=0; j<count; ++j )
          v.ptr()[j] = (real)pv[j];
        aabb += v;
      }
      real radius = 0;
      vec3 center = aabb.center();
      for(size_t i=0; i<size(); ++i)
      {
        vec3 v;
        const T_Scalar* pv = reinterpret_cast<const T_Scalar*>(&at(i));
        for( int j=0; j<count; ++j )
          v.ptr()[j] = (real)pv[j];
        real r = (v-center).lengthSquared();
        if (r > radius)
          radius = r;
      }
      return Sphere( center, sqrt(radius) );
    }

    AABB computeBoundingBox() const
    { 
      AABB aabb;
      const int count = T_GL_Size == 4 ? 3 : T_GL_Size;
      for(size_t i=0; i<size(); ++i)
      {
        vec3 v;
        const T_Scalar* pv = reinterpret_cast<const T_Scalar*>(&at(i));
        for( int j=0; j<count; ++j )
          v.ptr()[j] = (real)pv[j];
        aabb += v;
      }
      return aabb;
    }

    void transform(const mat4& m)
    {
      for(size_t i=0; i<size(); ++i)
      {
        vec4 v(0,0,0,1);
        T_Scalar* pv = reinterpret_cast<T_Scalar*>(&at(i));
        // read
        for( size_t j=0; j<T_GL_Size; ++j )
          v.ptr()[j] = (real)pv[j];
        // transform
        v = m * v;
        // write
        for( size_t j=0; j<T_GL_Size; ++j )
          pv[j] = (T_Scalar)v.ptr()[j];
      }
    }

    void normalize()
    {
      for(size_t i=0; i<size(); ++i)
      {
        vec4 v(0,0,0,0);
        T_Scalar* pv = reinterpret_cast<T_Scalar*>(&at(i));
        // read
        for( size_t j=0; j<T_GL_Size; ++j )
          v.ptr()[j] = (real)pv[j];
        // normalize
        v.normalize();
        // write
        for( unsigned j=0; j<T_GL_Size; ++j )
          pv[j] = (T_Scalar)v.ptr()[j];
      }
    }

    vec4 getAsVec4(size_t vector_index) const
    {
      vec4 v(0,0,0,1);
      const T_Scalar* pv = reinterpret_cast<const T_Scalar*>(&at(vector_index));
      for( size_t j=0; j<T_GL_Size; ++j )
        v.ptr()[j] = (real)pv[j];
      return v;
    }

    vec3 getAsVec3(size_t vector_index) const
    {
      vec3 v;
      const T_Scalar* pv = reinterpret_cast<const T_Scalar*>(&at(vector_index));
      const int count = T_GL_Size <= 3 ? T_GL_Size : 3;
      for( int j=0; j<count; ++j )
        v.ptr()[j] = (real)pv[j];
      return v;
    }

    vec2 getAsVec2(size_t vector_index) const
    {
      vec2 v;
      const T_Scalar* pv = reinterpret_cast<const T_Scalar*>(&at(vector_index));
      const int count = T_GL_Size <= 2 ? T_GL_Size : 2;
      for( int j=0; j<count; ++j )
        v.ptr()[j] = (real)pv[j];
      return v;
    }

    int compare(int a, int b) const
    {
      const T_Scalar* pa = reinterpret_cast<const T_Scalar*>(&at(a));
      const T_Scalar* pb = reinterpret_cast<const T_Scalar*>(&at(b));
      for( size_t i=0; i<T_GL_Size; ++i )
        if ( pa[i] != pb[i] )
          return pa[i] < pb[i] ? -1 : +1;
      return 0;
    }

    void initFrom(const std::vector<T_VectorType>& vector)
    {
      resize(vector.size());
      if (vector.empty())
        return;
      else
        memcpy(ptr(),&vector[0],sizeof(vector[0])*vector.size());
    }
  };
//-----------------------------------------------------------------------------
// Array typedefs
//-----------------------------------------------------------------------------


  //! An array of \p GLfloat
  class ArrayFloat1: public Array<GLfloat, GLfloat, 1, GL_FLOAT> { VL_INSTRUMENT_CLASS(vl::ArrayFloat1, VL_GROUP(Array<GLfloat, GLfloat, 1, GL_FLOAT>)) virtual ref<ArrayAbstract> createArray() const { return new ArrayFloat1; } };
  //! An array of vl::fvec2
  class ArrayFloat2: public Array<fvec2,   GLfloat, 2, GL_FLOAT> { VL_INSTRUMENT_CLASS(vl::ArrayFloat2, VL_GROUP(Array<fvec2,   GLfloat, 2, GL_FLOAT>)) virtual ref<ArrayAbstract> createArray() const { return new ArrayFloat2; } };
  //! An array of vl::fvec3
  class ArrayFloat3: public Array<fvec3,   GLfloat, 3, GL_FLOAT> { VL_INSTRUMENT_CLASS(vl::ArrayFloat3, VL_GROUP(Array<fvec3,   GLfloat, 3, GL_FLOAT>)) virtual ref<ArrayAbstract> createArray() const { return new ArrayFloat3; } };
  //! An array of vl::fvec4
  class ArrayFloat4: public Array<fvec4,   GLfloat, 4, GL_FLOAT> { VL_INSTRUMENT_CLASS(vl::ArrayFloat4, VL_GROUP(Array<fvec4,   GLfloat, 4, GL_FLOAT>)) virtual ref<ArrayAbstract> createArray() const { return new ArrayFloat4; } };

  //! An array of \p GLdouble
  class ArrayDouble1: public Array<GLdouble, GLdouble, 1, GL_DOUBLE> { VL_INSTRUMENT_CLASS(vl::ArrayDouble1, VL_GROUP(Array<GLdouble, GLdouble, 1, GL_DOUBLE>)) virtual ref<ArrayAbstract> createArray() const { return new ArrayDouble1; } };
  //! An array of vl::dvec2
  class ArrayDouble2: public Array<dvec2,    GLdouble, 2, GL_DOUBLE> { VL_INSTRUMENT_CLASS(vl::ArrayDouble2, VL_GROUP(Array<dvec2,    GLdouble, 2, GL_DOUBLE>)) virtual ref<ArrayAbstract> createArray() const { return new ArrayDouble2; } };
  //! An array of vl::dvec3
  class ArrayDouble3: public Array<dvec3,    GLdouble, 3, GL_DOUBLE> { VL_INSTRUMENT_CLASS(vl::ArrayDouble3, VL_GROUP(Array<dvec3,    GLdouble, 3, GL_DOUBLE>)) virtual ref<ArrayAbstract> createArray() const { return new ArrayDouble3; } };
  //! An array of vl::dvec4
  class ArrayDouble4: public Array<dvec4,    GLdouble, 4, GL_DOUBLE> { VL_INSTRUMENT_CLASS(vl::ArrayDouble4, VL_GROUP(Array<dvec4,    GLdouble, 4, GL_DOUBLE>)) virtual ref<ArrayAbstract> createArray() const { return new ArrayDouble4; } };

  //! An array of \p GLint
  class ArrayInt1: public Array<GLint, GLint, 1, GL_INT> { VL_INSTRUMENT_CLASS(vl::ArrayInt1, VL_GROUP(Array<GLint, GLint, 1, GL_INT>)) virtual ref<ArrayAbstract> createArray() const { return new ArrayInt1; } };
  //! An array of vl::ivec2
  class ArrayInt2: public Array<ivec2, GLint, 2, GL_INT> { VL_INSTRUMENT_CLASS(vl::ArrayInt2, VL_GROUP(Array<ivec2, GLint, 2, GL_INT>)) virtual ref<ArrayAbstract> createArray() const { return new ArrayInt2; } };
  //! An array of vl::ivec3
  class ArrayInt3: public Array<ivec3, GLint, 3, GL_INT> { VL_INSTRUMENT_CLASS(vl::ArrayInt3, VL_GROUP(Array<ivec3, GLint, 3, GL_INT>)) virtual ref<ArrayAbstract> createArray() const { return new ArrayInt3; } };
  //! An array of vl::ivec4
  class ArrayInt4: public Array<ivec4, GLint, 4, GL_INT> { VL_INSTRUMENT_CLASS(vl::ArrayInt4, VL_GROUP(Array<ivec4, GLint, 4, GL_INT>)) virtual ref<ArrayAbstract> createArray() const { return new ArrayInt4; } };

  //! An array of \p GLuint
  class ArrayUInt1: public Array<GLuint,GLuint, 1, GL_UNSIGNED_INT> { VL_INSTRUMENT_CLASS(vl::ArrayUInt1, VL_GROUP(Array<GLuint,GLuint, 1, GL_UNSIGNED_INT>)) virtual ref<ArrayAbstract> createArray() const { return new ArrayUInt1; } };
  //! An array of vl::uvec2
  class ArrayUInt2: public Array<uvec2, GLuint, 2, GL_UNSIGNED_INT> { VL_INSTRUMENT_CLASS(vl::ArrayUInt2, VL_GROUP(Array<uvec2, GLuint, 2, GL_UNSIGNED_INT>)) virtual ref<ArrayAbstract> createArray() const { return new ArrayUInt2; } };
  //! An array of vl::uvec3
  class ArrayUInt3: public Array<uvec3, GLuint, 3, GL_UNSIGNED_INT> { VL_INSTRUMENT_CLASS(vl::ArrayUInt3, VL_GROUP(Array<uvec3, GLuint, 3, GL_UNSIGNED_INT>)) virtual ref<ArrayAbstract> createArray() const { return new ArrayUInt3; } };
  //! An array of vl::uvec4
  class ArrayUInt4: public Array<uvec4, GLuint, 4, GL_UNSIGNED_INT> { VL_INSTRUMENT_CLASS(vl::ArrayUInt4, VL_GROUP(Array<uvec4, GLuint, 4, GL_UNSIGNED_INT>)) virtual ref<ArrayAbstract> createArray() const { return new ArrayUInt4; } };

  //! An array of \p GLbyte
  class ArrayByte1: public Array<GLbyte, GLbyte, 1, GL_BYTE> { VL_INSTRUMENT_CLASS(vl::ArrayByte1, VL_GROUP(Array<GLbyte, GLbyte, 1, GL_BYTE>)) virtual ref<ArrayAbstract> createArray() const { return new ArrayByte1; } };
  //! An array of vl::bvec2
  class ArrayByte2: public Array<bvec2,  GLbyte, 2, GL_BYTE> { VL_INSTRUMENT_CLASS(vl::ArrayByte2, VL_GROUP(Array<bvec2,  GLbyte, 2, GL_BYTE>)) virtual ref<ArrayAbstract> createArray() const { return new ArrayByte2; } };
  //! An array of vl::bvec3
  class ArrayByte3: public Array<bvec3,  GLbyte, 3, GL_BYTE> { VL_INSTRUMENT_CLASS(vl::ArrayByte3, VL_GROUP(Array<bvec3,  GLbyte, 3, GL_BYTE>)) virtual ref<ArrayAbstract> createArray() const { return new ArrayByte3; } };
  //! An array of vl::bvec4
  class ArrayByte4: public Array<bvec4,  GLbyte, 4, GL_BYTE> { VL_INSTRUMENT_CLASS(vl::ArrayByte4, VL_GROUP(Array<bvec4,  GLbyte, 4, GL_BYTE>)) virtual ref<ArrayAbstract> createArray() const { return new ArrayByte4; } };

  //! An array of \p GLubyte
  class ArrayUByte1: public Array<GLubyte, GLubyte, 1, GL_UNSIGNED_BYTE> { VL_INSTRUMENT_CLASS(vl::ArrayUByte1, VL_GROUP(Array<GLubyte, GLubyte, 1, GL_UNSIGNED_BYTE>)) virtual ref<ArrayAbstract> createArray() const { return new ArrayUByte1; } };
  //! An array of vl::ubvec2
  class ArrayUByte2: public Array<ubvec2,  GLubyte, 2, GL_UNSIGNED_BYTE> { VL_INSTRUMENT_CLASS(vl::ArrayUByte2, VL_GROUP(Array<ubvec2,  GLubyte, 2, GL_UNSIGNED_BYTE>)) virtual ref<ArrayAbstract> createArray() const { return new ArrayUByte2; } };
  //! An array of vl::ubvec3
  class ArrayUByte3: public Array<ubvec3,  GLubyte, 3, GL_UNSIGNED_BYTE> { VL_INSTRUMENT_CLASS(vl::ArrayUByte3, VL_GROUP(Array<ubvec3,  GLubyte, 3, GL_UNSIGNED_BYTE>)) virtual ref<ArrayAbstract> createArray() const { return new ArrayUByte3; } };
  //! An array of vl::ubvec4
  class ArrayUByte4: public Array<ubvec4,  GLubyte, 4, GL_UNSIGNED_BYTE> { VL_INSTRUMENT_CLASS(vl::ArrayUByte4, VL_GROUP(Array<ubvec4,  GLubyte, 4, GL_UNSIGNED_BYTE>)) virtual ref<ArrayAbstract> createArray() const { return new ArrayUByte4; } };

  //! An array of \p GLshort
  class ArrayShort1: public Array<GLshort, GLshort, 1, GL_SHORT> { VL_INSTRUMENT_CLASS(vl::ArrayShort1, VL_GROUP(Array<GLshort, GLshort, 1, GL_SHORT>)) virtual ref<ArrayAbstract> createArray() const { return new ArrayShort1; } };
  //! An array of vl::svec2
  class ArrayShort2: public Array<svec2,   GLshort, 2, GL_SHORT> { VL_INSTRUMENT_CLASS(vl::ArrayShort2, VL_GROUP(Array<svec2,   GLshort, 2, GL_SHORT>)) virtual ref<ArrayAbstract> createArray() const { return new ArrayShort2; } };
  //! An array of vl::svec3
  class ArrayShort3: public Array<svec3,   GLshort, 3, GL_SHORT> { VL_INSTRUMENT_CLASS(vl::ArrayShort3, VL_GROUP(Array<svec3,   GLshort, 3, GL_SHORT>)) virtual ref<ArrayAbstract> createArray() const { return new ArrayShort3; } };
  //! An array of vl::svec4
  class ArrayShort4: public Array<svec4,   GLshort, 4, GL_SHORT> { VL_INSTRUMENT_CLASS(vl::ArrayShort4, VL_GROUP(Array<svec4,   GLshort, 4, GL_SHORT>)) virtual ref<ArrayAbstract> createArray() const { return new ArrayShort4; } };

  //! An array of \p GLushort
  class ArrayUShort1: public Array<GLushort, GLushort, 1, GL_UNSIGNED_SHORT> { VL_INSTRUMENT_CLASS(vl::ArrayUShort1, VL_GROUP(Array<GLushort, GLushort, 1, GL_UNSIGNED_SHORT>)) virtual ref<ArrayAbstract> createArray() const { return new ArrayUShort1; } };
  //! An array of vl::usvec2
  class ArrayUShort2: public Array<usvec2,   GLushort, 2, GL_UNSIGNED_SHORT> { VL_INSTRUMENT_CLASS(vl::ArrayUShort2, VL_GROUP(Array<usvec2,   GLushort, 2, GL_UNSIGNED_SHORT>)) virtual ref<ArrayAbstract> createArray() const { return new ArrayUShort2; } };
  //! An array of vl::usvec3
  class ArrayUShort3: public Array<usvec3,   GLushort, 3, GL_UNSIGNED_SHORT> { VL_INSTRUMENT_CLASS(vl::ArrayUShort3, VL_GROUP(Array<usvec3,   GLushort, 3, GL_UNSIGNED_SHORT>)) virtual ref<ArrayAbstract> createArray() const { return new ArrayUShort3; } };
  //! An array of vl::usvec4
  class ArrayUShort4: public Array<usvec4,   GLushort, 4, GL_UNSIGNED_SHORT> { VL_INSTRUMENT_CLASS(vl::ArrayUShort4, VL_GROUP(Array<usvec4,   GLushort, 4, GL_UNSIGNED_SHORT>)) virtual ref<ArrayAbstract> createArray() const { return new ArrayUShort4; } };

  //! An array of \p GL_HALF_FLOAT
  class ArrayHFloat1: public Array<half,  half, 1, GL_HALF_FLOAT> { VL_INSTRUMENT_CLASS(vl::ArrayHFloat1, VL_GROUP(Array<half,  half, 1, GL_HALF_FLOAT>)) virtual ref<ArrayAbstract> createArray() const { return new ArrayHFloat1; } };
  //! A 2d array of GL_HALF_FLOAT vectors
  class ArrayHFloat2: public Array<hvec2, half, 2, GL_HALF_FLOAT> { VL_INSTRUMENT_CLASS(vl::ArrayHFloat2, VL_GROUP(Array<hvec2, half, 2, GL_HALF_FLOAT>)) virtual ref<ArrayAbstract> createArray() const { return new ArrayHFloat2; } };
  //! A 3d array of GL_HALF_FLOAT vectors
  class ArrayHFloat3: public Array<hvec3, half, 3, GL_HALF_FLOAT> { VL_INSTRUMENT_CLASS(vl::ArrayHFloat3, VL_GROUP(Array<hvec3, half, 3, GL_HALF_FLOAT>)) virtual ref<ArrayAbstract> createArray() const { return new ArrayHFloat3; } };
  //! A 4d array of GL_HALF_FLOAT vectors
  class ArrayHFloat4: public Array<hvec4, half, 4, GL_HALF_FLOAT> { VL_INSTRUMENT_CLASS(vl::ArrayHFloat4, VL_GROUP(Array<hvec4, half, 4, GL_HALF_FLOAT>)) virtual ref<ArrayAbstract> createArray() const { return new ArrayHFloat4; } };

  //! An array of \p GL_FIXED
  class ArrayFixed1: public Array<GLuint,GLuint, 1, GL_FIXED> { VL_INSTRUMENT_CLASS(vl::ArrayFixed1, VL_GROUP(Array<GLuint,GLuint, 1, GL_FIXED>)) virtual ref<ArrayAbstract> createArray() const { return new ArrayFixed1; } };
  //! An array 2d GL_FIXED vectors
  class ArrayFixed2: public Array<uvec2, GLuint, 2, GL_FIXED> { VL_INSTRUMENT_CLASS(vl::ArrayFixed2, VL_GROUP(Array<uvec2, GLuint, 2, GL_FIXED>)) virtual ref<ArrayAbstract> createArray() const { return new ArrayFixed2; } };
  //! An array 3d GL_FIXED vectors
  class ArrayFixed3: public Array<uvec3, GLuint, 3, GL_FIXED> { VL_INSTRUMENT_CLASS(vl::ArrayFixed3, VL_GROUP(Array<uvec3, GLuint, 3, GL_FIXED>)) virtual ref<ArrayAbstract> createArray() const { return new ArrayFixed3; } };
  //! An array 4d GL_FIXED vectors
  class ArrayFixed4: public Array<uvec4, GLuint, 4, GL_FIXED> { VL_INSTRUMENT_CLASS(vl::ArrayFixed4, VL_GROUP(Array<uvec4, GLuint, 4, GL_FIXED>)) virtual ref<ArrayAbstract> createArray() const { return new ArrayFixed4; } };

  //! An array of \p GL_INT_2_10_10_10_REV
  class ArrayInt_2_10_10_10_REV1: public Array<GLint, GLint, 1, GL_INT_2_10_10_10_REV> { VL_INSTRUMENT_CLASS(vl::ArrayInt_2_10_10_10_REV1, VL_GROUP(Array<GLint, GLint, 1, GL_INT_2_10_10_10_REV>)) virtual ref<ArrayAbstract> createArray() const { return new ArrayInt_2_10_10_10_REV1; } };
  //! A 2d array of GL_INT_2_10_10_10_REV vectors
  class ArrayInt_2_10_10_10_REV2: public Array<ivec2, GLint, 2, GL_INT_2_10_10_10_REV> { VL_INSTRUMENT_CLASS(vl::ArrayInt_2_10_10_10_REV2, VL_GROUP(Array<ivec2, GLint, 2, GL_INT_2_10_10_10_REV>)) virtual ref<ArrayAbstract> createArray() const { return new ArrayInt_2_10_10_10_REV2; } };
  //! A 3d array of GL_INT_2_10_10_10_REV vectors
  class ArrayInt_2_10_10_10_REV3: public Array<ivec3, GLint, 3, GL_INT_2_10_10_10_REV> { VL_INSTRUMENT_CLASS(vl::ArrayInt_2_10_10_10_REV3, VL_GROUP(Array<ivec3, GLint, 3, GL_INT_2_10_10_10_REV>)) virtual ref<ArrayAbstract> createArray() const { return new ArrayInt_2_10_10_10_REV3; } };
  //! A 4d array of GL_INT_2_10_10_10_REV vectors
  class ArrayInt_2_10_10_10_REV4: public Array<ivec4, GLint, 4, GL_INT_2_10_10_10_REV> { VL_INSTRUMENT_CLASS(vl::ArrayInt_2_10_10_10_REV4, VL_GROUP(Array<ivec4, GLint, 4, GL_INT_2_10_10_10_REV>)) virtual ref<ArrayAbstract> createArray() const { return new ArrayInt_2_10_10_10_REV4; } };

  //! An array of \p GL_UNSIGNED_INT_2_10_10_10_REV
  class ArrayUInt_2_10_10_10_REV1: public Array<GLuint,GLuint, 1, GL_UNSIGNED_INT_2_10_10_10_REV> { VL_INSTRUMENT_CLASS(vl::ArrayUInt_2_10_10_10_REV1, VL_GROUP(Array<GLuint,GLuint, 1, GL_UNSIGNED_INT_2_10_10_10_REV>)) virtual ref<ArrayAbstract> createArray() const { return new ArrayUInt_2_10_10_10_REV1; } };
  //! A 2d array of GL_UNSIGNED_INT_2_10_10_10_REV vectors
  class ArrayUInt_2_10_10_10_REV2: public Array<uvec2, GLuint, 2, GL_UNSIGNED_INT_2_10_10_10_REV> { VL_INSTRUMENT_CLASS(vl::ArrayUInt_2_10_10_10_REV2, VL_GROUP(Array<uvec2, GLuint, 2, GL_UNSIGNED_INT_2_10_10_10_REV>)) virtual ref<ArrayAbstract> createArray() const { return new ArrayUInt_2_10_10_10_REV2; } };
  //! A 3d array of GL_UNSIGNED_INT_2_10_10_10_REV vectors
  class ArrayUInt_2_10_10_10_REV3: public Array<uvec3, GLuint, 3, GL_UNSIGNED_INT_2_10_10_10_REV> { VL_INSTRUMENT_CLASS(vl::ArrayUInt_2_10_10_10_REV3, VL_GROUP(Array<uvec3, GLuint, 3, GL_UNSIGNED_INT_2_10_10_10_REV>)) virtual ref<ArrayAbstract> createArray() const { return new ArrayUInt_2_10_10_10_REV3; } };
  //! A 4d array of GL_UNSIGNED_INT_2_10_10_10_REV vectors
  class ArrayUInt_2_10_10_10_REV4: public Array<uvec4, GLuint, 4, GL_UNSIGNED_INT_2_10_10_10_REV> { VL_INSTRUMENT_CLASS(vl::ArrayUInt_2_10_10_10_REV4, VL_GROUP(Array<uvec4, GLuint, 4, GL_UNSIGNED_INT_2_10_10_10_REV>)) virtual ref<ArrayAbstract> createArray() const { return new ArrayUInt_2_10_10_10_REV4; } };
}

#endif
