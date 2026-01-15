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

#ifndef BufferObject_INCLUDE_ONCE
#define BufferObject_INCLUDE_ONCE

#include <vlCore/Vector2.hpp>
#include <vlCore/Vector3.hpp>
#include <vlCore/Vector4.hpp>
#include <vlCore/Buffer.hpp>
#include <vlGraphics/OpenGL.hpp>
#include <vlCore/vlnamespace.hpp>
#include <vlCore/Vector4.hpp>
#include <vlCore/Sphere.hpp>
#include <vlCore/AABB.hpp>

namespace vl
{
//-----------------------------------------------------------------------------
// BufferObject
//-----------------------------------------------------------------------------
  /**
   * The BufferObject class is a Buffer that can upload its data on the GPU memory. 
   * \remarks
   * BufferObject is the storage used by ArrayAbstract and subclasses like ArrayFloat3, ArrayUByte4 etc.
  */
  class BufferObject: public Buffer
  {
    VL_INSTRUMENT_CLASS(vl::BufferObject, Buffer)

  public:
    BufferObject()
    {
      VL_DEBUG_SET_OBJECT_NAME()
      mHandle = 0;
      mUsage = BU_STATIC_DRAW;
      mByteCountBufferObject = 0;
    }

    BufferObject(const BufferObject& other): Buffer(other)
    {
      VL_DEBUG_SET_OBJECT_NAME()
      mHandle = 0;
      mUsage = BU_STATIC_DRAW;
      mByteCountBufferObject = 0;
      // copy local data
      *this = other;
    }

    // deletes the BufferObject data and copyes only the local data
    BufferObject& operator=(const BufferObject& other) 
    { 
      deleteBufferObject();
      super::operator=(other); 
      return *this;
    }

    // swaps data with another BufferObject, including BufferObject handle, usage and local data.
    void swap(BufferObject& other)
    {
      // swap local data
      Buffer::swap(other);
      // tmp
      unsigned int tmp_handle = mHandle;
      EBufferObjectUsage tmp_usage = mUsage;
      GLsizeiptr tmp_bytes = mByteCountBufferObject;
      // this <- other
      mHandle = other.mHandle;
      mUsage = tmp_usage;
      mByteCountBufferObject = other.mByteCountBufferObject;
      // other <- this
      other.mHandle = tmp_handle;
      other.mUsage = tmp_usage;
      other.mByteCountBufferObject = tmp_bytes;
    }

    ~BufferObject()
    {
      deleteBufferObject();
    }

    void setHandle(unsigned int handle) { mHandle = handle; }

    unsigned int handle() const { return mHandle; }

    GLsizeiptr byteCountBufferObject() const { return mByteCountBufferObject; }

    void createBufferObject()
    {
      VL_CHECK_OGL();
      VL_CHECK(Has_BufferObject)
      if (Has_BufferObject && handle() == 0)
      {
        VL_CHECK(mByteCountBufferObject == 0)
        VL_glGenBuffers( 1, &mHandle ); VL_CHECK_OGL();
        mByteCountBufferObject = 0;
        VL_CHECK(handle())
      }
    }

    void deleteBufferObject()
    {
      // mic fixme: it would be nice to re-enable these
      // VL_CHECK_OGL();
      VL_CHECK(Has_BufferObject || handle() == 0)
      if (Has_BufferObject && handle() != 0)
      {
        VL_glDeleteBuffers( 1, &mHandle ); // VL_CHECK_OGL();
        mHandle = 0;
        mByteCountBufferObject = 0;
      }
    }

    void downloadBufferObject()
    {
      VL_CHECK(Has_BufferObject)
      if ( Has_BufferObject && handle() )
      {
        resize( byteCountBufferObject() );
        void* vbo_ptr = mapBufferObject(BA_READ_ONLY);
        memcpy( ptr(), vbo_ptr, byteCountBufferObject() );
        unmapBufferObject();
      }
    }

    // Modifies the BufferObject using data from the local storage.
    // @note Discarding the local storage might delete data used by other Arrays.
    void setBufferData( EBufferObjectUsage usage, bool discard_local_storage=false )
    {
      setBufferData( (int)bytesUsed(), ptr(), usage );
      mUsage = usage;
      if (discard_local_storage)
        clear();
    }

    // Modifies the BufferObject using the supplied data.
    // @note Use this function to initialize or resize the BufferObject and set it's usage flag.
    // If data == NULL the buffer will be allocated but no data will be written to the BufferObject.
    // If data != NULL such data will be copied into the BufferObject.
    void setBufferData( GLsizeiptr byte_count, const GLvoid* data, EBufferObjectUsage usage )
    {
      VL_CHECK_OGL();
      VL_CHECK(Has_BufferObject)
      if ( Has_BufferObject )
      {
        createBufferObject();
        // we use the GL_ARRAY_BUFFER slot to send the data for no special reason
        VL_glBindBuffer( GL_ARRAY_BUFFER, handle() ); VL_CHECK_OGL();
        VL_glBufferData( GL_ARRAY_BUFFER, byte_count, data, usage ); VL_CHECK_OGL();
        VL_glBindBuffer( GL_ARRAY_BUFFER, 0 ); VL_CHECK_OGL();
        mByteCountBufferObject = byte_count;
        mUsage = usage;
      }
    }

    // Modifies an existing BufferObject using data from the local storage.
    // @note You can use this function only on already initialized BufferObjects, use setBufferData() to initialize a BufferObject.
    // @note Discarding the local storage might delete data used by other Arrays.
    void setBufferSubData( GLintptr offset=0, GLsizeiptr byte_count=-1, bool discard_local_storage=false )
    {
      byte_count = byte_count < 0 ? byteCountBufferObject() : byte_count;
      setBufferSubData( offset, byte_count, ptr() );
      if (discard_local_storage)
        clear();
    }

    // Modifies an existing BufferObject using the supplied data.
    // @note You can use this function only on already initialized BufferObjects, use setBufferData() to initialize a BufferObject.
    // @param[in] data Must be != NULL, pointer to the data being written to the BufferObject.
    void setBufferSubData( GLintptr offset, GLsizeiptr byte_count, const GLvoid* data )
    {
      VL_CHECK_OGL();
      VL_CHECK(Has_BufferObject)
      VL_CHECK(data);
      VL_CHECK(handle())
      if (Has_BufferObject && data && handle())
      {
        // we use the GL_ARRAY_BUFFER slot to send the data for no special reason
        VL_glBindBuffer( GL_ARRAY_BUFFER, handle() ); VL_CHECK_OGL();
        VL_glBufferSubData( GL_ARRAY_BUFFER, offset, byte_count, data ); VL_CHECK_OGL();
        VL_glBindBuffer( GL_ARRAY_BUFFER, 0 ); VL_CHECK_OGL();
      }
    }

    // Maps a BufferObject so that it can be read or written by the CPU.
    // @note You can map only one BufferObject at a time and you must unmap it before using the BufferObject again or mapping another one.
    void* mapBufferObject(EBufferObjectAccess access)
    {
      VL_CHECK_OGL();
      VL_CHECK(Has_BufferObject)
      if ( Has_BufferObject )
      {
        createBufferObject();
        VL_glBindBuffer( GL_ARRAY_BUFFER, handle() ); VL_CHECK_OGL();
        void* ptr = VL_glMapBuffer( GL_ARRAY_BUFFER, access ); VL_CHECK_OGL();
        VL_glBindBuffer( GL_ARRAY_BUFFER, 0 ); VL_CHECK_OGL();
        return ptr;
      }
      else
        return NULL;
    }

    // Unmaps a previously mapped BufferObject.
    // @return Returs true or false based on what's specified in the OpenGL specs:
    // "UnmapBuffer returns TRUE unless data values in the buffer’s data store have
    // become corrupted during the period that the buffer was mapped. Such corruption
    // can be the result of a screen resolution change or other window system-dependent
    // event that causes system heaps such as those for high-performance graphics memory
    // to be discarded. GL implementations must guarantee that such corruption can
    // occur only during the periods that a buffer’s data store is mapped. If such corruption
    // has occurred, UnmapBuffer returns FALSE, and the contents of the buffer’s
    // data store become undefined."
    bool unmapBufferObject()
    {
      VL_CHECK_OGL();
      VL_CHECK(Has_BufferObject)
      if ( Has_BufferObject )
      {
        createBufferObject();
        VL_glBindBuffer( GL_ARRAY_BUFFER, handle() ); VL_CHECK_OGL();
        bool ok = VL_glUnmapBuffer( GL_ARRAY_BUFFER ) == GL_TRUE; VL_CHECK_OGL();
        VL_glBindBuffer( GL_ARRAY_BUFFER, 0 ); VL_CHECK_OGL();
        VL_CHECK_OGL();
        return ok;
      }
      else
        return false;
    }

    //! BufferObject usage flag as specified by setBufferData().
    EBufferObjectUsage usage() const { return mUsage; }

  protected:
    unsigned int mHandle;
    GLsizeiptr mByteCountBufferObject;
    EBufferObjectUsage mUsage;
  };
}

#endif
