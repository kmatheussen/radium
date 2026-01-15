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

#ifndef Buffer_INCLUDE_ONCE
#define Buffer_INCLUDE_ONCE

#include <vlCore/Object.hpp>
#include <string.h>

namespace vl
{
  //-----------------------------------------------------------------------------
  // Buffer
  //-----------------------------------------------------------------------------
  /**
   * Implements a buffer whose storage is in local memory.
  */
  class Buffer: public Object
  {
    VL_INSTRUMENT_CLASS(vl::Buffer, Object)

  public:
    typedef enum
    {
      UserAllocatedBuffer,
      AutoAllocatedBuffer
    } EAllocationMode;

  public:
    Buffer()
    {
      VL_DEBUG_SET_OBJECT_NAME()
      mPtr = NULL;
      mByteCount = 0;
      mAlignment = VL_DEFAULT_BUFFER_BYTE_ALIGNMENT;
      mAllocationMode = AutoAllocatedBuffer;
    }
    Buffer(const Buffer& other): Object(other)
    {
      VL_DEBUG_SET_OBJECT_NAME()
      mPtr = NULL;
      mByteCount = 0;
      mAlignment = VL_DEFAULT_BUFFER_BYTE_ALIGNMENT;
      mAllocationMode = AutoAllocatedBuffer;
      // copy local data
      *this = other;
    }
    Buffer& operator=(const Buffer& other) 
    { 
      if ( mAllocationMode == AutoAllocatedBuffer)
      {
        // same alignment
        mAlignment = other.mAlignment;
        // make space for new data
        resize(other.bytesUsed());
        // copy new data
        memcpy(mPtr, other.ptr(), bytesUsed());
      }
      else
      {
        VL_CHECK(mPtr);
        VL_CHECK(other.mPtr);
        VL_CHECK(mByteCount == other.mByteCount);
        memcpy(mPtr, other.ptr(), bytesUsed());
      }
      return *this;
    }

    // swaps the data
    void swap(Buffer& other)
    {
      // temp
      unsigned char* tmp_ptr = mPtr;
      size_t tmp_byte_count = mByteCount;
      size_t tmp_alignment = mAlignment;
      // this <- other
      mPtr = other.mPtr;
      mByteCount = other.mByteCount;
      mAlignment = other.mAlignment;
      // this -> other
      other.mPtr = tmp_ptr;
      other.mByteCount = tmp_byte_count;
      other.mAlignment = tmp_alignment;
    }

    ~Buffer()
    {
      clear();
    }

    void clear()
    {
      if ( mAllocationMode == AutoAllocatedBuffer )
        alignedFree(mPtr);
      mPtr = NULL;
      mByteCount = 0;
    }

    // if alignment < 1 uses the last specified alignment or the default one
    void resize(size_t byte_count, size_t alignment = 0)
    {
      VL_CHECK( mAllocationMode == AutoAllocatedBuffer );

      if (byte_count == 0)
      {
        clear();
        return;
      }

      alignment = alignment >= 1 ? alignment : mAlignment;
      if ( byte_count != mByteCount || alignment != mAlignment)
      {
        mAlignment = alignment;
        // allocate the new chunk
        unsigned char* ptr = NULL;
        if (byte_count)
          ptr = (unsigned char*)alignedMalloc(byte_count, mAlignment);
        if (mPtr)
        {
          if (byte_count)
          {
            size_t min = mByteCount < byte_count ? mByteCount : byte_count;
            // copy the old content brutally
            memcpy(ptr, mPtr, min);
          }
          // free the old pointer
          alignedFree(mPtr);
        }
        // set the new pointer
        mPtr = ptr;
        // set the new reserved bytes
        mByteCount = byte_count;
      }
    }

    /** 
     * Uses a user-allocated buffer as storage.
     * After calling this function any call to resize() is illegal.
     * Calling this function enables the UserAllocatedBuffer mode. Call
     * setAllocationMode( AutoAllocatedBuffer ) to revert to the default
     * behaviour.
     */
    void setUserAllocatedBuffer(void* ptr, size_t bytes)
    {
      clear();
      mPtr = (unsigned char*)ptr;
      mByteCount = bytes;
      mAlignment = 0;
      mAllocationMode = UserAllocatedBuffer;
    }

    void setAllocationMode( EAllocationMode mode ) 
    { 
      if ( mAllocationMode != mode )
      {
        clear();
        mAllocationMode = mode;
        // reset buffer data
        mPtr = 0;
        mByteCount = 0;
        mAlignment = 0;
      }
    }

    EAllocationMode allocationMode() const { return mAllocationMode; }

    size_t bytesUsed() const { return mByteCount; }

    bool empty() const { return mByteCount == 0; }

    unsigned char* ptr() { return mPtr; }

    const unsigned char* ptr() const { return mPtr; }

    size_t alignment() const { return mAlignment; }

    // returns a chunck of memory aligned to the specified amount
    // alignment must be a power of two
    static void *alignedMalloc(size_t bytes, size_t alignment)
    {
      // alignment must be a power of two
      if ( alignment & (alignment-1))
        return NULL;

      size_t actual_byte_count = bytes + alignment - 1; 

      // reserve space to store the delta
      actual_byte_count += sizeof(int);

      // allocation
      char *original_ptr = new char[actual_byte_count];

      if (original_ptr == NULL)
        return NULL;

      // move base pointer by the space required for our delta
      char *base_ptr = (char *)original_ptr + sizeof(int);

      // align the pointer
      #if 1
        // warning-free workaround
        unsigned long long long_long_ptr = base_ptr - (char*)0;
        while( long_long_ptr % alignment ) ++long_long_ptr;
        void *aligned_ptr = (char*)0 + long_long_ptr;
      #else
        // this generates too many warnings
        void *aligned_ptr = (void *) (((unsigned long long)base_ptr + alignment - 1) & ~((unsigned long long)alignment - 1));
      #endif

      // compute the delta
      int delta = (int)((char*)aligned_ptr - (char*)original_ptr);

      // store the delta just sizeof(int) bytes before the aligned pointer
      *((int *)aligned_ptr - 1) = delta;

      return aligned_ptr;
    }

    static void alignedFree(void *ptr)
    {
      if (ptr == NULL)
        return;

      // gets the previously stored delta
      int delta = *( (int *)ptr - 1);

      // calculate the pointer returned by malloc()
      char *original_ptr = (char*)ptr - delta;

      // finally free the original pointer
      delete [] original_ptr;
    }

  protected:
    unsigned char* mPtr;
    size_t mByteCount;
    size_t mAlignment;
    EAllocationMode mAllocationMode;
  };

}

#endif
