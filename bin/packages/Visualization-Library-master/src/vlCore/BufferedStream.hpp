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

#ifndef BufferedStream_INCLUDE_ONCE
#define BufferedStream_INCLUDE_ONCE

#include <vlCore/String.hpp>
#include <vlCore/VirtualFile.hpp>
#include <string>
#include <vector>

namespace vl
{
//-----------------------------------------------------------------------------
// BufferedStream
//-----------------------------------------------------------------------------
  /**
   * The BufferedStream class is a template class that that performs a buffered read of Element_Type data from a VirtualFile.
  */
  template<class Element_Type, int Chunk_Size>
  class BufferedStream: public Object
  {
    VL_INSTRUMENT_CLASS(VL_GROUP(vl::BufferedStream<Element_Type, Chunk_Size>), Object)

  public:
    BufferedStream()
    {
      mSize = 0;
      mPtr  = 0;
      mBuffer.resize(Chunk_Size);
      mIsEndOfFile = true;
    }

    void seek(long long pos)
    {
      mSize = 0;
      mPtr  = 0;
      if ( inputFile() )
        inputFile()->seekSet(pos);
    }

    bool readToken(Element_Type* token)
    {
      if ( mUngetBuffer.size() )
      {
        *token = mUngetBuffer.back();
        mUngetBuffer.pop_back();
        return true;
      }

      if (bufferEmpty())
      {
        fillBuffer();
      }

      if (bufferEmpty())
      {
        mIsEndOfFile = true;
        return false;
      }
      else
      {
        *token = mBuffer[mPtr];
        mPtr++;
        return true;
      }
    }

    bool readTextChar(Element_Type& ch)
    {
      if (!readToken(&ch))
        return false;

      Element_Type ch2 = 0;
      switch(ch)
      {
      case 10:
        ch = '\n';
        if (readToken(&ch2) && ch2 != 13)
          ungetToken(ch2);
        break;

      case 13:
        ch = '\n';
        if (readToken(&ch2) && ch2 != 10)
          ungetToken(ch2);
        break;
      }

      return true;
    }

    void ungetToken(const Element_Type& token)
    {
      mUngetBuffer.push_back(token);
    }

    bool bufferEmpty()
    {
      return mPtr == mSize;
    }

    int fillBuffer()
    {
      if ( inputFile() )
      {
        if ( !inputFile()->isOpen() )
          if(!inputFile()->open(OM_ReadOnly))
            return 0;

        mPtr = 0;
        mSize = (int)inputFile()->read(&mBuffer[0], Chunk_Size);
        return mSize;
      }
      else
      {
        return 0;
      }
    }

    bool isEndOfFile() const { return mIsEndOfFile; }

    void setInputFile(VirtualFile* file)
    {
      mInputFile = file;
      mIsEndOfFile = false;
    }

    VirtualFile* inputFile() { return mInputFile.get(); }

    const VirtualFile* inputFile() const { return mInputFile.get(); }

  protected:
    ref<VirtualFile> mInputFile;
    std::vector<Element_Type> mUngetBuffer;
    std::vector<Element_Type> mBuffer;
    int mPtr;
    int mSize;
    bool mIsEndOfFile;
  };
}

#endif
