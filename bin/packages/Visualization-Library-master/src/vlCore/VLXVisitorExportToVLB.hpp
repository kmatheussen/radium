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

#ifndef VLXVisitorExportToVLB_INCLUDE_ONCE
#define VLXVisitorExportToVLB_INCLUDE_ONCE

#include <vlCore/VLXVisitor.hpp>
#include <vlCore/VLXValue.hpp>
#include <vlCore/VLXBinaryDefs.hpp>
#include <vlCore/VirtualFile.hpp>

namespace vl
{
  /** Translates a VLX hierarchy into VLB format writing to the provided VirtualFile. */
  class VLXVisitorExportToVLB: public VLXVisitor
  {
    VL_INSTRUMENT_CLASS(vl::VLXVisitorExportToVLB, VLXVisitor)

  public:
    VLXVisitorExportToVLB(VirtualFile* file = NULL)
    {
      mIDSet = NULL;
      setOutputFile(file);
    }

    bool isUsed(const std::string& uid)
    {
      if (mIDSet)
      {
        std::map< std::string, int >::iterator it = mIDSet->find(uid);
        if (it != mIDSet->end())
          return it->second > 1;
        else
        {
          // should not happen
          VL_TRAP()
          return false;
        }
      }
      else
        return true;
    }

    void writeValue(VLXValue& value)
    {
      switch(value.type())
      {

      case VLXValue::Structure:
        value.getStructure()->acceptVisitor(this);
        break;

      case VLXValue::List:
        value.getList()->acceptVisitor(this);
        break;

      /*
      case VLXValue::ArrayString:
        break;

      case VLXValue::ArrayIdentifier:
        break;

      case VLXValue::ArrayID:
        break;
      */

      case VLXValue::ArrayInteger:
        value.getArrayInteger()->acceptVisitor(this);
        break;

      case VLXValue::ArrayReal:
        value.getArrayReal()->acceptVisitor(this);
        break;

      case VLXValue::RawtextBlock:
      {
        VLXRawtextBlock* fblock = value.getRawtextBlock();
        // header
        mOutputFile->writeUInt8( VLB_ChunkRawtext );
        // tag
        writeString( fblock->tag().c_str() );
        // value
        writeString( fblock->value().c_str() ); // no decoding needed
      }
      break;

      case VLXValue::String:
        // header
        mOutputFile->writeUInt8( VLB_ChunkString );
        // value
        writeString( value.getString().c_str() );
        break;

      case VLXValue::Identifier:
        // header
        mOutputFile->writeUInt8( VLB_ChunkIdentifier );
        // value
        writeString( value.getIdentifier().c_str() );
        break;

      case VLXValue::ID:
        // header
        mOutputFile->writeUInt8( VLB_ChunkID );
        // value
        writeString( value.getID().c_str() );
        break;

      case VLXValue::Bool:
        // header
        mOutputFile->writeUInt8( VLB_ChunkBool );
        // value
        mOutputFile->writeUInt8( value.getBool() );
        break;

      case VLXValue::Integer:
        // header
        mOutputFile->writeUInt8( VLB_ChunkInteger);
        // value
        writeInteger( value.getInteger() );
        break;

      case VLXValue::Real:
        // header
        mOutputFile->writeUInt8( VLB_ChunkRealDouble);
        // value
        mOutputFile->writeDouble( value.getReal() );
        break;
      }
    }

    virtual void visitStructure(VLXStructure* obj)
    {
      if (isVisited(obj))
      {
        mOutputFile->writeUInt8( VLB_ChunkID );
        writeString( obj->uid().c_str() );
        return;
      }

      // header
      mOutputFile->writeUInt8( VLB_ChunkStructure );
      
      // tag
      writeString( obj->tag().c_str() );
      
      // ID
      writeString( obj->uid().c_str() );

      // key/value count
      writeInteger( obj->value().size() );

      // values
      for(size_t i=0; i<obj->value().size(); ++i)
      {
        // key
        writeString(obj->value()[i].key().c_str());

        // value
        writeValue(obj->value()[i].value());
      }
    }

    virtual void visitList(VLXList* list)
    {
      // this should happen only if the user manually creates loops
      if (isVisited(list))
      {
        Log::warning("VLXVisitorExportToVLT: cycle detected on VLXList.\n");
        return;
      }

      // header
      mOutputFile->writeUInt8( VLB_ChunkList );
      
      // tag
      writeString( list->tag().c_str() );
      
      // value count
      writeInteger( list->value().size() );

      // values
      for(size_t i=0; i<list->value().size(); ++i)
        writeValue(list->value()[i]);
    }

    virtual void visitArray(VLXArrayInteger* arr)
    {
      // header
      mOutputFile->writeUInt8( VLB_ChunkArrayInteger );

      // tag
      writeString(arr->tag().c_str());

      // value count
      writeInteger(arr->value().size());

      // value
      if (arr->value().size() > 0)
      {
        std::vector<unsigned char> encoded;
        encodeIntegers(&arr->value()[0], (int)arr->value().size(), encoded); VL_CHECK(encoded.size())
        writeInteger(encoded.size());
        mOutputFile->writeUInt8(&encoded[0], encoded.size());
      }
    }

    bool needsDoublePrecision(const double* in, size_t count)
    {
      for(size_t i=0; i<count; ++i)
      {
        float f = (float)in[i];
        if ((double)f != in[i])
          return true;
      }

      return false;
    }

    virtual void visitArray(VLXArrayReal* arr)
    {
      bool needs_double = arr->value().empty() ? false : needsDoublePrecision(&arr->value()[0], arr->value().size());

      // header
      mOutputFile->writeUInt8( (unsigned char)(needs_double ? VLB_ChunkArrayRealDouble : VLB_ChunkArrayRealFloat) );
      // tag
      writeString(arr->tag().c_str());
      // count
      writeInteger(arr->value().size());
      // value
      if (arr->value().size())
      {
#if 1
        if (needs_double)
          mOutputFile->writeDouble(&arr->value().front(), arr->value().size());
        else
        {
          std::vector<float> floats;
          floats.resize(arr->value().size());
          for(size_t i=0; i<arr->value().size(); ++i)
            floats[i] = (float)arr->value()[i];
          mOutputFile->writeFloat(&floats[0], floats.size());
        }
#else
        std::vector<unsigned char> zipped;
        compress( &arr->value()[0], arr->value().size() * sizeof(arr->value()[0]), zipped, 1 );
        writeInteger( zipped.size() );
        mOutputFile->write(&zipped[0], zipped.size());
#endif
      }
    }

    /*
    virtual void visitArray(VLXArrayString* arr)
    {
    }

    virtual void visitArray(VLXArrayIdentifier* arr)
    {
    }

    virtual void visitArray(VLXArrayID* arr)
    {
    }
    */

    void writeHeader()
    {
      // see http://www.khronos.org/opengles/sdk/tools/KTX/file_format_spec/ for more info on why I choose these characters.
      unsigned char vlx_identifier[] = { 0xAB, 'V', 'L', 'X', 0xBB, 0x0D, 0x0A, 0x1A, 0x0A };

      mOutputFile->write(vlx_identifier, sizeof(vlx_identifier));
      mOutputFile->writeUInt16(100);    // "version" (16 bits uint)
      mOutputFile->write("ascii", 5+1); // "encoding" (zero terminated string)
      mOutputFile->writeUInt32(0);      // "flags" (reserved for the future)
    }

    void writeString(const char* str)
    {
      size_t len = strlen(str);
      writeInteger(len);
      mOutputFile->write(str, len);
    }

    void writeInteger(long long n)
    {
#if 0
      mOutputFile->writeSInt64(n);
#else
      const unsigned char nxt_flag = 0x80;
      const unsigned char neg_flag = 0x40;
      unsigned char bytes[12]; memset(bytes, 0, sizeof(bytes)); // should take maximum 10 bytes
      unsigned char* byte = bytes;
      if (n < 0)
      {
        n = -n;
        *byte = neg_flag;
      }
      // lower 6 bits
      *byte |= n & 0x3F; n >>= 6;
      *byte |= n ? nxt_flag : 0;
      ++byte; // --> output
      // rest of the bytes
      while (n)
      {
        *byte = n & 0x7F; n >>= 7;
        *byte |= n ? nxt_flag : 0;
        ++byte; // --> output
      }
      mOutputFile->write(bytes, byte - bytes);
#endif
    }

    void encodeIntegers(long long* val, int count, std::vector<unsigned char>& out)
    {
      const unsigned char nxt_flag = 0x80;
      const unsigned char neg_flag = 0x40;
      out.reserve(count);
      for( int i=0; i<count; ++i)
      {
        unsigned char byte = 0;
        long long n = val[i];
        if (n < 0)
        {
          n = -n;
          byte = neg_flag;
        }
        // lower 6 bits
        byte |= n & 0x3F; n >>= 6;
        byte |= n ? nxt_flag : 0;
        out.push_back(byte);
        // rest of the bytes
        while (n)
        {
          byte = n & 0x7F; n >>= 7;
          byte |= n ? nxt_flag : 0;
          out.push_back(byte);
        }
      }
    }

    void setIDSet(std::map< std::string, int >* uids) { mIDSet = uids; }

    std::map< std::string, int >* uidSet() { return mIDSet; }

    const std::map< std::string, int >* uidSet() const { return mIDSet; }

    void setOutputFile(VirtualFile* file) 
    { 
      mOutputFile = file;
      if (file)
      {
        file->close();
        file->open(OM_WriteOnly);
      }
    }

    VirtualFile* outputFile() { return mOutputFile.get(); }

    const VirtualFile* outputFile() const { return mOutputFile.get(); }

  private:
    std::map< std::string, int >* mIDSet;
    ref<VirtualFile> mOutputFile;
  };
}

#endif
