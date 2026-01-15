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

#ifndef VLXParserVLB_INCLUDE_ONCE
#define VLXParserVLB_INCLUDE_ONCE

#include <vlCore/VLXParser.hpp>
#include <vlCore/VLXBinaryDefs.hpp>

namespace vl
{
  /** Parses a VLT file translating it into a VLX hierarchy. */
  class VLXParserVLB: public VLXParser
  {
    VL_INSTRUMENT_CLASS(vl::VLXParserVLB, VLXParser)

  public:
    VLXParserVLB()
    {
      mVersion = 0;
    }

    bool parseHeader()
    {
      mVersion = 0;
      mEncoding.clear();
      mFlags = 0;

      // check the header is fine
      unsigned char vlx_identifier[] = { 0xAB, 'V', 'L', 'X', 0xBB, 0x0D, 0x0A, 0x1A, 0x0A };
      unsigned char vlx[sizeof(vlx_identifier)];
      memset(vlx, 0, sizeof(vlx));
      inputFile()->read(vlx, sizeof(vlx));
      if ( memcmp(vlx, vlx_identifier, sizeof(vlx)) != 0 )
        return false;

      if ( inputFile()->readUInt16(&mVersion,1) != 2 )
        return false;

      unsigned char ch = 0xFF;
      for( ; inputFile()->readUInt8(&ch, 1) && ch ; ch = 0xFF )
        mEncoding.push_back(ch);
      if (ch)
        return false;

      if ( inputFile()->readUInt32(&mFlags, 1) != 4 )
        return false;

      return true;
    }

    bool readChunk(unsigned char& chunk) { return inputFile()->read(&chunk, 1) == 1; }

    bool readInteger(long long& n)
    {
#if 0
      return inputFile()->read(&n, sizeof(n)) == sizeof(n);
#else
      const unsigned char nxt_flag = 0x80;
      const unsigned char neg_flag = 0x40;
      unsigned char byte = 0;
      if ( inputFile()->read(&byte, 1) != 1 )
        return false;
      bool is_neg = (byte & neg_flag) != 0;
      n = byte & 0x3F;
      int shift = 6;
      while(byte & nxt_flag)
      {
        if ( inputFile()->read(&byte, 1) != 1 )
          return false;
        n |= (long long)(byte & 0x7F) << shift;
        shift += 7;
      }
      if (is_neg)
        n = -n;
      return true;
#endif
    }

    void decodeIntegers(const std::vector<unsigned char>& in, std::vector<long long>& out)
    {
      out.reserve(in.size());
      const unsigned char nxt_flag = 0x80;
      const unsigned char neg_flag = 0x40;
      for( size_t i=0 ; i<in.size() ; )
      {
        unsigned char byte = in[i++];
        bool is_neg = (byte & neg_flag) != 0;
        long long n = byte & 0x3F;
        int shift = 6;
        while(byte & nxt_flag)
        {
          byte = in[i++];
          n |= (long long)(byte & 0x7F) << shift;
          shift += 7;
        }
        if (is_neg)
          n = -n;
        // --> output
        out.push_back(n);
      }
    }

    bool readString(std::string& str)
    {
      long long len = 0;
      if (!readInteger(len))
        return false;
      VL_CHECK(len >= 0 );
      if (len < 0)
        return false;
      if (len == 0)
        return true;
      str.resize((size_t)len);
      bool ok = (size_t)inputFile()->read(&str[0], str.length()) == str.length();
      return ok;
    }

    bool parse()
    {
      class CloseFileClass
      {
      public:
        CloseFileClass(VirtualFile* f): mFile(f) {}
        ~CloseFileClass()
        {
          if (mFile)
            mFile->close();
        }
      private:
        ref<VirtualFile> mFile;
      } CloseFile(inputFile());

      inputFile()->close();
      inputFile()->open(OM_ReadOnly);

      // clear metadata
      mMetadata.clear();

      // read version and encoding
      mVersion = 0;
      mEncoding.clear();

      if (!parseHeader())
      {
        Log::error("VLXParserVLB : error parsing VLB header.\n");
        return false;
      }

      if (mVersion != 100)
      {
        Log::error("VLX version not supported.\n");
        return false;
      }

      if (mEncoding != "ascii")
      {
        Log::error("Encoding not supported.\n");
        return false;
      }

      unsigned char chunk;
      std::string str;

      while(readChunk(chunk))
      {
        if(chunk == VLB_ChunkStructure)
        {
          ref<VLXStructure> st = new VLXStructure;

          if (!parseStructure(st.get()))
          {
            Log::error( Say("Error parsing binary file at offset %n.\n") << inputFile()->position() );
            return false;
          }

          mStructures.push_back(st);
        }
        else
        {
          Log::error( Say("Error parsing binary file at offset %n. Expected chunk structure.\n") << inputFile()->position() );
          return false;
        }
      }

      parseMetadata();

      return true;
    }

    bool parseStructure(VLXStructure* st)
    {
      std::string str;
      
      // tag
      if (!readString(str))
        return false;
      st->setTag(str.c_str());

      // ID
      if (!readString(str))
        return false;
      st->setID(str.c_str());

      // read key/value count
      long long count = 0;
      if (!readInteger(count))
        return false;

      // values
      for(int i=0; i<count; ++i)
      {
        VLXStructure::Value val;
        
        // key
        if (!readString(str))
          return false;
        val.setKey(str.c_str());

        // value
        if (!readValue(val.value()))
          return false;
        st->value().push_back(val);
      }
      
      return true;
    }

    bool parseList(VLXList* list)
    {
      std::string str;
      
      // tag
      if (!readString(str))
        return false;
      list->setTag(str.c_str());

      // read value count
      long long count = 0;
      if (!readInteger(count))
        return false;

      // values
      for(int i=0; i<count; ++i)
      {
        VLXValue val;
        
        if (!readValue(val))
          return false;
        else
          list->value().push_back(val);
      }

      return true;
    }

    bool readValue(VLXValue& val)
    {
      unsigned char chunk = 0;

      if (!readChunk(chunk))
        return false;

      std::string str;

      switch(chunk)
      {

      case VLB_ChunkStructure:
        val.setStructure( new VLXStructure );
        return parseStructure( val.getStructure() );
      
      case VLB_ChunkList:
        val.setList( new VLXList );
        return parseList( val.getList() );

      case VLB_ChunkArrayInteger:
        {
          // tag
          if (!readString(str))
            return false;
          else
            val.setArrayInteger( new VLXArrayInteger( str.c_str() ) );

          // count
          long long count = 0;
          if (!readInteger(count))
            return false;

          // values
          VLXArrayInteger& arr = *val.getArrayInteger();
          if (count)
          {
            long long encode_count = 0;
            if (!readInteger(encode_count))
              return false;
            VL_CHECK(encode_count >= 0)
            if (encode_count)
            {
              std::vector<unsigned char> encoded;
              encoded.resize((size_t)encode_count);
              inputFile()->readUInt8(&encoded[0], encode_count);
              decodeIntegers(encoded, arr.value());
            }
          }
          VL_CHECK((size_t)count == arr.value().size())
          return (size_t)count == arr.value().size();
        }

      case VLB_ChunkArrayRealDouble:
        {
          // tag
          if (!readString(str))
            return false;
          else
            val.setArrayReal( new VLXArrayReal( str.c_str() ) );
          // count
          long long count = 0;
          if (!readInteger(count))
            return false;
          // values
          VLXArrayReal& arr = *val.getArrayReal();
          arr.value().resize( (size_t)count );
          if (count)
          {
#if 1
            long long c = inputFile()->readDouble( &arr.value()[0], count );
            VL_CHECK(c == count * (int)sizeof(double))
            return c == count * (int)sizeof(double);
#elif 0
            long long zsize = 0;
            readInteger(zsize);
            std::vector<unsigned char> zipped;
            zipped.resize((size_t)zsize);
            inputFile()->read(&zipped[0], zipped.size());
            bool ok = decompress(&zipped[0], (size_t)zsize, &arr.value()[0]);
            VL_CHECK(ok);
            return ok;
#endif
          }
          else
            return true;
        }

      case VLB_ChunkArrayRealFloat:
        {
          // tag
          if (!readString(str))
            return false;
          else
            val.setArrayReal( new VLXArrayReal( str.c_str() ) );
          // count
          long long count = 0;
          if (!readInteger(count))
            return false;
          // values
          VLXArrayReal& arr = *val.getArrayReal();
          arr.value().resize( (size_t)count );
          if (count)
          {
#if 1
            std::vector<float> floats;
            floats.resize( (size_t)count );
            long long c = inputFile()->readFloat( &floats[0], count );
            // copy over floats to doubles
            for(size_t i=0; i<floats.size(); ++i)
              arr.value()[i] = floats[i];
            VL_CHECK(c == count * (int)sizeof(float))
            return c == count * (int)sizeof(float);
#elif 0
            long long zsize = 0;
            readInteger(zsize);
            std::vector<unsigned char> zipped;
            zipped.resize((size_t)zsize);
            inputFile()->read(&zipped[0], zipped.size());
            bool ok = decompress(&zipped[0], (size_t)zsize, &arr.value()[0]);
            VL_CHECK(ok);
            return ok;
#endif
          }
          else
            return true;
        }

      case VLB_ChunkRawtext:
        // tag
        if (!readString(str))
          return false;
        else
          val.setRawtextBlock( new VLXRawtextBlock( str.c_str() ) );
        // value
        if (!readString(str))
          return false;
        else
        {
          val.getRawtextBlock()->setValue( str.c_str() );
          return true;
        }

      case VLB_ChunkInteger:
        {
          long long i = 0;
          if (!readInteger(i))
            return false;
          else
          {
            val.setInteger(i);
            return true;
          }
        }
     
      case VLB_ChunkRealDouble:
        {
          double d = 0;
          if (inputFile()->readDouble(&d, 1) != sizeof(double))
            return false;
          else
          {
            val.setReal(d);
            return true;
          }
        }

      case VLB_ChunkString:
        if (!readString(str))
          return false;
        else
        {
          val.setString(str.c_str());
          return true;
        }

      case VLB_ChunkIdentifier:
        if (!readString(str))
          return false;
        else
        {
          val.setIdentifier(str.c_str());
          return true;
        }

      case VLB_ChunkID:
        if (!readString(str))
          return false;
        else
        {
          val.setID(str.c_str());
          return true;
        }

      case VLB_ChunkBool:
        {
          unsigned char boolean = false;
          if ( inputFile()->readUInt8(&boolean, 1) != 1 )
            return false;
          else
          {
            val.setBool( boolean != 0 );
            return true;
          }
        }

      default:
        return false;

      }
    }

    void setInputFile(VirtualFile* file) { mInputFile = file; }

    VirtualFile* inputFile() { return mInputFile.get(); }

    const VirtualFile* inputFile() const { return mInputFile.get(); }

  private:
    unsigned int mFlags;
    ref<VirtualFile> mInputFile;
  };
}

#endif
