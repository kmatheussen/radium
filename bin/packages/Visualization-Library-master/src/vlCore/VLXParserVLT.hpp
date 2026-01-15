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

#ifndef VLXParserVLT_INCLUDE_ONCE
#define VLXParserVLT_INCLUDE_ONCE

#include <vlCore/VLXParser.hpp>
#include <vlCore/VLTTokenizer.hpp>
#include <cstdlib>

#ifdef _MSC_VER
  #define atoll _atoi64
#endif

namespace vl
{
  /** Parses a VLT file translating it into a VLX hierarchy. */
  class VLXParserVLT: public VLXParser
  {
    VL_INSTRUMENT_CLASS(vl::VLXParserVLT, VLXParser)

  public:
    VLXParserVLT()
    {
      mTokenizer = new VLTTokenizer;
      mVersion = 0;
    }

    bool getToken(VLTToken& token) { return mTokenizer->getToken(token); }

    bool parseHeader()
    {
      mVersion = 0;
      mEncoding.clear();

      // VLX
      if (!getToken(mToken) || mToken.mType != VLTToken::Identifier || mToken.mString != "VLX")
      {
        Log::error("'VLX' header not found!\n");
        return false;
      }

      // version
      if (!getToken(mToken) || mToken.mType != VLTToken::Identifier || mToken.mString != "version")
        return false;

      if (!getToken(mToken) || mToken.mType != VLTToken::Equals)
        return false;

      if (!getToken(mToken) || mToken.mType != VLTToken::Integer || mToken.mString != "100")
        return false;
      else
        mVersion = (unsigned short)atoi( mToken.mString.c_str() );

      // encoding
      if (!getToken(mToken) || mToken.mType != VLTToken::Identifier || mToken.mString != "encoding")
        return false;

      if (!getToken(mToken) || mToken.mType != VLTToken::Equals)
        return false;

      if (!getToken(mToken) || mToken.mType != VLTToken::Identifier || mToken.mString != "ascii")
        return false;
      else
        mEncoding = mToken.mString;

      return true;
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
      } CloseFile(tokenizer()->inputFile());

      // clear metadata
      mMetadata.clear();

      // read version and encoding
      if (!parseHeader())
      {
        Log::error( Say("Line %n : error parsing VLT header at '%s'.\n") << tokenizer()->lineNumber() << mToken.mString );
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

      while(getToken(mToken) && mToken.mType != VLTToken::TOKEN_EOF)
      {
        if(mToken.mType == VLTToken::TagHeader)
        {
          mLastTag = mToken.mString;

          if(getToken(mToken) && mToken.mType == VLTToken::LeftCurlyBracket)
          {
            ref<VLXStructure> st = new VLXStructure;
            st->setLineNumber( tokenizer()->lineNumber() );

            if (!parseStructure(st.get()))
            {
              if (mToken.mString.length())
                Log::error( Say("Line %n : parse error at '%s'.\n") << mTokenizer->lineNumber() << mToken.mString.c_str() );
              else
                Log::error( Say("Line %n : parse error.\n") << mTokenizer->lineNumber() );
              return false;
            }

            mStructures.push_back(st);
          }
          else
          {
            Log::error( Say("Line %n : parse error at '%s'.\n") << mTokenizer->lineNumber() << mToken.mString.c_str() );
            return false;
          }
        }
        else
        {
          Log::error( Say("Line %n : parse error at '%s'.\n") << mTokenizer->lineNumber() << mToken.mString.c_str() );
          return false;
        }
      }

      parseMetadata();

      VL_CHECK(mToken.mType == VLTToken::TOKEN_EOF)
      return mToken.mType == VLTToken::TOKEN_EOF;
    }

    bool parseStructure(VLXStructure* object)
    {
      // consume last tag if there was one
      if (!mLastTag.empty())
      {
        object->setTag(mLastTag.c_str());
        mLastTag.clear();
      }

      while(getToken(mToken))
      {
        if (mToken.mType == VLTToken::RightCurlyBracket)
        {
          return true;
        }
        else
        if (mToken.mType == VLTToken::Identifier)
        {
          // ID field requires a proper #identifier
          if (mToken.mString.length() == 2)
          {
            if (mToken.mString == "ID")
            {
              // Check if ID has already been set
              if (!object->uid().empty() && object->uid() != "#NULL")
              {
                Log::error("ID already set.\n");
                return false;
              }

              // Equals
              if (!getToken(mToken) || mToken.mType != VLTToken::Equals)
                return false;

              // #identifier
              if (getToken(mToken) && mToken.mType == VLTToken::ID)
              {
                object->setID(mToken.mString.c_str());
                continue;
              }
              else
                return false;
            }
            else
            // ID is a reserved keyword: all the other case combinations are illegal
            if (mToken.mString == "Id" || mToken.mString == "iD" || mToken.mString == "id")
              return false;
          }

          // non-ID key-values
          object->value().push_back( VLXStructure::Value() );
          VLXStructure::Value& name_value = object->value().back();

          // Key
          name_value.setKey( mToken.mString.c_str() );

          // Equals
          if (!getToken(mToken) || mToken.mType != VLTToken::Equals)
            return false;

          // Member value
          if (getToken(mToken))
          {
            name_value.value().setLineNumber( tokenizer()->lineNumber() );

            // A new <Tag>
            if (mToken.mType == VLTToken::TagHeader)
            {
              if (mLastTag.empty())
              {
                mLastTag = mToken.mString;
                if (!getToken(mToken))
                  return false;
              }
              else
                return false;
            }

            // A new { Structure }
            if (mToken.mType == VLTToken::LeftCurlyBracket)
            {
              ref<VLXStructure> object = new VLXStructure;
              object->setLineNumber( tokenizer()->lineNumber() );
              name_value.value().setStructure(object.get());
              if (!parseStructure( object.get() ) )
                return false;
            }
            else
            // An [ list ]
            if (mToken.mType == VLTToken::LeftSquareBracket)
            {
              ref<VLXList> list = new VLXList;
              list->setLineNumber( tokenizer()->lineNumber() );
              name_value.value().setList(list.get());
              if ( !parseList( list.get() ) )
                return false;
            }
            else
            // An ( array )
            if (mToken.mType == VLTToken::LeftRoundBracket)
            {
              ref<VLXArray> arr;
              if ( parseArray( arr ) )
                name_value.value().setArray(arr.get());
              else
                return false;
            }
            else
            // A {< rawtext block >}
            if (mToken.mType == VLTToken::LeftFancyBracket)
            {
              if(!getToken(mToken) || mToken.mType != VLTToken::RawtextBlock)
                return false;
              name_value.value().setRawtextBlock( new VLXRawtextBlock(mLastTag.c_str()) );
              name_value.value().getRawtextBlock()->setValue( mToken.mString.c_str() );
              // consume the tag
              mLastTag.clear();
              if(!getToken(mToken) || mToken.mType != VLTToken::RightFancyBracket)
                return false;
            }
            else
            // A "string"
            if (mToken.mType == VLTToken::String)
            {
              if (!mLastTag.empty())
                return false;
              name_value.value().setString(mToken.mString.c_str());
            }
            else
            // An Identifier
            if (mToken.mType == VLTToken::Identifier)
            {
              if (!mLastTag.empty())
                return false;
              name_value.value().setIdentifier(mToken.mString.c_str());
            }
            else
            // An #id
            if (mToken.mType == VLTToken::ID)
            {
              if (!mLastTag.empty())
                return false;
              name_value.value().setID(mToken.mString.c_str());
            }
            else
            // A boolean true/false
            if (mToken.mType == VLTToken::Boolean)
            {
              if (!mLastTag.empty())
                return false;
              name_value.value().setBool(mToken.mString == "true");
            }
            else
            // An integer
            if (mToken.mType == VLTToken::Integer)
            {
              if (!mLastTag.empty())
                return false;
              name_value.value().setInteger( atoll(mToken.mString.c_str()) );
            }
            else
            // A float
            if (mToken.mType == VLTToken::real)
            {
              if (!mLastTag.empty())
                return false;
              name_value.value().setReal( atof(mToken.mString.c_str()) );
            }
            else
              return false;
          }
        }
        else
          return false;
      }
      return false;
    }

    bool parseList(VLXList* list)
    {
      // consume last tag if there was one
      if (!mLastTag.empty())
      {
        list->setTag(mLastTag.c_str());
        mLastTag.clear();
      }

      while(getToken(mToken))
      {
        if (mToken.mType == VLTToken::RightSquareBracket)
          return true;
        else
        {
          VLXValue value;
          value.setLineNumber( tokenizer()->lineNumber() );
          switch( mToken.mType )
          {
            // <tag>
            case VLTToken::TagHeader:
              {
                if (mLastTag.empty())
                  mLastTag = mToken.mString;
                else
                  return false;
                break;
              }

            // object
            case VLTToken::LeftCurlyBracket:
              {
                ref<VLXStructure> object = new VLXStructure;
                object->setLineNumber( tokenizer()->lineNumber() );
                if ( parseStructure( object.get() ) )
                {
                  value.setStructure(object.get());
                  list->value().push_back( value );
                }
                else
                  return false;
                break;
              }

            // list
            case VLTToken::LeftSquareBracket:
              {
                ref<VLXList> sub_list = new VLXList;
                sub_list->setLineNumber( tokenizer()->lineNumber() );
                if ( parseList( sub_list.get() ) )
                {
                  value.setList( sub_list.get() );
                  list->value().push_back( value );
                }
                else
                  return false;
                break;
              }

            // array
            case VLTToken::LeftRoundBracket:
              {
                ref<VLXArray> arr;
                if (parseArray(arr))
                {
                  value.setArray(arr.get());
                  list->value().push_back(value);
                }
                else
                  return false;
                break;
              }

            // string
            case VLTToken::String:
              if (!mLastTag.empty())
                return false;
              value.setString( mToken.mString.c_str() ); list->value().push_back( value );
              break;

            // identifier
            case VLTToken::Identifier:
              if (!mLastTag.empty())
                return false;
              value.setIdentifier( mToken.mString.c_str() ); list->value().push_back( value );
              break;

            // A {< rawtext block >}
            case VLTToken::LeftFancyBracket:
            {
              if(!getToken(mToken) || mToken.mType != VLTToken::RawtextBlock)
                return false;
              
              value.setRawtextBlock( new VLXRawtextBlock(mLastTag.c_str()) );
              value.getRawtextBlock()->setValue( mToken.mString.c_str() );
              list->value().push_back( value );
              // consume the tag
              mLastTag.clear();

              if(!getToken(mToken) || mToken.mType != VLTToken::RightFancyBracket)
                return false;
              break;
            }

            // ID
            case VLTToken::ID:
              if (!mLastTag.empty())
                return false;
              value.setID( mToken.mString.c_str() ); list->value().push_back( value );
              break;

            // boolean
            case VLTToken::Boolean:
              if (!mLastTag.empty())
                return false;
              value.setBool( mToken.mString == "true" ); list->value().push_back( value );
              break;

            // int
            case VLTToken::Integer:
              if (!mLastTag.empty())
                return false;
              value.setInteger( atoll(mToken.mString.c_str()) ); list->value().push_back( value );
              break;

            // float
            case VLTToken::real:
              if (!mLastTag.empty())
                return false;
              value.setReal( atof(mToken.mString.c_str()) ); list->value().push_back( value );
              break;

          default:
            return false;
          }
        }
      }
      return false;
    }

    bool parseArray(ref<VLXArray>& arr)
    {
      // consume last tag if there was one
      struct struct_consume_tag
      {
        struct_consume_tag(ref<VLXArray>* p1, std::string* p2): p_arr(p1), p_tag(p2) {}

       ~struct_consume_tag()
        {
          if ((*p_arr).get() && !p_tag->empty())
          {
            (*p_arr)->setTag(p_tag->c_str());
            p_tag->clear();
          }
        }

        ref<VLXArray>* p_arr;
        std::string* p_tag;
      } consume_tag(&arr, &mLastTag);

      if(getToken(mToken))
      {
        // (1) from the fist token we decide what kind of array it is going to be
        // (2) empty arrays default to empty VLXArrayInteger

        if (mToken.mType == VLTToken::RightRoundBracket)
        {
          arr = new VLXArrayInteger;
          return true;
        }
        /*
        else
        if (mToken.mType == VLTToken::String)
        {
          ref<VLXArrayString> arr_string;
          arr = arr_string = new VLXArrayString;
          do 
            arr_string->mValue.push_back(mToken.mString);
          while(getToken(mToken) && mToken.mType == VLTToken::String);
          return mToken.mType == VLTToken::RightRoundBracket;
        }
        else
        if (mToken.mType == VLTToken::Identifier)
        {
          ref<VLXArrayIdentifier> arr_identifier;
          arr = arr_identifier = new VLXArrayIdentifier;
          do 
            arr_identifier->mValue.push_back(mToken.mString);
          while(getToken(mToken) && mToken.mType == VLTToken::Identifier);
          return mToken.mType == VLTToken::RightRoundBracket;
        }
        else
        if (mToken.mType == VLTToken::ID)
        {
          ref<VLXArrayID> arr_uid;
          arr = arr_uid = new VLXArrayID;
          do
            arr_uid->mValue.push_back(mToken.mString.c_str());
          while(getToken(mToken) && mToken.mType == VLTToken::ID);
          return mToken.mType == VLTToken::RightRoundBracket;
        }
        */
        else
        if (mToken.mType == VLTToken::Integer)
        {
          ref<VLXArrayInteger> arr_integer;
          arr = arr_integer = new VLXArrayInteger;
          do
          {
            switch(mToken.mType)
            {
            case VLTToken::Integer: arr_integer->value().push_back( atoll( mToken.mString.c_str() ) ); break;
            case VLTToken::RightRoundBracket: return true;
            default:
              return false;
            }
          }
          while(getToken(mToken));
          return false;
        }
        else
        if (mToken.mType == VLTToken::real)
        {
          ref<VLXArrayReal> arr_floating;
          arr = arr_floating = new VLXArrayReal;
          arr_floating->value().reserve(1024);
          // mic fixme: 
          // READING ARRAYS OF NUMBERS IS THE MAIN HOT SPOT FOR THE VLT PARSER
          // - we could speed things up by quickly tokenizing and aotf-ing all the numbers here
          // - we could also use a quicker atof() that works for numbers of the type +1234.1234 using the tokenizer to determine the sub-type
          // - ideally we should mix the following: 
          //   - read big chunks of bytes instead of one by one like now
          //   - merge the quick atof with the tokenizer in one single operation, fall back to standard atof() when complex numbers are detected
          //   - proceed until we find ")"
          //   - we could disallow comments in Arrays to speedup the parsing
          do
          {
            switch(mToken.mType)
            {
            case VLTToken::Integer:
            case VLTToken::real: arr_floating->value().push_back( atof( mToken.mString.c_str() ) ); break;
            case VLTToken::RightRoundBracket: return true;
            default:
              return false;
            }
          }
          while(getToken(mToken));
          return false;
        }
        else
          return false;
      }

      return false;
    }

    // for debug only
    void listTokens()
    {
      while(getToken(mToken) && mToken.mType != VLTToken::TOKEN_EOF)
      {
        switch(mToken.mType)
        {
          case VLTToken::LeftRoundBracket:   printf("LeftSquareBracket (\n"); break;
          case VLTToken::RightRoundBracket:  printf("RightSquareBracket )\n"); break;
          case VLTToken::LeftSquareBracket:  printf("LeftSquareBracket [\n"); break;
          case VLTToken::RightSquareBracket: printf("RightSquareBracket ]\n"); break;
          case VLTToken::LeftCurlyBracket:   printf("LeftCurlyBracket {\n"); break;
          case VLTToken::RightCurlyBracket:  printf("RightCurlyBracket } \n"); break;
          case VLTToken::LeftFancyBracket:   printf("LeftFancyBracket >}\n"); break;
          case VLTToken::RightFancyBracket:  printf("RightFancyBracket {< \n"); break;
          case VLTToken::Equals:             printf("Equals =\n"); break;
          case VLTToken::String:             printf("String = %s\n", mToken.mString.c_str()); break;
          case VLTToken::ID:                printf("ID = %s\n", mToken.mString.c_str()); break;
          case VLTToken::Identifier:         printf("Identifier = %s\n", mToken.mString.c_str()); break;
          case VLTToken::RawtextBlock:       printf("RawtextBlock = %s\n", mToken.mString.c_str()); break;
          case VLTToken::real:               printf("real = %s\n", mToken.mString.c_str()); break;
          case VLTToken::Integer:            printf("Integer = %s\n", mToken.mString.c_str()); break;
          case VLTToken::TagHeader:          printf("TagHeader = %s\n", mToken.mString.c_str()); break;
          default:
            break;
        }
      }
      if (mToken.mType != VLTToken::TOKEN_EOF)
      {
        printf("Line %d: syntax error : '%s'.\n", mTokenizer->lineNumber(), mToken.mString.c_str());
      }
    }

    VLTTokenizer* tokenizer() { return mTokenizer.get(); }

    const VLTTokenizer* tokenizer() const { return mTokenizer.get(); }

  private:
    std::string mLastTag;
    ref<VLTTokenizer> mTokenizer;
    VLTToken mToken;
  };
}

#ifdef _MSC_VER
  #undef atoll
#endif

#endif
