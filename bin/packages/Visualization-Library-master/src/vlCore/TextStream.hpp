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

#ifndef TextStream_INCLUDE_ONCE
#define TextStream_INCLUDE_ONCE

#include <vlCore/BufferedStream.hpp>

namespace vl
{

//-----------------------------------------------------------------------------
// TextStream
//-----------------------------------------------------------------------------
  /**
   * The TextStream class can be used to conveniently read or parse utf8-encoded text files.
  */
  class VLCORE_EXPORT TextStream: public BufferedStream<unsigned char, 128*1024>
  {
    VL_INSTRUMENT_CLASS(vl::TextStream, VL_GROUP(BufferedStream<unsigned char, 128*1024>))

  public:
    TextStream(VirtualFile* file=NULL)
    {
      setInputFile(file);
    }
    ~TextStream()
    {
      if(inputFile())
        inputFile()->close();
    }

    void ungetLine(const std::string& utf8)
    {
      ungetToken('\n');
      for(size_t i=utf8.size(); i--;)
        ungetToken(utf8[i]);
    }
    bool readLine(std::string& utf8)
    {
      utf8.clear();
      if ( !inputFile()->isOpen() ) 
        if(!inputFile()->open(OM_ReadOnly))
          return false;

      unsigned char ch = 0;
      bool ok = false;
      while( readToken(&ch) )
      {
        ok = true;
        if ( ch == '\r' || ch == '\n' )
        {
          readToken(&ch);
          if ( ch != '\r' && ch != '\n' )
            ungetToken(ch); 
          break;
        }
        else
          utf8 += ch;
      }
      return ok;
    }

    //! Reads a CR or LF or CR/LF or LF/CR terminated line
    bool readLine(String& line)
    {
      line.clear();
      std::vector<unsigned char> utf8;

      if ( !inputFile()->isOpen() ) 
        if(!inputFile()->open(OM_ReadOnly))
          return false;

      unsigned char ch = 0;
      bool ok = false;
      while( readToken(&ch) )
      {
        ok = true;
        if ( ch == '\r' || ch == '\n' )
        {
          readToken(&ch);
          if ( ch != '\r' && ch != '\n' )
            ungetToken(ch);
          break;
        }
        else
          utf8.push_back(ch);
      }
      if(!utf8.empty())
        line = String::fromUTF8((char*)&utf8[0], (int)utf8.size());
      return ok;
    }

    //! Reads a CR terminated line
    bool readLineCR(String& line)
    {
      line.clear();
      std::vector<unsigned char> utf8;

      if ( !inputFile()->isOpen() ) 
        if(!inputFile()->open(OM_ReadOnly))
          return false;

      unsigned char ch = 0;
      while( readToken(&ch) )
      {
        if ( ch == '\r' )
          break;
        else
          utf8.push_back(ch);
      }
      if(!utf8.empty())
        line = String::fromUTF8((char*)&utf8[0], (int)utf8.size());
      return !line.empty();
    }

    //! Reads a LF terminated line
    bool readLineLF(String& line)
    {
      line.clear();
      std::vector<unsigned char> utf8;

      if ( !inputFile()->isOpen() ) 
        if(!inputFile()->open(OM_ReadOnly))
          return false;

      unsigned char ch = 0;
      while( readToken(&ch) )
      {
        if ( ch == '\n' )
          break;
        else
          utf8.push_back(ch);
      }
      if(!utf8.empty())
        line = String::fromUTF8((char*)&utf8[0], (int)utf8.size());
      return !line.empty();
    }

    bool readInt(int& i, bool hex=false);

    bool readDouble(double& d);

    bool readString(String& token)
    {
      token.clear();
      unsigned char ch = 0;
      while ( readToken(&ch) )
      {
        if ( ch == '\r' || ch == '\n' || ch == '\t' || ch == ' ' )
        {
          if ( token.empty() )
            continue;
          else
            return true;
        }
        else
          token += ch;
      }
      return !token.empty();
    }

    bool readStdString(std::string& token)
    {
      token.clear();
      unsigned char ch = 0;
      while ( readToken(&ch) )
      {
        if ( ch == '\r' || ch == '\n' || ch == '\t' || ch == ' ' )
        {
          if ( token.empty() )
            continue;
          else
            return true;
        }
        else
          token += ch;
      }
      return !token.empty();
    }

    bool readQuotedString(String& token)
    {
      bool open = false;
      token.clear();
      unsigned char ch = 0;
      while ( readToken(&ch) )
      {
        // cannot be a multiline quote
        if ( ch == '\r' || ch == '\n' || (!open && ( ch == '\t' || ch == ' ')) )
        {
          if ( token.empty() )
            continue;
          else
            return true;
        }
        else
          token += ch;

        if (ch == '\"' || ch == '\'')
          open = !open;
      }
      return !token.empty();
    }

  protected:
    String mTmpStr;
    std::string mTmpStdStr;
  };
//-----------------------------------------------------------------------------
}

#endif
