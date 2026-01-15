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

#ifndef VLXVisitorExportToVLT_INCLUDE_ONCE
#define VLXVisitorExportToVLT_INCLUDE_ONCE

#include <vlCore/VLXVisitor.hpp>
#include <vlCore/VLXValue.hpp>
#include <cstdarg>

namespace vl
{
  /** Translates a VLX hierarchy into VLT notation. */
  class VLXVisitorExportToVLT: public VLXVisitor
  {
    VL_INSTRUMENT_CLASS(vl::VLXVisitorExportToVLT, VLXVisitor)

  public:
    VLXVisitorExportToVLT()
    {
      mIndent = 0;
      mAssign = false;
      mIDSet = NULL;
      mFormatBuffer.resize(4096);
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

    void indent()
    {
      if (mAssign)
        mAssign = false;
      else
      {
        switch(mIndent)
        {
        case 0: break;
        case 1: output("\t"); break;
        case 2: output("\t\t"); break;
        case 3: output("\t\t\t"); break;
        case 4: output("\t\t\t\t"); break;
        case 5: output("\t\t\t\t\t"); break;
        case 6: output("\t\t\t\t\t\t"); break;
        case 7: output("\t\t\t\t\t\t\t"); break;
        case 8: output("\t\t\t\t\t\t\t\t"); break;
        case 9: output("\t\t\t\t\t\t\t\t\t"); break;
        default:
          output("\t\t\t\t\t\t\t\t\t");
          for(int i=9; i<mIndent; ++i)
            output("\t");
        }
      }
    }

    void format(const char* fmt, ...)
    {
      mFormatBuffer[0] = 0;

      va_list ap;
      va_start(ap, fmt);
      vsnprintf(&mFormatBuffer[0], mFormatBuffer.size(), fmt, ap);
      va_end(ap);

      output(&mFormatBuffer[0]);
    }

    void visitValue(VLXValue& value)
    {
      switch(value.type())
      {
        case VLXValue::Structure:
          value.getStructure()->acceptVisitor(this);
          break;

        case VLXValue::List:
          value.getList()->acceptVisitor(this);
          break;

        case VLXValue::ArrayInteger:
          value.getArrayInteger()->acceptVisitor(this);
          break;

        case VLXValue::ArrayReal:
          value.getArrayReal()->acceptVisitor(this);
          break;

        /*
        case VLXValue::ArrayString:
          value.getArrayString()->acceptVisitor(this);
          break;

        case VLXValue::ArrayIdentifier:
          value.getArrayIdentifier()->acceptVisitor(this);
          break;

        case VLXValue::ArrayID:
          value.getArrayID()->acceptVisitor(this);
          break;
        */

        case VLXValue::RawtextBlock:
        {
          VLXRawtextBlock* fblock = value.getRawtextBlock();
          if (!fblock->tag().empty())
            format("%s", fblock->tag().c_str());
          output("\n"); indent(); format("{<\n%s>}\n", rawtextEncode(fblock->value().c_str()).c_str());
        }
        break;

        case VLXValue::String:
          indent(); format("\"%s\"\n", stringEncode( value.getString().c_str() ).c_str() );
          break;

        case VLXValue::Identifier:
          indent(); format("%s\n", value.getIdentifier().c_str() ); VL_CHECK( !value.getIdentifier().empty() )
          break;

        case VLXValue::ID:
          indent(); format("%s\n", value.getID().c_str()); VL_CHECK( !value.getID().empty() )
          break;

        case VLXValue::Bool:
          indent(); format("%s\n", value.getBool() ? "true" : "false");
          break;

        case VLXValue::Integer:
          indent(); format("%lld\n", value.getInteger());
          break;

        case VLXValue::Real:
          indent(); format("%f\n", value.getReal());
          break;
      }
    }

    virtual void visitStructure(VLXStructure* obj)
    {
      if (isVisited(obj))
      {
        indent(); format("%s\n", obj->uid().c_str());
        return;
      }

      // header tag
      if (obj->tag().empty())
      {
        if (mAssign)
        {
          mAssign = false;
          output("\n");
        }
      }
      else
      {
        indent();
        format("%s", obj->tag().c_str());
        output("\n");
      }
      indent();
      output("{\n");

      mIndent++;
      if ( obj->uid().length() && obj->uid() != "#NULL" && isUsed(obj->uid()) )
      {
        indent(); format("ID = %s\n", obj->uid().c_str());
      }

      for(size_t i=0; i<obj->value().size(); ++i)
      {
        indent(); format("%s = ", obj->value()[i].key().c_str());
        mAssign = true;
        visitValue(obj->value()[i].value());
      }
      mIndent--;
      indent(); output("}\n");
    }

    virtual void visitList(VLXList* list)
    {
      // this should happen only if the user manually creates loops
      if (isVisited(list))
      {
        Log::warning("VLXVisitorExportToVLT: cycle detected on VLXList.\n");
        return;
      }

      if (list->value().size() == 0)
      {
        indent(); output("[ ]\n");
        return;
      }

      // header tag
      if (list->tag().empty())
      {
        if (mAssign)
        {
          mAssign = false;
          output("\n");
        }
      }
      else
      {
        indent();
        format("%s", list->tag().c_str());
        output("\n");
      }
      indent();
      output("[\n");

      mIndent++;
      for(size_t i=0; i<list->value().size(); ++i)
        visitValue(list->value()[i]);
      mIndent--;
      indent(); output("]\n");
    }

    virtual void visitArray(VLXArrayInteger* arr)
    {
      indent(); if (!arr->tag().empty()) format("%s ", arr->tag().c_str()); output("( ");
      // output in chunks of 10 numbers
      int i = 0;
      int size = (int)arr->value().size() - 10;
      for( ; i < size; i += 10)
      {
        format("%lld %lld %lld %lld %lld %lld %lld %lld %lld %lld ",
          arr->value()[i+0], arr->value()[i+1], arr->value()[i+2], arr->value()[i+3], arr->value()[i+4],
          arr->value()[i+5], arr->value()[i+6], arr->value()[i+7], arr->value()[i+8], arr->value()[i+9] );
      }
      for( ; i < (int)arr->value().size(); ++i )
        format("%lld ", arr->value()[i]);
      VL_CHECK( i == (int)arr->value().size() )
      output(")\n");
    }

    virtual void visitArray(VLXArrayReal* arr)
    {
      indent(); if (!arr->tag().empty()) format("%s ", arr->tag().c_str()); output("( ");
      // output in chunks of 10 numbers
      int i = 0;
      int size = (int)arr->value().size() - 10;
      for( ; i < size; i += 10)
      {
        format("%f %f %f %f %f %f %f %f %f %f ",
          arr->value()[i+0], arr->value()[i+1], arr->value()[i+2], arr->value()[i+3], arr->value()[i+4],
          arr->value()[i+5], arr->value()[i+6], arr->value()[i+7], arr->value()[i+8], arr->value()[i+9] );
      }
      for( ; i < (int)arr->value().size(); ++i )
        format("%f ", arr->value()[i]);
      VL_CHECK( i == (int)arr->value().size() )
      output(")\n");
    }

    /*
    virtual void visitArray(VLXArrayString* arr)
    {
      indent(); if (!arr->tag().empty()) format("%s ", arr->tag().c_str()); output("( ");
      for(size_t i=0 ;i<arr->value().size(); ++i)
        output(std::string("\"") + stringEncode(arr->value()[i].c_str()) + "\" ");
      output(")\n");
    }

    virtual void visitArray(VLXArrayIdentifier* arr)
    {
      indent(); if (!arr->tag().empty()) format("%s ", arr->tag().c_str()); output("( ");
      for(size_t i=0 ;i<arr->value().size(); ++i)
        format("%s ", arr->value()[i].c_str());
      output(")\n");
    }

    virtual void visitArray(VLXArrayID* arr)
    {
      indent(); if (!arr->tag().empty()) format("%s ", arr->tag().c_str()); output("( ");
      for(size_t i=0 ;i<arr->value().size(); ++i)
        format("%s ", arr->value()[i].uid());
      output(")\n");
    }
    */

    std::string rawtextEncode(const char* str)
    {
      std::string out;
      out.reserve(32);

      for(size_t i=0; str[i]; ++i)
      {
        if ( str[i] == '}' && !out.empty() && out[ out.size()-1 ] == '>')
        {
          out.resize( out.size() - 1 );
          out += "\\>}";
        }
        else
          out.push_back( str[i] );
      }
      return out;
    }

    // mic fixme: support \xHH hex notation both input and output.
    std::string stringEncode(const char* str)
    {
      std::string out;
      for(size_t i=0; str[i]; ++i)
      {
        if (str[i] == '"')
          out += "\\\"";
        else
        if (str[i] == '\\')
          out += "\\\\";
        else
        if (str[i] == '\b')
          out += "\\b";
        else
        if (str[i] == '\f')
          out += "\\f";
        else
        if (str[i] == '\n')
          out += "\\n";
        else
        if (str[i] == '\r')
          out += "\\r";
        else
        if (str[i] == '\t')
          out += "\\t";
        else
          out += str[i];
      }
      return out;
    }

    const std::string& text() const { return mText; }

    std::string& text() { return mText; }

    virtual void output(const std::string& str)
    {
      output(str.c_str());
    }

    virtual void output(const char* str)
    {
      // printf(str);
      mText += str;
    }

    void writeHeader()
    {
      mText = "VLX version=100 encoding=ascii\n\n";
    }

    void setIDSet(std::map< std::string, int >* uids) { mIDSet = uids; }

    std::map< std::string, int >* uidSet() { return mIDSet; }

    const std::map< std::string, int >* uidSet() const { return mIDSet; }

  private:
    int mIndent;
    bool mAssign;
    std::string mText;
    std::map< std::string, int >* mIDSet;
    std::vector<char> mFormatBuffer;
  };
}

#endif
