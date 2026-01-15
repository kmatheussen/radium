/**************************************************************************************/
/*                                                                                    */
/*  Visualization Library                                                             */
/*  http://www.visualizationlibrary.org                                               */
/*                                                                                    */
/*  Copyright (c) 2005-2011, Michele Bosi                                             */
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

#include <vlCore/String.hpp>
#include <vlCore/String_Tables.hpp>
#include <vlCore/Log.hpp>
#include <vlCore/Say.hpp>
#include <vlCore/FileSystem.hpp>
#include <vlCore/VirtualFile.hpp>
#include <vlCore/VisualizationLibrary.hpp>
#include <stdio.h>
#include <stdarg.h>

using namespace vl;

//-----------------------------------------------------------------------------
// String
//-----------------------------------------------------------------------------
String::String()
{
#if VL_STRING_COPY_ON_WRITE == 0
  acquireData();
#endif
}
//-----------------------------------------------------------------------------
String::String(const String& other)
{
  operator=(other);
}
//-----------------------------------------------------------------------------
String::String(const wchar_t* wstr)
{
#if VL_STRING_COPY_ON_WRITE == 0
  acquireData();
#endif
  if (wstr)
    *this = wstr;
}
//-----------------------------------------------------------------------------
String::String(const char* str)
{
#if VL_STRING_COPY_ON_WRITE == 0
  acquireData();
#endif
  if (str)
    *this = str;
}
//-----------------------------------------------------------------------------
String::String(wchar_t ch, int count)
{
#if VL_STRING_COPY_ON_WRITE == 0
  acquireData();
#endif
  for(int i=0; i<count; ++i)
    *this += ch;
}
//-----------------------------------------------------------------------------
String String::loadText(const String& path, EStringEncoding default_encoding)
{
  ref<VirtualFile> file = defFileSystem()->locateFile(path);
  if (file)
    return loadText( file.get(), default_encoding );
  else
  {
    Log::error( Say("Could not locate '%s'.\n") << path );
    return String();
  }
}
//-----------------------------------------------------------------------------
String String::loadText(VirtualFile* file, EStringEncoding default_encoding)
{
  std::vector<char> buffer;
  file->load( buffer );
  file->close();

  if ( buffer.size() )
  {
    return loadText( &buffer[0], (int)buffer.size(), default_encoding );
  }
  else
  {
    return String();
  }
}
//-----------------------------------------------------------------------------
String String::loadText(void* data, int bytes, EStringEncoding default_encoding )
{
  EStringEncoding enc = detectEncoding( data, bytes, default_encoding );
  String text;
  switch(enc)
  {
    case SE_ASCII:
      return fromAscii((char*)data, bytes);
      break;
    case SE_LATIN1:
      return fromLatin1((char*)data, bytes);
      break;
    case SE_UTF8:
      return fromUTF8((char*)data, bytes);
      break;
    case SE_UTF16_BE:
      return fromUTF16BE((unsigned short*)data, bytes );
      break;
    case SE_UTF16_LE:
      return fromUTF16LE((unsigned short*)data, bytes );
      break;
    case SE_Unknown:
      Log::error("String::loadText() unknown encoding.\n");
      break;
    case SE_UTF32_BE:
    case SE_UTF32_LE:
      Log::error("String::loadText() SE_UTF32_BE/SE_UTF32_LE encoding not supported.\n");
      break;
  }
  return String();
}
//-----------------------------------------------------------------------------
String& String::resize(int character_count)
{
  acquireData();
  mString->resize(character_count);
  return *this;
}
//-----------------------------------------------------------------------------
String String::substring(int start, int count) const
{
  // createData();
  if ( empty() )
    return String();

  if (start<0)
    start = 0;
  if (count<0)
    count = length();
  int end_idx = start+count-1;
  if (end_idx > length()-1 )
    end_idx = length()-1;

  String str;
  str.acquireData();
  int sz = end_idx - start + 1;
  sz = sz < 0 ? 0 : sz;
  str.mString->resize( sz );
  for(int i=0; i<(int)str.mString->length(); ++i)
    (*str.mString)[i] = (*mString)[start+i];
  return str;
}
//-----------------------------------------------------------------------------
int String::findBackwards(wchar_t ch) const
{
  // createData();
  if (empty())
    return -1;

  for(int i=length(); i--; )
    if ((*mString)[i] == ch)
      return i;
  return -1;
}
//-----------------------------------------------------------------------------
int String::findBackwards(const String& str) const
{
  // createData();
  if (empty())
    return -1;

  if (str.length() < length())
  {
    for(int i = length() - str.length()+1; i--; )
    {
      int j=0;
      for(; j<str.length(); ++j)
      {
        if ( str[j] != (*mString)[i+j] )
          break;
      }
      if ( j == str.length() )
        return i;
    }
  }
  return -1;
}
//-----------------------------------------------------------------------------
bool String::contains(wchar_t ch) const
{
  return find(ch) != -1;
}
//-----------------------------------------------------------------------------
int String::find(wchar_t ch, int start) const
{
  // createData();
  if (empty())
    return -1;

  for(int i=start; i<length(); ++i)
    if ((*mString)[i] == ch)
      return i;
  return -1;
}
//-----------------------------------------------------------------------------
bool String::contains(const String& substr) const
{
  return find(substr) != -1;
}
//-----------------------------------------------------------------------------
namespace
{
  // warning: this function takes 256K on the stack!
  int String_Quick_Search(const wchar_t*x, int m, const wchar_t*y, int n)
  {
    int qsBc[0x10000];
    for (int i = 0; i < 0x10000; ++i)
      qsBc[i] = m + 1;
    for (int i = 0; i < m; ++i)
      qsBc[x[i]] = m - i;
    for(int j=0; j <= n - m; j += qsBc[y[j + m]] )
      if (memcmp(x, y + j, m*sizeof(wchar_t)) == 0)
         return j;
    return -1;
  }
}
//-----------------------------------------------------------------------------
int String::findInLargeText(const String& substr, int start) const
{
  // createData();
  if (empty())
    return -1;

  if ( substr.length() > length() || start >= length() || substr.empty() || empty() )
    return -1;
  {
    int pos = String_Quick_Search( &(*substr.mString)[0], substr.length(), &(*mString)[0]+start, length()-start );
    return pos >= 0 ? pos + start : pos;
  }
}
//-----------------------------------------------------------------------------
int String::find(const String& substr, int start) const
{
  // createData();
  if (empty())
    return -1;

  if ( substr.length() > length() || start >= length() || substr.empty() || empty() )
    return -1;
  {
    int max = length() - substr.length();
    for(int i=start; i<=max; ++i)
    {
      int j=0;
      for(; j<substr.length(); ++j)
      {
        if (substr[j] != (*mString)[i+j])
          break;
      }
      if (j == substr.length())
        return i;
    }
    return -1;
  }
}
//-----------------------------------------------------------------------------
void String::squeeze()
{
  if(empty())
    return;

  mString->squeeze();
}
//-----------------------------------------------------------------------------
String& String::fill(wchar_t ch)
{
  acquireData();

  for(int i=0; i<length(); ++i)
    (*mString)[i] = ch;
  return *this;
}
//-----------------------------------------------------------------------------
String& String::trim(const String& chars)
{
  acquireData();

  while( chars.length() )
  {
    int len = length();
    for( int i=0; i<chars.length(); ++i)
      trim(chars[i]);
    if ( len == length())
      break;
  }
  return *this;
}
//-----------------------------------------------------------------------------
String& String::trim(wchar_t ch)
{
  acquireData();

  if (length())
  {
    int pos = 0;
    while( (*mString)[pos] == ch )
      pos++;
    if (pos)
      *this = substring(pos);
    pos = length()-1;
    while( pos >=0 && (*mString)[pos] == ch )
      pos--;
    pos++;
    if (pos != length())
      *this = substring(0,pos);
  }
  return *this;
}
//-----------------------------------------------------------------------------
String& String::trim()
{
  acquireData();

  trim("\n\r\t\v ");

  return *this;
}
//-----------------------------------------------------------------------------
void String::split(const String& separator_list, std::vector<String>& fields, bool remove_empty) const
{
  fields.clear();

  // createData();
  if (empty())
    return;

  if ( length() )
  {
    fields.push_back( String() );
    fields.back().acquireData();
    fields.back().mString->clear();
    for(int i=0; i<length(); ++i)
    {
      if ( separator_list.contains((*mString)[i]) )
      {
        fields.push_back( String() );
        fields.back().acquireData();
        fields.back().mString->clear();
        continue;
      }
      fields.back().mString->push_back( (*mString)[i] );
    }
  }

  if (remove_empty)
  {
    for ( size_t i=fields.size(); i--; )
      if (fields[i].empty())
        fields.erase(fields.begin() + i);
  }
}
//-----------------------------------------------------------------------------
void String::split(wchar_t separator, std::vector<String>& fields, bool remove_empty) const
{
  fields.clear();

  // createData();
  if (empty())
    return;

  if ( length() )
  {
    fields.push_back( String() );
    fields.back().acquireData();
    fields.back().mString->clear();
    for(int i=0; i<length(); ++i)
    {
      if ((*mString)[i] == separator)
      {
        fields.push_back( String() );
        fields.back().acquireData();
        fields.back().mString->clear();
        continue;
      }
      fields.back().mString->push_back( (*mString)[i] );
    }
  }

  if (remove_empty)
  {
    for ( size_t i=fields.size(); i--; )
      if (fields[i].empty())
        fields.erase(fields.begin() + i);
  }
}
//-----------------------------------------------------------------------------
void String::splitLines(std::vector<String>& lines) const
{
  lines.clear();

  // createData();
  if (empty())
    return;

  if ( length() )
  {
    lines.push_back( String() );
    lines.back().acquireData();
    lines.back().mString->clear();
    for(int i=0; i<length(); ++i)
    {
      if ((*mString)[i] == '\n' || (*mString)[i] == '\r')
      {
        lines.push_back( String() );
        lines.back().acquireData();
        lines.back().mString->clear();
        // no need to check i+1<lenght()-1 since the buffer is 0 terminated
        if ((*mString)[i] == '\n' && (*mString)[i+1] == '\r')
          ++i;
        else
        if ((*mString)[i] == '\r' && (*mString)[i+1] == '\n')
          ++i;
        continue;
      }
      lines.back().mString->push_back( (*mString)[i] );
    }
  }
}
//-----------------------------------------------------------------------------
String String::field(wchar_t separator, int field_index) const
{
  String field;
  int field_count = 0;
  int i=0;
  for(; i<length() && field_count<field_index; ++i)
  {
    if ( (*this)[i] == separator )
      ++field_count;
  }
  // fill field data
  for(; i<length() && (*this)[i] != separator; ++i)
    field+=(*this)[i];
  return field;
}
//-----------------------------------------------------------------------------
String& String::remove(const String& str, int start, int count)
{
  acquireData();

  if (count == 0)
    return *this;
  if (count<0)
    count = length();
  int removed = 0;
  for( int pos = find(str, start); pos != -1 && removed<count; start=pos, pos=find(str, start), ++removed)
    remove( pos, str.length() );
  return *this;
}
//-----------------------------------------------------------------------------
String& String::remove( int start, int count )
{
  if (count == 0)
    return *this;

  acquireData();

  String tmp;
  tmp.acquireData();
  tmp.mString->clear();
  int end = start + count-1;
  for( int i=0; i<length(); i++ )
    if (i<start || i>end)
      tmp.mString->push_back((*mString)[i]);
  /*tmp.mString->push_back(0);*/
  mString = tmp.mString;
  return *this;
}
//-----------------------------------------------------------------------------
String& String::remove(wchar_t ch, int start, int count)
{
  acquireData();

  if (count<0)
    count = length();
  String tmp = *this;
  tmp.acquireData();
  mString->clear();
  int removed = 0;
  for(int i=0; i<tmp.length(); ++i)
    if ( tmp[i]!=ch || removed==count || i<start)
      mString->push_back( tmp[i] );
    else
      ++removed;
  /*mString->push_back(0);*/
  return *this;
}
//-----------------------------------------------------------------------------
String& String::reverse()
{
  acquireData();
  int count = length() / 2;
  for(int i=0; i<count; ++i)
  {
    wchar_t tmp = (*this)[i];
    (*this)[i] = (*this)[length() - 1 - i];
    (*this)[length() - 1 - i] = tmp;
  }
  return *this;
}
//-----------------------------------------------------------------------------
String& String::normalizeSlashes() 
{ 
  // convert all '\' to '/'
  replace('\\', '/');
  // remove double slashes
  int len=0;
  do
  {
    len=length();
    replace("//", "/"); 
  }
  while(len!=length());

  bool beg_slash = startsWith('/');

  bool end_slash = endsWith('/');

  // resolve . and ..
  std::vector<String> parts;
  split('/', parts, true);
  std::vector<String> new_parts;
  for(size_t i=0; i<parts.size(); ++i)
  {
    if (parts[i] == ".")
      continue;
    else
    if (parts[i] == ".." && !new_parts.empty())
    {
      new_parts.pop_back();
      continue;
    }
    else
      new_parts.push_back(parts[i]);
  }

  // recreate the string

  clear();
  if (beg_slash)
    *this += '/';

  for(size_t i=0; i<new_parts.size(); ++i)
  {
    *this += new_parts[i];
    if(i != new_parts.size()-1)
      *this += '/';
  }

  if (end_slash)
    *this += '/';

  return *this; 
}
//-----------------------------------------------------------------------------
String& String::append(wchar_t ch, int count)
{
  acquireData();

  for(int i=0; i<count; ++i)
    mString->push_back(ch);
  return *this;
}
//-----------------------------------------------------------------------------
String& String::append(const String& other)
{
  acquireData();

  for(int i=0; i<other.length(); ++i)
    mString->push_back(other[i]);
  return *this;
}
//-----------------------------------------------------------------------------
String& String::prepend(const String& str)
{
  return insert(0, str);
}
//-----------------------------------------------------------------------------
String& String::prepend(wchar_t ch, int count)
{
  return insert(0, ch, count);
}
//-----------------------------------------------------------------------------
String& String::replace( int start, int count, const String& str )
{
  remove(start, count);
  insert(start, str);
  return *this;
}
//-----------------------------------------------------------------------------
String& String::replace( const String& oldstr, const String& newstr, bool case_sensitive )
{
  acquireData();
  String supstr = case_sensitive ? *this  : toLowerCase();
  String substr = case_sensitive ? oldstr : oldstr.toLowerCase();

  std::vector<int> positions;
  for( int pos = 0; (pos=supstr.find(substr,pos)) != -1; pos += substr.length() )
    positions.push_back(pos);

  // replace backwards to support new/old string of different sizes
  for(unsigned i=positions.size(); i--; )
    replace(positions[i], oldstr.length(), newstr);

  return *this;
}
//-----------------------------------------------------------------------------
String& String::replace( int start, int count, wchar_t ch )
{
  acquireData();

  if (start < 0 )
    start = 0;
  if (count < 0)
    count = length();
  int end = start + count;
  if (end > length())
    end = length();
  for(int i=start; i<end; ++i)
    (*mString)[i] = ch;
  return *this;
}
//-----------------------------------------------------------------------------
String& String::replace( wchar_t old_ch, wchar_t new_ch )
{
  acquireData();

  for(int i=0; i<length(); ++i)
    if ((*mString)[i] == old_ch)
      (*mString)[i] = new_ch;
  return *this;
}
//-----------------------------------------------------------------------------
int String::count(wchar_t ch, int start) const
{
  // createData();
  if (empty())
    return 0;

  int num = 0;
  for(int i=start; i<length(); ++i)
    if ((*mString)[i] == ch)
      ++num;
  return num;
}
//-----------------------------------------------------------------------------
int String::count(const String& str, int start) const
{
  // createData();
  if (empty())
    return 0;

  int found = 0;
  for( int pos = find(str, start); pos != -1; start=pos+str.length(), pos=find(str, start))
    ++found;
  return found;
}
//-----------------------------------------------------------------------------
int String::compare(const String& other) const
{
  createData();

  int min = length() < other.length() ? length() : other.length();
  for(int i=0; i<min; ++i)
  {
    if ( (*mString)[i] != (*other.mString)[i] )
      return (int)(*mString)[i] - (int)(*other.mString)[i];
  }
  // their common subsection is equal, shortest goes first
  return length() - other.length();
}
//-----------------------------------------------------------------------------
bool String::endsWith(const String& str) const
{
  //createData();
  if (empty())
    return false;

  if (length() < str.length() || empty() || str.empty() )
    return false;
  else
  {
    int offset = length() - str.length();
    return memcmp( &(*mString)[0] + offset, &(*str.mString)[0], sizeof((*mString)[0])*str.length() ) == 0;
  }
}
//-----------------------------------------------------------------------------
bool String::startsWith(const String& str) const
{
  //createData();
  if (str.empty())
    return true;

  if (empty())
    return false;

  if (length() < str.length() || empty() || str.empty() )
    return false;
  else
  {
    return memcmp( &(*mString)[0], &(*str.mString)[0], sizeof((*mString)[0])*str.length() ) == 0;
  }
}
//-----------------------------------------------------------------------------
bool String::endsWith(wchar_t ch) const
{
  //createData();
  if (empty())
    return false;

  return length() > 0 && (*mString)[length()-1] == ch;
}
//-----------------------------------------------------------------------------
bool String::startsWith(wchar_t ch) const
{
  //createData();
  if (empty())
    return false;

  return length() > 0 && (*mString)[0] == ch;
}
//-----------------------------------------------------------------------------
String String::toLowerCase() const
{
  //createData();
  if (empty())
    return String();

  String lower = *this;
  lower.acquireData();
  for(int i=0; i<length(); ++i)
    (*lower.mString)[i] = getLowerCase( (*lower.mString)[i] );
  return lower;
}
//-----------------------------------------------------------------------------
String String::toUpperCase() const
{
  //createData();
  if (empty())
    return String();

  String lower = *this;
  lower.acquireData();
  for(int i=0; i<length(); ++i)
    (*lower.mString)[i] = getUpperCase( (*lower.mString)[i] );
  return lower;
}
//-----------------------------------------------------------------------------
String& String::insert(int pos, const String& str)
{
  if (str.empty())
    return *this;

  acquireData();

  if (pos > length())
    return append(str);
  int remaining = length() - pos;
  mString->resize( mString->length() + str.length() );
  memmove( &(*mString)[0]+pos+str.length(), &(*mString)[0]+pos, sizeof(str[0])*remaining );
  memcpy( &(*mString)[0]+pos, &(*str.mString)[0], sizeof(str[0])*str.length() );
  return *this;
}
//-----------------------------------------------------------------------------
String& String::insert(int pos, wchar_t ch, int count)
{
  if (count == 0)
    return *this;

  acquireData();

  if (pos >= length())
    return append(ch, count);
  int remaining = length() - pos;
  mString->resize( mString->length() + count );
  memmove( &(*mString)[0]+pos+count, &(*mString)[0]+pos, sizeof((*mString)[0])*remaining );
  for(int i=0; i<count && i+pos<length(); ++i)
    (*mString)[i+pos] = ch;
  return *this;
}
//-----------------------------------------------------------------------------
String String::left(int count) const
{
  if (count<0)
    return substring(0, length()+count);
  else
    return substring(0, count);
}
//-----------------------------------------------------------------------------
String String::right(int count) const
{
  if (count<0)
    return substring(-count, length()+count);
  else
    return substring(length()-count, count);
}
//-----------------------------------------------------------------------------
String String::extractPath() const
{
  //createData();
  if (empty())
    return String();

  String path = *this;
  path.normalizeSlashes();
  int slash_pos = path.findBackwards('/');
  if (slash_pos<0)
    return String();
  else
    return path.substring(0,slash_pos+1);
}
//-----------------------------------------------------------------------------
String String::extractFileName() const
{
  //createData();
  if (empty())
    return String();

  int a = findBackwards('/');
  int b = findBackwards('\\');
  int slash_pos = a > b ? a : b;
  return substring(slash_pos+1);
}
//-----------------------------------------------------------------------------
String String::extractFileExtension(bool require_dot) const
{
  //createData();
  if (empty())
    return String();

  int dot_pos = findBackwards('.');
  if (require_dot && dot_pos == -1)
    return String();
  else
    return substring(dot_pos+1);
}
//-----------------------------------------------------------------------------
String String::fromStdWString(const std::wstring& str)
{
  String s;
  s.acquireData();

  s.mString->clear();
  for(int i=0; i<(int)str.length(); ++i)
    s.mString->push_back( str[i] );
  return s;
}
//-----------------------------------------------------------------------------
String String::fromStdString(const std::string& str, bool utf8)
{
  if (utf8)
    return fromUTF8( str.c_str(), str.length());
  else
    return fromAscii( str.c_str() );
}
//-----------------------------------------------------------------------------
String String::fromAscii(const char* str, int size)
{
  String s;
  s.acquireData();

  if (size<0)
    size = (int)strlen(str);
  const unsigned char* ascii = (const unsigned char*)str;
  s.mString->clear();
  for(int i=0; i<size; ++i)
  {
    if( ascii[i] < 128 )
      s.mString->push_back( ascii[i] );
    else

      s.mString->push_back( L'?' );
  }
  /*s.mString->push_back(0);*/
  return s;
}
//-----------------------------------------------------------------------------
String String::fromUTF16BE(const unsigned short* str, int byte_count)
{
  String s;
  s.acquireData();

  VL_COMPILE_TIME_CHECK( sizeof(unsigned short) == 2 )
  int character_count = byte_count < 0 ? -1 : byte_count / 2;

  // detect character_count
  if (character_count<0)
    for(character_count=0; str[character_count]; ) ++character_count;

  // skip header
  if (str[0] == 65534)
  {
    str++;
    --character_count;
  }

  s.mString->clear();
  for(int i=0; i<character_count; ++i)
  {
    const unsigned char* bytes = (const unsigned char*)(str+i);
    unsigned int code = bytes[1] + (bytes[0]<<8);
    // skip surrogate pair
    if (code>=0xD800 && code <=0xDC00)
    {
      s.mString->push_back( '?' );
      ++i;
    }
    else
      s.mString->push_back( (wchar_t)code );
  }
  return s;
}
//-----------------------------------------------------------------------------
String String::fromUTF16LE(const unsigned short* str, int byte_count)
{
  String s;
  s.acquireData();

  VL_COMPILE_TIME_CHECK( sizeof(unsigned short) == 2 )
  int character_count = byte_count < 0 ? -1 : byte_count / 2;

  // detect character_count
  if (character_count<0)
    for(character_count=0; str[character_count]; ) ++character_count;

  // skip header
  if (str[0] == 65279)
  {
    str++;
    --character_count;
  }

  s.mString->clear();
  for(int i=0; i<character_count; ++i)
  {
    unsigned char* bytes = (unsigned char*)(str+i);
    unsigned int code = bytes[0] + (bytes[1]<<8);
    // skip surrogate pair
    if (code>=0xD800 && code <=0xDC00)
    {
      s.mString->push_back( '?' );
      ++i;
    }
    else
      s.mString->push_back( (wchar_t)code );
  }
  return s;
}
//-----------------------------------------------------------------------------
String String::fromUTF16(const unsigned short* str, int byte_count)
{
  String s;
  s.acquireData();

  if (str[0] == 65279)
    s = fromUTF16LE(str, byte_count);
  else
  if (str[0] == 65534)
    s = fromUTF16BE(str, byte_count);
  else
  {
    Log::error("String::fromUTF16(): not UTF16 BE nor LE found.\n");
    s.clear();
  }
  return s;
}
//-----------------------------------------------------------------------------
String String::fromUTF8(const char* str, int byte_count)
{
  String s;
  s.acquireData();

  unsigned char* utf8 = (unsigned char*)str;
  int start=0;
  // skip header EF BB BF if present
  if ( utf8[0] == 0xEF && utf8[1] == 0xBB && utf8[2] == 0xBF )
    start=3;
  // detect size
  if (byte_count<0)
    for(byte_count=0; utf8[byte_count]; ) ++byte_count;

  s.mString->clear();
  const int UTF8_1BYTE = 128;
  const int UTF8_2BYTE = 128+64;
  const int UTF8_3BYTE = 128+64+32;
  const int UTF8_4BYTE = 128+64+32+16;

  for( int i=start; i<byte_count; ++i )
  {
    // Unicode            Byte1     Byte2     Byte3     Byte4
    // U+000000-U+00007F  0xxxxxxx
    // U+000080-U+0007FF  110xxxxx  10xxxxxx
    // U+000800-U+00FFFF  1110xxxx  10xxxxxx  10xxxxxx
    // U+010000-U+10FFFF  11110xxx  10xxxxxx  10xxxxxx  10xxxxxx

    unsigned int unicode_code_point = 0;
    if (utf8[i] < UTF8_1BYTE)
      unicode_code_point = utf8[i];
    else
    if ( (utf8[i] & UTF8_3BYTE) == UTF8_2BYTE )
    {
      unicode_code_point = ((utf8[i]-UTF8_2BYTE)<<6) + (utf8[i+1]&0x3f);
      i+=1;
    }
    else
    if ( (utf8[i] & UTF8_4BYTE) == UTF8_3BYTE )
    {
      unicode_code_point = ((utf8[i]-UTF8_3BYTE)<<12) + ((utf8[i+1]&0x3f)<<6) + (utf8[i+2]&0x3f);
      i+=2;
    }
    else
    {
      unicode_code_point = ((utf8[i]-UTF8_4BYTE)<<18) + ((utf8[i+1]&0x3f)<<12) + ((utf8[i+2]&0x3f)<<6) + (utf8[i+3]&0x3f);
      i+=3;
    }

    if (unicode_code_point <= 0xFFFF)
      s.mString->push_back((wchar_t)unicode_code_point);
    else // characters outside the BMP
      s.mString->push_back(L'?');
  }
  return s;
}
//-----------------------------------------------------------------------------
String String::fromLatin1(const char* str, int character_count)
{
  String s;
  s.acquireData();

  unsigned char* latin1 = (unsigned char*)str;
  if (character_count<0)
    for(character_count=0; latin1[character_count]; ) ++character_count;

  s.mString->clear();
  for(int i=0; i<character_count; ++i)
    s.mString->push_back( latin1_to_unicode[ latin1[i] ] );
  return s;
}
//-----------------------------------------------------------------------------
String String::fromPointer(const void* value)
{
  char buffer[32];
  memset(buffer, 0, sizeof(buffer));
  sprintf(buffer, "%p", value);
  return fromAscii(buffer);
}
//-----------------------------------------------------------------------------
String String::fromInt(int value)
{
  char buffer[256];
  memset(buffer, 0, sizeof(buffer));
  sprintf(buffer, "%d", value);
  return fromAscii(buffer);
}
//-----------------------------------------------------------------------------
String String::fromUInt(unsigned int value)
{
  char buffer[256];
  memset(buffer, 0, sizeof(buffer));
  sprintf(buffer, "%u", value);
  return fromAscii(buffer);
}
//-----------------------------------------------------------------------------
String String::fromLongLong(long long value)
{
  char buffer[256];
  memset(buffer, 0, sizeof(buffer));
  sprintf(buffer, "%lld", value);
  return fromAscii(buffer);
}
//-----------------------------------------------------------------------------
String String::fromULongLong(unsigned long long value)
{
  char buffer[256];
  memset(buffer, 0, sizeof(buffer));
  sprintf(buffer, "%llu", value);
  return fromAscii(buffer);
}
//-----------------------------------------------------------------------------
String String::fromDouble(double value, int decimals)
{
  char buffer[256];
  memset(buffer, 0, sizeof(buffer));
  switch(decimals)
  {
    case 0: sprintf(buffer, "%.0lf", value); break;
    case 1: sprintf(buffer, "%.1lf", value); break;
    case 2: sprintf(buffer, "%.2lf", value); break;
    case 3: sprintf(buffer, "%.3lf", value); break;
    case 4: sprintf(buffer, "%.4lf", value); break;
    case 5: sprintf(buffer, "%.5lf", value); break;
    case 6: sprintf(buffer, "%.6lf", value); break;
    case 7: sprintf(buffer, "%.7lf", value); break;
    case 8: sprintf(buffer, "%.8lf", value); break;
    case 9: sprintf(buffer, "%.9lf", value); break;
    case 10: sprintf(buffer, "%.10lf", value); break;
    case 11: sprintf(buffer, "%.11lf", value); break;
    case 12: sprintf(buffer, "%.12lf", value); break;
    case 13: sprintf(buffer, "%.13lf", value); break;
    case 14: sprintf(buffer, "%.14lf", value); break;
    case 15: sprintf(buffer, "%.15lf", value); break;
    case 16: sprintf(buffer, "%.16lf", value); break;
    case 17: sprintf(buffer, "%.17lf", value); break;
    case 18: sprintf(buffer, "%.18lf", value); break;
    case 19: sprintf(buffer, "%.19lf", value); break;
    case 20: sprintf(buffer, "%.20lf", value); break;
    default: sprintf(buffer, "%.6lf", value); break;
  }
  return fromAscii(buffer);
}
//-----------------------------------------------------------------------------
std::wstring String::toStdWString() const
{
  //createData();
  if (empty())
    return std::wstring();

  std::wstring ws;
  for(int i=0; i<length(); ++i)
    ws += (*mString)[i];
  return ws;
}
//-----------------------------------------------------------------------------
std::string String::toStdString() const
{
  //createData();
  if (empty())
    return std::string();
  std::string std_string;

  std::vector<unsigned char> utf8;
  toUTF8(utf8, false);
  if (utf8.size()>1)
  {
    std_string.resize(utf8.size()-1);
    memcpy(&std_string[0], &utf8[0], utf8.size()-1);
  }

  return std_string;
}
//-----------------------------------------------------------------------------
void String::toAscii(std::string& ascii, bool translate_non_ascii_chars) const
{
  //createData();
  if (empty())
  {
    ascii.clear();
    return;
  }

  ascii.clear();
  if (mString->length())
  {
    for(int i=0; i<(int)mString->length() && (*mString)[i]; ++i)
    {
      if ( (*mString)[i] < 128 || !translate_non_ascii_chars )
        ascii += (char)((*mString)[i] & 0xFF);
      else
      {
        const char* translation = unicode_to_ascii( (*mString)[i] );
        if (translation)
        {
          for(int j=0; translation[j]; ++j)
            ascii += translation[j];
        }
        else
          ascii += '?';
      }
    }
  }
  // no need to add the 0 terminator
}
//-----------------------------------------------------------------------------
void String::toUTF8(std::string& str, bool include_utf8_signature) const
{
  std::vector<unsigned char> utf8;
  toUTF8(utf8, include_utf8_signature);
  str.clear();
  if (utf8.size())
  {
    for(int i=0; utf8[i]; ++i)
      str.push_back(utf8[i]);
  }
}
//-----------------------------------------------------------------------------
void String::toUTF8(std::vector<unsigned char>& utf8, bool include_utf8_signature) const
{
  utf8.clear();
  if(include_utf8_signature)
  {
    utf8.push_back(0xEF);
    utf8.push_back(0xBB);
    utf8.push_back(0xBF);
  }

  //createData();
  if (empty())
  {
    utf8.push_back(0);
    return;
  }

  // Unicode            Byte1     Byte2     Byte3     Byte4
  // U+000000-U+00007F  0xxxxxxx
  // U+000080-U+0007FF  110xxxxx  10xxxxxx
  // U+000800-U+00FFFF  1110xxxx  10xxxxxx  10xxxxxx
  // U+010000-U+10FFFF  11110xxx  10xxxxxx  10xxxxxx  10xxxxxx
  for(int i=0; i<length(); ++i)
  {
    if ( (*mString)[i] < 0x80)
      utf8.push_back( (unsigned char)(*mString)[i] );
    else
    if ( (*mString)[i] < 0x800)
    {
      int a = 0xC0 | ((*mString)[i]>>6);
      int b = 0x80 | ((*mString)[i]&0x3F);
      utf8.push_back( (unsigned char)a );
      utf8.push_back( (unsigned char)b );
    }
    else
    {
      int a = 0xE0 | ((*mString)[i]>>12);
      int b = 0x80 | (((*mString)[i]>>6)&0x3F);
      int c = 0x80 | ((*mString)[i]&0x3F);
      utf8.push_back( (unsigned char)a );
      utf8.push_back( (unsigned char)b );
      utf8.push_back( (unsigned char)c );
    }
  }

  utf8.push_back(0);
}
//-----------------------------------------------------------------------------
void String::toUTF16BE(std::vector<unsigned char>& utf16, bool include_utf16be_signature) const
{
  utf16.clear();
  if (include_utf16be_signature)
  {
    utf16.push_back(0xFE);
    utf16.push_back(0xFF);
  }

  //createData();
  if (empty())
  {
    utf16.push_back(0);
    return;
  }

  for(int i=0; i<length(); ++i)
  {
    int x = ((*mString)[i]>>8) & 0xFF;
    int y = (*mString)[i]      & 0xFF;
    utf16.push_back( (unsigned char)x );
    utf16.push_back( (unsigned char)y );
  }
  utf16.push_back(0);
}
//-----------------------------------------------------------------------------
void String::toUTF16LE(std::vector<unsigned char>& utf16, bool include_utf16le_signature) const
{
  utf16.clear();
  if (include_utf16le_signature)
  {
    utf16.push_back(0xFF);
    utf16.push_back(0xFE);
  }

  //createData();
  if (empty())
  {
    utf16.push_back(0);
    return;
  }

  for(int i=0; i<length(); ++i)
  {
    int x = (*mString)[i] & 0xFF;
    int y = ((*mString)[i]>>8) & 0xFF;
    utf16.push_back( (unsigned char)x );
    utf16.push_back( (unsigned char)y );
  }
  utf16.push_back(0);
}
//-----------------------------------------------------------------------------
void String::toLatin1(std::vector<unsigned char>& latin1) const
{
  latin1.clear();

  //createData();
  if (empty())
  {
    latin1.push_back(0);
    return;
  }

  for(int i=0; i<length(); ++i)
  {
    if ((*mString)[i] < 128)
      latin1.push_back((unsigned char)(*mString)[i]);
    else
    {
      // search candidate
      int j=128;
      for(; latin1_to_unicode[j]; ++j)
      {
        if ( latin1_to_unicode[j] == (*mString)[i] )
        {
          latin1.push_back((unsigned char)j);
          break;
        }
      }
      if (j==256)
        latin1.push_back('?');
    }
  }
  latin1.push_back(0);
}
//-----------------------------------------------------------------------------
int String::toInt(bool hex) const
{
  //createData();
  if (empty())
    return 0;

  if (hex)
  {
    int i=0;
    sscanf(toStdString().c_str(), "%x", &i);
    return i;
  }
  else
    return atoi( toStdString().c_str() );
}
//-----------------------------------------------------------------------------
double String::toDouble() const
{
  //createData();
  if (empty())
    return 0.0;

  return atof( toStdString().c_str() );
}
//-----------------------------------------------------------------------------
void String::filterStrings(std::vector<String>& strings, const String& filter)
{
  String match = filter;
  int filter_type = 0;
  bool filter_ok = filter.empty();

  if ( filter.startsWith('*') )
  {
    filter_type--; // -1
    match.remove(0, 1);
    filter_ok = true;
  }

  if ( filter.endsWith('*') )
  {
    filter_type++; // +1 or 0
    match.remove(match.length()-1, 1);
    filter_ok = true;
  }

  if ( !filter_ok )
  {
    Log::error( Say("unacceptable filter '%s'.\n") << filter );
    return;
  }

  if ( filter_type && filter.length() > 1 )
  {
    for( int i=(int)strings.size(); i--; )
      switch(filter_type)
      {
        case 0:  if(  strings[i].find(match) == -1 ) strings.erase( strings.begin() + i ); break;
        case -1: if( !strings[i].endsWith(match)   ) strings.erase( strings.begin() + i ); break;
        case +1: if( !strings[i].startsWith(match) ) strings.erase( strings.begin() + i ); break;
      }
  }
}
//-----------------------------------------------------------------------------
EStringEncoding String::detectEncoding(const void* str, int byte_count, EStringEncoding default_encoding)
{
  const unsigned char* h = (unsigned char*)str;
  // UTF32 LE -> FF FE 00 00
  // UTF32 BE -> 00 00 FE FF
  // UTF8     -> EF BB BF
  // UTF16 BE -> FE FF
  // UTF16 LE -> FF FE

  if (byte_count>4 && h[0] == 0xFF && h[1] == 0xFE && h[2] == 0    && h[3] == 0   ) return SE_UTF32_LE;
  if (byte_count>4 && h[0] == 0    && h[1] == 0    && h[2] == 0xFE && h[3] == 0xFF) return SE_UTF32_BE;
  if (byte_count>3 && h[0] == 0xEF && h[1] == 0xBB && h[2] == 0xBF                ) return SE_UTF8;
  if (byte_count>2 && h[0] == 0xFE && h[1] == 0xFF                                ) return SE_UTF16_BE;
  if (byte_count>2 && h[0] == 0xFF && h[1] == 0xFE                                ) return SE_UTF16_LE;
  return default_encoding;
}
//-----------------------------------------------------------------------------
unsigned short String::getUpperCase(unsigned short ch)
{
  for(int i=0; i<107; ++i)
  {
    if (ch >= case_table_start_min_max[i][1] && ch <= case_table_start_min_max[i][2])
    {
      int index = ch - case_table_start_min_max[i][1] + case_table_start_min_max[i][0];
      return case_table_upper_lower_title[index][0];
    }
  }
  return ch;
}
//-----------------------------------------------------------------------------
unsigned short String::getLowerCase(unsigned short ch)
{
  for(int i=0; i<107; ++i)
  {
    if (ch >= case_table_start_min_max[i][1] && ch <= case_table_start_min_max[i][2])
    {
      int index = ch - case_table_start_min_max[i][1] + case_table_start_min_max[i][0];
      return case_table_upper_lower_title[index][1];
    }
  }
  return ch;
}
//-----------------------------------------------------------------------------
unsigned short String::getTitleCase(unsigned short ch)
{
  for(int i=0; i<107; ++i)
  {
    if (ch >= case_table_start_min_max[i][1] && ch <= case_table_start_min_max[i][2])
    {
      int index = ch - case_table_start_min_max[i][1] + case_table_start_min_max[i][0];
      return case_table_upper_lower_title[index][2];
    }
  }
  return ch;
}
//-----------------------------------------------------------------------------
std::string String::trimStdString(const std::string& text)
{
  std::string trimmed;
  for(unsigned i=0; i<text.length(); ++i)
  {
    if(text[i] == ' '  ||
       text[i] == '\n' ||
       text[i] == '\t' ||
       text[i] == '\v' ||
       text[i] == '\b' ||
       text[i] == '\a' ||
       text[i] == '\f' ||
       text[i] == '\r' )
      continue;
    else
    {
      trimmed = text.c_str() + i;
      break;
    }
  }
  int i = (int)trimmed.length();
  while( i-- )
  {
    if(trimmed[i] == ' '  ||
       trimmed[i] == '\n' ||
       trimmed[i] == '\t' ||
       trimmed[i] == '\v' ||
       trimmed[i] == '\b' ||
       trimmed[i] == '\a' ||
       trimmed[i] == '\f' ||
       trimmed[i] == '\r' )
      continue;
    else
      break;
  }
  trimmed.resize( i+1 );
  return trimmed;
}
//-----------------------------------------------------------------------------
String String::printf(const char* fmt, ...)
{
  std::vector<char> buffer;
  buffer.resize(1024 + strlen(fmt));
  buffer[0] = 0;

  va_list ap;
  va_start(ap, fmt);
  vsnprintf(&buffer[0], buffer.size(), fmt, ap);
  va_end(ap);
  return &buffer[0];
}
//-----------------------------------------------------------------------------
