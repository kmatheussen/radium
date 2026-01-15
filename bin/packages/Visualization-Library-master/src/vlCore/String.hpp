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

#ifndef String_INCLUDE_ONCE
#define String_INCLUDE_ONCE

#include <vlCore/vlnamespace.hpp>
#include <vlCore/Object.hpp>
#include <vector>
#include <string.h>

#if defined(VL_PLATFORM_WINDOWS)
  #define VL_PLATFORM_DEFAULT_ENCODING SE_LATIN1
#else
  #define VL_PLATFORM_DEFAULT_ENCODING SE_UTF8
#endif

namespace vl
{
  class VirtualFile;

  /** The String class implements an advanced UTF16 (Unicode BMP) string manipulation engine.

  The String class features:
  - Lazy data copy on write that speeds up String manipulation, copy and comparison.
  - Encoding and decoding functions for UTF8, UTF16 BE, UTF16 LE, Latin1 and Ascii, with and without signature.
  - Interoperability to and from std::string, std::wstring, char* and wchar_t*.
  - Unicode to Ascii character translation when possible.
  - Extended character case manipulation for most UTF16/BMP codes.
  - Advanced string manipulation routines like line/field splitting, comparison, search, replace, trimming, matching, counting, filling etc.
  - Fast substring find function based on the Quick Find algorithm.
  - Loads text from a variety of formats and sources.
  */
  class VLCORE_EXPORT String
  {
  public:
    //! Detects the encoding
    static EStringEncoding detectEncoding(const void* str, int byte_count, EStringEncoding encoding = VL_PLATFORM_DEFAULT_ENCODING);
    
    //! Loads a String from the specified path.
    static String loadText(const String& path, EStringEncoding encoding = VL_PLATFORM_DEFAULT_ENCODING);
    
    //! Loads a String from the specified path.
    static String loadText(const char* path, EStringEncoding encoding = VL_PLATFORM_DEFAULT_ENCODING) { return loadText(String(path),encoding); }
    
    //! Loads a String from the specified VirtualFile.
    static String loadText(VirtualFile* file, EStringEncoding encoding = VL_PLATFORM_DEFAULT_ENCODING);
    
    //! Loads a String from the specified memory buffer.
    static String loadText(void* data, int bytes, EStringEncoding encoding = VL_PLATFORM_DEFAULT_ENCODING);
    
    //! Returns the upper-case version of the specified character.
    static unsigned short getUpperCase(unsigned short ch);
    
    //! Returns the lower-case version of the specified character.
    static unsigned short getLowerCase(unsigned short ch);
    
    //! Returns the title-case version of the specified character.
    static unsigned short getTitleCase(unsigned short ch);
    
    //! Filters the specified Strings using the given filter. The \p filter must be of the type \p "*abc", \p "abc*" and \p "*abc*".
    static void filterStrings(std::vector<String>& strings, const String& filter);
    
    //! Returns '\' under windows and '/' under Linux and Mac.
    static wchar_t platformSlash()
    {
      #if defined(VL_PLATFORM_WINDOWS)
        return '\\';
      #else
        return '/';
      #endif
    }

    //! Remove the spaces before and after an std::string
    static std::string trimStdString(const std::string& text);

    //! Constructor.
    String();
    
    //! Constructor.
    String(const String& other);
    
    //! Constructor.
    String(const wchar_t* wstr);
    
    //! Constructor.
    String(const char* str);
    
    //! Constructor.
    explicit String(wchar_t ch, int count=1);

    //! Returns the 0-terminated utf16 string
    const wchar_t* ptr() const { createData(); return &(*mString)[0]; }

    //! Returns the 0-terminated utf16 string
    wchar_t* ptr() { acquireData(); return &(*mString)[0]; }

    //! Returns the length of the string
    int length() const { if (!mString) return 0; createData(); return (int)mString->length(); };

    //! Returns \p true if length() == 0
    bool empty() const { return length() == 0; }

    //! Returns \p true if the String has never been assigned or resized.
    bool null() const { return !mString; }

    //! Clears the string
    String& clear() { acquireData(); mString->clear(); return *this; }

    //! Returns the character at position \p i
    const wchar_t& operator[](int i) const { createData(); return (*mString)[i]; }

    //! Returns the character at position \p i
    wchar_t& operator[](int i) { acquireData(); return (*mString)[i]; }

    //! Replaces the character 'old_ch' with 'new_ch'
    String& replace( wchar_t old_ch, wchar_t new_ch );

    //! Replaces \p count characters starting at position \p start with \p ch
    String& replace( int start, int count, wchar_t ch );

    //! Equivalent to remove(start, count); insert(start, str);
    String& replace( int start, int count, const String& str );

    //! Replaces all the occurrences of oldstr with newstr
    String& replace( const String& oldstr, const String& newstr, bool case_sensitive=true );

    //! Removes 'count' occurrences of the character 'ch' after position 'start'.
    //! If 'count' is set -1 there is no limit to the number of occurrences removed.
    String& remove(wchar_t ch, int start=0, int count=-1);

    //! Removes 'count' occurrences of the string 'str' after position 'start'.
    //! If 'count' is set -1 there is no limit to the number of occurrences removed.
    String& remove(const String& str, int start=0, int count=-1);

    //! Removes 'count' characters starting at position 'start'.
    String& remove( int start, int count );

    //! Reverses the order of the characters in the string.
    String& reverse();

    //! Transform \ slashes in / slashes and removes duplicates.
    String& normalizeSlashes();

    //! Returns the number of occurrences of the given character after position 'start'
    int count(wchar_t ch, int start=0) const;

    //! Returns the number of occurrences of the given string after position 'start'
    int count(const String& str, int start=0) const;

    //! Performs a lexicographic comparison.
    //! Returns < 0 if this string comes before 'other'.
    //! Returns > 0 if 'other' comes before this string.
    //! Returns 0 if the two strings are equal.
    int compare(const String& other) const;

    //! Returns \p true if a String ends with the specified String \p str.
    bool endsWith(const String& str) const;

    //! Returns \p true if a String starts with the specified String \p str.
    bool startsWith(const String& str) const;

    //! Returns \p true if a String ends with the specified character.
    bool endsWith(wchar_t ch) const;

    //! Returns \p true if a String starts with the specified character.
    bool startsWith(wchar_t ch) const;

    //! Inserts \p str at position \p pos
    String& insert(int pos, const String& str);

    //! Inserts \p count characters \p ch starting at position \p pos
    String& insert(int pos, wchar_t ch, int count=1);

    //! Returns the \p count leftmost caracters of a String. If \p count is negative the function returns all but the abs(count) rightmost caracters.
    String left(int count) const;

    //! Returns the \p count rightmost caracters of a String. If \p count is negative the function returns all but the abs(count) leftmost caracters.
    String right(int count) const;

    //! If the String contains a file path the function returns the path with trailing slash, without the file name.
    String extractPath() const;

    //! If the String contains a file path the function returns the file name without the path.
    String extractFileName() const;

    //! If the String contains a file name or file path the function returns the extension of the file.
    String extractFileExtension(bool require_dot=true) const;

    //! Resizes the string to the specified character count.
    String& resize(int character_count);

    //! Returns the \p count characters long substring starting a character index \p start. If \p count == -1 the returned substring will contain all the characters from \p start to the end of the String.
    String substring(int start, int count=-1) const;

    //! Fills the string with the specified character \p ch.
    String& fill(wchar_t ch);

    //! Removes the specified character \p ch from the beginning and the end of the String.
    String& trim(wchar_t ch);

    //! Removes the characters contained in \p chars from the beginning and the end of the String.
    String& trim(const String& chars);

    //! Equivalent to \p trim("\n\r\t\v "), that is, removes all the tabs, spaces and new-lines from the beginning and end of a String.
    String& trim();

    //! Splits a String into a set of fields. The fields are separated by the specified \p separator and are returned in \p fields.
    void split(wchar_t separator, std::vector<String>& fields, bool remove_empty_fields=false) const;

    //! Splits a String into a set of fields. The fields are separated by the specified \p separator_list and are returned in \p fields.
    void split(const String& separator_list, std::vector<String>& fields, bool remove_empty_fields=false) const;

    //! Splits a String into a set of Strings each of which contains a line.
    void splitLines(std::vector<String>& lines) const;

    //! Splits a String into a set of fields based on the specified \p separator and returns the filed at position \p field_index.
    String field(wchar_t separator, int field_index) const;

    //! Appends the specified String to another String.
    String& append(const String& other);

    //! Appends \p count characters \p ch to another String.
    String& append(wchar_t ch, int count=1);

    //! Prepends the specified String to another String.
    String& prepend(const String& str);

    //! Prepends \p count characters \p ch to another String.
    String& prepend(wchar_t ch, int count);

    //! Searches for the specified character \p ch starting at position \p start and returns the index of the first occurrence or -1 if no occurrence was found.
    int find(wchar_t ch, int start=0) const;

    //! Searches for the specified string \p substr starting at position \p start and returns the index of the first occurrence or -1 if no occurrence was found.
    int find(const String& substr, int start=0) const;

    //! Searches for the specified string \p substr starting at position \p start and returns the index of the first occurrence or -1 if no occurrence was found.
    //! This function can be substantially quicker that find() when searching in large texts but it can be slower than find() for small ones.
    int findInLargeText(const String& substr, int start=0) const;

    //! Searches for the specified character \a chstarting from the end of the string and returns the index of the first occurrence or -1 if no occurrence was found.
    int findBackwards(wchar_t ch) const;

    //! Searches for the specified String \a chstr from the end of the string and returns the index of the first occurrence or -1 if no occurrence was found.
    int findBackwards(const String& str) const;

    //! Returns true if a String contains the specified character.
    bool contains(wchar_t ch) const;

    //! Returns true if a String contains the specified String
    bool contains(const String& str) const;

    //! Minimizes the memory buffer used to store the String.
    void squeeze();

    // transcoding routines

    //! Creates a string representing the given pointer
    static String fromPointer(const void* value);

    //! Creates a string representing the given integer value
    static String fromInt(int value);

    //! Creates a string representing the given unsigned integer value
    static String fromUInt(unsigned int value);

    //! Creates a string representing the given long long value
    static String fromLongLong(long long value);

    //! Creates a string representing the given unsigned long long value
    static String fromULongLong(unsigned long long value);

    //! Creates a string representing the given double value
    //! The value of 'decimals' can be between 0 and 20.
    static String fromDouble(double value, int decimals=6);

    //! Initializes the string from a std::string using fromUTF() if \a utf8 == true (default) otherwise uses fromAscii().
    static String fromStdString(const std::string& str, bool utf8=true);

    //! Initializes the string from a std::string using the fromAscii() function.
    static String fromStdWString(const std::wstring& str);

    //! Initializes the string from a 7 bit ascii string
    static String fromAscii(const char* str, int size=-1);

    //! Accepts strings with and without UTF16 BE signature
    //! Supports natively the characters from the BMP, the other characters are substituted with '?'
    //! The size of the buffer pointed by 'str' must be at least 'byte_count' bytes large or null terminated.
    static String fromUTF16BE(const unsigned short* str, int byte_count=-1);

    //! Accepts strings with and without UTF16 LE signature
    //! Supports natively the characters from the BMP, the other characters are substituted with '?'
    //! The size of the buffer pointed by 'str' must be at least 'byte_count' bytes large or null terminated.
    static String fromUTF16LE(const unsigned short* str, int byte_count=-1);

    //! str must have UTF16 LE or UTF16 BE signature
    //! The size of the buffer pointed by 'str' must be at least 'byte_count' bytes large or null terminated.
    static String fromUTF16(const unsigned short* str, int byte_count=-1);

    //! Accepts strings with and without UTF8 signature
    static String fromUTF8(const char* str, int byte_count=-1);

    //! The size of the buffer pointed by 'str' must be at least 'character_count' bytes large or null terminated.
    static String fromLatin1(const char* str, int character_count=-1);

    //! Returns the int number represented by the string. The conversion is done using the standard atoi() function.
    int toInt(bool hex=false) const;

    //! Returns the double number represented by the string. The conversion is done using the standard atof() function.
    double toDouble() const;

    //! Returns the float number represented by the string. The conversion is done using the standard atof() function.
    float toFloat() const { return (float)toDouble(); }

    //! Returns a formatted string using the legacy printf syntax. The resulting string can be maximum 1024 + strlen(fmt) characters long.
    static String printf(const char* fmt, ...);

    //! Returns a UTF8 encoded std::string.
    std::string toStdString() const;

    //! Returns the std::wstring representation of a String.
    std::wstring toStdWString() const;

    //! Provides some basic character translation of code points outside of the ASCII range.
    void toAscii(std::string& ascii, bool translate_non_ascii_chars=true) const;

    //! Encodes the String into a UTF8 encoded string.
    void toUTF8(std::vector<unsigned char>& utf8, bool include_utf8_signature=true) const;

    //! Encodes the String into a UTF8 encoded std::string.
    void toUTF8(std::string& utf8, bool include_utf8_signature=true) const;

    //! Encodes the String into a UTF16 big endian encoded string.
    void toUTF16BE(std::vector<unsigned char>& utf16, bool include_utf16be_signature=true) const;

    //! Encodes the String into a UTF16 little endian encoded string.
    void toUTF16LE(std::vector<unsigned char>& utf16, bool include_utf16le_signature=true) const;

    //! Encodes the String into a Latin1 encoded string.
    void toLatin1(std::vector<unsigned char>& latin1) const;

    //! Returns the lower-case version of a String
    String toLowerCase() const;

    //! Returns the upper-case version of a String
    String toUpperCase() const;

    //! Lexicographic sorting, equivalent to \p "compare(other) < 0"
    bool operator<(const String& other) const
    {
      return compare(other) < 0;
    }

    //! Equivalent to '*this = fromUTF8(str);'
    String& operator=(const char* str)
    {
      *this = fromUTF8(str);
      return *this;
    }

    //! Equivalent to '*this = fromUTF8(str.c_str());'
    String& operator=(const std::string& str)
    {
      *this = fromUTF8(str.c_str());
      return *this;
    }

    String& operator=(const wchar_t* wstr)
    {
      acquireData();

      mString->clear();
      if (wstr)
      {
        for(int i=0; wstr[i]; ++i)
          mString->push_back(wstr[i]);
      }
      return *this;
    }

    String& operator=(const std::wstring& str)
    {
      return operator=(str.c_str());
    }

#if VL_STRING_COPY_ON_WRITE == 0
    String& operator=(const String& other)
    {
      other.acquireData();
      mString = new StringData(*other.mString);
      return *this;
    }
#endif

    bool operator==(const String& other) const
    {
      if ( empty() && other.empty() )
        return true;
      if ( empty() && !other.empty() )
        return false;
      if ( !empty() && other.empty() )
        return false;

      createData();

      if (mString == other.mString)
        return true;
      else
      if ( other.length() == length() )
      {
        return memcmp( ptr(), other.ptr(), sizeof(wchar_t)*length() ) == 0;
      }
      else
        return false;
    }

    bool operator==(const std::string& other) const
    {
      createData();

      if ( (int)other.length() == length() )
      {
        for(int i=0; i<length(); ++i)
          if ((*mString)[i] != (wchar_t)other[i])
            return false;
        return true;
      }
      else
        return false;
    }

    bool operator==(const std::wstring& other) const
    {
      createData();

      if ( (int)other.length() == length() )
      {
        for(int i=0; i<length(); ++i)
          if ((*mString)[i] != other[i])
            return false;
        return true;
      }
      else
        return false;
    }

    bool operator==(const char* other) const
    {
      createData();

      int i=0;
      for(; other[i] && i<length(); ++i)
        if ( (*mString)[i] != (wchar_t)other[i] )
          return false;
      return i == length() && other[i] == 0;
    }

    bool operator==(const wchar_t* other) const
    {
      createData();

      int i=0;
      for(; other[i] && i<length(); ++i)
        if ( (*mString)[i] != other[i] )
          return false;
      return i == length() && other[i] == 0;
    }

    bool operator!=(const String& other) const
    {
      return !this->operator==(other);
    }

    bool operator!=(const std::string& other) const
    {
      return !this->operator==(other);
    }

    bool operator!=(const std::wstring& other) const
    {
      return !this->operator==(other);
    }

    bool operator!=(const char* other) const
    {
      return !this->operator==(other);
    }

    bool operator!=(const wchar_t* other) const
    {
      return !this->operator==(other);
    }

    String& operator+=(wchar_t ch)
    {
      acquireData();
      mString->push_back(ch);
      return *this;
    }

    String operator+(wchar_t ch) const
    {
      String tmp = *this;
      tmp += ch;
      return tmp;
    }

    String& operator+=(const String& other)
    {
      return append(other);
    }

    String operator+(const String& other) const
    {
      String tmp = *this;
      tmp.append(other);
      return tmp;
    }

    //! Acquires a private copy of the data if the string has been copied from another one.
    void acquireData() const
    {
      createData();

      if (mString->referenceCount() > 1)
        mString = new StringData(*mString);
    }

  protected:
    void createData() const { if (!mString) mString = new StringData; }

  private:
    class StringData: public Object
    {
    public:
      void clear() { mWString.clear(); }
      void push_back(wchar_t a) { mWString.push_back(a); }
      const wchar_t& operator[](int i) const { return mWString[i]; }
      wchar_t& operator[](int i) { return mWString[i]; }
      int length() const { return (int)mWString.length(); }
      void resize(int size) { mWString.resize(size); }
      void squeeze()
      {
        std::wstring new_string = mWString;
        mWString.swap( new_string );
      }
    protected:
      std::wstring mWString;
    };

    mutable ref<StringData> mString;
  };
//-----------------------------------------------------------------------------
  inline String operator+(const wchar_t* pstr, const String& str)
  {
    return String(pstr) + str;
  }
//-----------------------------------------------------------------------------
  inline String operator+(const char* pstr, const String& str)
  {
    return String(pstr) + str;
  }
//-----------------------------------------------------------------------------
  inline String operator+(wchar_t ch, const String& str)
  {
    return String(ch,1) + str;
  }
//-----------------------------------------------------------------------------
  inline String operator+(char ch, const String& str)
  {
    return String(ch,1) + str;
  }
//-----------------------------------------------------------------------------
}

#endif
