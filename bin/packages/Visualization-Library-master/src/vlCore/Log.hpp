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

#ifndef Log_INCLUDE_ONCE
#define Log_INCLUDE_ONCE

#include <vlCore/String.hpp>
#include <vlCore/IMutex.hpp>
#include <fstream>

namespace vl
{
  //------------------------------------------------------------------------------
  // Log
  //-----------------------------------------------------------------------------
  /** Utility class to generate logs. */
  class VLCORE_EXPORT Log: public Object
  {
    VL_INSTRUMENT_ABSTRACT_CLASS(vl::Log, Object)

  public:
    Log()
    {
      VL_DEBUG_SET_OBJECT_NAME()
      mLogLevel = LL_LogPrint; // only used by operator<<()
    }
    
    Log& operator<<( ELogLevel log_level ) { mLogLevel = log_level; return *this; }

    Log& operator<<( const String& str )
    { 
      switch(mLogLevel)
      {
      case LL_LogNotify: notify(str); break;
      case LL_LogPrint: print(str); break;
      case LL_LogBug: bug(str); break;
      case LL_LogError: error(str); break;
      case LL_LogWarning: warning(str); break;
      case LL_LogDebug: debug(str); break;
      }
      return *this;
    }

    Log& operator<<( const std::string& v )
    {
      *this << String::fromStdString(v);
      return *this;
    }

    Log& operator<<( const void* v )
    {
      *this << String::fromPointer(v);
      return *this;
    }

    Log& operator<<( const char* v )
    {
      *this << String(v);
      return *this;
    }

    Log& operator<<( char v )
    {
      *this << String(v);
      return *this;
    }

    Log& operator<<( unsigned char v )
    {
      *this << String::fromUInt(v);
      return *this;
    }

    Log& operator<<( short v )
    {
      *this << String::fromInt(v);
      return *this;
    }

    Log& operator<<( unsigned short v )
    {
      *this << String::fromUInt(v);
      return *this;
    }

    Log& operator<<( int v )
    {
      *this << String::fromInt(v);
      return *this;
    }

    Log& operator<<( unsigned int v )
    {
      *this << String::fromUInt(v);
      return *this;
    }

    Log& operator<<( long v )
    {
      *this << String::fromLongLong(v);
      return *this;
    }

    Log& operator<<( unsigned long v )
    {
      *this << String::fromULongLong(v);
      return *this;
    }

    Log& operator<<( long long v )
    {
      *this << String::fromLongLong(v);
      return *this;
    }

    Log& operator<<( unsigned long long v )
    {
      *this << String::fromULongLong(v);
      return *this;
    }

    Log& operator<<( double v )
    {
      *this << String::fromDouble(v);
      return *this;
    }

    Log& operator<<( float v )
    {
      *this << String::fromDouble(v);
      return *this;
    }

  protected:
    virtual void printImplementation(ELogLevel level, const String& message) = 0;

    ELogLevel mLogLevel; // only used by operator<<()

    // ---  static methods ---

  public:
    //! The mutex used to synchronize concurrent calls to the log functions.
    //! You should always install a log mutex when using VL in multi-threaded applications.
    static void setLogMutex(IMutex* mutex) { mLogMutex = mutex; }

    //! The mutex used to synchronize concurrent calls to the log functions.
    static IMutex* logMutex() { return mLogMutex; }

    /** Important application message for the user.
     * The message will be printed with an hightlighed color.
     * \note Log generated only if verbosity level != vl::VEL_VERBOSITY_SILENT */
    static void notify(const String& message);

    /** Application message for the user.
     * \note Log generated only if verbosity level != vl::VEL_VERBOSITY_SILENT */
    static void print(const String& message);
    
    /** Use this function to provide extra information useful to investigate and solve problems.
      * \note Log generated only if verbosity level >= vl::VEL_VERBOSITY_DEBUG */
    static void debug(const String& message);

    /** Use this function to provide information about situations that might lead to errors or loss of data.
      * \note Log generated only if verbosity level >= vl::VEL_VERBOSITY_ERROR */
    static void warning(const String& message);

    /** Use this function to provide information about run-time errors: file not found, out of memory, OpenGL version too old etc.
      * \note Log generated only if verbosity level >= vl::VEL_VERBOSITY_ERROR */
    static void error(const String& message);

    /** Use this function to provide information about programming errors: wrong parameter initialization, division by zero, imminent crash, inconsistent program state etc.
      * \note Log generated only if verbosity level >= vl::VEL_VERBOSITY_ERROR */
    static void bug(const String& message);

    //! Logs VL and system information.
    static void logSystemInfo();

  private:
    static IMutex* mLogMutex;
  };

  //-----------------------------------------------------------------------------
  // Default logger
  //-----------------------------------------------------------------------------
  //! Installs the default logger used by Visualization Library. Setting this to NULL will disable logging.
  VLCORE_EXPORT void setDefLogger(Log* logger);

  //! Returns the currently installed default logger.
  VLCORE_EXPORT Log* defLogger();

  // Log macros
  #define VL_LOG (*::vl::defLogger())
  #define VL_LOG_NOTIFY (VL_LOG << ::vl::LL_LogNotify)
  #define VL_LOG_PRINT (VL_LOG << ::vl::LL_LogPrint)
  #define VL_LOG_BUG (VL_LOG << ::vl::LL_LogBug)
  #define VL_LOG_ERROR (VL_LOG << ::vl::LL_LogError)
  #define VL_LOG_WARNING (VL_LOG << ::vl::LL_LogWarning)
  #define VL_LOG_DEBUG (VL_LOG << ::vl::LL_LogDebug)

  //-----------------------------------------------------------------------------
  // StandardLog
  //-----------------------------------------------------------------------------
  /** The StandardLog class outputs the log messages on the stdout device and optionally also on a specified file. */
  class VLCORE_EXPORT StandardLog: public Log
  {
    VL_INSTRUMENT_CLASS(vl::StandardLog, Log)

  public:
    void setLogFile(const String& file);
    const String& logFile() const { return mLogFile; }

  protected:
    virtual void printImplementation(ELogLevel level, const String& message);
    String mLogFile;
    std::ofstream mFile;
  };
}

#endif
