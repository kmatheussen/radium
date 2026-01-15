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

#ifndef Settings_INCLUDE_ONCE
#define Settings_INCLUDE_ONCE

#include <vlCore/vlnamespace.hpp>
#include <vlCore/String.hpp>

namespace vl
{
  //! Global application settings controlling how Visualization Library behaves.
  class VLCORE_EXPORT GlobalSettings: public Object
  {
    friend class VisualizationLibrary;

    VL_INSTRUMENT_CLASS(vl::GlobalSettings, Object)

  public:
    GlobalSettings();

    /** If \p true VL checks at the beginning of each rendering that the OpenGL states are
      * clean and ready to be used by VL. If the test fails it can mean that either there 
      * is a bug in VL or that the user did not restore the OpenGL states to a VL friendly
      * state after modifying them.
      * \note This can slow down the rendering. Enabled by default in DEBUG mode only. */
    void setCheckOpenGLStates(bool check_clean) { mCheckOpenGLStates = check_clean; }

    /** If \p true VL checks at the beginning of each rendering that the OpenGL states are
      * clean and ready to be used by VL. If the test fails it can mean that either there 
      * is a bug in VL or that the user did not restore the OpenGL states to a VL friendly
      * state after modifying them.
      * \note This can slow down the rendering. Enabled by default in DEBUG mode only. */
    bool checkOpenGLStates() const { return mCheckOpenGLStates; }

    /** The verbosity level of VL. This applies to all the logs generated via vl::Log::*. */
    void setVerbosityLevel(EVerbosityLevel verb_level) { mVerbosityLevel = verb_level; }

    /** The verbosity level of VL. This applies to all the logs generated via vl::Log::*. */
    EVerbosityLevel verbosityLevel() const { return mVerbosityLevel; }

    /** The path of the default log file. */
    const String& defaultLogPath() const { return mDefaultLogPath; }

    /** The path of the default data directory. */
    const String& defaultDataPath() const { return mDefaultDataPath; }
    
  protected:
    EVerbosityLevel mVerbosityLevel;
    bool mCheckOpenGLStates;
    String mDefaultLogPath;
    String mDefaultDataPath;
  };

  //! Returns VisulizationLibrary's global settings.
  VLCORE_EXPORT GlobalSettings* globalSettings();
}

#endif
