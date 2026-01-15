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

#include <vlCore/GlobalSettings.hpp>
#include <cstdlib>
#include <cstdio>

using namespace vl;

//-----------------------------------------------------------------------------
GlobalSettings::GlobalSettings()
{
  VL_DEBUG_SET_OBJECT_NAME()

  #ifndef NDEBUG
    mVerbosityLevel  = vl::VEL_VERBOSITY_NORMAL;
    mCheckOpenGLStates = true;
  #else
    mVerbosityLevel  = vl::VEL_VERBOSITY_ERROR;
    mCheckOpenGLStates = false;
  #endif

  // initialize from environment variables

  char* val = NULL;

  // log file

  val = getenv("VL_LOGFILE_PATH");
  if (val)
    mDefaultLogPath = val;
  else
    mDefaultLogPath = "log.txt";

  // data path

  val = getenv("VL_DATA_PATH");
  if (val)
    mDefaultDataPath = val;
  else
    mDefaultDataPath = "../data";

  // verbosity level

  val = getenv("VL_VERBOSITY_LEVEL");
  if (val)
  {
    if ( String(val).toUpperCase() == "SILENT")
      setVerbosityLevel(vl::VEL_VERBOSITY_SILENT);
    else
    if ( String(val).toUpperCase() == "ERROR")
      setVerbosityLevel(vl::VEL_VERBOSITY_ERROR);
    else
    if ( String(val).toUpperCase() == "NORMAL")
      setVerbosityLevel(vl::VEL_VERBOSITY_NORMAL);
    else
    if ( String(val).toUpperCase() == "DEBUG")
      setVerbosityLevel(vl::VEL_VERBOSITY_DEBUG);
    else
    {
      // no log here yet.
      fprintf(stderr,"VL_VERBOSITY_LEVEL variable has unknown value %s! Legal values: SILENT, ERROR, NORMAL, DEBUG\n\n", val);
    }
  }

  // opengl state checks

  val = getenv("VL_CHECK_GL_STATES");
  if (val)
  {
    if ( String(val).toUpperCase() == "YES" )
      setCheckOpenGLStates(true);
    else
    if ( String(val).toUpperCase() == "NO" )
      setCheckOpenGLStates(false);
    else
    {
      // no log here yet.
      fprintf(stderr,"VL_CHECK_GL_STATES variable has unknown value '%s'! Legal values: YES, NO.\n\n", val);
    }
  }
}
//-----------------------------------------------------------------------------
