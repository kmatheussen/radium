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

#ifndef VisualizationLibrary_INCLUDE_ONCE
#define VisualizationLibrary_INCLUDE_ONCE

#include <vlCore/config.hpp>
#include <vlGraphics/link_config.hpp>

// TODO: it would be nice not to have this VLGRAPHICS/VLCORE_EXPORT mix and not to include <vlGraphics/link_config.hpp> here.

namespace vl
{
  //! Used to initialize/shutdown VisualizationLibrary and to access important global data.
  class VisualizationLibrary
  {
  public:
    //! Initializes VLCore and VLGraphics libraries.
    //! Call initCore() instead of init() when using only VLCore.
    VLGRAPHICS_EXPORT static void init(bool log_info=true);

    //! Releases all the resources acquired by VLCore and VLGraphics.
    //! Call shutdownCore() instead of shutdown() when using only VLCore.
    VLGRAPHICS_EXPORT static void shutdown();

    //! Initializes only VLCore library.
    //! Call initCore() instead of init() ONLY when using VLCore alone.
    VLCORE_EXPORT static void initCore(bool log_info=true);

    //! Releases all the resources acquired by Visualization Library Core
    //! Call shutdownCore() instead of shutdown() ONLY when using VLCore alone.
    VLCORE_EXPORT static void shutdownCore();

    //! Returns true if VLCore library is initialized and shutdown has not been called.
    VLCORE_EXPORT static bool isCoreInitialized();

    //! Returns true if VLGraphics library is initialized and shutdown has not been called.
    VLGRAPHICS_EXPORT static bool isGraphicsInitialized();

    //! Returns the Visualization Library's version string.
    VLCORE_EXPORT static const char* versionString();
  
  private:
    VLGRAPHICS_EXPORT static void initGraphics();
    VLGRAPHICS_EXPORT static void shutdownGraphics();
  };

  //! Shows a console window that displays the standard output. This function is meant to be used only under Windows only.
  VLCORE_EXPORT void showWin32Console();
}

#endif
