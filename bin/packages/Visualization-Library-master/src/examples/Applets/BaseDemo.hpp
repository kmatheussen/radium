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

#ifndef BaseDemo_INCLUDE_ONCE
#define BaseDemo_INCLUDE_ONCE

#include <vlGraphics/Applet.hpp>
#include <vlGraphics/Rendering.hpp>
#include <vlCore/VisualizationLibrary.hpp>
#include <vlCore/ResourceDatabase.hpp>
#include <vlCore/Time.hpp>

class BaseDemo: public vl::Applet
{
public:
  BaseDemo()
  {
    mFPSTimer.start();
  }

  void updateEvent()
  {
    vl::Applet::updateEvent();

    if ( mFPSTimer.elapsed() > 2 )
    {
      mFPSTimer.start();
      openglContext()->setWindowTitle( vl::Say("[%.1n] %s") << fps() << appletName()  + " - " + vl::String("VL ") + vl::VisualizationLibrary::versionString() );
      vl::Log::print( vl::Say("FPS=%.1n\n") << fps() );
    }
  }

  virtual vl::String appletInfo()
  {
    return "Applet info: " + appletName() + "\n" +
    "Keys:\n" +
    "- Escape: quits the application.\n" +
    "- T:  enables the TrackballManipulator.\n" +
    "- F:  enables the GhostCameraManipulator (use A/D S/W keys).\n" +
    "- F1: toggles fullscreen mode if supported.\n" +
    "- F5: saves a screenshot of the current OpenGL window.\n" +
    "- C:  toggles the continuous update of the OpenGL window.\n" +
    "- U:  force update of the OpenGL window.\n" +
    "\n";
  }

private:
  vl::Time mFPSTimer;
};

#endif
