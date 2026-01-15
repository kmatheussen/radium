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

#include <unistd.h>

#include <vlCore/DiskDirectory.hpp>
#include <vlQt4/Qt4ThreadedWidget.hpp>
#include "Applets/App_RotatingCube.hpp"
#include <vlCore/Log.hpp>
#include <vlCore/Say.hpp>
#include "tests.hpp"

using namespace vl;
using namespace vlQt4;

class MyQt4ThreadedWidget : public Qt4ThreadedWidget {
public:
  
  ref<Applet> applet;

  MyQt4ThreadedWidget(vl::OpenGLContextFormat vlFormat, QWidget *parent=0)
    : Qt4ThreadedWidget(vlFormat, parent)
  {
    /* create the applet to be run */
    applet = Create_App_VectorGraphics(); //new App_RotatingCube;
    //ref<Applet> applet = new App_RotatingCube;

    applet->initialize();

  }

  virtual void init_vl(vl::OpenGLContext *glContext) {
    
    glContext->initGLContext();

    /* bind the applet so it receives all the GUI events related to the OpenGLContext */
    glContext->addEventListener(applet.get());

    /* target the window so we can render on it */
    applet->rendering()->as<Rendering>()->renderer()->setFramebuffer(glContext->framebuffer() );

    /* black background */
    applet->rendering()->as<Rendering>()->camera()->viewport()->setClearColor( black );

    /* define the camera position and orientation */
    vec3 eye    = vec3(0,10,35); // camera position
    vec3 center = vec3(0,0,0);   // point the camera is looking at
    vec3 up     = vec3(0,1,0);   // up direction
    mat4 view_mat = mat4::getLookAt(eye, center, up);
    applet->rendering()->as<Rendering>()->camera()->setViewMatrix( view_mat );

    applet->setAppletName("hello");
  }
};

//-----------------------------------------------------------------------------
int main(int argc, char *argv[])
{
  QCoreApplication::setAttribute(Qt::AA_X11InitThreads);

  QApplication app(argc, argv);

  VisualizationLibrary::init();

  /* setup the OpenGL context format */
  vl::OpenGLContextFormat vlFormat;
  //vlFormat.setDoubleBuffer(true);
  vlFormat.setDoubleBuffer(true);
  vlFormat.setRGBABits( 8,8,8,8 );
  vlFormat.setDepthBufferBits(24);
  vlFormat.setFullscreen(false);
  vlFormat.setMultisampleSamples(8);
  vlFormat.setMultisample(true);
  //vlFormat.setMultisample(false);

  MyQt4ThreadedWidget widget(vlFormat);
  widget.resize(1000,1000);

  widget.show();


  app.exec();

  return 0;
}
