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

#include "BaseDemo.hpp"
#include <vlGraphics/GeometryPrimitives.hpp>
#include <vlCore/VisualizationLibrary.hpp>
#include <vlGraphics/Effect.hpp>
#include <vlGraphics/Light.hpp>
#include <vlGraphics/Geometry.hpp>
#include <vlGraphics/Text.hpp>
#include <vlGraphics/FontManager.hpp>
#include <vlCore/Interpolator.hpp>
#include <vlGraphics/Extrusion.hpp>

class App_Extrusion: public BaseDemo
{
public:
  App_Extrusion(): mTest(0) {}

  // Creates a simple extrusion
  void showSimplePipe()
  {
    // Generates the actual extrusion
    vl::Extrusion extrusion;

    // Define the silhouette to be extruded: 24 sided circle
    const int sides = 24;
    const float radius = 1.0f;
    for(int i=0; i<sides; ++i)
    {
      float t = (float)i/sides*vl::fPi*2.0f;
      extrusion.silhouette().push_back(vl::fvec2(::cos(t)*radius, ::sin(t)*radius));
    }

    // Define the path along which the silhouette is extruded.
    // Note: the first and last control points are not part of the actual extrusion 
    // but are used to define the orientation of the first and start segment.
    extrusion.positionPath().push_back(vl::fvec3(-5, 0, 0) + vl::fvec3(-1,0,0));
    extrusion.positionPath().push_back(vl::fvec3(-5, 0, 0));
    extrusion.positionPath().push_back(vl::fvec3(-5, 5, 0));
    extrusion.positionPath().push_back(vl::fvec3(+5, 5, 0));
    extrusion.positionPath().push_back(vl::fvec3(+5, 0, 0));
    extrusion.positionPath().push_back(vl::fvec3(+5, 0, 0) + vl::fvec3(+1,0,0));

    // Setup the extrusion options.
    extrusion.setSilhouetteMode(vl::SilhouetteClosed);
    extrusion.setSmooth(false);
    extrusion.setFillBottom(true); // start of the extrusion is closed
    extrusion.setFillTop(true);    // end of the extrusion is closed

    // Setup a simple white effect.
    vl::ref<vl::Effect> effect = new vl::Effect;
    effect->shader()->setRenderState( new vl::Light, 0 );
    effect->shader()->enable(vl::EN_LIGHTING);
    effect->shader()->enable(vl::EN_DEPTH_TEST);

    // Generates the extrusion.
    vl::ref<vl::Geometry> geom = extrusion.extrude();
    sceneManager()->tree()->addActor( geom.get(), effect.get(), NULL );

    // Utility function that visualizes the extrusion path.
    showPath(extrusion.positionPath(),vl::red);
  }

  // Creates a fancy arch applying rotation and scaling to the extrusion
  void showFancyArch()
  {
    // Generates the actual extrusion
    vl::Extrusion extrusion;

    // Define the silhouette to be extruded (a quad).
    const int sides = 4;
    const float radius = 1.0f;
    for(int i=0; i<sides; ++i)
    {
      float t = (float)i/sides*vl::fPi*2.0f;
      extrusion.silhouette().push_back(vl::fvec2(::cos(t)*radius, ::sin(t)*radius));
    }

    // Define the path along which the silhouette is extruded.
    // Note: the first and last control points are not part of the actual extrusion 
    // but are used to define the orientation of the first and start segment.
    extrusion.positionPath().push_back(vl::fvec3(-5, 0, 0) + vl::fvec3(0,-1,0));
    extrusion.positionPath().push_back(vl::fvec3(-5, 0, 0));
    for(int j=0; j<640; ++j)
    {
      float t = (float)j/(640-1)*vl::fPi;
      extrusion.positionPath().push_back(vl::fvec3(-cos(t)*5, sin(t)*5, 0) + vl::fvec3(0,5,0));
    }
    extrusion.positionPath().push_back(vl::fvec3(+5, 0, 0));
    extrusion.positionPath().push_back(vl::fvec3(+5, 0, 0) + vl::fvec3(0,-1,0));

    // Defines a rotation key (in degrees) for each segment generated in order
    // to rotate the silhouette along the extrusion path.
    // Note that the segments generated are extrusion.positionPath().size()-2 as
    // the first and last control points are not part of the actual extrusion 
    // but are used to define the orientation of the first and start segment.
    for(unsigned i=0; i<extrusion.positionPath().size()-2; ++i)
    {
      float t = (float)i/(extrusion.positionPath().size()-2-1);
      // rotate of 360*2 degrees (turns around twice)
      extrusion.rotationPath().push_back(t*360.0f*2.0f);
    }

    // Defines a scaling key (in degrees) for each segment generated in order
    // to rotate the silhouette along the extrusion path.
    // Note that the segments generated are extrusion.positionPath().size()-2 as
    // the first and last control points are not part of the actual extrusion 
    // but are used to define the orientation of the first and start segment.
    for(unsigned i=0; i<extrusion.positionPath().size()-2; ++i)
    {
      float t = (float)i/(extrusion.positionPath().size()-2-1);
      // make it smaller in the middle
      float s = 1.0f - ::sin( t*vl::fPi )*0.8f;
      extrusion.scalingPath().push_back(s);
    }

    // Setup the extrusion options.
    extrusion.setSilhouetteMode(vl::SilhouetteClosed);
    extrusion.setSmooth(false);
    extrusion.setFillBottom(false); // start of the extrusion is closed
    extrusion.setFillTop(false);    // end of the extrusion is closed

    // Setup a simple white effect.
    vl::ref<vl::Effect> effect = new vl::Effect;
    effect->shader()->setRenderState( new vl::Light, 0 );
    effect->shader()->enable(vl::EN_LIGHTING);
    effect->shader()->enable(vl::EN_DEPTH_TEST);

    // Generates the extrusion.
    vl::ref<vl::Geometry> geom = extrusion.extrude();
    sceneManager()->tree()->addActor( geom.get(), effect.get(), NULL );

    // Utility function that visualizes the extrusion path.
    showPath(extrusion.positionPath(),vl::red);
  }

  // Creates a vortex whose section is a flower.
  void showVortex()
  {
    vl::Extrusion extrusion;

    // Generate flower-like silhouette.
    const int lati = 128;
    for(int i=0; i<lati; ++i)
    {
      float t = (float)i/lati*vl::fPi*2.0f+3.14159365f/2.0f;
      extrusion.silhouette().push_back(vl::fvec2(::cos(t)*0.4f, ::sin(t)*0.4f));
      extrusion.silhouette().back() *= 2.0f + fabs(sin(t*8.0f)*2.0f-1.0f)*0.1f;
    }

    // Generate fancy vortex path.
    float freq   = 5.0f;
    float width  = 10.0f;
    const int size = 250;
    extrusion.positionPath().resize( size );
    for(int i=0; i<size; ++i)
    {
      float t = (float)i/(size-1);
      extrusion.positionPath()[i].y() = (t - 0.5f)*width;
      float a = t * vl::fPi*2.0f*freq - vl::fPi/4.0f;
      float s = ::sin( t*vl::fPi/2.0f );
      s = 5.0f*s + 0.5f*(1.0f-s);
      extrusion.positionPath()[i].x() = ::sin(a)*s;
      extrusion.positionPath()[i].z() = ::cos(a)*s;    
    }

    // Generate color path: linearly interpolate from blue to yellow.
    for(int i=0; i<size-2; ++i)
    {
      float t = (float)i/(size-2-1);
      vl::fvec4 c = vl::yellow*t + vl::blue*(1.0f-t);
      extrusion.colorPath().push_back(c);
    }

    // Scaling factor along the vortex: starts small, ends big.
    for(unsigned i=0; i<extrusion.positionPath().size()-2; ++i)
    {
      float t = (float)i/(extrusion.positionPath().size()-2-1);
      float s = ::sin( t*vl::fPi/2.0f )*0.8f;
      extrusion.scalingPath().push_back(s);
    }

    // Extrusion options.
    extrusion.setSilhouetteMode(vl::SilhouetteClosed);
    extrusion.setSmooth(false);
    extrusion.setFillBottom(true);
    extrusion.setFillTop(true);

    // Simple white effect.
    vl::ref<vl::Effect> effect = new vl::Effect;
    effect->shader()->setRenderState( new vl::Light, 0 );
    effect->shader()->enable(vl::EN_LIGHTING);
    effect->shader()->enable(vl::EN_DEPTH_TEST);
    // Enable color material to visualize the generated vertex colors.
    effect->shader()->gocMaterial()->setColorMaterialEnabled(true);

    // Generates the actual extrusion.
    vl::ref<vl::Geometry> extr = extrusion.extrude();
    sceneManager()->tree()->addActor( extr.get(), effect.get(), NULL );
  }

  // Creates a vortex whose section is a flower.
  void showNail()
  {
    vl::Extrusion extrusion;

    // Defines the silhouette as a cross.
    float diam = 1.0f;
    extrusion.silhouette().push_back(vl::fvec2(-diam*3.0f,-diam));
    extrusion.silhouette().push_back(vl::fvec2(-diam,-diam));
    extrusion.silhouette().push_back(vl::fvec2(-diam,-diam*3.0f));
    extrusion.silhouette().push_back(vl::fvec2(+diam,-diam*3.0f));
    extrusion.silhouette().push_back(vl::fvec2(+diam,-diam));
    extrusion.silhouette().push_back(vl::fvec2(+diam*3.0f,-diam));
    extrusion.silhouette().push_back(vl::fvec2(+diam*3.0f,+diam));
    extrusion.silhouette().push_back(vl::fvec2(+diam,+diam));
    extrusion.silhouette().push_back(vl::fvec2(+diam,+diam*3.0f));
    extrusion.silhouette().push_back(vl::fvec2(-diam,+diam*3.0f));
    extrusion.silhouette().push_back(vl::fvec2(-diam,+diam));
    extrusion.silhouette().push_back(vl::fvec2(-diam*3.0f,+diam));

    // Extrusion path.
    int segments = 500;
    for(int i=0; i<segments; ++i)
    {
      float t = (float)i/(segments-1);
      extrusion.positionPath().push_back( vl::fvec3( 0, t*20.0f-10.0f, 0) );
    }

    // Extrusion color.
    for(int i=0; i<segments-2; ++i)
    {
      float t = (float)i/(segments-2-1);
      vl::fvec4 c = vl::gold * t + vl::royalblue*(1.0f-t);
      extrusion.colorPath().push_back(c);
    }

    // Extrusion color.
    for(int i=0; i<segments-2; ++i)
    {
      float t = (float)i/(segments-2-1);
      extrusion.scalingPath().push_back( t );
    }

    // Extrusion rotaion.
    for(int i=0; i<segments-2; ++i)
    {
      float t = (float)i/(segments-2-1);
      extrusion.rotationPath().push_back(360*t);
    }

    // Extrusion options.
    extrusion.setSilhouetteMode(vl::SilhouetteClosed);
    extrusion.setSmooth(false);
    extrusion.setFillBottom(true);
    extrusion.setFillTop(true);

    // Simple effect
    vl::ref<vl::Effect> effect = new vl::Effect;
    effect->shader()->setRenderState( new vl::Light, 0 );
    effect->shader()->enable(vl::EN_LIGHTING);
    effect->shader()->enable(vl::EN_DEPTH_TEST);
    // Enable color material in order to see the generated vertex colors.
    effect->shader()->gocMaterial()->setColorMaterialEnabled(true);

    // Generates the actual extrusion.
    vl::ref<vl::Geometry> geom = extrusion.extrude();
    sceneManager()->tree()->addActor( geom.get(), effect.get(), NULL );
  }

  // Utility function to display a path
  void showPath(const std::vector<vl::fvec3>& ctrl_points, const vl::fvec4& color)
  {
    // generate line geometry with lines and points
    vl::ref<vl::Geometry>   geom       = new vl::Geometry;
    vl::ref<vl::ArrayFloat3> vert_array = new vl::ArrayFloat3;
    geom->setVertexArray( vert_array.get() );
    vert_array->initFrom(ctrl_points);
    geom->drawCalls()->push_back(new vl::DrawArrays(vl::PT_LINE_STRIP, 0, (int)vert_array->size())); // lines
    geom->drawCalls()->push_back(new vl::DrawArrays(vl::PT_POINTS,     0, (int)vert_array->size())); // points

    // setup simple effect
    vl::ref<vl::Effect> effect = new vl::Effect;
    effect->shader()->gocColor()->setValue(color);
    effect->shader()->enable(vl::EN_LINE_STIPPLE);
    effect->shader()->gocPointSize()->set(3);
    effect->shader()->gocLineStipple()->setPattern(0x3333);
    effect->setRenderRank(1); // always draw over the pipe

    sceneManager()->tree()->addActor( geom.get(), effect.get(), NULL );
  }

  // Press the left/right arrow buttons to navigate through the examples.
  void keyPressEvent(unsigned short ch, vl::EKey key)
  {
    BaseDemo::keyPressEvent(ch,key);

    if (key == vl::Key_Left || key == vl::Key_Right)
    {
      if (key == vl::Key_Left)
        mTest--;
      if (key == vl::Key_Right)
        mTest++;
      if (mTest < 0) mTest = 3;
      if (mTest > 3) mTest = 0;
      sceneManager()->tree()->actors()->clear();
      switch(mTest)
      {
        case 0: showSimplePipe(); break;
        case 1: showFancyArch(); break;
        case 2: showVortex(); break;
        case 3: showNail(); break;
      }
      showText();
    }
  }

  // displays the text
  void showText()
  {
    vl::ref<vl::Text> text = new vl::Text;
    text->setText("Press the left/right arrow keys to change test.");
    text->setFont( vl::defFontManager()->acquireFont("/font/bitstream-vera/VeraMono.ttf", 10) );
    text->setAlignment( vl::AlignHCenter | vl::AlignTop );
    text->setViewportAlignment( vl::AlignHCenter | vl::AlignTop );
    text->translate(0,-5,0);
    text->setColor(vl::white);
    vl::ref<vl::Effect> effect = new vl::Effect;
    effect->shader()->enable(vl::EN_BLEND);
    sceneManager()->tree()->addActor(text.get(), effect.get());
  }

  void initEvent()
  {
    vl::Log::notify(appletInfo());
    showSimplePipe();
    showText();
  }

protected:
  int mTest;
};

// Have fun!

BaseDemo* Create_App_Extrusion() { return new App_Extrusion; }
