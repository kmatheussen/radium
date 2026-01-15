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
#include <vlGraphics/Text.hpp>
#include <vlGraphics/FontManager.hpp>
#include <vlCore/CatmullRomInterpolator.hpp>
#include <vlCore/LinearInterpolator.hpp>
#include <vlGraphics/Extrusion.hpp>

class App_Interpolators: public BaseDemo
{
public:
  // Constructor.
  App_Interpolators()
  { 
    mTest = 0; 
    mLinearInterpolator     = new vl::LinearInterpolatorFVec3;
    mCatmullRomInterpolator = new vl::CatmullRomInterpolatorFVec3; 
    mTransform1 = new vl::Transform; // will follow linear interpolation
    mTransform2 = new vl::Transform; // will follow Catmull-Rom interpolation
  }

  void initEvent()
  {
    vl::Log::notify(appletInfo());
    rendering()->as<vl::Rendering>()->transform()->addChild(mTransform1.get());
    rendering()->as<vl::Rendering>()->transform()->addChild(mTransform2.get());
    showCatmullRomPentagonOpen();
    showText();
  }

  void showCatmullRomPentagonOpen()
  {
    // generate the pentagon control points
    std::vector<vl::fvec3> pentagon;
    float radius = 5.0f;
    for(int i=0; i<5; ++i)
    {
      float a = (float)i/5.0f*vl::fPi*2.0f + vl::fPi/2.0f;
      pentagon.push_back(vl::fvec3(cos(a)*radius,sin(a)*radius,0));
    }
    // show the pentagon
    showPath(pentagon, vl::green);

    // Catmull-Rom interpolation over the pentagon control points
    std::vector<vl::fvec3> pentagon_spline;
    vl::CatmullRomInterpolatorFVec3 catmull;
    catmull.interpolator()->setPath( pentagon );
    // automatically generate the appropriate Catmull-Rom spline end-points
    catmull.interpolator()->setupEndPoints(false/*loop = no*/);
    // sample the spline over 41 points
    int segments = 41;
    for(int i=0; i<segments; ++i)
    {
      float t = (float)i/(segments-1); // interpolate from 0.0 to 1.0 included
      pentagon_spline.push_back( catmull.computePoint(t) );
    }
    // show the interpolated pentagon
    showPath(pentagon_spline, vl::red, true/*show points*/);
  }

  void showCatmullRomPentagonLoop()
  {
    // generate the pentagon
    std::vector<vl::fvec3> pentagon;
    float radius = 5.0f;
    for(int i=0; i<5; ++i)
    {
      float a = (float)i/5.0f*vl::fPi*2.0f + vl::fPi/2.0f;
      pentagon.push_back(vl::fvec3(cos(a)*radius,sin(a)*radius,0));
    }
    showPath(pentagon, vl::green);

    // Catmull-Rom interpolation over the pentagon
    vl::CatmullRomInterpolatorFVec3 catmull;
    catmull.interpolator()->setPath( pentagon );
    std::vector<vl::fvec3> pentagon_loop;
    // automatically generate the appropriate Catmull-Rom spline end-points
    catmull.interpolator()->setupEndPoints(true/*loop = yes*/);
    int segments = 50;
    for(int i=0; i<segments; ++i)
    {
      // interpolate from 0 to 0.9x, never reach 1.0 otherwise the last point would be the same as the first one
      float t = (float)i/segments; 
      pentagon_loop.push_back( catmull.computePoint(t) );
    }
    showPath(pentagon_loop, vl::yellow, true/*show points*/, true/*close line loop*/);
  }

  void showLinearPentagon()
  {
    // generate the pentagon
    std::vector<vl::fvec3> pentagon;
    float radius = 5.0f;
    for(int i=0; i<5; ++i)
    {
      float a = (float)i/5.0f*vl::fPi*2.0f + vl::fPi/2.0f;
      pentagon.push_back(vl::fvec3(cos(a)*radius,sin(a)*radius,0));
    }
    showPath(pentagon, vl::green);

    // linear interpolation over the pentagon
    vl::LinearInterpolatorFVec3 linear;
    linear.interpolator()->setPath( pentagon );
    std::vector<vl::fvec3> pentagon_linear;
    int segments = 20; /* 21 would perfectly overlap the pentagon */
    for(int i=0; i<segments; ++i)
    {
      // interpolate from 0 to 1.0
      float t = (float)i/(segments-1); 
      pentagon_linear.push_back( linear.computePoint(t) );
    }
    vl::Actor* line = showPath(pentagon_linear, vl::blue, true/*show points*/);
    // ensure the blue line is over the green one.
    line->setRenderRank(1);
  }

  // creates two paths one linearly-interpolated and the other Catmull-Rom-interpolated
  void showInterpolatorAnimation()
  {
    // generate a 5-points star.
    std::vector<vl::fvec3> pentagon;
    float radius = 5.0f;
    for(int i=0; i<5; ++i)
    {
      float a = (float)i/5.0f*vl::fPi*2.0f*2.0f + vl::fPi/2.0f;
      pentagon.push_back(vl::fvec3(cos(a)*radius,sin(a)*radius,0));
    }

    // Catmull-Rom interpolation over the star
    mCatmullRomInterpolator->interpolator()->setPath( pentagon );
    // generate line for visual feedback.
    std::vector<vl::fvec3> pentagon_loop;
    // automatically generate the appropriate Catmull-Rom spline end-points
    mCatmullRomInterpolator->interpolator()->setupEndPoints(true/*loop = yes*/);
    int segments = 50;
    for(int i=0; i<segments; ++i)
    {
      // interpolate from 0 to 0.9x, never reach 1.0 otherwise the last point would be the same as the first one
      float t = (float)i/segments; 
      pentagon_loop.push_back( mCatmullRomInterpolator->computePoint(t)*1.2f /*1.2f -> makes it a bit larger*/ );
    }
    showPath(pentagon_loop, vl::yellow, true/*show points*/, true/*close line loop*/);

    // linear interpolation over the star (loop)
    mLinearInterpolator->interpolator()->setPath( pentagon );
    // close the pentagon
    mLinearInterpolator->interpolator()->path().push_back(pentagon[0]);
    // generate line for visual feedback.
    std::vector<vl::fvec3> pentagon_linear;
    segments = 20;
    for(int i=0; i<segments; ++i)
    {
      // interpolate from 0 to 0.9x, never reach 1.0 otherwise the last point would be the same as the first one
      float t = (float)i/segments; 
      pentagon_linear.push_back( mLinearInterpolator->computePoint(t) );
    }
    showPath(pentagon_linear, vl::blue, true/*show points*/, true/*close line loop*/);

    // display animated arrows
    vl::ref<vl::Geometry> arrow = vl::makePyramid(vl::vec3(0,0,0),0.5f,1.0f);
    arrow->transform( vl::mat4::getRotation(-90.0f,1,0,0) );
    arrow->computeNormals();
    vl::ref<vl::Effect> arrow_fx = new vl::Effect;
    arrow_fx->shader()->enable(vl::EN_DEPTH_TEST);
    arrow_fx->shader()->enable(vl::EN_LIGHTING);
    arrow_fx->shader()->setRenderState(new vl::Light, 0);
    sceneManager()->tree()->addActor( arrow.get(), arrow_fx.get(), mTransform1.get() );
    sceneManager()->tree()->addActor( arrow.get(), arrow_fx.get(), mTransform2.get() );
  }

  // displays a closed or open path, with or without points
  vl::Actor* showPath(const std::vector<vl::fvec3>& ctrl_points, const vl::fvec4& color, bool points=false, bool loop=false)
  {
    // simple effect
    vl::ref<vl::Effect> effect = new vl::Effect;
    effect->shader()->enable(vl::EN_DEPTH_TEST);
    effect->shader()->gocPointSize()->set(4);
    effect->shader()->gocColor()->setValue(color);

    // generates the line/points geometry
    vl::ref<vl::Geometry>   geom       = new vl::Geometry;
    vl::ref<vl::ArrayFloat3> vert_array = new vl::ArrayFloat3;
    geom->setVertexArray( vert_array.get() );
    vert_array->initFrom(ctrl_points);
    geom->drawCalls()->push_back(new vl::DrawArrays(loop ? vl::PT_LINE_LOOP : vl::PT_LINE_STRIP, 0, (int)vert_array->size()));
    if (points) 
      geom->drawCalls()->push_back(new vl::DrawArrays(vl::PT_POINTS, 0, (int)vert_array->size()));

    // adds the geometry to the scene
    return sceneManager()->tree()->addActor( geom.get(), effect.get(), NULL );
  }

  // Animates the arrows along the path.
  void updateScene()
  {
    if (mTest == 3)
    {
      float delta = 1.0f/50.0f;
      vl::fvec3 up(0,0,1);
      // get the fractional part of the time to loop 0..1
      float eye  = vl::fract((float)vl::Time::currentTime() * 0.1f);
      float look = vl::fract(eye+delta); // we have to remain in the range 0..1
      vl::fmat4 m;
      // linear interpolation
      m = vl::fmat4::getLookAtModeling(mLinearInterpolator->computePoint(eye),mLinearInterpolator->computePoint(look),up);
      mTransform1->setLocalMatrix((vl::mat4)m);
      // Catmull-Rom interpolation
      m = vl::fmat4::getLookAtModeling(mCatmullRomInterpolator->computePoint(eye)*1.2f,mCatmullRomInterpolator->computePoint(look)*1.2f,up);
      mTransform2->setLocalMatrix((vl::mat4)m);
    }
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
        case 0: showCatmullRomPentagonOpen(); break;
        case 1: showCatmullRomPentagonLoop(); break;
        case 2: showLinearPentagon(); break;
        case 3: showInterpolatorAnimation(); break;
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

protected:
  int mTest;
  vl::ref<vl::LinearInterpolatorFVec3>     mLinearInterpolator;
  vl::ref<vl::CatmullRomInterpolatorFVec3> mCatmullRomInterpolator;
  vl::ref<vl::Transform> mTransform1;
  vl::ref<vl::Transform> mTransform2;
};

// Have fun!

BaseDemo* Create_App_Interpolators() { return new App_Interpolators; }
