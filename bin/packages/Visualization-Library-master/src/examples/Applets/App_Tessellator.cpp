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
#include <vlCore/VisualizationLibrary.hpp>
#include <vlGraphics/Effect.hpp>
#include <vlGraphics/Geometry.hpp>
#include <vlGraphics/Tessellator.hpp>

class App_Tessellator: public BaseDemo
{
public:
  App_Tessellator() {}

  void initEvent()
  {
    // Basic initialization
    vl::Log::notify(appletInfo());

    // Filled effect
    vl::ref<vl::Effect> filled_fx = new vl::Effect;

    // Wireframe effect
    vl::ref<vl::Effect> wireframe_fx = new vl::Effect;
    wireframe_fx->shader()->gocPolygonMode()->set(vl::PM_LINE,vl::PM_LINE);

    // Add empty Actors
    mStar1 = sceneManager()->tree()->addActor( new vl::Actor(NULL, filled_fx.get(), new vl::Transform) );
    mStar2 = sceneManager()->tree()->addActor( new vl::Actor(NULL, wireframe_fx.get(), new vl::Transform) );
    rendering()->as<vl::Rendering>()->transform()->addChild(mStar1->transform());
    rendering()->as<vl::Rendering>()->transform()->addChild(mStar2->transform());
  }

  void updateScene() 
  {
    // Animation: compute rotation matrix to rotate the small star, 45°/sec rotation.
    vl::dmat4 m = vl::dmat4::getRotation( vl::Time::currentTime()*45, 0,0,1 );
    // Filled star on the left, wireframe star on the right
    mStar1->transform()->setLocalMatrix( vl::mat4::getTranslation(-4,0,0) );
    mStar2->transform()->setLocalMatrix( vl::mat4::getTranslation(+4,0,0) );

    // Concave and self-intersecting polygons cannot be directly rendered by OpenGL, for this
    // reason in order to render them we have to tessellate them first, i.e. we have to decompose
    // them into a set of triangles using the Tessellator class.

    // The Tessellator class takes as input a set of contours defining a complex polygon and 
    // outputs a series of triangles that can be rendered by OpenGL and Visualization Library
    vl::Tessellator tess;

    // Setup tessellation options, see also gluTessProperty() documentation.
    tess.setWindingRule(vl::TW_TESS_WINDING_ODD); // default
    tess.setTessNormal(vl::fvec3(0,0,1)); // default is vl::fvec3(0,0,0)
    tess.setBoundaryOnly(false); // default
    tess.setTolerance(0.0); // default

    // Outline #1 - generate 5-points star (small)
    int size = 2;
    tess.contours().push_back(5);
    for(int i=0; i<5; ++i)
    {
      float t = (float)i/5.0f*vl::fPi*2.0f*2.0f + vl::fPi/2.0f;
      // Fill the first contour
      tess.contourVerts().push_back( m * vl::dvec3(cos(t)*size,sin(t)*size,0.0f) );
    }

    // Outline #2 - generate 5-points star (big)
    size = 4;
    tess.contours().push_back(5);
    for(int i=0; i<5; ++i)
    {
      float t = (float)i/5.0f*vl::fPi*2.0f*2.0f + vl::fPi/2.0f;
      // Fill the second contour
      tess.contourVerts().push_back( vl::dvec3(cos(t)*size,sin(t)*size,0.0f) );
    }

    // Tessellate the two contours into a single polygon
    tess.setTessellateIntoSinglePolygon(true); // default
    tess.tessellate();

    /*
    You can also tessellate each contour separately selecting setTessellateIntoSinglePolygon(false).
    This way each contour will be processed separately and will generate its own set of triangles, this 
    is useful when you want to tessellate a large number of polygons with a single tessellate() call.
    */

    // Create a new Geometry and vertex array with the tessellated triangles
    vl::ref<vl::Geometry> tess_poly = new vl::Geometry;
    vl::ref<vl::ArrayFloat3> vert_array = new vl::ArrayFloat3;
    tess_poly->setVertexArray(vert_array.get());
    // Fill the vertex array with the tessellated triangles
    vert_array->initFrom(tess.tessellatedTris());
    // Add the primitive description
    tess_poly->drawCalls()->push_back( new vl::DrawArrays(vl::PT_TRIANGLES, 0, (int)vert_array->size()) );

    // Bind the created Geometry to the star Actor
    mStar1->setLod(0, tess_poly.get());
    mStar2->setLod(0, tess_poly.get());
  }

protected:
  // the Actor defining our complex star
  vl::Actor* mStar1;
  vl::Actor* mStar2;
};

// Have fun!

BaseDemo* Create_App_Tessellator() { return new App_Tessellator; }
