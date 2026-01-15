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
#include <vlGraphics/RayIntersector.hpp>
#include <vlGraphics/ReadPixels.hpp>
#include <vlGraphics/Light.hpp>
#include <ctime>

using namespace vl;

class App_Picking: public BaseDemo
{
public:

  void mouseDownEvent(EMouseButton, int x, int y)
  {
    Camera* camera = rendering()->as<Rendering>()->camera();

    // convert Y coordinates to the OpenGL conventions
    y = openglContext()->height() - y;
    // compute the ray passing through the selected pixel
    Ray ray = camera->computeRay(x,y);
    // instance our ray-intersector
    RayIntersector intersector;
    // compute a frustum along the ray to accelerate the intersection test
    intersector.setFrustum( camera->computeRayFrustum( x,y ) );
    // compute the intersections!
    // (1) short way
    intersector.intersect(ray, sceneManager());
    // (2) long way
    /*
    // specify the Actor[s] to be tested
    intersector.actors()->clear();
    sceneManager()->extractActors( *intersector.actors() );
    // set the intersecting ray
    intersector.setRay(ray);
    // run intersection test
    intersector.intersect();
    */

    // inspect our intersections, the intersections returned are sorted according to their distance, the first one is the closest.
    if (intersector.intersections().size())
    {
      // highlight the intersection point by moving the green sphere there
      mIntersectionPoint->setLocalMatrix( mat4() );
      mIntersectionPoint->translate( intersector.intersections()[0]->intersectionPoint() );
      mIntersectionPoint->computeWorldMatrix();

      // print the name of the picked object
      Log::print( Say("Intersections detected = %n (%s).\n") << intersector.intersections().size() << intersector.intersections()[0]->actor()->objectName() );
    }
    else
      Log::print("No intersections detected.\n");
  }

  virtual void initEvent()
  {
    Log::notify(appletInfo());

    srand((int)time(NULL));

    // populate our scene with some random objects

    int   count    = 1;
    float displace = 2.0f;
    for(int z=-count; z<=count; ++z)
    for(int y=-count; y<=count; ++y)
    for(int x=-count; x<=count; ++x)
    {
      // random color
      ref<Effect> fx = new Effect;
      fx->shader()->enable(EN_DEPTH_TEST);
      fx->shader()->enable(EN_LIGHTING);
      fx->shader()->gocLight(0)->setLinearAttenuation(0.025f);
      fx->shader()->gocMaterial()->setDiffuse( fvec4((float)random(0,1), (float)random(0,1), (float)random(0,1),1.0f) );

      ref<Geometry> geom = randomObject();
      Actor* act = sceneManager()->tree()->addActor( geom.get(), fx.get(), new Transform );
      act->setObjectName(geom->objectName().c_str());
      act->transform()->translate(x*displace, y*displace, z*displace);
      act->transform()->computeWorldMatrix();
    }

    // create a uv-sphere used to highlight the intersection point

    // random color
    ref<Effect> fx = new Effect;
    fx->shader()->enable(EN_DEPTH_TEST);
    fx->shader()->enable(EN_LIGHTING);
    fx->shader()->gocLight(0)->setLinearAttenuation(0.025f);
    fx->shader()->gocMaterial()->setDiffuse( green );

    ref<Geometry> intersection_point_geom = makeUVSphere(vec3(0,0,0), 0.1f);
    intersection_point_geom->computeNormals();
    Actor* intersection_point_act = sceneManager()->tree()->addActor( intersection_point_geom.get(), fx.get(), new Transform );
    mIntersectionPoint = intersection_point_act->transform();
  }

  // generate random objects
  ref<Geometry> randomObject()
  {
    ref<Geometry> geom;
    // random shape
    switch(rand() % 7)
    {
      case 0: geom = makeIcosphere(vec3(0,0,0), 1, 2, false); break;
      case 1: geom = makeBox(vec3(0,0,0), 1, 1, 1); break;
      case 2: geom = makeCone(vec3(0,0,0),1,1); break;
      case 3: geom = makeUVSphere(vec3(0,0,0),1); break;
      case 4: geom = makeCylinder(vec3(0,0,0),1,1); break;
      case 5: geom = makeTorus(vec3(0,0,0),2,0.5f,20,20); break;
      case 6: geom = makeTeapot(vec3(0,0,0),2); break;
    }
    // normals are needed for lighting
    if (!geom->normalArray())
      geom->computeNormals();
    return geom;
  }

protected:
  Transform* mIntersectionPoint;
};

// Have fun!

BaseDemo* Create_App_Picking() { return new App_Picking; }
