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
#include <vlGraphics/Light.hpp>

using namespace vl;

class App_Primitives: public BaseDemo
{
public:
  void initEvent()
  {
    vl::Log::notify(appletInfo());

    const float objdim = 1;
    const float scene_radius = 4;

    /* solid fill shader */
    ref<Effect> fx = new Effect;
    fx->shader()->enable(EN_DEPTH_TEST);
    fx->shader()->enable(EN_LIGHTING);
    fx->shader()->setRenderState( new Light, 0 );
    fx->shader()->gocMaterial()->setDiffuse(vl::royalblue);
    
#if defined(VL_OPENGL)
    /* wireframe shader */
    fx->lod(0)->push_back( new Shader );
    fx->shader(0,1)->enable(EN_BLEND);
    fx->shader(0,1)->enable(EN_LINE_SMOOTH);
    fx->shader(0,1)->enable(EN_DEPTH_TEST);
    fx->shader(0,1)->enable(EN_POLYGON_OFFSET_LINE);
    fx->shader(0,1)->gocPolygonOffset()->set(-1.0f, -1.0f);
    fx->shader(0,1)->gocPolygonMode()->set(PM_LINE, PM_LINE);
    fx->shader(0,1)->gocColor()->setValue(vl::lightgreen);

    // speedup tricks: 
    // we don't use lighting here so these won't be used, however including them in the second pass 
    // will allow VL to avoid restoring and resetting them back and forth all the time.
    fx->shader(0,1)->setRenderState( fx->shader()->getMaterial() );
    fx->shader(0,1)->setRenderState( fx->shader()->getLight(0), 0 );
#endif

    /* create our primitives */
    mGeometries.push_back( vl::makeBox( vec3( 0, 0, 0 ), objdim*0.8f, objdim*0.8f, objdim*0.8f )  );
    mGeometries.push_back( vl::makeCone( vec3( 0, 0, 0 ), objdim, objdim, 20 )  );
    mGeometries.push_back( vl::makePyramid( vec3( 0, 0, 0 ), objdim, objdim )  );
    mGeometries.push_back( vl::makeIcosahedron( vec3( 0, 0, 0 ), objdim )  );
    mGeometries.push_back( vl::makeIcosphere( vec3( 0, 0, 0 ), objdim )  );
    mGeometries.push_back( vl::makeUVSphere( vec3( 0, 0, 0 ), objdim )  );
    mGeometries.push_back( vl::makeCylinder( vec3( 0, 0, 0 ), objdim, objdim, 20 )  );
    mGeometries.push_back( vl::makeTorus( vec3( 0, 0, 0 ), objdim*1.2f, 0.2f, 20, 40  )  );
    mGeometries.push_back( vl::makeCapsule( objdim/2.0f, objdim/2.0f, 20, CC_RoundedCap, CC_RoundedCap, vl::lightgreen, vl::lightgreen )  );
    mGeometries.push_back( vl::makeTeapot( vec3( 0, 0, 0 ), objdim*1.5f )  );
    mGeometries.push_back( vl::makeGrid( vec3( 0, 0, 0 ), objdim, objdim, 10, 10 )  );

    /* populate the scene */
    for(size_t i=0; i<mGeometries.size(); ++i)
    {
      /* arrange objects in a circle */
      ref<Transform> tr = new Transform;
      rendering()->as<Rendering>()->transform()->addChild( tr.get() );

      mat4 m = mat4::getRotation( 360 * i / (real)mGeometries.size(), 0, 1, 0) * 
               mat4::getTranslation(0,0,-scene_radius);
      tr->setLocalMatrix( m );

      /* center the geometry */
      mGeometries[i]->computeBounds();
      m = mat4::getTranslation( -mGeometries[i]->boundingBox().center() );
      mGeometries[i]->transform( m );

      /* computes normals if not present */
      if (!mGeometries[i]->normalArray())
        mGeometries[i]->computeNormals();

      /* add object to the scene */
      sceneManager()->tree()->addActor( mGeometries[i].get(), fx.get(), tr.get() );
    }

    /* settings for the current visible object */
    mRot0  = 0;
    mRot1  = 0;
    mAngle = 360.0f / mGeometries.size(); 
  }

  /* press left/right arrow to rotate objects left/right */
  void keyPressEvent(unsigned short ch, vl::EKey key)
  {
    BaseDemo::keyPressEvent(ch, key);

    if ( !mTimer.isStarted() )
    {
      if (key == vl::Key_Left)
      {
        mTimer.start();
        mRot0 = mRot1;
        mRot1 = mRot1 + mAngle;
      }
      else
      if (key == vl::Key_Right)
      {
        mTimer.start();
        mRot0 = mRot1;
        mRot1 = mRot1 - mAngle;
      }
    }
  }

  /* animate rotation */
  void updateScene()
  {
    if ( mTimer.isStarted() )
    {
      float t = mTimer.elapsed();
      if (t>1)
      {
        t = 1;
        mTimer.stop();
      }
      t = smoothstep(0.0f, 1.0f, t);
      mat4 m = mat4::getRotation( mRot0*(1.0f-t) + mRot1*t, 0, 1, 0 );
      rendering()->as<Rendering>()->transform()->setLocalMatrix(m);
    }
  }

protected:
  std::vector< ref<Geometry> > mGeometries;
  Time mTimer;
  float mRot0;
  float mRot1;
  float mAngle;
};

// Have fun!

BaseDemo* Create_App_Primitives() { return new App_Primitives; }

