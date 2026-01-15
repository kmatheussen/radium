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
#include <vlCore/Colors.hpp>
#include <vlGraphics/SceneManager.hpp>
#include <vlGraphics/RenderingTree.hpp>
#include <vlGraphics/Light.hpp>
#include <vlGraphics/Geometry.hpp>

using namespace vl;

class App_MultipleCameras: public BaseDemo
{
public:
  virtual void initEvent()
  {
    vl::Log::notify(appletInfo());

    // save to be used later
    ref<Renderer> renderer = rendering()->as<Rendering>()->renderer();
    ref<Framebuffer> framebuffer = rendering()->as<Rendering>()->renderer()->framebuffer();
    // install new rendering tree
    mRenderingTree = new RenderingTree;
    setRendering(mRenderingTree.get());
    mRendering0 = new Rendering;
    mRendering1 = new Rendering;
    mRendering2 = new Rendering;
    mRendering3 = new Rendering;
    mRenderingTree->subRenderings()->push_back(mRendering0.get());
    mRenderingTree->subRenderings()->push_back(mRendering1.get());
    mRenderingTree->subRenderings()->push_back(mRendering2.get());
    mRenderingTree->subRenderings()->push_back(mRendering3.get());

    _tr1 = new Transform;
    _tr2 = new Transform;
    _tr1->addChild( _tr2.get() );

    ref<ResourceDatabase> res_db = loadResource("/models/3ds/monkey.3ds");
    if (!res_db)
      return;

    // compute normals
    for(unsigned i=0, count=res_db->count<Geometry>(); i<count; ++i)
    {
      Geometry* geom = res_db->get<Geometry>(i);
      if (!geom->normalArray())
        geom->computeNormals();
    }

    ref<Light> light = new Light;
    light->setAmbient( fvec4( .1f, .1f, .1f, 1.0f) );
    light->setSpecular( fvec4( .1f, .1f, .1f, 1.0f) );

    for(unsigned i=0, count=res_db->count<Actor>(); i<count; ++i)
    {
      res_db->get<Actor>(i)->effect()->shader()->setRenderState( light.get(), 0 );
      res_db->get<Actor>(i)->effect()->shader()->enable(EN_LIGHTING);
      res_db->get<Actor>(i)->effect()->shader()->enable(EN_DEPTH_TEST);
    }

    ref<Effect> wirefx = new Effect;
    wirefx->shader()->disable( EN_CULL_FACE );
    wirefx->shader()->enable( EN_LIGHTING );
    wirefx->shader()->enable( EN_DEPTH_TEST );
    wirefx->shader()->enable( EN_LINE_SMOOTH );
    wirefx->shader()->enable( EN_BLEND );
    wirefx->shader()->gocLightModel()->setTwoSide(true);
#if defined(VL_OPENGL)
    wirefx->shader()->gocPolygonMode()->set(PM_LINE, PM_LINE);
#endif
    wirefx->shader()->gocMaterial()->setDiffuse( white );
    wirefx->shader()->setRenderState( light.get(), 0 );

    std::vector< ref<Actor> > moneky_w;

    moneky_w.resize( res_db->count<Actor>() );
    for(unsigned i=0; i<res_db->count<Actor>(); ++i)
    {
      moneky_w[i] = new Actor( *res_db->get<Actor>(i) );
      moneky_w[i]->setEffect(wirefx.get());
    }

    for( int i=0; i<mRenderingTree->subRenderings()->size(); ++i )
    {
      mRenderingTree->subRenderings()->at(i)->as<Rendering>()->renderer()->setFramebuffer( framebuffer.get() );
      mRenderingTree->subRenderings()->at(i)->as<Rendering>()->setRenderer( renderer.get() );
      mRenderingTree->subRenderings()->at(i)->as<Rendering>()->setCamera( new Camera );
      mRenderingTree->subRenderings()->at(i)->as<Rendering>()->setTransform( _tr1.get() );
      ref<SceneManagerActorTree> scene_manager = new SceneManagerActorTree;
      mRenderingTree->subRenderings()->at(i)->as<Rendering>()->sceneManagers()->push_back( scene_manager.get() );

      switch(i)
      {
        case 0:
          for(unsigned j=0; j<res_db->count<Actor>(); ++j)
          {
            scene_manager->tree()->addActor( res_db->get<Actor>(j) );
            res_db->get<Actor>(j)->setTransform( _tr2.get() );
          }
          break;
        case 1:
        case 2:
        case 3:
          for(unsigned j=0; j<moneky_w.size(); ++j)
          {
            scene_manager->tree()->addActor( moneky_w[j].get() );
            moneky_w[j]->setTransform( _tr2.get() );
          }
      }

      switch(i)
      {
      case 0: mRenderingTree->subRenderings()->at(i)->as<Rendering>()->camera()->viewport()->setClearColor(black); break;
      case 1: mRenderingTree->subRenderings()->at(i)->as<Rendering>()->camera()->viewport()->setClearColor(yellow); break;
      case 2: mRenderingTree->subRenderings()->at(i)->as<Rendering>()->camera()->viewport()->setClearColor(red); break;
      case 3: mRenderingTree->subRenderings()->at(i)->as<Rendering>()->camera()->viewport()->setClearColor(green); break;
      }

      mat4 m;
      switch(i)
      {
      case 0: m = mat4::getLookAt( vec3(0,1,3.5f), vec3(0,0,0), vec3(0,1,0) ); break;
      case 1: m = mat4::getLookAt( vec3(0,1,3.5f), vec3(0,0,0), vec3(0,1,0) ); break;
      case 2: m = mat4::getLookAt( vec3(3.5,1,0), vec3(0,0,0), vec3(0,1,0) ); break;
      case 3: m = mat4::getLookAt( vec3(0,3.5,0), vec3(0,0,0), vec3(0,0,-1) ); break;
      }
      mRenderingTree->subRenderings()->at(i)->as<Rendering>()->camera()->setViewMatrix(m);
    }
  }

  virtual void updateScene()
  {
    _tr1->setLocalMatrix( mat4::getRotation(Time::currentTime()*45,0,1,0) );
  }

  virtual void resizeEvent(int w, int h)
  {
    if ( mRenderingTree->subRenderings()->size() < 4 )
      return;

    int hw = w/2;
    int hh = h/2;

    mRenderingTree->subRenderings()->at(2)->as<Rendering>()->renderer()->framebuffer()->setWidth(w);
    mRenderingTree->subRenderings()->at(2)->as<Rendering>()->renderer()->framebuffer()->setHeight(h);
    mRenderingTree->subRenderings()->at(2)->as<Rendering>()->camera()->viewport()->set(0,0,hw,hh);
    mRenderingTree->subRenderings()->at(2)->as<Rendering>()->camera()->setProjectionPerspective();

    mRenderingTree->subRenderings()->at(1)->as<Rendering>()->renderer()->framebuffer()->setWidth(w);
    mRenderingTree->subRenderings()->at(1)->as<Rendering>()->renderer()->framebuffer()->setHeight(h);
    mRenderingTree->subRenderings()->at(1)->as<Rendering>()->camera()->viewport()->set(hw,hh,w-hw,h-hh);
    mRenderingTree->subRenderings()->at(1)->as<Rendering>()->camera()->setProjectionPerspective();

    mRenderingTree->subRenderings()->at(0)->as<Rendering>()->renderer()->framebuffer()->setWidth(w);
    mRenderingTree->subRenderings()->at(0)->as<Rendering>()->renderer()->framebuffer()->setHeight(h);
    mRenderingTree->subRenderings()->at(0)->as<Rendering>()->camera()->viewport()->set(0,hh,hw,h-hh);
    mRenderingTree->subRenderings()->at(0)->as<Rendering>()->camera()->setProjectionPerspective();

    mRenderingTree->subRenderings()->at(3)->as<Rendering>()->renderer()->framebuffer()->setWidth(w);
    mRenderingTree->subRenderings()->at(3)->as<Rendering>()->renderer()->framebuffer()->setHeight(h);
    mRenderingTree->subRenderings()->at(3)->as<Rendering>()->camera()->viewport()->set(hw,0,w-hw,hh);
    mRenderingTree->subRenderings()->at(3)->as<Rendering>()->camera()->setProjectionPerspective();

    Rendering* rend = mRenderingTree->subRenderings()->at(0)->as<Rendering>();
    bindManipulators( rend->camera() );
  }

  void mouseDownEvent(EMouseButton, int x, int y)
  {
    for( int i=0; i<mRenderingTree->subRenderings()->size(); ++i )
    {
      int height = mRenderingTree->subRenderings()->at(i)->as<Rendering>()->renderer()->framebuffer()->height();
      if ( mRenderingTree->subRenderings()->at(i)->as<Rendering>()->camera()->viewport()->isPointInside(x,y,height) )
      {
        Rendering* rend = mRenderingTree->subRenderings()->at(i)->as<Rendering>();
          bindManipulators( rend->camera() );
         trackball()->setTransform( _tr2.get() );
        break;
      }
    }
  }

protected:
  ref<Transform> _tr1;
  ref<Transform> _tr2;
  ref<RenderingTree> mRenderingTree;
  ref<Rendering> mRendering0;
  ref<Rendering> mRendering1;
  ref<Rendering> mRendering2;
  ref<Rendering> mRendering3;
};

// Have fun!

BaseDemo* Create_App_MultipleCameras() { return new App_MultipleCameras; }
