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
#include <vlGraphics/RenderingTree.hpp>
#include <vlGraphics/Rendering.hpp>
#include <vlGraphics/StereoCamera.hpp>

using namespace vl;

class App_Stereo: public BaseDemo
{
public:
  virtual String appletInfo()
  {
    return BaseDemo::appletInfo();
  }

  void updateScene()
  {
    /* update the left and right cameras to reflect the movement of the mono camera */
    mStereoCamera->updateLeftRightCameras();

    /* animate the rotating spheres */
    mRootTransform->setLocalMatrix( mat4::getRotation( Time::currentTime() * 45, 0,1,0 ) );
    mRootTransform->computeWorldMatrixRecursive();
  }

  void initEvent()
  {
    Log::notify(appletInfo());

    /* save for later */
    OpenGLContext* gl_context = rendering()->as<Rendering>()->renderer()->framebuffer()->openglContext();

    /* let the two left and right cameras follow the mono camera */
    mMonoCamera = rendering()->as<Rendering>()->camera();
    mStereoCamera = new StereoCamera;
    mStereoCamera->setMonoCamera(mMonoCamera.get());

    /* install two renderings, one for the left eye and one for the right */
    mLeftRendering  = new Rendering;
    mRightRendering = new Rendering;
    mMainRendering  = new RenderingTree;
    mMainRendering->subRenderings()->push_back(mLeftRendering.get());
    mMainRendering->subRenderings()->push_back(mRightRendering.get());
    setRendering(mMainRendering.get());

    /* let the left and right scene managers share the same scene */
    mLeftRendering->sceneManagers()->push_back(sceneManager());
    mRightRendering->sceneManagers()->push_back(sceneManager());
    
    /* let the left and right rendering write on the same framebuffer */
    mLeftRendering->renderer()->setFramebuffer(gl_context->framebuffer());
    mRightRendering->renderer()->setFramebuffer(gl_context->framebuffer());

    /* set left/right cameras to the cameras of the left and right rendering,
       the viewport will be automatically taken from the mono camera. */
    mStereoCamera->setLeftCamera(mLeftRendering->camera());
    mStereoCamera->setRightCamera(mRightRendering->camera());

    /* set adequate eye separation and convergence */
    mStereoCamera->setConvergence(20);
    mStereoCamera->setEyeSeparation(1);

    /* setup color masks for red (left) / cyan (right) glasses */
    mLeftRendering->renderer()->overriddenDefaultRenderStates().push_back(RenderStateSlot(new ColorMask(false, true, true),-1));
    /* for the right we set the clear flags to clear only the depth buffer, not the color buffer */
    mRightRendering->renderer()->overriddenDefaultRenderStates().push_back(RenderStateSlot(new ColorMask(true, false, false),-1));
    mRightRendering->renderer()->setClearFlags(CF_CLEAR_DEPTH);

    /* let the trackball rotate the mono camera */
    trackball()->setCamera(mMonoCamera.get());
    trackball()->setTransform(NULL); 

    /* populate the scene */
    setupScene();
  }

  // populates the scene
  void setupScene()
  {
    ref<Light> camera_light = new Light;
    ref<EnableSet> enables = new EnableSet;
    enables->enable(EN_DEPTH_TEST);
    enables->enable(EN_LIGHTING);

    ref<Effect> sphere_fx = new Effect;
    sphere_fx->shader()->setEnableSet(enables.get());
    sphere_fx->shader()->gocMaterial()->setDiffuse(gray);
    sphere_fx->shader()->setRenderState(camera_light.get(), 0);
    sphere_fx->shader()->gocPolygonMode()->set(vl::PM_LINE, vl::PM_LINE);

    ref<Effect> fx = new Effect;
    fx->shader()->setEnableSet(enables.get());
    fx->shader()->gocMaterial()->setDiffuse(gray);
    fx->shader()->setRenderState(camera_light.get(), 0);

    mRootTransform = new Transform;

    // central sphere
    ref<Geometry> sphere = makeUVSphere(vec3(0,0,0), 4);
    sphere->computeNormals();
    sceneManager()->tree()->addActor( sphere.get(), sphere_fx.get(), mRootTransform.get());
    
    // rotating spheres
    float count = 10;
    for(size_t i=0; i<count; ++i)
    {
      ref<Geometry> satellite = makeUVSphere(vec3(7,0,0), 2.5);
      satellite->computeNormals();
      ref<Transform> child_transform = new Transform;
      mRootTransform->addChild(child_transform.get());
      child_transform->setLocalMatrix( mat4::getRotation(360.0f * (i/count), 0,1,0) );
      sceneManager()->tree()->addActor( satellite.get(), fx.get(), child_transform.get());
    }
  }

  void resizeEvent(int w, int h)
  {
    /* update the viewport of the main camera */
    mMonoCamera->viewport()->setWidth ( w );
    mMonoCamera->viewport()->setHeight( h );
    /* update the left and right cameras since the viewport has changed */
    mStereoCamera->updateLeftRightCameras();
  }

  void loadModel(const std::vector<String>& files)
  {
    // resets the scene
    sceneManager()->tree()->actors()->clear();

    for(unsigned int i=0; i<files.size(); ++i)
    {
      ref<ResourceDatabase> resource_db = loadResource(files[i],true);

      if (!resource_db || resource_db->count<Actor>() == 0)
      {
        Log::error("No data found.\n");
        continue;
      }

      std::vector< ref<Actor> > actors;
      resource_db->get<Actor>(actors);
      for(unsigned i=0; i<actors.size(); ++i)
      {
        ref<Actor> actor = actors[i].get();
        // define a reasonable Shader
        actor->effect()->shader()->setRenderState( new Light, 0 );
        actor->effect()->shader()->enable(EN_DEPTH_TEST);
        actor->effect()->shader()->enable(EN_LIGHTING);
        actor->effect()->shader()->gocLightModel()->setTwoSide(true);
        // add the actor to the scene
        sceneManager()->tree()->addActor( actor.get() );
      }
    }

    // position the camera to nicely see the objects in the scene
    trackball()->adjustView( sceneManager(), vec3(0,0,1)/*direction*/, vec3(0,1,0)/*up*/, 1.0f/*bias*/ );

    /* try to adjust the convergence and eye separation to reasonable values */
    sceneManager()->computeBounds();
    real convergence = sceneManager()->boundingSphere().radius() / 2;
    real eye_separation = convergence/20;
    mStereoCamera->setConvergence(convergence);
    mStereoCamera->setEyeSeparation(eye_separation);
    Log::notify(Say("Convergence = %n\n") << convergence);
    Log::notify(Say("Eye separation = %n\n") << eye_separation);
  }

  // laod the files dropped in the window
  void fileDroppedEvent(const std::vector<String>& files) { loadModel(files); }

protected:
  ref<RenderingTree> mMainRendering;
  ref<Rendering> mLeftRendering;
  ref<Rendering> mRightRendering;
  ref<Rendering> mCompositingRendering;
  ref<Camera> mMonoCamera;
  ref<StereoCamera> mStereoCamera;
  ref<Transform> mRootTransform;
};

// Have fun!

BaseDemo* Create_App_Stereo() { return new App_Stereo; }
