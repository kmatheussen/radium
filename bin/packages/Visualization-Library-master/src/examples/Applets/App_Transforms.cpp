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

class App_Transforms: public BaseDemo
{
public:
  virtual void updateScene()
  {
    // rotate arm0
    mTransfArm0->setLocalMatrix( vl::mat4::getRotation(vl::Time::currentTime()*30.0f, 0.0f, 1.0f, 0.0f) );

    // rotate arm1
    mTransfArm1->setLocalMatrix( vl::mat4::getTranslation(0,10,0)*vl::mat4::getRotation( sin(vl::Time::currentTime())*120.0f, 1.0f, 0, 0.0f) );

    // rotate arm2
    mTransfArm2->setLocalMatrix( vl::mat4::getTranslation(0,10,0)*vl::mat4::getRotation( sin(vl::Time::currentTime())*120.0f, 0.0f, 0, 1.0f) );

    // rotate arm3
    mTransfArm3->setLocalMatrix( vl::mat4::getTranslation(0,10,0)*vl::mat4::getRotation( sin(vl::Time::currentTime())*120.0f, -1.0f, 0, 0.0f) );

    // rotate finger1
    mTransfHand1->setLocalMatrix( vl::mat4::getTranslation(0,10,0)*vl::mat4::getRotation( (sin(vl::Time::currentTime()*8.0f)*0.5f+0.5f)*60.0f, 1.0f, 0, 0.0f) );

    // rotate finger2
    mTransfHand2->setLocalMatrix( vl::mat4::getTranslation(0,10,0)*vl::mat4::getRotation( (sin(vl::Time::currentTime()*8.0f)*0.5f+0.5f)*-60.0f, 1.0f, 0, 0.0f) );
  }

  virtual void initEvent()
  {
    vl::Log::notify(appletInfo());

    /* basic render states */

    vl::ref<vl::Light> light = new vl::Light;

    // simple red material
    vl::ref<vl::Effect> fx_red = new vl::Effect;
    fx_red->shader()->setRenderState( light.get(), 0 );
    fx_red->shader()->enable(vl::EN_DEPTH_TEST);
    fx_red->shader()->enable(vl::EN_LIGHTING);
    fx_red->shader()->gocMaterial()->setDiffuse( vl::red );

    // simple gray material
    vl::ref<vl::Effect> fx_gray = new vl::Effect;
    fx_gray->shader()->setRenderState( light.get(), 0 );
    fx_gray->shader()->enable(vl::EN_DEPTH_TEST);
    fx_gray->shader()->enable(vl::EN_LIGHTING);
    fx_gray->shader()->gocMaterial()->setDiffuse( vl::gray );

    // simple crimson material
    vl::ref<vl::Effect> fx_crimson = new vl::Effect;
    fx_crimson->shader()->setRenderState( light.get(), 0 );
    fx_crimson->shader()->enable(vl::EN_DEPTH_TEST);
    fx_crimson->shader()->enable(vl::EN_LIGHTING);
    fx_crimson->shader()->gocMaterial()->setDiffuse( vl::crimson );

    // simple orange material
    vl::ref<vl::Effect> fx_orange = new vl::Effect;
    fx_orange->shader()->setRenderState( light.get(), 0 );
    fx_orange->shader()->enable(vl::EN_DEPTH_TEST);
    fx_orange->shader()->enable(vl::EN_LIGHTING);
    fx_orange->shader()->gocMaterial()->setDiffuse( vl::orange );

    // simple green material
    vl::ref<vl::Effect> fx_green = new vl::Effect;
    fx_green->shader()->setRenderState( light.get(), 0 );
    fx_green->shader()->enable(vl::EN_DEPTH_TEST);
    fx_green->shader()->enable(vl::EN_LIGHTING);
    fx_green->shader()->gocMaterial()->setDiffuse( vl::green );

    // simple blue material
    vl::ref<vl::Effect> fx_blue = new vl::Effect;
    fx_blue->shader()->setRenderState( light.get(), 0 );
    fx_blue->shader()->enable(vl::EN_DEPTH_TEST);
    fx_blue->shader()->enable(vl::EN_LIGHTING);
    fx_blue->shader()->gocMaterial()->setDiffuse( vl::blue );

    /* working desk */

    vl::ref<vl::Geometry> plane = vl::makeGrid(vl::vec3(0,0,0), 50,50, 2,2);
    plane->computeNormals();
    sceneManager()->tree()->addActor(plane.get(), fx_gray.get());

    /* buttons */

    /* shows how to use Transforms if they don't need to be dynamically
       animated: first of all you don't put them in the rendering()->as<vl::Rendering>()->transform()'s
       hierarchy like the other transforms; secondly you have to manually call
       computeWorldMatrix()/computeWorldMatrixRecursive() to compute
       the final matrix used for the rendering. This way the rendering pipeline
       won't call computeWorldMatrix()/computeWorldMatrixRecursive()
       continuously for the Transforms we know are not going to change over time,
       thus saving precious time. */

    vl::ref<vl::Transform> tr;

    vl::ref<vl::Geometry> button = vl::makeCylinder(vl::vec3(0,0.5,0), 1.5, 1);
    button->computeNormals();

    tr = new vl::Transform( vl::mat4::getTranslation(-6,0,10) );
    tr->computeWorldMatrix(NULL);
    sceneManager()->tree()->addActor(button.get(), fx_orange.get(), tr.get());

    tr = new vl::Transform( vl::mat4::getTranslation(-4,0,10) );
    tr->computeWorldMatrix(NULL);
    sceneManager()->tree()->addActor(button.get(), fx_orange.get(), tr.get());

    tr = new vl::Transform( vl::mat4::getTranslation(-2,0,10) );
    tr->computeWorldMatrix(NULL);
    sceneManager()->tree()->addActor(button.get(), fx_orange.get(), tr.get());

    tr = new vl::Transform( vl::mat4::getTranslation(+2,0,10) );
    tr->computeWorldMatrix(NULL);
    sceneManager()->tree()->addActor(button.get(), fx_orange.get(), tr.get());

    tr = new vl::Transform( vl::mat4::getTranslation(+4,0,10) );
    tr->computeWorldMatrix(NULL);
    sceneManager()->tree()->addActor(button.get(), fx_orange.get(), tr.get());

    tr = new vl::Transform( vl::mat4::getTranslation(+6,0,10) );
    tr->computeWorldMatrix(NULL);
    sceneManager()->tree()->addActor(button.get(), fx_orange.get(), tr.get());

    /* robot base */

    vl::ref<vl::Geometry>  arm_base = vl::makeBox(vl::vec3(0,0.5,0), 12, 1, 12);
    arm_base ->computeNormals();
    sceneManager()->tree()->addActor(arm_base.get(), fx_blue.get());

    /* robot arms */

    vl::ref<vl::Geometry>  arm0    = vl::makeBox(vl::vec3(0,5,0), 2, 10, 2);
    arm0->computeNormals();
    mTransfArm0 = new vl::Transform;
    sceneManager()->tree()->addActor(arm0.get(), fx_red.get(), mTransfArm0.get());

    vl::ref<vl::Geometry>  arm1    = vl::makeCylinder(vl::vec3(0,5,0), 2, 10);
    arm1->computeNormals();
    mTransfArm1 = new vl::Transform;
    sceneManager()->tree()->addActor(arm1.get(), fx_green.get(), mTransfArm1.get());

    vl::ref<vl::Geometry>  arm2    = vl::makeCylinder(vl::vec3(0,5,0), 2, 10);
    arm2->computeNormals();
    mTransfArm2 = new vl::Transform;
    sceneManager()->tree()->addActor(arm2.get(), fx_green.get(), mTransfArm2.get());

    vl::ref<vl::Geometry>  arm3    = vl::makeCylinder(vl::vec3(0,5,0), 2, 10);
    arm3->computeNormals();
    mTransfArm3 = new vl::Transform;
    sceneManager()->tree()->addActor(arm3.get(), fx_green.get(), mTransfArm3.get());

    /* robot fingers */

    vl::ref<vl::Geometry>  finger1   = vl::makeBox(vl::vec3(0,2,0), 2, 4, 0.5f);
    finger1->computeNormals();
    mTransfHand1 = new vl::Transform;
    sceneManager()->tree()->addActor(finger1.get(), fx_crimson.get(), mTransfHand1.get());

    vl::ref<vl::Geometry>  finger2   = vl::makeBox(vl::vec3(0,2,0), 2, 4, 0.5f);
    finger2->computeNormals();
    mTransfHand2 = new vl::Transform;
    sceneManager()->tree()->addActor(finger2.get(), fx_crimson.get(), mTransfHand2.get());

    /* concatenate the transforms */

    rendering()->as<vl::Rendering>()->transform()->addChild(mTransfArm0.get());
    mTransfArm0->addChild(mTransfArm1.get());
    mTransfArm1->addChild(mTransfArm2.get());
    mTransfArm2->addChild(mTransfArm3.get());
    mTransfArm3->addChild(mTransfHand1.get());
    mTransfArm3->addChild(mTransfHand2.get());
  }

protected:
    vl::ref<vl::Transform> mTransfArm0;
    vl::ref<vl::Transform> mTransfArm1;
    vl::ref<vl::Transform> mTransfArm2;
    vl::ref<vl::Transform> mTransfArm3;
    vl::ref<vl::Transform> mTransfHand1;
    vl::ref<vl::Transform> mTransfHand2;
};

// Have fun!

BaseDemo* Create_App_Transforms() { return new App_Transforms; }
