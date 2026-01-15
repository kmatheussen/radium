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
#include <vlGraphics/Light.hpp>
#include <vlGraphics/GeometryPrimitives.hpp>
#include <vlGraphics/Text.hpp>
#include <vlGraphics/FontManager.hpp>

class App_Lights: public BaseDemo
{
public:
  virtual void initEvent()
  {
    vl::Log::notify(appletInfo());

    mCameraLightType = Disable;

    // setup our simple scene with a plane and a few columns

    mLightFx = new vl::Effect;
    mLightFx->shader()->enable(vl::EN_LIGHTING); /* IMPORTANT! */
    mLightFx->shader()->enable(vl::EN_DEPTH_TEST);

    vl::ref<vl::Geometry> ground = vl::makeGrid( vl::vec3(0,0,0), 20, 20, 100,100 );
    ground->computeNormals();
    sceneManager()->tree()->addActor(ground.get(), mLightFx.get());

    for(int i=0; i<9; ++i)
    {
      vl::ref<vl::Geometry> pillar = vl::makeCylinder( vl::vec3(6,2,-4*1.1f + i*1.1f), 1, 4, 100, 100, true, false);
      pillar->computeNormals();
      sceneManager()->tree()->addActor(pillar.get(), mLightFx.get());
    }

    for(int i=0; i<5; ++i)
    {
      vl::ref<vl::Geometry> pillar = vl::makeCylinder( vl::vec3(-3,2,-2*1.1f + i*1.1f), 1, 4, 100, 100, true, false);
      pillar->computeNormals();
      sceneManager()->tree()->addActor(pillar.get(), mLightFx.get());
    }

    // light setup

    // A typical OpenGL implementation supports up to 8 lights at the same time. It means that your scene can have
    // as many lights as you want but each object can be lit "only" by 8 lights at a time, this is usually never the
    // case as lighting absorbs considerable computational resources.

    // In our example each object will be lit up to 5 lights at the same time.

    mLight0 = new vl::Light;
    mLight1 = new vl::Light;
    mLight2 = new vl::Light;
    mLight3 = new vl::Light;
    mLight4 = new vl::Light;

    // Add the lights to the shader in order to use them (we save light #4 for the camera)
    mLightFx->shader()->setRenderState( mLight0.get(), 0 );
    mLightFx->shader()->setRenderState( mLight1.get(), 1 );
    mLightFx->shader()->setRenderState( mLight2.get(), 2 );
    mLightFx->shader()->setRenderState( mLight3.get(), 3 );

    // Define the light diffuse color. 
    // See the OpenGL Programmer's Guide for more details about the OpenGL lighting model and equations.
    mLight0->setDiffuse(vl::red);
    mLight1->setDiffuse(vl::green);
    mLight2->setDiffuse(vl::blue);
    mLight3->setDiffuse(vl::white);
    mLight4->setDiffuse(vl::white);

    // Setup lights #0, #1 and #2 as positional lights with constant, linear and quadratic attenuation.
    // Again, see the OpenGL Programmer's Guide for more details about the OpenGL lighting model and equations.
    mLight0->setPosition(vl::fvec4(0,0,0,1)); /* default */
    mLight0->setConstantAttenuation(1.0f);
    mLight0->setLinearAttenuation(0.0f);
    mLight0->setQuadraticAttenuation(0.0f);

    mLight1->setPosition(vl::fvec4(0,0,0,1)); /* default */
    mLight1->setConstantAttenuation(0.0f);
    mLight1->setLinearAttenuation(1.0f);
    mLight1->setQuadraticAttenuation(0.0f);

    mLight2->setPosition(vl::fvec4(0,0,0,1)); /* default */
    mLight2->setConstantAttenuation(0.0f);
    mLight2->setLinearAttenuation(0.0f);
    mLight2->setQuadraticAttenuation(1.0f);

    // Setup light #3 as a spot light.
    // Valid values are from 0.0f to 90.0f plus the special value 180.0f (default) which disables the spot lighting.
    mLight3->setSpotCutoff(45.0f);
    mLight3->setSpotExponent(5.0f);
    mLight3->setSpotDirection(vl::fvec3(1,0,0));
    // A spot light is essentially a special positional light so these apply to spot lights as well.
    mLight3->setConstantAttenuation(1.0f);
    mLight3->setLinearAttenuation(0.0f);
    mLight3->setQuadraticAttenuation(0.0f);

    // Setup light's Transform to animate them
    mLight0_Transform = new vl::Transform;
    mLight1_Transform = new vl::Transform;
    mLight2_Transform = new vl::Transform;
    mLight3_Transform = new vl::Transform;
    rendering()->as<vl::Rendering>()->transform()->addChild(mLight0_Transform.get());
    rendering()->as<vl::Rendering>()->transform()->addChild(mLight1_Transform.get());
    rendering()->as<vl::Rendering>()->transform()->addChild(mLight2_Transform.get());
    rendering()->as<vl::Rendering>()->transform()->addChild(mLight3_Transform.get());
    // light 0..3 follow the relative transform
    mLight0->bindTransform(mLight0_Transform.get());
    mLight1->bindTransform(mLight1_Transform.get());
    mLight2->bindTransform(mLight2_Transform.get());
    mLight3->bindTransform(mLight3_Transform.get());
    // light 4 follows the camera
    mLight4->bindTransform(NULL);

    // add a white sphere for each light to have a better visual feedback of the light animation.
    vl::ref<vl::Geometry> bulb = vl::makeUVSphere(vl::vec3(0,0,0), 0.5f);
    vl::ref<vl::Effect> bulb_fx = new vl::Effect;
    bulb_fx->shader()->enable(vl::EN_DEPTH_TEST);
    bulb_fx->shader()->gocColor()->setValue(vl::white);
    sceneManager()->tree()->addActor(bulb.get(), bulb_fx.get(), mLight0_Transform.get());
    sceneManager()->tree()->addActor(bulb.get(), bulb_fx.get(), mLight1_Transform.get());
    sceneManager()->tree()->addActor(bulb.get(), bulb_fx.get(), mLight2_Transform.get());
    sceneManager()->tree()->addActor(bulb.get(), bulb_fx.get(), mLight3_Transform.get());

    // Simple text to inform the user of the currently active light setup.
    vl::ref<vl::Effect> text_fx = new vl::Effect;
    text_fx->shader()->enable(vl::EN_BLEND);
    mText = new vl::Text;
    mText->setFont( vl::defFontManager()->acquireFont("/font/bitstream-vera/VeraMono.ttf", 8) );
    mText->setAlignment( vl::AlignHCenter | vl::AlignTop );
    mText->setViewportAlignment( vl::AlignHCenter | vl::AlignTop );
    mText->translate(0,-10,0);
    sceneManager()->tree()->addActor(mText.get(), text_fx.get());
    updateText();
  }

  void updateText()
  {
    switch(mCameraLightType)
    {
      case Disable:          mText->setText("Camera light: Disable");           break;
      case DirectionalLight: mText->setText("Camera light: Directional Light"); break;
      case PositionalLight:  mText->setText("Camera light: Positional Light");  break;
      case SpotLight:        mText->setText("Camera light: Spot Light");        break;
    }
  }

  // Pressing space we cycle around 4 different light setup for light #4 (the one following the camera)
  void keyPressEvent(unsigned short ch, vl::EKey key)
  {
    BaseDemo::keyPressEvent(ch, key);
    if (key == vl::Key_Space)
    {
      mCameraLightType = (ECameraLightType)((mCameraLightType+1)%4);

      switch(mCameraLightType)
      {
      case Disable:
        // removes light #4 from the Shader
        mLightFx->shader()->eraseRenderState(mLight4.get(), 4);
        // you can also do:
        // mLightFx->shader()->removeRenderState(vl::RS_Light4);
        // or even
        // mLightFx->shader()->removeRenderState(vl::Light(4).type());
        break;
      case DirectionalLight:
        // add the light to the shader
        mLightFx->shader()->setRenderState(mLight4.get(), 4);
        // since the fourth component is 0 OpenGL considers this a direction
        mLight4->setPosition(vl::fvec4(0,0,1,0));
        // disable spot light
        mLight4->setSpotCutoff(180.0f);
        break;
      case PositionalLight:
        // add the light to the shader (just for clarity)
        mLightFx->shader()->setRenderState(mLight4.get(), 4);
        // since the fourth component is 1 OpenGL considers this a position
        mLight4->setPosition(vl::fvec4(0,0,0,1));
        // positional light attenuation
        mLight4->setConstantAttenuation(1.0f);
        mLight4->setLinearAttenuation(0.0f);
        mLight4->setQuadraticAttenuation(0.0f);
        // disable spot light
        mLight4->setSpotCutoff(180.0f);
        break;
      case SpotLight:
        // add the light to the shader (just for clarity)
        mLightFx->shader()->setRenderState(mLight4.get(), 4);
        // since the fourth component is 1 OpenGL considers this a position
        mLight4->setPosition(vl::fvec4(0,0,0,1));
        // positional light attenuation
        mLight4->setConstantAttenuation(1.0f);
        mLight4->setLinearAttenuation(0.0f);
        mLight4->setQuadraticAttenuation(0.0f);
        // enable and setup spot light
        mLight4->setSpotCutoff(25.0f);
        mLight4->setSpotExponent(40.0f);
        mLight4->setSpotDirection(vl::fvec3(0,0,-1));
        break;
      }
    }
    updateText();
  }

  // Animate the lights
  virtual void updateScene()
  {
    vl::real phase = 120.0f;
    vl::mat4 m;

    m.translate(5,1,0);
    m.rotate( phase*0.0f + 1.0f*vl::Time::currentTime()*vl::fPi*15, 0, 1.0f, 0);
    mLight0_Transform->setLocalMatrix(m);

    m.setIdentity();
    m.translate(5,1,0);
    m.rotate( phase*1.0f + 1.3f*vl::Time::currentTime()*vl::fPi*15, 0, 1.0f, 0);
    mLight1_Transform->setLocalMatrix(m);

    m.setIdentity();
    m.translate(5,1,0);
    m.rotate( phase*2.0f + 1.7f*vl::Time::currentTime()*vl::fPi*15, 0, 1.0f, 0);
    mLight2_Transform->setLocalMatrix(m);

    m.setIdentity();
    m.translate( sin(vl::Time::currentTime()*vl::fPi),0.5f,0);
    m.rotate( 0.5f*vl::Time::currentTime()*vl::fPi*15, 0, 1.0f, 0);
    mLight3_Transform->setLocalMatrix(m);
  }

protected:
  vl::ref< vl::Light > mLight0;
  vl::ref< vl::Light > mLight1;
  vl::ref< vl::Light > mLight2;
  vl::ref< vl::Light > mLight3;
  vl::ref< vl::Light > mLight4;
  vl::ref< vl::Transform > mLight0_Transform;
  vl::ref< vl::Transform > mLight1_Transform;
  vl::ref< vl::Transform > mLight2_Transform;
  vl::ref< vl::Transform > mLight3_Transform;
  vl::ref< vl::Effect > mLightFx;
  vl::ref< vl::Text > mText;
  enum ECameraLightType { Disable, DirectionalLight, PositionalLight, SpotLight } mCameraLightType;
};

// Have fun!

BaseDemo* Create_App_Lights() { return new App_Lights; }
