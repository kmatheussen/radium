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
#include <vlGraphics/SceneManagerActorKdTree.hpp>
#include <vlGraphics/Light.hpp>
#include <vlGraphics/Text.hpp>
#include <vlGraphics/FontManager.hpp>
#include <vlGraphics/GeometryPrimitives.hpp>

class App_CullingBenchmark: public BaseDemo
{
public:
  App_CullingBenchmark() {}

  virtual void initEvent()
  {
    vl::Log::notify(appletInfo());

    mSceneKdTree = new vl::SceneManagerActorKdTree;

    rendering()->as<vl::Rendering>()->sceneManagers()->push_back(mSceneKdTree.get());

    createScene(mActors);

    sceneManager()->tree()->actors()->set(mActors);
    sceneManager()->setCullingEnabled(true);

    mText = new vl::Text;
    mText->setFont( vl::defFontManager()->acquireFont("/font/bitstream-vera/VeraMono.ttf", 10, false) );
    mText->setAlignment(vl::AlignHCenter | vl::AlignTop);
    mText->setViewportAlignment(vl::AlignHCenter | vl::AlignTop);
    mText->setColor(vl::white);
    mText->translate(0,-10,0);
    vl::Actor* text_act = sceneManager()->tree()->addActor(mText.get(), new vl::Effect);
    text_act->effect()->shader()->enable(vl::EN_BLEND);
    mText->setText("Press 1, 2 or 3 to select culling method");
  }

  void keyPressEvent(unsigned short ch, vl::EKey key)
  {
    BaseDemo::keyPressEvent(ch,key);
    if (key == vl::Key_3)
    {
      sceneManager()->tree()->actors()->clear();
      vl::Time timer;
      timer.start();
      vl::Log::print("KdTree compilation in progres...\n");
      mSceneKdTree->tree()->buildKdTree(mActors);
      mSceneKdTree->setBoundsDirty(true);
      vl::Log::print( vl::Say("KdTree compilation time: %.1n, %.1n obj/s\n") << timer.elapsed() << mActors.size() / timer.elapsed() );

      vl::Actor* text_act = sceneManager()->tree()->addActor(mText.get(), new vl::Effect);
      text_act->effect()->shader()->enable(vl::EN_BLEND);
      mText->setText("KdTree culling");
    }
    else
    if (key == vl::Key_2)
    {
      sceneManager()->tree()->actors()->clear();
      sceneManager()->tree()->actors()->set(mActors);
      sceneManager()->setCullingEnabled(true);

      vl::Actor* text_act = sceneManager()->tree()->addActor(mText.get(), new vl::Effect);
      text_act->effect()->shader()->enable(vl::EN_BLEND);
      mText->setText("Simple culling");
    }
    else
    if (key == vl::Key_1)
    {
      sceneManager()->tree()->actors()->clear();
      sceneManager()->tree()->actors()->set(mActors);
      sceneManager()->setCullingEnabled(false);

      vl::Actor* text_act = sceneManager()->tree()->addActor(mText.get(), new vl::Effect);
      text_act->effect()->shader()->enable(vl::EN_BLEND);
      mText->setText("Culling off");
    }
  }

  void createScene(vl::ActorCollection& actors)
  {
    actors.clear();

    vl::ref<vl::Effect> effect = new vl::Effect;
    effect->shader()->enable(vl::EN_DEPTH_TEST);
    effect->shader()->enable(vl::EN_LIGHTING);
    effect->shader()->setRenderState( new vl::Light, 0 );

    vl::ref<vl::Geometry> ball = vl::makeUVSphere(vl::vec3(0,0,0),1,20,20);
    ball->computeNormals();

    int volume = 1000;

    for(int i=0; i<100000; ++i)
    {
      vl::ref<vl::Actor> actor = new vl::Actor(ball.get(),effect.get(),new vl::Transform);
      actors.push_back(actor.get());
      actor->transform()->setLocalMatrix(vl::mat4::getTranslation(rand()%volume-volume/2.0f, rand()%volume-volume/2.0f, rand()%volume-volume/2.0f));
      actor->transform()->computeWorldMatrix();
    }
  }

protected:
  vl::ref<vl::SceneManagerActorKdTree> mSceneKdTree;
  vl::ActorCollection mActors;
  vl::ref<vl::Text> mText;
};

// Have fun!

BaseDemo* Create_App_CullingBenchmark() { return new App_CullingBenchmark; }
