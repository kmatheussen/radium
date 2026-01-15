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
#include <vlGraphics/GeometryPrimitives.hpp>
#include <vlGraphics/Light.hpp>
#include <vlGraphics/Text.hpp>
#include <vlGraphics/FontManager.hpp>
#include <vlGraphics/SceneManagerActorKdTree.hpp>
#include <vlGraphics/SceneManagerActorTree.hpp>

class App_KdTreeView: public BaseDemo
{
public:
  virtual void initEvent()
  {
    vl::Log::notify(appletInfo());

    mSceneKdTree = new vl::SceneManagerActorKdTree;
    rendering()->as<vl::Rendering>()->sceneManagers()->push_back(mSceneKdTree.get());

    mText = new vl::Text;
    mText->setFont( vl::defFontManager()->acquireFont("/font/bitstream-vera/VeraMono.ttf", 10, false) );
    mText->setAlignment(vl::AlignHCenter | vl::AlignTop);
    mText->setViewportAlignment(vl::AlignHCenter | vl::AlignTop);
    mText->setColor(vl::white);
    mText->translate(0,-10,0);
    mTextActor = sceneManager()->tree()->addActor(mText.get(), new vl::Effect);
    mTextActor->effect()->shader()->enable(vl::EN_BLEND);

    mTestNumber = 0;
    mViewDepth  = -1;

    createScene();
    mText->setText( mText->text() + "\nPress left/right to change the test number\nPress up/down to change the visible tree level");
  }

  void createScene()
  {
    vl::ActorCollection actors;

    vl::ref<vl::Effect> effect = new vl::Effect;
    effect->shader()->enable(vl::EN_DEPTH_TEST);
    effect->shader()->enable(vl::EN_LIGHTING);
    effect->shader()->setRenderState( new vl::Light, 0 );

    vl::ref<vl::Geometry> ball = vl::makeUVSphere(vl::vec3(0,0,0),1,20,20);
    ball->computeNormals();

    vl::ref<vl::Geometry> pyramid = vl::makePyramid(vl::vec3(0,0,0),2,2);
    pyramid->computeNormals();

    switch(mTestNumber)
    {
      case 0:
      {
        for(int i=-3; i<+3; ++i)
        {
          vl::ref<vl::Actor> actor = new vl::Actor(ball.get(),effect.get(),new vl::Transform);
          actors.push_back(actor.get());
          vl::mat4 mat = vl::mat4::getTranslation(i*2.0f,0,0);
          actor->transform()->setLocalMatrix(mat);
          actor->transform()->computeWorldMatrix();
        }
        break;
      }
      case 1:
      {
        for(int i=-3; i<+3; ++i)
        {
          vl::ref<vl::Actor> actor = new vl::Actor(ball.get(),effect.get(),new vl::Transform);
          actors.push_back(actor.get());
          vl::mat4 mat = vl::mat4::getTranslation(0,i*2.0f,0);
          actor->transform()->setLocalMatrix(mat);
          actor->transform()->computeWorldMatrix();
        }
        break;
      }
      case 2:
      {
        for(int i=-3; i<+3; ++i)
        {
          vl::ref<vl::Actor> actor = new vl::Actor(ball.get(),effect.get(),new vl::Transform);
          actors.push_back(actor.get());
          vl::mat4 mat = vl::mat4::getTranslation(0,0,i*2.0f);
          actor->transform()->setLocalMatrix(mat);
          actor->transform()->computeWorldMatrix();
        }
        break;
      }
      case 3:
      {
        for(int x=-4; x<+4; ++x)
        {
          for(int y=-4; y<+4; ++y)
          {
            vl::ref<vl::Actor> actor = new vl::Actor(pyramid.get(),effect.get(),new vl::Transform);
            actors.push_back(actor.get());
            vl::mat4 mat = vl::mat4::getTranslation(x*2.0f,0,y*2.0f);
            actor->transform()->setLocalMatrix(mat);
            actor->transform()->computeWorldMatrix();
          }
        }
        break;
      }
      case 4:
      {
        for(int x=-4; x<+4; ++x)
        {
          for(int y=-4; y<+4; ++y)
          {
            vl::ref<vl::Actor> actor = new vl::Actor(pyramid.get(),effect.get(),new vl::Transform);
            actors.push_back(actor.get());
            vl::mat4 mat = vl::mat4::getTranslation(x*2.0f,y*2.0f,0);
            actor->transform()->setLocalMatrix(mat);
            actor->transform()->computeWorldMatrix();
          }
        }
        break;
      }
      case 5:
      {
        for(int x=-4; x<+4; ++x)
        {
          for(int y=-4; y<+4; ++y)
          {
            vl::ref<vl::Actor> actor = new vl::Actor(pyramid.get(),effect.get(),new vl::Transform);
            actors.push_back(actor.get());
            vl::mat4 mat = vl::mat4::getTranslation(0,x*2.0f,y*2.0f);
            actor->transform()->setLocalMatrix(mat);
            actor->transform()->computeWorldMatrix();
          }
        }
        break;
      }
      case 6:
      {
        for(int i=0; i<10; ++i)
        {
          vl::ref<vl::Actor> actor = new vl::Actor(pyramid.get(),effect.get(),new vl::Transform);
          actors.push_back(actor.get());
          vl::mat4 mat = vl::mat4::getTranslation(rand()%8-4.0f, rand()%8-4.0f, rand()%8-4.0f);
          actor->transform()->setLocalMatrix(mat);
          actor->transform()->computeWorldMatrix();
        }
        break;
      }
    }

    /* compile ActorKdTree */
    mSceneKdTree->tree()->buildKdTree(actors);

    updateKdTreeView();
  }

  void updateKdTreeView()
  {
    /* regenerate scene manager containing the text and the boxes representing the ActorKdTree nodes */
    sceneManager()->tree()->actors()->clear();
    
    sceneManager()->tree()->addActor( mTextActor.get() );
    mText->setText( vl::Say("Test #%n, Level #%n") << mTestNumber << mViewDepth );

    viewTreeNodes(mSceneKdTree->tree(),0);
  }

  void viewTreeNodes(vl::ActorTreeAbstract* tree, int level)
  {
    if (tree && !tree->aabb().isNull())
    {
      if (level == mViewDepth || mViewDepth == -1)
      {
        vl::ref<vl::Effect> effect = new vl::Effect;
        effect->shader()->gocPolygonMode()->set(vl::PM_LINE, vl::PM_LINE);
        effect->shader()->enable(vl::EN_DEPTH_TEST);
        effect->shader()->gocColor()->setValue(vl::gold);
        vl::ref<vl::Geometry> box = vl::makeBox(tree->aabb());
        sceneManager()->tree()->addActor(box.get(), effect.get());

        if (mViewDepth != -1)
          return;
      }

      for(int i=0; i<tree->childrenCount(); ++i)
        viewTreeNodes(tree->child(i), level+1);
    }
  }  

  void keyPressEvent(unsigned short ch, vl::EKey key)
  {
    BaseDemo::keyPressEvent(ch,key);
    switch(key)
    {
      case vl::Key_Up:    mViewDepth--;  break;
      case vl::Key_Down:  mViewDepth++;  break;
      case vl::Key_Left:  mTestNumber--; break;
      case vl::Key_Right: mTestNumber++; break;
      default:
        break;
    }

    mViewDepth  = vl::clamp(mViewDepth,-1,10);
    mTestNumber = vl::clamp(mTestNumber,0,10);
    switch(key)
    {
      case vl::Key_Up:    updateKdTreeView(); break;
      case vl::Key_Down:  updateKdTreeView(); break;
      case vl::Key_Left:  createScene();      break;
      case vl::Key_Right: createScene();      break;
      default:
        break;
    }
  }

protected:
  vl::ref<vl::SceneManagerActorKdTree> mSceneKdTree;
  vl::ref<vl::Text> mText;
  vl::ref<vl::Actor> mTextActor;
  int mTestNumber;
  int mViewDepth;
};

// Have fun!

BaseDemo* Create_App_KdTreeView() { return new App_KdTreeView; }
