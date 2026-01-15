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
#include <vlGraphics/Text.hpp>
#include <vlGraphics/FontManager.hpp>

using namespace vl;

class App_RenderOrder: public BaseDemo
{
public:
  App_RenderOrder(): mTestNumber(0) {}

  void initEvent()
  {
    vl::Log::notify(appletInfo());

    mTransform1 = new Transform;
    mTransform2 = new Transform;
    rendering()->as<Rendering>()->transform()->addChild(mTransform1.get());
    rendering()->as<Rendering>()->transform()->addChild(mTransform2.get());

    mText = new Text;
    mText->setText("...");
    mText->setFont( defFontManager()->acquireFont("/font/bitstream-vera/VeraMono.ttf", 10) );
    mText->setAlignment( AlignHCenter | AlignTop );
    mText->setViewportAlignment( AlignHCenter| AlignTop );
    mText->setTextAlignment(TextAlignLeft);
    mText->translate(0,-5,0);
    mText->setColor(white);
    mText->setBackgroundColor(black);
    mText->setBackgroundEnabled(true);

    populateScene();
  }

  void populateScene()
  {
    // erase the scene

    sceneManager()->tree()->actors()->clear();

    // regenerate the scene

    ref<Effect> text_fx = new Effect;
    text_fx->shader()->enable(EN_BLEND);
    sceneManager()->tree()->addActor(mText.get(), text_fx.get());

    ref<Geometry> box    = makeBox(vec3(0,0,-2), 1,1,1);
    ref<Geometry> sphere = makeUVSphere(vec3(0,0,0),0.5f);
    ref<Geometry> cone   = makeCone(vec3(0,0,+2), 1, 1, 10, true);
    box   ->computeNormals();
    sphere->computeNormals();
    cone  ->computeNormals();

    ref<Light> light = new Light;

    if (mTestNumber == 0)
    {
      // rendering order: 
      // red -> yellow
      // box -> sphere -> cone
      mText->setText("red -> yellow\nbox -> sphere -> cone");

      ref<Effect> red_fx = new Effect;
      red_fx->setRenderRank(1);
      red_fx->shader()->disable(EN_DEPTH_TEST);
      red_fx->shader()->enable(EN_CULL_FACE);
      red_fx->shader()->enable(EN_LIGHTING);
      red_fx->shader()->setRenderState( light.get(), 0 );
      red_fx->shader()->gocMaterial()->setDiffuse(red);

      sceneManager()->tree()->addActor( box.get(),    red_fx.get(), mTransform1.get() )->setRenderRank( 1 );
      sceneManager()->tree()->addActor( sphere.get(), red_fx.get(), mTransform1.get() )->setRenderRank( 2 );
      sceneManager()->tree()->addActor( cone.get(),   red_fx.get(), mTransform1.get() )->setRenderRank( 3 );

      ref<Effect> yellow_fx = new Effect;
      yellow_fx->setRenderRank(2);
      yellow_fx->shader()->disable(EN_DEPTH_TEST);
      yellow_fx->shader()->enable(EN_CULL_FACE);
      yellow_fx->shader()->enable(EN_LIGHTING);
      yellow_fx->shader()->setRenderState( light.get(), 0 );
      yellow_fx->shader()->gocMaterial()->setDiffuse(yellow);

      sceneManager()->tree()->addActor( box.get(),  yellow_fx.get(), mTransform2.get() )->setRenderRank( 1 );
      sceneManager()->tree()->addActor( cone.get(), yellow_fx.get(), mTransform2.get() )->setRenderRank( 2 );
    }
    else
    {
      /* transp_fx */

      ref<Effect> transp_fx = new Effect;
      transp_fx->shader()->enable(EN_BLEND);
      transp_fx->shader()->enable(EN_DEPTH_TEST);
      transp_fx->shader()->enable(EN_CULL_FACE);
      transp_fx->shader()->enable(EN_LIGHTING);
      transp_fx->shader()->setRenderState( light.get(), 0 );
      transp_fx->shader()->gocMaterial()->setDiffuse(blue);
      transp_fx->shader()->gocMaterial()->setTransparency(0.5f);

      /* solid_fx */

      ref<Effect> solid_fx = new Effect;
      solid_fx->shader()->enable(EN_DEPTH_TEST);
      solid_fx->shader()->enable(EN_CULL_FACE);
      solid_fx->shader()->enable(EN_LIGHTING);
      solid_fx->shader()->setRenderState( light.get(), 0 );
      solid_fx->shader()->gocMaterial()->setDiffuse(yellow);

      /* add to the scene in an intertwined way */
      sceneManager()->tree()->addActor( box.get(),    transp_fx.get(), mTransform1.get() );
      sceneManager()->tree()->addActor( box.get(),    solid_fx.get(), mTransform2.get() );
      sceneManager()->tree()->addActor( sphere.get(), transp_fx.get(), mTransform1.get() );
      sceneManager()->tree()->addActor( cone.get(),   solid_fx.get(), mTransform2.get() );
      sceneManager()->tree()->addActor( cone.get(),   transp_fx.get(), mTransform1.get() );

      if (mTestNumber == 1) // depth-sort only alpha blended objects (default settings)
      {
        mText->setText("depth-sort only alpha blended objects (default settings)");
        ref<RenderQueueSorterStandard> list_sorter = new RenderQueueSorterStandard;
        list_sorter->setDepthSortMode(AlphaDepthSort);
        rendering()->as<Rendering>()->setRenderQueueSorter( list_sorter.get() );
      }
      else
      if (mTestNumber == 2) // depth-sort solid and alpha blended objects
      {
        solid_fx->shader()->disable(EN_DEPTH_TEST);
        mText->setText("depth-sort solid and alpha blended objects");
        ref<RenderQueueSorterStandard> list_sorter = new RenderQueueSorterStandard;
        list_sorter->setDepthSortMode(AlwaysDepthSort);
        rendering()->as<Rendering>()->setRenderQueueSorter( list_sorter.get() );
      }
      else
      if (mTestNumber == 3) // depth-sort alpha blended back to front | depth-sort solid object front to back
      {
        mText->setText("depth-sort alpha blended back to front\ndepth-sort solid object front to back");
        ref<RenderQueueSorterOcclusion> list_sorter = new RenderQueueSorterOcclusion;
        rendering()->as<Rendering>()->setRenderQueueSorter( list_sorter.get() );
      }
      else
      if (mTestNumber == 4) // no depth sorting
      {
        mText->setText("no depth sorting");
        ref<RenderQueueSorterStandard> list_sorter = new RenderQueueSorterStandard;
        list_sorter->setDepthSortMode(NeverDepthSort);
        rendering()->as<Rendering>()->setRenderQueueSorter( list_sorter.get() );
      }
      else
      if (mTestNumber == 5) // no sorting at all
      {
        mText->setText("no sorting at all");
        rendering()->as<Rendering>()->setRenderQueueSorter( NULL );
      }
    }
  }

  void updateScene()
  {
    mTransform1->setLocalMatrix( mat4::getRotation(Time::currentTime() * 5,      0, 1, 0 ) );
    mTransform2->setLocalMatrix( mat4::getRotation(Time::currentTime() * 5 + 90, 0, 1, 0 ) );
  }

  void keyPressEvent(unsigned short, EKey key)
  {
    if (key == Key_Left)
    {
      --mTestNumber;
      mTestNumber = clamp(mTestNumber,0,5);
      populateScene();
    }
    else
    if(key == Key_Right)
    {
      ++mTestNumber;
      mTestNumber = clamp(mTestNumber,0,5);
      populateScene();
    }
  }

public:
  ref<Text> mText;
  ref<Transform> mTransform1;
  ref<Transform> mTransform2;
  int mTestNumber;
};

// Have fun!

BaseDemo* Create_App_RenderOrder() { return new App_RenderOrder; }
