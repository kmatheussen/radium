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
#include <vlGraphics/PolygonSimplifier.hpp>
#include <vlGraphics/Geometry.hpp>
#include <vlGraphics/Light.hpp>
#include <vlGraphics/Text.hpp>
#include <vlGraphics/FontManager.hpp>
#include <vlGraphics/DrawElements.hpp>

using namespace vl;

static const float golden = (1.0f + sqrt(5.0f)) / 2.0f; // sezione aurea
static const float ratios[] = { 1.0f, 1.0f / pow(golden, 1), 1.0f / pow(golden, 2), 1.0f / pow(golden, 3), 1.0f / pow(golden, 4), 1.0f / pow(golden, 5) };
static const int stages = sizeof(ratios) / sizeof(ratios[0]);

class App_PolygonReduction: public BaseDemo
{
public:
  App_PolygonReduction(const String& path): mFileName(path), mActiveActor(0) {}

  void initEvent()
  {
    vl::Log::notify(appletInfo());

    /* Transform */
    mTransform = new Transform;
    mTransform->setLocalAndWorldMatrix( mat4::getRotation(20, 1,0,0) );

    /* Effect */
    mEffect = new Effect;

    /* solid shader */
    mEffect->shader(0,0)->setRenderState( new Light, 0 );
    mEffect->shader(0,0)->enable(EN_LIGHTING);
    mEffect->shader(0,0)->gocLightModel()->setTwoSide(true);
    mEffect->shader(0,0)->gocMaterial()->setDiffuse(vl::royalblue);
    mEffect->shader(0,0)->gocMaterial()->setSpecular(vl::white);
    mEffect->shader(0,0)->gocMaterial()->setShininess(50.0f);
    mEffect->shader(0,0)->enable(EN_DEPTH_TEST);

    /* wireframe shader */
#if defined(VL_OPENGL)
    mEffect->lod(0)->push_back( new Shader );
    mEffect->shader(0,1)->setRenderState( new Light, 0 );
    mEffect->shader(0,1)->enable(EN_LIGHTING);
    mEffect->shader(0,1)->gocLightModel()->setTwoSide(true);
    mEffect->shader(0,1)->gocMaterial()->setDiffuse(vl::lightgreen);
    mEffect->shader(0,1)->gocPolygonMode()->set(vl::PM_LINE, vl::PM_LINE);
    mEffect->shader(0,1)->gocPolygonOffset()->set(-0.5f, 0);
    mEffect->shader(0,1)->enable(vl::EN_POLYGON_OFFSET_LINE);
    mEffect->shader(0,1)->enable(EN_DEPTH_TEST);
#endif

    /* load model */
    ref<ResourceDatabase> res_db;
    res_db = loadResource(mFileName); 
    if ( res_db && res_db->count<Geometry>() ) 
      loadSimplifyGeometry( res_db->get<Geometry>(0) );

    /* triangle count label */
    mText = new Text;
    mText->setMatrix( fmat4::getTranslation(0,5,0) );
    mText->setText( "Triangle Count: -" );
    mText->setFont( defFontManager()->acquireFont("/font/bitstream-vera/VeraMono.ttf", 10) );
    mText->setAlignment( AlignHCenter | AlignBottom );
    mText->setViewportAlignment( AlignHCenter | AlignBottom );
    mText->setBackgroundEnabled(true);
    mText->setBackgroundColor(fvec4(0,0,0,0.5f));
    ref<Effect> effect = new Effect;
    effect->shader()->enable(EN_BLEND);
    mTextActor = sceneManager()->tree()->addActor(mText.get(), effect.get());
  }

  void loadSimplifyGeometry(Geometry* geom)
  {
    // -------------- simplification -------------------

    PolygonSimplifier simplifier;
    simplifier.setQuick(false);
    simplifier.setVerbose(true);

    simplifier.setIntput( geom );

    for(int i=0; i<stages; ++i)
      simplifier.targets().push_back( size_t(geom->vertexArray()->size() * ratios[i]) );

    simplifier.simplify();

    for(int i=0; i<stages; ++i)
      mGeom[i] = simplifier.output()[i];

    // -------------- simplification end -------------------

    /* compute normals & create actors for each simplification level */
    for(int i=0; i<stages; ++i)
    {
      mGeom[i]->computeNormals();
      mActors[i] = new Actor( mGeom[i].get(), mEffect.get(), mTransform.get() );
    }

    sceneManager()->tree()->actors()->clear();
    sceneManager()->tree()->addActor(mActors[0].get());

    /* position the camera to nicely see the scene */
    trackball()->adjustView( sceneManager(), vec3(0,0,1)/*direction*/, vec3(0,1,0)/*up*/, 0.75f/*bias*/ );
  }

  virtual void updateScene()
  {
    mActiveActor = int(Time::currentTime()) % stages;
    /* activate current actor */
    sceneManager()->tree()->actors()->clear();
    sceneManager()->tree()->addActor( mActors[mActiveActor].get() );
    sceneManager()->tree()->addActor( mTextActor.get() );
    /* update label */
    float percent = 100.0f * mGeom[mActiveActor]->drawCalls()->at(0)->as<vl::DrawElementsUInt>()->indexBuffer()->size() / mGeom[0]->drawCalls()->at(0)->as<vl::DrawElementsUInt>()->indexBuffer()->size();
    mText->setText( Say("Triangle Count: %n (%.1n%%)") << mGeom[mActiveActor]->drawCalls()->at(0)->as<vl::DrawElementsUInt>()->indexBuffer()->size() / 3 << percent );
  }

  void fileDroppedEvent(const std::vector<vl::String>& files)
  {
    ref<ResourceDatabase> res_db;
    res_db = loadResource(files[0]); 
    if ( res_db && res_db->count<Geometry>() ) 
      loadSimplifyGeometry( res_db->get<Geometry>(0) );
  }

protected:
  ref<Transform> mTransform;
  String mFileName;
  ref<Effect> mEffect;
  ref<Actor> mActors[stages];
  int mActiveActor;
  ref<Actor> mTextActor; 
  ref<Text> mText;
  ref<Geometry> mGeom[stages];
};

// Have fun!

BaseDemo* Create_App_PolygonReduction(const String& path) { return new App_PolygonReduction(path); }
