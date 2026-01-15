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
#include <vlGraphics/Geometry.hpp>
#include <vlGraphics/Billboard.hpp>
#include <vlGraphics/Light.hpp>
#include <ctime>

class App_Billboards: public BaseDemo
{
public:
  // initialization
  void initEvent()
  {
    vl::Log::notify(appletInfo());
    srand((unsigned int)time(NULL));

    generateTrees (100.0f, 500);
    generateStars (100.0f, 500);
    generateGround(100.0f);
    generateLunapark();
  }

  void generateTrees(float side, int tree_count)
  {
    // simple effect to render a tree billboard
    vl::ref<vl::Effect> effect = new vl::Effect;
    // speedup tip: this allows VL to batch all the trees together greatly speeding up the rendering and avoiding switching textures back and forth with the stars.
    effect->setRenderRank(2);
    effect->shader()->setRenderState( new vl::Light, 0 );
    effect->shader()->enable(vl::EN_BLEND);
    effect->shader()->enable(vl::EN_DEPTH_TEST);
    effect->shader()->gocDepthMask()->set(false);
    effect->shader()->enable(vl::EN_LIGHTING);
    effect->shader()->gocLight(0)->setLinearAttenuation(0.025f);
    effect->shader()->gocTextureSampler(0)->setTexture( new vl::Texture("images/tree.png") );

    vl::ref<vl::Geometry> tree = generateQuad();
    tree->transform( vl::mat4::getTranslation(0,+0.5f,0) );
    tree->transform( vl::mat4::getScaling(1.0f,+2.0f,1.0f) );
    tree->computeNormals();

    for(int i=0; i<tree_count; i++)
    {
      // new billboard
      vl::ref<vl::Billboard> billboard = new vl::Billboard;
      // set axis-aligned billboard type: rotate the billboard towards the camera but only around the specified axis.
      billboard->setType(vl::BT_AxisAlignedBillboard);
      billboard->setAxis(vl::vec3(0,1.0f,0));
      // bind billboard to the transform tree
      rendering()->as<vl::Rendering>()->transform()->addChild(billboard.get());
      // generate a random position
      vl::real x = vl::random(-side/2.0f,+side/2.0f);
      vl::real y = 0;
      vl::real z = vl::random(-side/2.0f,+side/2.0f);
      billboard->setPosition( vl::vec3(x,y,z) );
      // add the tree actor
      sceneManager()->tree()->addActor(tree.get(), effect.get(), billboard.get());
    }
  }

  void generateStars(float side, int star_count)
  {
    // simple effect to render a star billboard
    vl::ref<vl::Effect> effect = new vl::Effect;
    // speedup tip: this allows VL to batch all the stars together greatly speeding up the rendering and avoiding switching textures back and forth with the trees.
    effect->setRenderRank(1);
    effect->shader()->enable(vl::EN_BLEND);
    effect->shader()->enable(vl::EN_DEPTH_TEST);
    effect->shader()->gocTextureSampler(0)->setTexture( new vl::Texture("images/sun.png", vl::TF_RGBA, true) );

    vl::ref<vl::Geometry> star = generateQuad();

    for(int i=0; i<star_count; i++)
    {
      // new billboard
      vl::ref<vl::Billboard> billboard = new vl::Billboard;
      // set spherical billboard type: orient the object always towards the camera.
      billboard->setType(vl::BT_SphericalBillboard);
      // add billboard to the transform tree
      rendering()->as<vl::Rendering>()->transform()->addChild(billboard.get());
      // compute a random point on the skydome
      vl::real x = vl::random(-1.0f,+1.0f);
      vl::real y = vl::random(0,2.0f);
      vl::real z = vl::random(-1.0f,+1.0f);
      vl::vec3 n(x,y,z);
      n.normalize();
      n = n * sqrt(side*side/2.0f);
      n.y() *= 0.2f;
      // set the billboard position and rotation center.
      billboard->setPosition( n );
      // add the star actor
      sceneManager()->tree()->addActor(star.get(), effect.get(), billboard.get());
    }
  }

  // generates the ground
  void generateGround(float side)
  {
    // orange effect using lighting
    vl::ref<vl::Effect> effect = new vl::Effect;
    effect->shader()->setRenderState( new vl::Light, 0 );
    effect->shader()->enable(vl::EN_LIGHTING);
    effect->shader()->gocLight(0)->setLinearAttenuation(0.025f);
    effect->shader()->gocMaterial()->setDiffuse(vl::orange);
    effect->shader()->enable(vl::EN_DEPTH_TEST);

    // use a simple plane to render the ground
    vl::ref<vl::Geometry> geom = vl::makeGrid(vl::vec3(0,0,0), side, side, 200, 200);
    geom->computeNormals();
    sceneManager()->tree()->addActor( geom.get(), effect.get(), NULL);
  }

  // returns a Geometry that renders a single quad
  vl::ref<vl::Geometry> generateQuad()
  {
    // geometry
    vl::ref<vl::Geometry> quad = new vl::Geometry;
    // quad vertices
    vl::ref<vl::ArrayFloat3> vert = new vl::ArrayFloat3;
    vert->resize( 4 );
    quad->setVertexArray(vert.get());
    vert->at(0) = vl::fvec3( -0.5f, -0.5f, 0.0f );
    vert->at(1) = vl::fvec3( +0.5f, -0.5f, 0.0f );
    vert->at(2) = vl::fvec3( +0.5f, +0.5f, 0.0f );
    vert->at(3) = vl::fvec3( -0.5f, +0.5f, 0.0f );
    // texture coords
    vl::ref<vl::ArrayFloat2> texc = new vl::ArrayFloat2;
    texc->resize( 4 );
    quad->setTexCoordArray(0,texc.get());
    texc->at(0) = vl::fvec2( 0.0f, 0.0f );
    texc->at(1) = vl::fvec2( 1.0f, 0.0f );
    texc->at(2) = vl::fvec2( 1.0f, 1.0f );
    texc->at(3) = vl::fvec2( 0.0f, 1.0f );
    // quad primitive
    quad->drawCalls()->push_back( new vl::DrawArrays(vl::PT_TRIANGLE_FAN, 0, 4) );
    return quad;
  }

  // This function demonstrates how a billboard behaves when it is put under an animated transform hierarchy.
  // Note how the cube always faces the camera even if it follows its parent's rotation.
  void generateLunapark()
  {
    vl::ref<vl::Effect> effect = new vl::Effect;
    effect->shader()->setRenderState( new vl::Light, 0 );
    effect->shader()->enable(vl::EN_BLEND);
    effect->shader()->enable(vl::EN_DEPTH_TEST);
    effect->shader()->enable(vl::EN_LIGHTING);

    vl::ref<vl::Geometry>  arm_g = vl::makeCylinder(vl::vec3(0,0,0), 0.5f, 4.0f);
    arm_g->computeNormals();
    vl::ref<vl::Transform> arm1_t = new vl::Transform;
    vl::ref<vl::Transform> arm2_t = new vl::Transform;
    rendering()->as<vl::Rendering>()->transform()->addChild(arm1_t.get());
    arm1_t->addChild(arm2_t.get());
    arm2_t->setLocalMatrix(vl::mat4::getTranslation(0.0f,2.0f,0.0f) * vl::mat4::getRotation(90,0,0,1) * vl::mat4::getTranslation(0.0f,2.0f,0.0f));

    sceneManager()->tree()->addActor( arm_g.get(), effect.get(), arm1_t.get());
    sceneManager()->tree()->addActor( arm_g.get(), effect.get(), arm2_t.get());

    vl::ref<vl::Geometry> box = vl::makeBox(vl::vec3(0,-0.75f,0), 1, 1, 1);
    box->computeNormals();

    // the billboard
    vl::ref<vl::Billboard> billboard = new vl::Billboard;
    // use an axis aligned billboard
    billboard->setType(vl::BT_AxisAlignedBillboard);
    // the axis is always in world coordinates
    billboard->setAxis(vl::vec3(0,1,0));
    // remember that "position" is relative to the billboard's parent's coordinates
    billboard->setPosition(0,2,0);
    // add the billboard to its transform parent
    arm2_t->addChild(billboard.get());
    // add the box actor
    sceneManager()->tree()->addActor( box.get(), effect.get(), billboard.get());

    // to be animated below
    mArm1Transform = arm1_t;
  }

  // animate the lunapark
  void updateScene()
  {
    mArm1Transform->setLocalMatrix( 
      vl::mat4::getRotation(vl::Time::currentTime()*45,0,1,0) * 
      vl::mat4::getTranslation(0.0f,2.0f,0.0f)
    );
  }

protected:
  vl::ref<vl::Transform> mArm1Transform;
};

// Have fun!

BaseDemo* Create_App_Billboards() { return new App_Billboards; }
