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

#ifndef App_DrawCalls_INCLUDE_ONCE
#define App_DrawCalls_INCLUDE_ONCE

#include "BaseDemo.hpp"
#include <vlGraphics/GeometryPrimitives.hpp>
#include <vlGraphics/MultiDrawElements.hpp>
#include <vlGraphics/DrawRangeElements.hpp>
#include <vlGraphics/SceneManagerActorTree.hpp>
#include <vlGraphics/Actor.hpp>
#include <vlGraphics/Effect.hpp>
#include <vlGraphics/Light.hpp>
#include <vlGraphics/TriangleStripGenerator.hpp>
#include <vlGraphics/RenderingTree.hpp>

using namespace vl;

class App_DrawCalls: public BaseDemo
{
  void setupScene_MultiDrawElements()
  {
    ref<Geometry> torus1 = vl::makeTorus( vec3(-10, -9, 0), 8.0f, 1.0f, 20, 20 );
    ref<Geometry> torus2 = vl::makeTorus( vec3(  0, -9, 0), 8.0f, 2.0f, 10, 10 );
    ref<Geometry> torus3 = vl::makeTorus( vec3(+10, -9, 0), 8.0f, 3.0f, 7, 7 );

    // merge vertices
    ref<ArrayFloat3> vert = new ArrayFloat3;
    ref<ArrayFloat3> torus1_vert = vl::cast<ArrayFloat3>(torus1->vertexArray());
    ref<ArrayFloat3> torus2_vert = vl::cast<ArrayFloat3>(torus2->vertexArray());
    ref<ArrayFloat3> torus3_vert = vl::cast<ArrayFloat3>(torus3->vertexArray());

    vert->resize( torus1_vert->size() + torus2_vert->size() + torus3_vert->size() );
    memcpy( vert->ptr(), torus1_vert->ptr(), torus1_vert->bytesUsed() );
    memcpy( vert->ptr() + torus1_vert->bytesUsed(), torus2_vert->ptr(), torus2_vert->bytesUsed() );
    memcpy( vert->ptr() + torus1_vert->bytesUsed() + torus2_vert->bytesUsed(), torus3_vert->ptr(), torus3_vert->bytesUsed() );

    // merge indices
    ref<MultiDrawElementsUInt> mde = new MultiDrawElementsUInt(PT_QUADS);
    ref<DrawCall> torus1_dc = torus1->drawCalls()->at(0);
    ref<DrawCall> torus2_dc = torus2->drawCalls()->at(0);
    ref<DrawCall> torus3_dc = torus3->drawCalls()->at(0);

    int torus1_index_count = (int)torus1_dc->countIndices();
    int torus2_index_count = (int)torus2_dc->countIndices();
    int torus3_index_count = (int)torus3_dc->countIndices();
    mde->indexBuffer()->resize( torus1_index_count + torus2_index_count + torus3_index_count );

    MultiDrawElementsUInt::index_type* p_idx = mde->indexBuffer()->begin();
    for( IndexIterator it = torus1_dc->indexIterator(); it.hasNext(); it.next(), ++p_idx )
      *p_idx = it.index();

    for( IndexIterator it = torus2_dc->indexIterator(); it.hasNext(); it.next(), ++p_idx )
      *p_idx = it.index() + (int)torus1_vert->size();

    for( IndexIterator it = torus3_dc->indexIterator(); it.hasNext(); it.next(), ++p_idx )
      *p_idx = it.index() + (int)torus1_vert->size() + (int)torus2_vert->size();

    VL_CHECK(p_idx == mde->indexBuffer()->end());

    // define how many indices for each draw call
    GLsizei count_vector[] = { torus1_index_count, torus2_index_count, torus3_index_count };
    mde->setCountVector( count_vector, 3 );

    ref<Geometry> geom = new Geometry;
    geom->setVertexArray( vert.get() );
    geom->drawCalls()->push_back( mde.get() );

    // compute normals must be done after the draw calls have been added.
    geom->computeNormals();

    ref<Effect> fx = new Effect;
    fx->shader()->setRenderState( new Light, 0 );
    fx->shader()->enable(vl::EN_LIGHTING);
    fx->shader()->enable(vl::EN_DEPTH_TEST);
    fx->shader()->gocMaterial()->setDiffuse( vl::pink );

    sceneManager()->tree()->addActor( geom.get(), fx.get(), NULL );
  }

  void setupScene_BaseVertex()
  {
    // creat three meshes with the same topology but different shapes
    ref<Geometry> torus1 = vl::makeTorus( vec3(-10, 9, 0), 6.0f, 1.0f, 15, 15 );
    ref<Geometry> torus2 = vl::makeTorus( vec3(  0, 9, 0), 7.0f, 2.0f, 15, 15 );
    ref<Geometry> torus3 = vl::makeTorus( vec3(+10, 9, 0), 8.0f, 3.0f, 15, 15 );

    // merge vertices
    ref<ArrayFloat3> vert = new ArrayFloat3;
    ref<ArrayFloat3> torus1_vert = vl::cast<ArrayFloat3>(torus1->vertexArray());
    ref<ArrayFloat3> torus2_vert = vl::cast<ArrayFloat3>(torus2->vertexArray());
    ref<ArrayFloat3> torus3_vert = vl::cast<ArrayFloat3>(torus3->vertexArray());

    vert->resize( torus1_vert->size() + torus2_vert->size() + torus3_vert->size() );
    memcpy( vert->ptr(), torus1_vert->ptr(), torus1_vert->bytesUsed() );
    memcpy( vert->ptr() + torus1_vert->bytesUsed(), torus2_vert->ptr(), torus2_vert->bytesUsed() );
    memcpy( vert->ptr() + torus1_vert->bytesUsed() + torus2_vert->bytesUsed(), torus3_vert->ptr(), torus3_vert->bytesUsed() );

    ref<Geometry> geom = new Geometry;
    geom->setVertexArray( vert.get() );

    // create three DrawElementsUInt sharing the same index buffer and using base-vertex functionality.
    ref<DrawElementsUInt> torus1_de = vl::cast<DrawElementsUInt>(torus1->drawCalls()->at(0));

    // (1)
    ref<DrawElementsUInt> de1 = new DrawElementsUInt(PT_QUADS);
    de1->setIndexBuffer( torus1_de->indexBuffer() );
    de1->setBaseVertex(0);
    geom->drawCalls()->push_back( de1.get() );
    // (2)
    ref<DrawElementsUInt> de2 = new DrawElementsUInt(PT_QUADS);
    de2->setIndexBuffer( torus1_de->indexBuffer() );
    de2->setBaseVertex( (int)torus1_vert->size() ); // skip the vertices of the first torus
    geom->drawCalls()->push_back( de2.get() );
    // (3)
    ref<DrawElementsUInt> de3 = new DrawElementsUInt(PT_QUADS);
    de3->setIndexBuffer( torus1_de->indexBuffer() );
    de3->setBaseVertex( (int)torus1_vert->size() + (int)torus2_vert->size() ); // skip the vertices of the first and second torus
    geom->drawCalls()->push_back( de3.get() );

    // compute normals must be done after the draw calls have been added.
    geom->computeNormals();

    ref<Effect> fx = new Effect;
    fx->shader()->setRenderState( new Light, 0 );
    fx->shader()->enable(vl::EN_LIGHTING);
    fx->shader()->enable(vl::EN_DEPTH_TEST);
    fx->shader()->gocMaterial()->setDiffuse( vl::yellow );

    sceneManager()->tree()->addActor( geom.get(), fx.get(), NULL );
  }

  void setupScene_PrimitiveRestart()
  {
    // QUICK TEST RESULTS:
    // - PT_TRIANGLE_STRIP merged: ~250 FPS (1 draw call).
    // - PT_TRIANGLE_STRIP merged with primitive restart: ~250 FPS (1 draw call).
    // - PT_TRIANGLE_STRIP non-merged: ~50 FPS (169 draw calls in total).
    // - PT_QUADS: ~250 FPS (1 draw call).
    // - PT_TRIANGLE_STRIP merged with MultiDrawElements: ~175 FPS.
    // ERGO:
    // - PT_TRIANGLE_STRIP are not faster than PT_QUADS.
    // - If using PT_TRIANGLE_STRIP just merge them into one, no need for primitive restart.
    // - Primitive restart is mainly useful for PT_POLYGON and PT_TRIANGLE_FAN where merging is not possible.
    // - If you use PT_TRIANGLES or PT_QUADS you don't need primitive restart at all of course.

    ref<Geometry> teapot_p_restart = makeTeapot( vec3(0,0,0), 10, 8 );
    teapot_p_restart->computeNormals();
    TriangleStripGenerator::stripfy(teapot_p_restart.get(), 22, false, false, true);
    ref<Geometry> teapot_multi = teapot_p_restart->shallowCopy();
    
    // teapot_p_restart will merge all triangle strip using primitive restart
    teapot_p_restart->mergeDrawCallsWithPrimitiveRestart(PT_TRIANGLE_STRIP);
    
    // teapot_multi will merge all triangle strip using MultiDrawElements
    teapot_multi->mergeDrawCallsWithMultiDrawElements(PT_TRIANGLE_STRIP);

    ref<Effect> red_fx = new Effect;
    red_fx->shader()->setRenderState( new Light, 0 );
    red_fx->shader()->enable(vl::EN_LIGHTING);
    red_fx->shader()->enable(vl::EN_DEPTH_TEST);
    red_fx->shader()->gocMaterial()->setDiffuse( vl::crimson );

    ref<Effect> green_fx = new Effect;
    green_fx->shader()->setRenderState( new Light, 0 );
    green_fx->shader()->enable(vl::EN_LIGHTING);
    green_fx->shader()->enable(vl::EN_DEPTH_TEST);
    green_fx->shader()->gocMaterial()->setDiffuse( vl::green );

    Actor* pot_actor1 = sceneManager()->tree()->addActor( teapot_p_restart.get(), red_fx.get(), new Transform(mat4::getTranslation(-10,0,0)) );
    Actor* pot_actor2 = sceneManager()->tree()->addActor( teapot_multi.get(), green_fx.get(), new Transform(mat4::getTranslation(+10,0,0)) );
    pot_actor1->transform()->computeWorldMatrix();
    pot_actor2->transform()->computeWorldMatrix();

    // Test Scene
    //for(int x=-50; x<=50; x+=5)
    //for(int y=-50; y<=50; y+=5)
    //  sceneManager()->tree()->addActor( teapot.get(), fx.get(), new Transform( mat4::getTranslation(x,y,0) ) )->transform()->computeWorldMatrix();
  }

  bool testTriangleIterators()
  {
    ref<Geometry> teapot1 = makeTeapot( vec3(0,0,0), 10, 8);
    teapot1->computeNormals();
    TriangleStripGenerator::stripfy(teapot1.get(), 22, false, false, true);

    // remove the last draw calls generated by stripfy() which is a PT_TRIANGLES
    teapot1->drawCalls()->eraseAt( teapot1->drawCalls()->size() -1 );

    ref<Geometry> teapot2 = teapot1->shallowCopy();
    ref<Geometry> teapot3 = teapot1->shallowCopy();

    teapot2->mergeDrawCallsWithPrimitiveRestart(PT_TRIANGLE_STRIP);
    teapot3->mergeDrawCallsWithMultiDrawElements(PT_TRIANGLE_STRIP);

    std::vector<int> triangles1;
    std::vector<int> triangles2;
    std::vector<int> triangles3;

    int index_count1 = 0;
    int index_count2 = 0;
    int index_count3 = 0;

    // go backwards to reproduce the triangle order generated by mergeDrawCalls*()
    for(int i=0; i<teapot1->drawCalls()->size(); ++i )
    {
      index_count1 += (int)teapot1->drawCalls()->at(i)->countIndices();
      for( TriangleIterator it=teapot1->drawCalls()->at(i)->triangleIterator(); it.hasNext(); it.next() )
      {
        triangles1.push_back( it.a() );
        triangles1.push_back( it.b() );
        triangles1.push_back( it.c() );
      }
    }
    index_count2 = (int)teapot2->drawCalls()->at(0)->countIndices();
    index_count3 = (int)teapot3->drawCalls()->at(0)->countIndices();
    VL_CHECK( index_count1 == index_count2 );
    VL_CHECK( index_count1 == index_count3 );

    for( TriangleIterator it=teapot2->drawCalls()->at(0)->triangleIterator(); it.hasNext(); it.next() )
    {
      triangles2.push_back( it.a() );
      triangles2.push_back( it.b() );
      triangles2.push_back( it.c() );
    }

    for( TriangleIterator it=teapot3->drawCalls()->at(0)->triangleIterator(); it.hasNext(); it.next() )
    {
      triangles3.push_back( it.a() );
      triangles3.push_back( it.b() );
      triangles3.push_back( it.c() );
    }
    VL_CHECK( triangles1.size() == triangles2.size() );
    VL_CHECK( triangles1.size() == triangles3.size() );

    bool ok1 = memcmp( &triangles1[0], &triangles2[0], sizeof(triangles1[0])*triangles1.size() ) == 0;
    bool ok2 = memcmp( &triangles1[0], &triangles3[0], sizeof(triangles1[0])*triangles1.size() ) == 0;

    return ok1 && ok2 && triangles1.size() == triangles2.size() && triangles1.size() == triangles3.size() && index_count1 == index_count2 && index_count1 == index_count3;
  }

  void initEvent()
  {
    vl::Log::notify(appletInfo());

    if(!testTriangleIterators())
    {
      Log::bug("App_DrawCalls::testTriangleIterators() failed!\n");
      VL_TRAP();
    }
    else
    {
      if (Has_Base_Vertex)
        setupScene_BaseVertex();
      else
        Log::error("Base-vertex functionality not supported, test skipped.\n");

      setupScene_MultiDrawElements();

      if (Has_Primitive_Restart)
        setupScene_PrimitiveRestart();
      else
        Log::error("Primitive-restart functionality not supported, test skipped.\n");
    }
  }

};

// Have fun!

BaseDemo* Create_App_DrawCalls() { return new App_DrawCalls; }

#endif
