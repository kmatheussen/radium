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
#include <vlCore/Colors.hpp>
#include <vlGraphics/SceneManagerPortals.hpp>
#include <vlGraphics/Light.hpp>

// Here we define our dungeon which will be the test platform for our portal scene manager.
// X = room, for each room a sector is generated, each room is filled with a couple of spheres.
// When an X is next to another X a passage (and relative Portal) is generated.
const int map_size = 7;
const char* map[map_size] = 
{
  "       ", 
  " XXXXX ", 
  " XXXXX ", 
  " XXXXX ", 
  " XXXXX ", 
  " XXXXX ", 
  "       "
};
const float room_h        = 5.0f;
const float room_size_out = 20.0f;
const float room_size_in  = 18.0f;

// When the demo starts press the "f" key to activate the "fly" or "ghost" camera mode.
class App_PortalCulling: public BaseDemo
{
public:
  virtual vl::String appletInfo()
  {
    return BaseDemo::appletInfo() + 
    "- F6: toggles wireframe\n" +
    "- F7: toggles show portals\n" +
    "- F8: enable/disable portal-based culling\n" +
    "\n";
  }

  void initEvent()
  {
    // Basic initialization
    vl::Log::notify(appletInfo());
    ghostCameraManipulator()->setMovementSpeed(5.0f);

    generateDungeon();
  }

  // keyboard controls
  // F6 = toggle wireframe
  // F7 = toggle show portals
  // F8 = enable/disable portal-based culling
  void keyPressEvent(unsigned short ch, vl::EKey key)
  {
    BaseDemo::keyPressEvent(ch, key);
    if (key == vl::Key_F6)
    {
      if (mPolygonMode->frontFace() == vl::PM_LINE)
        mPolygonMode->set(vl::PM_FILL, vl::PM_FILL);
      else
        mPolygonMode->set(vl::PM_LINE, vl::PM_LINE);
    }
    else
    if (key == vl::Key_F7)
      mPortalSceneManager->setShowPortals( !mPortalSceneManager->showPortals() );
    else
    if (key == vl::Key_F8)
      mPortalSceneManager->setCullingEnabled( !mPortalSceneManager->cullingEnabled() );
  }

  // simple method to keep the camera on the floor
  void updateScene()
  {
    vl::mat4 im = trackball()->camera()->modelingMatrix();
    vl::vec3 t = im.getT(); t.y() = 1.5f; im.setT(t); 
    trackball()->camera()->setModelingMatrix(im);
  }

  // procedurally generate the dungeon with sectors, geometry and portals
  void generateDungeon()
  {
    // Initialize the Sectors.
    vl::ref<vl::Sector> sectors[map_size][map_size] =
    {
      { new vl::Sector, new vl::Sector, new vl::Sector, new vl::Sector, new vl::Sector, new vl::Sector, new vl::Sector }, 
      { new vl::Sector, new vl::Sector, new vl::Sector, new vl::Sector, new vl::Sector, new vl::Sector, new vl::Sector }, 
      { new vl::Sector, new vl::Sector, new vl::Sector, new vl::Sector, new vl::Sector, new vl::Sector, new vl::Sector }, 
      { new vl::Sector, new vl::Sector, new vl::Sector, new vl::Sector, new vl::Sector, new vl::Sector, new vl::Sector }, 
      { new vl::Sector, new vl::Sector, new vl::Sector, new vl::Sector, new vl::Sector, new vl::Sector, new vl::Sector }, 
      { new vl::Sector, new vl::Sector, new vl::Sector, new vl::Sector, new vl::Sector, new vl::Sector, new vl::Sector }, 
      { new vl::Sector, new vl::Sector, new vl::Sector, new vl::Sector, new vl::Sector, new vl::Sector, new vl::Sector }
    };

    // ############################################
    // # Install and use our SceneManagerPortals! #
    // ############################################
    mPortalSceneManager = new vl::SceneManagerPortals;
    mPortalSceneManager->setShowPortals(true);
    // remove all the other scene managers
    rendering()->as<vl::Rendering>()->sceneManagers()->clear(); 
    // install our SceneManagerPortals!
    rendering()->as<vl::Rendering>()->sceneManagers()->push_back(mPortalSceneManager.get()); 

    // Skip the boring code below and just pay attention to how the Sector and Portal classes are used!

    // generate the effects we will use for the walls, ceilings etc. of our dungeon
    mPolygonMode = new vl::PolygonMode;
    vl::ref<vl::Effect> floor_fx = new vl::Effect;
    floor_fx->shader()->enable(vl::EN_DEPTH_TEST);
    floor_fx->shader()->enable(vl::EN_LIGHTING);
    floor_fx->shader()->gocLight(0);
    floor_fx->shader()->gocLightModel()->setTwoSide(true);
    floor_fx->shader()->gocMaterial()->setDiffuse(vl::crimson);
    if (!vl::Has_GLES_Version_1_1)
      floor_fx->shader()->setRenderState(mPolygonMode.get());

    vl::ref<vl::Effect> ceiling_fx = new vl::Effect;
    ceiling_fx->shader()->enable(vl::EN_DEPTH_TEST);
    ceiling_fx->shader()->enable(vl::EN_LIGHTING);
    ceiling_fx->shader()->gocLight(0);
    ceiling_fx->shader()->gocLightModel()->setTwoSide(true);
    ceiling_fx->shader()->gocMaterial()->setDiffuse(vl::gray);
    if (!vl::Has_GLES_Version_1_1)
      ceiling_fx->shader()->setRenderState(mPolygonMode.get());

    vl::ref<vl::Effect> wall_fx = new vl::Effect;
    wall_fx->shader()->enable(vl::EN_DEPTH_TEST);
    wall_fx->shader()->enable(vl::EN_LIGHTING);
    wall_fx->shader()->gocLight(0)->setLinearAttenuation(0.025f);
    wall_fx->shader()->gocLightModel()->setTwoSide(true);
    wall_fx->shader()->gocMaterial()->setDiffuse(vl::gold);
    if (!vl::Has_GLES_Version_1_1)
      wall_fx->shader()->setRenderState(mPolygonMode.get());

    // boring code to generate the gometry of various kinds of walls, with out door, with door, with the passage and portal.

    vl::ref<vl::Geometry> wall_1_a = vl::makeGrid(vl::vec3(room_h/2.0f, 0, 0), room_h, room_size_in, 20, 20);
    wall_1_a->computeNormals();
    wall_1_a->transform(vl::mat4::getRotation(90, 0, 0, 1));

    vl::ref<vl::Geometry> wall_2_a = vl::makeGrid(vl::vec3(0, 0, room_h/2.0f), room_size_in, room_h, 20, 20);
    wall_2_a->computeNormals();
    wall_2_a->transform(vl::mat4::getRotation(-90, 1, 0, 0));

    float w1 = room_size_in / 6.0f;
    float h1 = room_h       / 3.0f * 2.0f;
    float w2 = room_size_in / 2.0f;
    float h2 = room_h;
    vl::ref<vl::ArrayFloat3> vert_array;
    vl::ref<vl::DrawElementsUInt> de;

    // wall 1 b
    vl::ref<vl::Geometry> wall_1_b = new vl::Geometry;
    vert_array = new vl::ArrayFloat3;
    wall_1_b->setVertexArray(vert_array.get());
    vert_array->resize(8);
    // 1-----------2
    // |           |
    // |   6---5   |
    // |   |   |   |
    // 0---7   4---3
    vert_array->at(0) = vl::fvec3(0, 0, -w2);
    vert_array->at(1) = vl::fvec3(0, +h2, -w2);
    vert_array->at(2) = vl::fvec3(0, +h2, +w2);
    vert_array->at(3) = vl::fvec3(0, 0, +w2);
    vert_array->at(4) = vl::fvec3(0, 0, +w1);
    vert_array->at(5) = vl::fvec3(0, h1, +w1);
    vert_array->at(6) = vl::fvec3(0, h1, -w1);
    vert_array->at(7) = vl::fvec3(0, 0, -w1);
    de = new vl::DrawElementsUInt(vl::PT_QUADS);
    wall_1_b->drawCalls()->push_back(de.get());
    de->indexBuffer()->resize(12);
    de->indexBuffer()->at(0) = 1; // a
    de->indexBuffer()->at(1) = 0;
    de->indexBuffer()->at(2) = 7;
    de->indexBuffer()->at(3) = 6;
    de->indexBuffer()->at(4) = 1; // b
    de->indexBuffer()->at(5) = 6;
    de->indexBuffer()->at(6) = 5;
    de->indexBuffer()->at(7) = 2;
    de->indexBuffer()->at(8)  = 5; // c
    de->indexBuffer()->at(9)  = 4;
    de->indexBuffer()->at(10) = 3;
    de->indexBuffer()->at(11) = 2;
    wall_1_b->computeNormals();

    // wall_1_c
    vl::fvec3 v1c(room_size_out-room_size_in, 0, 0);
    vl::ref<vl::Geometry> wall_1_c = new vl::Geometry;
    vert_array = new vl::ArrayFloat3;
    wall_1_c->setVertexArray(vert_array.get());
    vert_array->resize(8);
    // 1-----------2
    // |   6---5   |
    // |   |   |   |
    // |   7---4   |
    // 0-----------3
    vert_array->at(0) = vl::fvec3(0, 0, -w1);
    vert_array->at(1) = vl::fvec3(0, h1, -w1);
    vert_array->at(2) = vl::fvec3(0, h1, +w1);
    vert_array->at(3) = vl::fvec3(0, 0, +w1);
    vert_array->at(4) = vl::fvec3(0, 0, +w1) + v1c;
    vert_array->at(5) = vl::fvec3(0, h1, +w1) + v1c;
    vert_array->at(6) = vl::fvec3(0, h1, -w1) + v1c;
    vert_array->at(7) = vl::fvec3(0, 0, -w1) + v1c;
    de = new vl::DrawElementsUInt(vl::PT_QUADS);
    wall_1_c->drawCalls()->push_back(de.get());
    de->indexBuffer()->resize(16);
    de->indexBuffer()->at(0) = 1; // a
    de->indexBuffer()->at(1) = 0;
    de->indexBuffer()->at(2) = 7;
    de->indexBuffer()->at(3) = 6;
    de->indexBuffer()->at(4) = 1; // b
    de->indexBuffer()->at(5) = 6;
    de->indexBuffer()->at(6) = 5;
    de->indexBuffer()->at(7) = 2;
    de->indexBuffer()->at(8)  = 5; // c
    de->indexBuffer()->at(9)  = 4;
    de->indexBuffer()->at(10) = 3;
    de->indexBuffer()->at(11) = 2;
    de->indexBuffer()->at(12) = 0; // d
    de->indexBuffer()->at(13) = 3;
    de->indexBuffer()->at(14) = 4;
    de->indexBuffer()->at(15) = 7;
    wall_1_c->computeNormals();

    // wall 2 b
    vl::ref<vl::Geometry> wall_2_b = new vl::Geometry;
    vert_array = new vl::ArrayFloat3;
    wall_2_b->setVertexArray(vert_array.get());
    vert_array->resize(8);
    // 1-----------2
    // |           |
    // |   6---5   |
    // |   |   |   |
    // 0---7   4---3
    vert_array->at(0) = vl::fvec3(-w2, 0, 0);
    vert_array->at(1) = vl::fvec3(-w2, +h2, 0);
    vert_array->at(2) = vl::fvec3(+w2, +h2, 0);
    vert_array->at(3) = vl::fvec3(+w2, 0, 0);
    vert_array->at(4) = vl::fvec3(+w1, 0, 0);
    vert_array->at(5) = vl::fvec3(+w1, h1, 0);
    vert_array->at(6) = vl::fvec3(-w1, h1, 0);
    vert_array->at(7) = vl::fvec3(-w1, 0, 0);
    de = new vl::DrawElementsUInt(vl::PT_QUADS);
    wall_2_b->drawCalls()->push_back(de.get());
    de->indexBuffer()->resize(12);
    de->indexBuffer()->at(0) = 1; // a
    de->indexBuffer()->at(1) = 0;
    de->indexBuffer()->at(2) = 7;
    de->indexBuffer()->at(3) = 6;
    de->indexBuffer()->at(4) = 1; // b
    de->indexBuffer()->at(5) = 6;
    de->indexBuffer()->at(6) = 5;
    de->indexBuffer()->at(7) = 2;
    de->indexBuffer()->at(8)  = 5; // c
    de->indexBuffer()->at(9)  = 4;
    de->indexBuffer()->at(10) = 3;
    de->indexBuffer()->at(11) = 2;
    wall_2_b->computeNormals();

    // wall_2_c
    vl::fvec3 v2c(0, 0, room_size_out-room_size_in);
    vl::ref<vl::Geometry> wall_2_c = new vl::Geometry;
    vert_array = new vl::ArrayFloat3;
    wall_2_c->setVertexArray(vert_array.get());
    vert_array->resize(8);
    // 1-----------2
    // |   6---5   |
    // |   |   |   |
    // |   7---4   |
    // 0-----------3
    vert_array->at(0) = vl::fvec3(-w1, 0, 0);
    vert_array->at(1) = vl::fvec3(-w1, h1, 0);
    vert_array->at(2) = vl::fvec3(+w1, h1, 0);
    vert_array->at(3) = vl::fvec3(+w1, 0, 0);
    vert_array->at(4) = vl::fvec3(+w1, 0, 0) + v2c;
    vert_array->at(5) = vl::fvec3(+w1, h1, 0) + v2c;
    vert_array->at(6) = vl::fvec3(-w1, h1, 0) + v2c;
    vert_array->at(7) = vl::fvec3(-w1, 0, 0) + v2c;
    de = new vl::DrawElementsUInt(vl::PT_QUADS);
    wall_2_c->drawCalls()->push_back(de.get());
    de->indexBuffer()->resize(16);
    de->indexBuffer()->at(0) = 1; // a
    de->indexBuffer()->at(1) = 0;
    de->indexBuffer()->at(2) = 7;
    de->indexBuffer()->at(3) = 6;
    de->indexBuffer()->at(4) = 1; // b
    de->indexBuffer()->at(5) = 6;
    de->indexBuffer()->at(6) = 5;
    de->indexBuffer()->at(7) = 2;
    de->indexBuffer()->at(8)  = 5; // c
    de->indexBuffer()->at(9)  = 4;
    de->indexBuffer()->at(10) = 3;
    de->indexBuffer()->at(11) = 2;
    de->indexBuffer()->at(12) = 0; // d
    de->indexBuffer()->at(13) = 3;
    de->indexBuffer()->at(14) = 4;
    de->indexBuffer()->at(15) = 7;
    wall_2_c->computeNormals();

    // very heavy spheres to be added to the sectors to outline more clearly the advantages of portal-based culling.
    vl::ref<vl::Geometry> sphere = vl::makeUVSphere(vl::vec3(0, 3, 0), 1, 200, 200);
    sphere->computeNormals();
    vl::ref<vl::Effect> ball_fx = new vl::Effect;
    ball_fx->shader()->enable(vl::EN_DEPTH_TEST);
    ball_fx->shader()->enable(vl::EN_LIGHTING);
    ball_fx->shader()->gocLight(0);
    if (!vl::Has_GLES_Version_1_1)
      ball_fx->shader()->setRenderState(mPolygonMode.get());

    // for each cell of the dungeon
    for(int y=0; y<map_size; ++y)
    {
      for(int x=0; x<map_size; ++x)
      {
        if (map[y][x] == 'X')
        {
          // ###########################
          // # allocate the new Sector #
          // ###########################
          vl::Sector* sector1 = sectors[y][x].get();
          mPortalSceneManager->sectors().push_back(sector1);

          // add floor to the sector/room
          vl::ref<vl::Geometry> floor = vl::makeGrid(vl::vec3(x*room_size_out, 0, y*room_size_out), room_size_in, room_size_in, 20, 20);
          floor->computeNormals();
          sector1->actors()->push_back( new vl::Actor(floor.get(), floor_fx.get(), NULL) );

          // add ceiling to the sector/room
          vl::ref<vl::Geometry> ceiling = vl::makeGrid(vl::vec3(x*room_size_out, room_h, y*room_size_out), room_size_in, room_size_in, 20, 20);
          ceiling->computeNormals();
          sector1->actors()->push_back( new vl::Actor(ceiling.get(), ceiling_fx.get(), NULL) );

          // generate walls, doors and portals:

          vl::mat4 m;
          vl::ref<vl::Transform> tr;

          if(map[y][x-1] == 'X') // west door: the two sectors comunicate
          {
            m = vl::mat4::getTranslation(vl::vec3(x*room_size_out, 0, y*room_size_out) + vl::vec3(-room_size_in/2.0f, 0, 0));
            tr = new vl::Transform; tr->setLocalMatrix(m); tr->computeWorldMatrix();
            // add to the sector
            sector1->actors()->push_back( new vl::Actor(wall_1_b.get(), wall_fx.get(), tr.get()) );
          }
          else
          {
            m = vl::mat4::getTranslation(vl::vec3(x*room_size_out, 0, y*room_size_out) + vl::vec3(-room_size_in/2.0f, 0, 0));
            tr = new vl::Transform; tr->setLocalMatrix(m); tr->computeWorldMatrix();
            // add to the sector
            sector1->actors()->push_back( new vl::Actor(wall_1_a.get(), wall_fx.get(), tr.get()) );
          }

          if(map[y][x+1] == 'X') // east door: the two sectors comunicate
          {
            m = vl::mat4::getTranslation(vl::vec3(x*room_size_out, 0, y*room_size_out) + vl::vec3(+room_size_in/2.0f, 0, 0));
            tr = new vl::Transform; tr->setLocalMatrix(m); tr->computeWorldMatrix();
            // add to the sector
            sector1->actors()->push_back( new vl::Actor(wall_1_b.get(), wall_fx.get(), tr.get()) );
            sector1->actors()->push_back( new vl::Actor(wall_1_c.get(), wall_fx.get(), tr.get()) );

            // #####################################################
            // # create the two Portals to connect the two Sectors #
            // #####################################################
            // portal1: sector1 -> sector2
            // portal2: sector2 -> sector1
            // This is very important: in order to connect two sectors A and B you have to create a 
            // portal in A whose target is B, and other portal in B whose target is A. 
            // This way each sector can see the other.

            // new portal
            vl::ref<vl::Portal> portal1 = new vl::Portal;
            vl::ref<vl::Portal> portal2 = new vl::Portal;
            vl::Sector* sector2 = sectors[y][x+1].get();
            // link each sector to the other using their portals
            sector1->portals().push_back(portal1.get()); portal1->setTargetSector( sector2 );
            sector2->portals().push_back(portal2.get()); portal2->setTargetSector( sector1 );
            // setup portal geometry - note: the portal geometry must be a planar, convex polygon, in world space coordinates.
            portal1->geometry().resize(4);
            portal1->geometry().at(0) = (vl::fmat4)m * (vl::fvec3(0, 0, +w1) + v1c);
            portal1->geometry().at(1) = (vl::fmat4)m * (vl::fvec3(0, h1, +w1) + v1c);
            portal1->geometry().at(2) = (vl::fmat4)m * (vl::fvec3(0, h1, -w1) + v1c);
            portal1->geometry().at(3) = (vl::fmat4)m * (vl::fvec3(0, 0, -w1) + v1c);
            portal2->geometry().resize(4);
            portal2->geometry().at(0) = (vl::fmat4)m * (vl::fvec3(0, 0, +w1) + v1c);
            portal2->geometry().at(1) = (vl::fmat4)m * (vl::fvec3(0, h1, +w1) + v1c);
            portal2->geometry().at(2) = (vl::fmat4)m * (vl::fvec3(0, h1, -w1) + v1c);
            portal2->geometry().at(3) = (vl::fmat4)m * (vl::fvec3(0, 0, -w1) + v1c);
          }
          else
          {
            m = vl::mat4::getTranslation(vl::vec3(x*room_size_out, 0, y*room_size_out) + vl::vec3(+room_size_in/2.0f, 0, 0));
            tr = new vl::Transform; tr->setLocalMatrix(m); tr->computeWorldMatrix();
            sector1->actors()->push_back( new vl::Actor(wall_1_a.get(), wall_fx.get(), tr.get()) );
          }

          if(map[y-1][x] == 'X') // south door: the two sectors comunicate
          {
            m = vl::mat4::getTranslation(vl::vec3(x*room_size_out, 0, y*room_size_out) + vl::vec3(0, 0, -room_size_in/2.0f));
            tr = new vl::Transform; tr->setLocalMatrix(m); tr->computeWorldMatrix();
            sector1->actors()->push_back( new vl::Actor(wall_2_b.get(), wall_fx.get(), tr.get()) );
          }
          else
          {
            m = vl::mat4::getTranslation(vl::vec3(x*room_size_out, 0, y*room_size_out) + vl::vec3(0, 0, -room_size_in/2.0f));
            tr = new vl::Transform; tr->setLocalMatrix(m); tr->computeWorldMatrix();
            sector1->actors()->push_back( new vl::Actor(wall_2_a.get(), wall_fx.get(), tr.get()) );
          }

          if(map[y+1][x] == 'X') // north door: the two sectors comunicate
          {
            m = vl::mat4::getTranslation(vl::vec3(x*room_size_out, 0, y*room_size_out) + vl::vec3(0, 0, +room_size_in/2.0f));
            tr = new vl::Transform; tr->setLocalMatrix(m); tr->computeWorldMatrix();
            sector1->actors()->push_back( new vl::Actor(wall_2_b.get(), wall_fx.get(), tr.get()) );
            sector1->actors()->push_back( new vl::Actor(wall_2_c.get(), wall_fx.get(), tr.get()) );

            // same as seen above...
            // #####################################################
            // # create the two Portals to connect the two Sectors #
            // #####################################################
            // portal1: sector1 -> sector2
            // portal2: sector2 -> sector1
            // This is very important: in order to connect two sectors A and B you have to create a 
            // portal in A whose target is B, and other portal in B whose target is A.
            // This way each sector can see the other.

            // new portal
            vl::ref<vl::Portal> portal1 = new vl::Portal;
            vl::ref<vl::Portal> portal2 = new vl::Portal;
            vl::Sector* sector2 = sectors[y+1][x].get();
            // link each sector to the other using their portals
            sector1->portals().push_back(portal1.get()); portal1->setTargetSector( sector2 );
            sector2->portals().push_back(portal2.get()); portal2->setTargetSector( sector1 );
            // setup portal geometry - note: the portal geometry must be a planar, convex polygon, in world space coordinates.
            portal1->geometry().resize(4);
            portal1->geometry().at(0) = (vl::fmat4)m * (vl::fvec3(+w1, 0, 0) + v2c);
            portal1->geometry().at(1) = (vl::fmat4)m * (vl::fvec3(+w1, h1, 0) + v2c);
            portal1->geometry().at(2) = (vl::fmat4)m * (vl::fvec3(-w1, h1, 0) + v2c);
            portal1->geometry().at(3) = (vl::fmat4)m * (vl::fvec3(-w1, 0, 0) + v2c);
            portal2->geometry().resize(4);
            portal2->geometry().at(0) = (vl::fmat4)m * (vl::fvec3(+w1, 0, 0) + v2c);
            portal2->geometry().at(1) = (vl::fmat4)m * (vl::fvec3(+w1, h1, 0) + v2c);
            portal2->geometry().at(2) = (vl::fmat4)m * (vl::fvec3(-w1, h1, 0) + v2c);
            portal2->geometry().at(3) = (vl::fmat4)m * (vl::fvec3(-w1, 0, 0) + v2c);
          }
          else
          {
            m = vl::mat4::getTranslation(vl::vec3(x*room_size_out, 0, y*room_size_out) + vl::vec3(0, 0, +room_size_in/2.0f));
            tr = new vl::Transform; tr->setLocalMatrix(m); tr->computeWorldMatrix();
            sector1->actors()->push_back( new vl::Actor(wall_2_a.get(), wall_fx.get(), tr.get()) );
          }

          // add other actors to the scene
          for(unsigned int i=0; i<25; ++i)
          {
            vl::ref<vl::Transform> tr = new vl::Transform;
            float tx = (rand()%100) / 100.0f * room_size_in - room_size_in / 2.0f;
            float tz = (rand()%100) / 100.0f * room_size_in - room_size_in / 2.0f;
            m = vl::mat4::getTranslation(vl::vec3(x*room_size_out, 0, y*room_size_out) + vl::vec3(tx, 0, tz));
            tr->setLocalMatrix(m); tr->computeWorldMatrix();
            sector1->actors()->push_back( new vl::Actor(sphere.get(), ball_fx.get(), tr.get()) );
          }

          // ############################################
          // # simple way to define the Sector's volume #
          // ############################################
          // Each Sector must have a set of volumes that are checked at the beginning of each rendering
          // to decide in which one the camera is. In our case we just use the bounding box of the 
          // Sector itself. Note that the volumes of a Sector must not intersect the volumes of the other
          // Sectors, otherwise the algorithm won't be able to determine in which Sector the camera is in.
          // I.e, it will randomly pick the first found!
          sector1->volumes().push_back( sector1->computeBoundingBox() );
        }
      }
    }

    // #######################################
    // # initialize the portal scene manager #
    // #######################################
    // Precomputes some internal data and performs some sanity checks.
    mPortalSceneManager->initialize();
  }

protected:
  vl::ref<vl::PolygonMode> mPolygonMode;
  vl::ref<vl::SceneManagerPortals> mPortalSceneManager;
};

// Have fun!

BaseDemo* Create_App_PortalCulling() { return new App_PortalCulling; }
