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

#include <vlGraphics/SceneManagerPortals.hpp>
#include <vlGraphics/Geometry.hpp>
#include <vlCore/Say.hpp>
#include <vlGraphics/Camera.hpp>

using namespace vl;

//-----------------------------------------------------------------------------
// Portal
//-----------------------------------------------------------------------------
bool Portal::computeNormal()
{
  mNormal = 0;
  if (geometry().size() < 3)
  {
    vl::Log::error("Portal::computeNormal() error, no portal geometry defined.\n");
    return false;
  }
  if (!mTargetSector)
  {
    vl::Log::error("Portal::computeNormal() error, no sector bound to this portal.\n");
    return false;
  }

  fvec3 v0 = geometry()[0];
  fvec3 v1 = geometry()[1] - v0;
  fvec3 v2 = geometry()[2] - v0;
  mNormal = cross(v1,v2);

  return true;
}
//-----------------------------------------------------------------------------
// Sector
//-----------------------------------------------------------------------------
AABB Sector::computeBoundingBox()
{
  AABB aabb;
  for(int i=0; i<actors()->size(); ++i)
  {
    actors()->at(i)->computeBounds();
    aabb += actors()->at(i)->boundingBox();
  }
  return aabb;
}
//-----------------------------------------------------------------------------
void Sector::executeCallbacks(const Camera*cam,SceneManagerPortals* psm, Portal*p)
{
  for(unsigned i=0; i<callbacks().size(); ++i)
    callbacks()[i]->operator()(cam,psm,this,p);
}
//-----------------------------------------------------------------------------
// SceneManagerPortals
//-----------------------------------------------------------------------------
void SceneManagerPortals::initialize()
{
  computePortalNormals();
  for(unsigned i=0; i<mSectors.size(); ++i)
  {
    if (mSectors[i]->volumes().empty())
      vl::Log::error( vl::Say("Sector #%n does not have any volume!\n") << i );
    for(unsigned j=0; j<mSectors[i]->portals().size(); ++j)
      if (!mSectors[i]->portals()[j]->targetSector())
        vl::Log::error( vl::Say("In Sector #%n Portal #%n does not have any target sector!\n") << i << j);
  }
}
//-----------------------------------------------------------------------------
void SceneManagerPortals::renderPortal(Portal* portal)
{
  std::map<Portal*, ref<Actor> >::iterator it = mPortalActorMap.find(portal);
  if (it == mPortalActorMap.end())
  {
    const fvec4 portal_color = fvec4(1,0,0,0.25f);
    vl::ref<vl::Effect> portal_fx = new vl::Effect;
    /*portal_fx->shader()->enable(vl::EN_DEPTH_TEST);
    portal_fx->shader()->gocDepthFunc()->set(vl::FU_LEQUAL);*/
    portal_fx->shader()->enable(vl::EN_BLEND);
    portal_fx->shader()->gocLineWidth()->set(2.0f);
    portal_fx->shader()->gocColor()->setValue(portal_color);

    vl::ref<vl::Geometry> portal_geom = new vl::Geometry;
    vl::ref<vl::ArrayFloat3> vert_array = new vl::ArrayFloat3;
    portal_geom->setVertexArray(vert_array.get());
    vert_array->resize(portal->geometry().size());
    for(unsigned int i=0; i<portal->geometry().size(); ++i)
      vert_array->at(i) = portal->geometry()[i];
    portal_geom->drawCalls()->push_back( new vl::DrawArrays(vl::PT_LINE_LOOP, 0, (int)vert_array->size()) );
#if defined(VL_OPENGL)
    portal_geom->drawCalls()->push_back( new vl::DrawArrays(vl::PT_POLYGON,   0, (int)vert_array->size()) );
#endif
    ref<Actor> actor = new vl::Actor(portal_geom.get(), portal_fx.get(), NULL);
    mPortalActorMap[portal] = actor;
    mTempActors.push_back(actor);
  }
  else
    mTempActors.push_back(it->second);
}
//-----------------------------------------------------------------------------
void SceneManagerPortals::extractActors(ActorCollection& list)
{
  for(unsigned i=0; i<mSectors.size(); ++i)
    for(int j=0; j<mSectors[i]->actors()->size(); ++j)
      list.push_back( mSectors[i]->actors()->at(j) );
}
//-----------------------------------------------------------------------------
void SceneManagerPortals::extractVisibleActors(ActorCollection& list, const Camera* camera)
{
  if (cullingEnabled())
  {
    Sector* start = computeStartingSector(camera);
    if (!start)
      extractActors(list);
    else
    {
      ++mVisitTick;
      mTempActors.clear();
      mFrustumStack.clear();

      mFrustumStack.push_back(camera->frustum());
      start->executeCallbacks(camera,this,NULL);
      visitSector(NULL, start, camera->modelingMatrix().getT(), camera);

      // remove duplicates
      std::sort(mTempActors.begin(), mTempActors.end());
      std::vector< ref<Actor> >::iterator new_end = std::unique(mTempActors.begin(), mTempActors.end());
      for(std::vector< ref<Actor> >::iterator it = mTempActors.begin(); it != new_end; ++it)
        list.push_back(it->get());
    }
  }
  else
    extractActors(list);
}
//-----------------------------------------------------------------------------
void SceneManagerPortals::visitSector(Sector* prev, Sector* sector, const vec3& eye, const Camera* camera)
{
  // this sector is visible so we add the visible objects
  for(int j=0; j<sector->actors()->size(); ++j)
  {
    if (isEnabled(sector->actors()->at(j)))
    {
      sector->actors()->at(j)->computeBounds();
      bool visible = true;
      for(unsigned i=0; visible && i<mFrustumStack.size(); ++i)
        visible = visible & !mFrustumStack[i].cull( sector->actors()->at(j)->boundingBox() );
      if( visible )
        mTempActors.push_back( sector->actors()->at(j) );
    }
  }
  // check the visible portals
  for(unsigned j=0; j<sector->portals().size(); ++j)
  {
    if(showPortals())
      renderPortal(sector->portals()[j].get());

    // open/closed portals.
    if(!sector->portals()[j]->isOpen())
      continue;

    if (sector->portals()[j]->mVisitTick == mVisitTick)
      continue;
    else
      sector->portals()[j]->mVisitTick = mVisitTick;

    Sector* target_sec = sector->portals()[j]->targetSector();
    VL_CHECK(target_sec != sector)
    if ( target_sec != prev )
    {
      bool visible = true;
      for(unsigned i=0; visible && i<mFrustumStack.size(); ++i)
        visible = visible & !mFrustumStack[i].cull( sector->portals()[j]->geometry() );

      if (visible)
      {
        // make visiting_portal_frustum
        Frustum portal_frustum;
        portal_frustum.planes().resize(sector->portals()[j]->geometry().size());
        bool flip = dot((fvec3)eye - sector->portals()[j]->geometry()[0], sector->portals()[j]->normal()) < 0;
        for(unsigned i=0; i<sector->portals()[j]->geometry().size(); ++i)
        {
          int i2 = (i+1) % sector->portals()[j]->geometry().size();
          vec3 v1 = (vec3)sector->portals()[j]->geometry()[i]  - eye;
          vec3 v2 = (vec3)sector->portals()[j]->geometry()[i2] - eye;
          vec3 n = cross(v1,v2);
          n.normalize();
          if (flip)
            n = -n;
          portal_frustum.setPlane(i, Plane(dot(n,eye),n));
        }

        mFrustumStack.push_back(portal_frustum);
        sector->executeCallbacks(camera,this,sector->portals()[j].get());
        visitSector(sector, sector->portals()[j]->targetSector(), eye, camera);
        mFrustumStack.pop_back();
      }
    }
  }
}
//-----------------------------------------------------------------------------
void SceneManagerPortals::computePortalNormals()
{
  for(unsigned i=0; i<mSectors.size(); ++i)
    for(unsigned j=0; j<mSectors[i]->portals().size(); ++j)
      mSectors[i]->portals()[j]->computeNormal();
}
//-----------------------------------------------------------------------------
Sector* SceneManagerPortals::computeStartingSector(const Camera* camera)
{
  vec3 eye = camera->modelingMatrix().getT();
  for(unsigned i=0; i<mSectors.size(); ++i)
  {
    for(unsigned j=0; j<mSectors[i]->volumes().size(); ++j)
    {
      if (sectors()[i]->volumes()[j].isInside(eye))
        return sectors()[i].get();
    }
  }
  return externalSector();
}
//-----------------------------------------------------------------------------
