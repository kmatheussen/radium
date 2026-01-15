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

#include <vlGraphics/ActorTreeAbstract.hpp>
#include <vlGraphics/Camera.hpp>

using namespace vl;

//-----------------------------------------------------------------------------
ActorTreeAbstract::ActorTreeAbstract()
{
  VL_DEBUG_SET_OBJECT_NAME()
  mActors.setAutomaticDelete(false);
  mParent = NULL;
}
//-----------------------------------------------------------------------------
void ActorTreeAbstract::computeAABB()
{
  AABB aabb;
  for(int i=0; i<actors()->size(); ++i)
  {
    actors()->at(i)->computeBounds();
    aabb += actors()->at(i)->boundingBox();
  }
  for(int i=0; i<childrenCount(); ++i)
  {
    if (child(i))
    {
      child(i)->computeAABB();
      aabb += child(i)->aabb();
    }
  }
  mAABB = aabb;
}
//-----------------------------------------------------------------------------
void ActorTreeAbstract::extractActors(ActorCollection& list)
{
  for(int i=0; i<actors()->size(); ++i)
    list.push_back(actors()->at(i));
  for(int i=0; i<childrenCount(); ++i)
    if (child(i))
      child(i)->extractActors(list);
}
//-----------------------------------------------------------------------------
void ActorTreeAbstract::extractVisibleActors(ActorCollection& list, const Camera* camera, unsigned enable_mask)
{
  // try to cull the whole node
  if ( !camera->frustum().cull(aabb()) )
  {
    // cull Actor by Actor
    for(int i=0; i<actors()->size(); ++i)
    {
      if (enable_mask & actors()->at(i)->enableMask())
      {
        VL_CHECK(actors()->at(i)->lod(0))
        actors()->at(i)->computeBounds();
        if ( !camera->frustum().cull( actors()->at(i)->boundingSphere() ) )
          list.push_back(actors()->at(i));
      }
    }
    for(int i=0; i<childrenCount(); ++i)
      if (child(i))
        child(i)->extractVisibleActors(list, camera);
  }
}
//-----------------------------------------------------------------------------
ActorTreeAbstract* ActorTreeAbstract::eraseActor(Actor* actor)
{
  int pos = actors()->find(actor);
  if (pos != -1)
  {
    actors()->eraseAt(pos);
    return this;
  }
  else
  {
    ActorTreeAbstract* node = NULL;
    for(int i=0; !node && i<childrenCount(); ++i)
      if (child(i))
        node = child(i)->eraseActor(actor);
    return node;
  }
}
//-----------------------------------------------------------------------------
Actor* ActorTreeAbstract::addActor(Renderable* renderable, Effect* eff, Transform* tr)
{
  ref<Actor> act = new Actor(renderable,eff,tr);
  actors()->push_back( act.get() );
  return act.get();
}
//-----------------------------------------------------------------------------
Actor* ActorTreeAbstract::addActor(Actor* actor)
{
  actors()->push_back(actor);
  return actor;
}
//-----------------------------------------------------------------------------
void ActorTreeAbstract::prepareActors(ActorCollection& actors)
{
  // finds the root transforms

  std::set< Transform* > root_transforms;
  for(int i=0; i<(int)actors.size(); ++i)
  {
    for( Transform* root = actors[i]->transform(); root; root = root->parent() )
      if ( !root->parent() )
        root_transforms.insert(root);
  }

  // setup the matrices

  std::set< Transform* >::iterator tra = root_transforms.begin();
  while( tra != root_transforms.end() )
  {
    (*tra)->computeWorldMatrixRecursive();
    ++tra;
  }

  // setup the Actor[s] bounding box and bounding sphere

  for(int i=0; i<(int)actors.size(); ++i)
    actors[i]->computeBounds();
}
//-----------------------------------------------------------------------------
