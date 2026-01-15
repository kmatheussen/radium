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

#include <vlGraphics/ActorKdTree.hpp>
#include <vlCore/Log.hpp>
#include <algorithm>

using namespace vl;

namespace
{
  //-----------------------------------------------------------------------------
  // sortX
  //-----------------------------------------------------------------------------
  bool sortX(const ref<Actor>& a1, const ref<Actor>& a2) /*const*/
  {
    VL_CHECK(a1->lod(0))
    VL_CHECK(a2->lod(0))
    return a1->boundingBox().minCorner().x() < a2->boundingBox().minCorner().x();
  }
  //-----------------------------------------------------------------------------
  // sortY
  //-----------------------------------------------------------------------------
  bool sortY(const ref<Actor>& a1, const ref<Actor>& a2) /*const*/
  {
    VL_CHECK(a1->lod(0))
    VL_CHECK(a2->lod(0))
    return a1->boundingBox().minCorner().y() < a2->boundingBox().minCorner().y();
  }
  //-----------------------------------------------------------------------------
  // sortZ
  //-----------------------------------------------------------------------------
  bool sortZ(const ref<Actor>& a1, const ref<Actor>& a2) /*const*/
  {
    VL_CHECK(a1->lod(0))
    VL_CHECK(a2->lod(0))
    return a1->boundingBox().minCorner().z() < a2->boundingBox().minCorner().z();
  }
}

//-----------------------------------------------------------------------------
ref<ActorKdTree> ActorKdTree::kdtreeFromNonLeafyActors(int max_depth, float minimum_volume)
{
  ActorCollection acts;
  harvestNonLeafActors(acts);
  ref<ActorKdTree> newtree = new ActorKdTree;
  newtree->buildKdTree(acts, max_depth, minimum_volume);
  return newtree;
}
//-----------------------------------------------------------------------------
void ActorKdTree::harvestNonLeafActors(ActorCollection& acts)
{
  VL_CHECK( (actors()->size() && (mChildN == 0 && mChildP == 0)) || !(mChildN == 0 && mChildP == 0) );

  if ( mChildN || mChildP )
  {
    for(int i=0; i<(int)actors()->size(); ++i)
      acts.push_back(actors()->at(i));
    actors()->clear();
  }

  if(mChildN) childN()->harvestNonLeafActors( acts );
  if(mChildP) childP()->harvestNonLeafActors( acts );
}
//-----------------------------------------------------------------------------
void ActorKdTree::computeLocalAABB(const ActorCollection& acts)
{
  mAABB.setNull();
  for(int i=0; i<(int)acts.size(); ++i)
  {
    VL_CHECK( acts[i]->lod(0) )
    mAABB += acts[i]->boundingBox();
  }
}
//-----------------------------------------------------------------------------
void ActorKdTree::buildKdTree(ActorCollection& acts, int max_depth, float minimum_volume)
{
  int counter = 0;
  prepareActors(acts);
  compileTree_internal(acts, counter, max_depth, minimum_volume);
}
//-----------------------------------------------------------------------------
void ActorKdTree::rebuildKdTree(int max_depth, float minimum_volume)
{
  ActorCollection acts;
  extractActors(acts);
  buildKdTree(acts, max_depth, minimum_volume);
}
//-----------------------------------------------------------------------------
void ActorKdTree::compileTree_internal(ActorCollection& acts, int& counter, int max_depth, float minimum_volume)
{
  mChildN = NULL;
  mChildP = NULL;
  actors()->clear();
  mAABB.setNull();
  mPlane = Plane();

  if (acts.size() == 0)
    return;

  computeLocalAABB(acts);

  if (acts.size() == 1 || max_depth == 0 || mAABB.volume() < minimum_volume)
  {
    mActors = acts;
    return;
  }

  if ( !findBestPlane(mPlane, counter, acts) )
  {
    mActors = acts;
    return;
  }

  ActorCollection actorsN;
  ActorCollection actorsP;
  actorsN.reserve(acts.size());
  actorsP.reserve(acts.size());

  for(int i=0; i<(int)acts.size(); ++i)
  {
    VL_CHECK(acts[i]->lod(0))
    switch( mPlane.classify(acts[i]->boundingBox()) )
    {
    case  0: actors()->push_back(acts[i].get()); break;
    case -1: actorsN.  push_back(acts[i].get()); break;
    case +1: actorsP.  push_back(acts[i].get()); break;
    }    
  }

  int counter1 = counter;
  int counter2 = counter;
  if (actorsN.size())
  {
    setChildN(new ActorKdTree);
    childN()->compileTree_internal(actorsN, counter1, max_depth-1, minimum_volume);
  }

  if (actorsP.size())
  {
    setChildP(new ActorKdTree);
    childP()->compileTree_internal(actorsP, counter2, max_depth-1, minimum_volume);
  }

}
//-----------------------------------------------------------------------------
int ActorKdTree::scorePlane(const Plane& plane, const ActorCollection& acts)
{
  int cN=0, cC=0, cP=0;
  for(int i=0; i<(int)acts.size(); ++i)
  {
    VL_CHECK(acts[i]->lod(0))
    switch( plane.classify(acts[i]->boundingBox()) )
    {
    case -1: cN++; break;
    case 0:  cC++; break;
    case +1: cP++; break;
    }
  }
  return cC + ::abs(cN-cP);
}
//-----------------------------------------------------------------------------
//! Finds the best plane among different x/y/z orientation in order to divide the given list of actors included in the given AABB.
bool ActorKdTree::findBestPlane(Plane& plane, int& counter, ActorCollection& acts)
{
  int median = (int)acts.size() / 2;
  if (counter%3 == 0)
  {
    std::sort(acts.vector().begin(), acts.vector().end(), sortX);
    VL_CHECK(acts[median]->lod(0))
    plane = Plane(acts[median]->boundingBox().minCorner().x(), vec3(1,0,0));
    counter++;
    if (scorePlane(plane, acts) == (int)acts.size())
    {
      std::sort(acts.vector().begin(), acts.vector().end(), sortY);
      VL_CHECK(acts[median]->lod(0))
      plane = Plane(acts[median]->boundingBox().minCorner().y(), vec3(0,1,0));
      counter++;
      if (scorePlane(plane, acts) == (int)acts.size())
      {
        std::sort(acts.vector().begin(), acts.vector().end(), sortZ);
        VL_CHECK(acts[median]->lod(0))
        plane = Plane(acts[median]->boundingBox().minCorner().z(), vec3(0,0,1));
        counter++;
        if (scorePlane(plane, acts) == (int)acts.size())
          return false;
      }
    }
  }
  else
  if (counter%3 == 1)
  {
    std::sort(acts.vector().begin(), acts.vector().end(), sortY);
    VL_CHECK(acts[median]->lod(0))
    plane = Plane(acts[median]->boundingBox().minCorner().y(), vec3(0,1,0));
    counter++;
    if (scorePlane(plane, acts) == (int)acts.size())
    {
      std::sort(acts.vector().begin(), acts.vector().end(), sortZ);
      VL_CHECK(acts[median]->lod(0))
      plane = Plane(acts[median]->boundingBox().minCorner().z(), vec3(0,0,1));
      counter++;
      if (scorePlane(plane, acts) == (int)acts.size())
      {
        std::sort(acts.vector().begin(), acts.vector().end(), sortX);
        VL_CHECK(acts[median]->lod(0))
        plane = Plane(acts[median]->boundingBox().minCorner().x(), vec3(1,0,0));
        counter++;
        if (scorePlane(plane, acts) == (int)acts.size())
          return false;
      }
    }
  }
  else
  if (counter%3 == 2)
  {
    std::sort(acts.vector().begin(), acts.vector().end(), sortZ);
    VL_CHECK(acts[median]->lod(0))
    plane = Plane(acts[median]->boundingBox().minCorner().z(), vec3(0,0,1));
    counter++;
    if (scorePlane(plane, acts) == (int)acts.size())
    {
      std::sort(acts.vector().begin(), acts.vector().end(), sortX);
      VL_CHECK(acts[median]->lod(0))
      plane = Plane(acts[median]->boundingBox().minCorner().x(), vec3(1,0,0));
      counter++;
      if (scorePlane(plane, acts) == (int)acts.size())
      {
        std::sort(acts.vector().begin(), acts.vector().end(), sortY);
        VL_CHECK(acts[median]->lod(0))
        plane = Plane(acts[median]->boundingBox().minCorner().y(), vec3(0,1,0));
        counter++;
        if (scorePlane(plane, acts) == (int)acts.size())
          return false;
      }
    }
  }
  return true;
}
//-----------------------------------------------------------------------------
ActorKdTree* ActorKdTree::insertActor(Actor* actor)
{
  VL_CHECK(actor->lod(0))
  if (childN() == 0 && childP() == 0)
    actors()->push_back(actor);
  else
  {
    switch( mPlane.classify(actor->boundingBox()) )
    {
      case -1: if (!childN()) setChildN(new ActorKdTree); return childN()->insertActor(actor);
      case 0:  actors()->push_back(actor); break;
      case +1: if (!childP()) setChildP(new ActorKdTree); return childP()->insertActor(actor);
    }
  }
  return this;
}
//-----------------------------------------------------------------------------
int ActorKdTree::childrenCount() const
{
  if (mChildN && mChildP)
    return 2;
  else
  if (mChildN || mChildP)
    return 1;
  else
    return 0;
}
//-----------------------------------------------------------------------------
ActorTreeAbstract* ActorKdTree::child(int i)
{
  if (i == 0)
    return mChildN ? mChildN.get() : mChildP.get();
  else
  if (i == 1)
    return mChildP.get();
  else
  {
    vl::Log::error( "ActorKdTree::child(int): bad child node requested, a ActorKdTree can have only 2 child nodes.\n" );
    return NULL;
  }
}
//-----------------------------------------------------------------------------
const ActorTreeAbstract* ActorKdTree::child(int i) const
{
  if (i == 0)
    return mChildN ? mChildN.get() : mChildP.get();
  else
  if (i == 1)
    return mChildP.get();
  else
  {
    vl::Log::error( "ActorKdTree::child(int): bad child node requested, a ActorKdTree can have only 2 child nodes.\n" );
    return NULL;
  }
}
//-----------------------------------------------------------------------------
