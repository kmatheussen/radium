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

#include <vlGraphics/EdgeExtractor.hpp>
#include <vlGraphics/Rendering.hpp>
#include <vlGraphics/SceneManager.hpp>
#include <vlGraphics/RenderQueue.hpp>
#include <vlCore/Log.hpp>
#include <vlGraphics/Array.hpp>
#include <vlGraphics/Geometry.hpp>

using namespace vl;

//-----------------------------------------------------------------------------
void EdgeExtractor::addEdge(std::set<EdgeExtractor::Edge>& edges, const EdgeExtractor::Edge& e, const fvec3& n)
{
  std::set<EdgeExtractor::Edge>::iterator it = edges.find(e);
  if (it != edges.end())
  {
    VL_CHECK(!it->normal1().isNull())
    if (mWarnNonManifold && !it->normal2().isNull())
      vl::Log::error("EdgeExtractor: non-manifold mesh detected!\n");
      EdgeExtractor::Edge edge = e;
      edge.setNormal1(it->normal1());
      edge.setNormal2(n);
      edges.erase( it );
      edges.insert(edge);
  }
  else
  {
    VL_CHECK(!n.isNull());
    EdgeExtractor::Edge edge = e;
    edge.setNormal1(n);
    edges.insert(edge);
  }
}
//-----------------------------------------------------------------------------
//! Extracts the edges from the given Geometry and appends them to edges().
void EdgeExtractor::extractEdges(Geometry* geom)
{
  ArrayAbstract* verts = geom->vertexArray() ? geom->vertexArray() : geom->vertexAttribArray(vl::VA_Position) ? geom->vertexAttribArray(vl::VA_Position)->data() : NULL;

  // mic fixme:
  // here the bottle-neck seems to be the continuous allocation/deallocation and insertion/search time,
  // maybe a memory-pool-managed hash table would help?
  if (!verts)
  {
    vl::Log::error("EdgeExtractor::extractEdges(geom): 'geom' must have a vertex array of type ArrayFloat3.\n");
    return;
  }

  std::set<Edge> edges;

  // iterate all primitives
  for(int iprim=0; iprim<geom->drawCalls()->size(); ++iprim)
  {
    DrawCall* prim = geom->drawCalls()->at(iprim);
    // iterate triangles (if present)
    for(TriangleIterator trit = prim->triangleIterator(); trit.hasNext(); trit.next())
    {
      size_t a = trit.a();
      size_t b = trit.b();
      size_t c = trit.c();
      if (a == b || b == c || c == a)
        continue;
      // compute normal
      fvec3 v0 = (fvec3)verts->getAsVec3(a);
      fvec3 v1 = (fvec3)verts->getAsVec3(b) - v0;
      fvec3 v2 = (fvec3)verts->getAsVec3(c) - v0;
      fvec3 n = cross(v1,v2).normalize();
      if (n.isNull())
        continue;
      addEdge(edges, Edge( (fvec3)verts->getAsVec3(a), (fvec3)verts->getAsVec3(b) ), n );
      addEdge(edges, Edge( (fvec3)verts->getAsVec3(b), (fvec3)verts->getAsVec3(c) ), n );
      addEdge(edges, Edge( (fvec3)verts->getAsVec3(c), (fvec3)verts->getAsVec3(a) ), n );
    }
  }

  for(std::set<Edge>::iterator it = edges.begin(); it != edges.end(); ++it)
  {
    Edge e = *it;
    // boundary edge
    if (e.normal2().isNull())
      e.setIsCrease(true);
    else
    // crease edge
    {
      float cos1 = dot(e.normal1(), e.normal2());
      cos1 = vl::clamp(cos1,-1.0f,+1.0f);
      // return value in the interval [0,pi] radians
      float a1 = acos(cos1) / fPi * 180.0f;
      if( a1 > creaseAngle() )
        e.setIsCrease(true);
    }
    mEdges.push_back(e);
  }
}
//-----------------------------------------------------------------------------
ref<Geometry> EdgeExtractor::generateEdgeGeometry() const
{
  ref<Geometry> geom = new Geometry;
  geom->setBufferObjectEnabled(false);
  ref<ArrayFloat3> vert_array = new ArrayFloat3;
  geom->setVertexArray(vert_array.get());
  #ifdef SHOW_NORMALS
    vert_array->resize(edges().size()*6);
  #else
    vert_array->resize(edges().size()*2);
  #endif
  for(unsigned i=0; i<edges().size(); ++i)
  {
    vert_array->at(i*2+0) = edges()[i].vertex1();
    vert_array->at(i*2+1) = edges()[i].vertex2();
  }
  #ifdef SHOW_NORMALS
    int start = edges().size()*2;
    for(unsigned i=0; i<edges().size(); ++i)
    {
      fvec3 v = (edges()[i].vertex1() + edges()[i].vertex2()) * 0.5f;
      vert_array->at(start+i*2+0) = v;
      vert_array->at(start+i*2+1) = v + edges()[i].normal1()*0.25f;
    }
    start = edges().size()*4;
    for(unsigned i=0; i<edges().size(); ++i)
    {
      fvec3 v = (edges()[i].vertex1() + edges()[i].vertex2()) * 0.5f;
      vert_array->at(start+i*2+0) = v;
      vert_array->at(start+i*2+1) = v + edges()[i].normal2()*0.25f;
    }
  #endif
  geom->drawCalls()->push_back( new vl::DrawArrays(vl::PT_LINES, 0, (int)vert_array->size()) );
  return geom;
}
//-----------------------------------------------------------------------------
bool EdgeExtractor::extractEdges(Actor* actor)
{
  Geometry* geom = cast<Geometry>(actor->lod(0));
  if (geom)
    extractEdges(geom);
  return geom != NULL;
}
//-----------------------------------------------------------------------------
void EdgeExtractor::extractEdges(ActorCollection* actors)
{
  for(int i=0; i<actors->size(); ++i)
  {
    Geometry* geom = cast<Geometry>(actors->at(i)->lod(0));
    if (geom)
      extractEdges(geom);
  }
}
//-----------------------------------------------------------------------------
void EdgeExtractor::extractEdges(SceneManager* scene_manager)
{
  ref<ActorCollection> actors = new ActorCollection;
  scene_manager->extractActors(*actors);
  extractEdges(actors.get());
}
//-----------------------------------------------------------------------------
void EdgeExtractor::extractEdges(Rendering* rendering)
{
  for(int i=0; i<rendering->sceneManagers()->size(); ++i)
    extractEdges(rendering->sceneManagers()->at(i));
}
//-----------------------------------------------------------------------------
