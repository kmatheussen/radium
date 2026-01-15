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

#ifndef EdgeUpdateCallback_INCLUDE_ONCE
#define EdgeUpdateCallback_INCLUDE_ONCE

#include <vlGraphics/Actor.hpp>
#include <vlGraphics/EdgeExtractor.hpp>
#include <vlGraphics/Geometry.hpp>
#include <vlGraphics/Camera.hpp>

namespace vl
{
  //! The EdgeUpdateCallback class updates at every frame the edges of an Actor for the purpose of edge-enhancement.
  //! \sa EdgeExtractor
  class EdgeUpdateCallback: public ActorEventCallback
  {
    VL_INSTRUMENT_CLASS(vl::EdgeUpdateCallback, ActorEventCallback)

  public:
    EdgeUpdateCallback(): mShowCreases(true) 
    {
      VL_DEBUG_SET_OBJECT_NAME()
    }

    EdgeUpdateCallback(const std::vector<EdgeExtractor::Edge>& edge): mEdges(edge), mShowCreases(false) 
    {
      VL_DEBUG_SET_OBJECT_NAME()
    }

    //! If \p true only the edges forming the silhouette of an object will be rendered.
    void setShowCreases(bool sonly) { mShowCreases = sonly; }
    //! If \p true only the edges forming the silhouette of an object will be rendered.
    bool showCreases() const { return mShowCreases; }

    virtual void onActorDelete(Actor*) {}

    virtual void onActorRenderStarted(Actor* act, real /*frame_clock*/, const Camera* cam, Renderable* renderable, const Shader*, int pass)
    {
      if (pass != 0)
        return;

      fmat4 vmat = (fmat4)cam->viewMatrix();
      if (act->transform())
        vmat = vmat * (fmat4)act->transform()->worldMatrix();
      fmat4 nmat = vmat.as3x3();
      nmat = nmat.getInverse().transpose();

      ref<Geometry> geom = cast<Geometry>(renderable);
      ref<ArrayFloat3> vert_array = cast<ArrayFloat3>(geom->vertexArray());
      // VL_CHECK(vert_array->size() == edges().size()*2);
      for(unsigned i=0; i<edges().size(); ++i)
      {
        bool insert_edge = edges()[i].isCrease() && showCreases();
        if (!insert_edge)
        {
          fvec3 v1 = vmat * edges()[i].vertex1();
          fvec3 v2 = vmat * edges()[i].vertex2();
          fvec3 v  = ((v1+v2) * 0.5f).normalize();
          fvec3 n1 = nmat * edges()[i].normal1();
          fvec3 n2 = nmat * edges()[i].normal2();
          insert_edge = dot(n1, v) * dot(n2, v) < 0;
        }
        if ( insert_edge )
        {
          vert_array->at(i*2+0) = edges()[i].vertex1();
          vert_array->at(i*2+1) = edges()[i].vertex2();
        }
        else
        {
          // degenerate
          vert_array->at(i*2+0) = vert_array->at(i*2+1);
        }
      }
    }

    const std::vector<EdgeExtractor::Edge>& edges() const { return mEdges; }
    std::vector<EdgeExtractor::Edge>& edges() { return mEdges; }

  private:
    std::vector<EdgeExtractor::Edge> mEdges;
    bool mShowCreases;
  };

}

#endif
