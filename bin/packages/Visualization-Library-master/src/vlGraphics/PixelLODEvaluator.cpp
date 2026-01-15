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

#include <vlGraphics/PixelLODEvaluator.hpp>
#include <vlGraphics/Camera.hpp>

using namespace vl;

//-----------------------------------------------------------------------------
int PixelLODEvaluator::evaluate(Actor* actor, Camera* camera)
{
  if (mPixelRangeSet.empty())
    return 0;

  AABB aabb = actor->transform() ? actor->lod(0)->boundingBox().transformed( actor->transform()->worldMatrix() ) : actor->lod(0)->boundingBox();

  vec3 corner[] = 
  {
    vec3(aabb.minCorner().x(), aabb.minCorner().y(), aabb.minCorner().z()),
    vec3(aabb.minCorner().x(), aabb.maxCorner().y(), aabb.minCorner().z()),
    vec3(aabb.maxCorner().x(), aabb.maxCorner().y(), aabb.minCorner().z()),
    vec3(aabb.maxCorner().x(), aabb.minCorner().y(), aabb.minCorner().z()),
    vec3(aabb.minCorner().x(), aabb.minCorner().y(), aabb.maxCorner().z()),
    vec3(aabb.minCorner().x(), aabb.maxCorner().y(), aabb.maxCorner().z()),
    vec3(aabb.maxCorner().x(), aabb.maxCorner().y(), aabb.maxCorner().z()),
    vec3(aabb.maxCorner().x(), aabb.minCorner().y(), aabb.maxCorner().z())
  };

  mat4 proj_matrix = camera->projectionMatrix() * camera->viewMatrix();

  aabb.setNull();

  // project the 8 corners in the viewport
  for(int i=0; i<8; ++i)
  {
    vec4 out = proj_matrix * vec4(corner[i],1);

    if (out.w() == 0.0f)
      continue;

    out.x() /= out.w();
    out.y() /= out.w();
    out.z() /= out.w();

    // map to range 0-1
    out.x() = out.x() * 0.5f + 0.5f;
    out.y() = out.y() * 0.5f + 0.5f;
    out.z() = out.z() * 0.5f + 0.5f;

    // map to viewport
    out.x() = out.x() * camera->viewport()->width()  + camera->viewport()->x();
    out.y() = out.y() * camera->viewport()->height() + camera->viewport()->y();

    aabb.addPoint(out.xyz());
  }

  double pixels = aabb.width() * aabb.height();

  // we assume the distances are sorted in increasing order
  int i=0;
  for(; i<(int)mPixelRangeSet.size(); ++i)
  {
    if (pixels>mPixelRangeSet[mPixelRangeSet.size() - 1 - i])
      return i;
  }

  return i; // == mPixelRangeSet.size()
}
//-----------------------------------------------------------------------------
