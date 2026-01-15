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

#include <vlGraphics/Extrusion.hpp>
#include <vlGraphics/Tessellator.hpp>
#include <vlCore/glsl_math.hpp>

using namespace vl;

ref<Geometry> Extrusion::extrude()
{
  if (silhouette().empty())
  {
    Log::error("Extrusion::extrude(): no silhouette defined.\n");
    return NULL;
  }
  if (positionPath().empty())
  {
    Log::error("Extrusion::extrude() needs at least a non empty positionPath().\n");
    return NULL;
  }
  if (!scalingPath().empty() && scalingPath().size() != positionPath().size()-2)
  {
    Log::error("Extrusion::extrude(): scalingPath() must have the same number of control points as positionPath().\n");
    return NULL;
  }
  if (!rotationPath().empty() && rotationPath().size() != positionPath().size()-2)
  {
    Log::error("Extrusion::extrude(): rotationPath() must have the same number of control points as positionPath().\n");
    return NULL;
  }
  if (!colorPath().empty() && colorPath().size() != positionPath().size()-2)
  {
    Log::error("Extrusion::extrude(): colorPath() must have the same number of control points as positionPath().\n");
    return NULL;
  }

  ref<Geometry> geom = new Geometry;

  size_t segments = positionPath().size()-2;

  std::vector<fvec3> verts;
  verts.resize( silhouette().size() * segments );

  vl::fmat4 m = fmat4::getRotation(fvec3(0,1,0),positionPath()[1]-positionPath()[0]);

  // initialize silhouette on the x/z plane
  std::vector<vl::fvec3> projected_sil;
  projected_sil.resize(silhouette().size());
  for(unsigned i=0; i<silhouette().size(); ++i)
  {
    projected_sil[i] = m * vl::fvec3(silhouette()[i].x(),0,silhouette()[i].y()) + positionPath()[0];
  }

  // initialize plane normals from 1 to n-1 (end points are excluded)
  std::vector<fvec3> plane_normals;
  plane_normals.resize(positionPath().size());
  for(unsigned i=1; i<plane_normals.size()-1; ++i)
  {
    fvec3 p0 = positionPath()[i-1] - positionPath()[i];
    fvec3 p1 = positionPath()[i+1] - positionPath()[i];
    p0.normalize();
    p1.normalize();
    plane_normals[i] = (p1-p0).normalize();
  }

  for(unsigned i=1; i<positionPath().size()-1; ++i)
  {
    for(int j=0; j<(int)silhouette().size(); ++j)
    {
      fvec3 V = (positionPath()[i] - positionPath()[i-1]).normalize();
      const fvec3& P = projected_sil[j];
      const fvec3& orig = positionPath()[i];
      const fvec3& N = plane_normals [i];
      float d = dot(N,orig);
      float t = dot(V,N) ? (d-dot(P,N))/dot(V,N) : 0 /*error*/;
      // project current projected_sil on next plane along p0->p1 vector
      verts.at(j+silhouette().size()*(i-1)) = projected_sil[j] = P + V*t;
    }
  }

  // rotation
  if(!rotationPath().empty())
  {
    for(unsigned i=1; i<positionPath().size()-1; ++i)
    {
      fvec3 r = (positionPath()[i+1] - positionPath()[i]).normalize();
      fmat4 mat = vl::fmat4::getRotation(rotationPath()[i-1],r);
      fvec3 c;
      for(int j=0; j<(int)silhouette().size(); ++j)
        c += verts.at(j+silhouette().size()*(i-1));
      c /= (float)silhouette().size();
      for(int j=0; j<(int)silhouette().size(); ++j)
        verts.at(j+silhouette().size()*(i-1)) = (mat*(verts.at(j+silhouette().size()*(i-1))-c))+c;
    }
  }

  // scaling
  if(!scalingPath().empty())
  {
    for(unsigned i=1; i<positionPath().size()-1; ++i)
    {
      float s = scalingPath()[i-1];
      fvec3 c;
      for(int j=0; j<(int)silhouette().size(); ++j)
        c += verts.at(j+silhouette().size()*(i-1));
      c /= (float)silhouette().size();
      for(int j=0; j<(int)silhouette().size(); ++j)
        verts.at(j+silhouette().size()*(i-1)) = (s*(verts.at(j+silhouette().size()*(i-1))-c))+c;
    }
  }

  int prof_count = silhouetteMode() == SilhouetteClosed ? (int)silhouette().size() : (int)silhouette().size()-1;
  ref<DrawElementsUInt> de = new DrawElementsUInt(PT_QUADS);
  geom->drawCalls()->push_back(de.get());
  de->indexBuffer()->resize(4 * prof_count * (segments-1));
  for(size_t iseg=0; iseg<segments-1; ++iseg)
  {
    for(int iquad=0; iquad<prof_count; ++iquad)
    {
      de->indexBuffer()->at(iquad*4+iseg*4*prof_count + 3) = (iseg + 0) * (GLuint)silhouette().size() + iquad;
      de->indexBuffer()->at(iquad*4+iseg*4*prof_count + 2) = (iseg + 0) * (GLuint)silhouette().size() + (iquad+1)%silhouette().size();
      de->indexBuffer()->at(iquad*4+iseg*4*prof_count + 1) = (iseg + 1) * (GLuint)silhouette().size() + (iquad+1)%silhouette().size();
      de->indexBuffer()->at(iquad*4+iseg*4*prof_count + 0) = (iseg + 1) * (GLuint)silhouette().size() + iquad;
    }
  }

  // bottom/top caps

  size_t tess_bottom_count = 0;
  size_t tess_top_count    = 0;

  if(fillBottom())
  {
    size_t start = verts.size();
    Tessellator tessellator;
    tessellator.contours().push_back((int)silhouette().size());
    for(unsigned i=0; i<silhouette().size(); ++i)
      tessellator.contourVerts().push_back((dvec3)verts[i]);
    tessellator.setWindingRule(vl::TW_TESS_WINDING_NONZERO);
    tessellator.tessellate();
    for(unsigned i=0; i<tessellator.tessellatedTris().size(); ++i)
      verts.push_back(tessellator.tessellatedTris()[i]);
    if (tessellator.tessellatedTris().size())
      geom->drawCalls()->push_back( new DrawArrays(PT_TRIANGLES, start, tessellator.tessellatedTris().size()) );
    tess_bottom_count = tessellator.tessellatedTris().size();
  }
  if(fillTop())
  {
    size_t start = verts.size();
    Tessellator tessellator;
    tessellator.contours().push_back(silhouette().size());
    for(unsigned i=0; i<silhouette().size(); ++i)
      tessellator.contourVerts().push_back((dvec3)verts[verts.size()-i-1-tess_bottom_count]);
    tessellator.setWindingRule(vl::TW_TESS_WINDING_NONZERO);
    tessellator.tessellate();
    for(unsigned i=0; i<tessellator.tessellatedTris().size(); ++i)
      verts.push_back(tessellator.tessellatedTris()[i]);
    if (tessellator.tessellatedTris().size())
      geom->drawCalls()->push_back( new DrawArrays(PT_TRIANGLES, start, tessellator.tessellatedTris().size()) );
    tess_top_count = tessellator.tessellatedTris().size();
  }

  ref<ArrayFloat3> vert_array = new ArrayFloat3;
  geom->setVertexArray( vert_array.get() );
  vert_array->initFrom(verts);

  if (!colorPath().empty())
  {
    ref<ArrayFloat4> col_array = new ArrayFloat4;
    geom->setColorArray(col_array.get());
    col_array->resize(geom->vertexArray()->size());
    int offs = 0;
    for(size_t iseg=0; iseg<segments; ++iseg)
    {
      for(unsigned j=0; j<silhouette().size(); ++j, ++offs)
        col_array->at(offs) = colorPath()[iseg];
    }
    if (fillBottom())
    {
      for(unsigned j=0; j<tess_bottom_count; ++j, ++offs)
        col_array->at(offs) = colorPath()[0];
    }
    if (fillTop())
    {
      for(unsigned j=0; j<tess_top_count; ++j, ++offs)
        col_array->at(offs) = colorPath().back();
    }
  }

  if (!smooth())
    geom->convertDrawCallToDrawArrays();

  geom->computeNormals();

  return geom;
}
