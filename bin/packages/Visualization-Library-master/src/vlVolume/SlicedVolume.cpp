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

#include <vlVolume/SlicedVolume.hpp>
#include <vlGraphics/GLSL.hpp>
#include <vlGraphics/Camera.hpp>
#include <vlCore/Time.hpp>

using namespace vl;

/** \class vl::SlicedVolume
 * A ActorEventCallback used to render a volume using viewport aligned slices.
 *
 * Pictures from: \ref pagGuideSlicedVolume tutorial.
 * <center>
 * <table border=0 cellspacing=0 cellpadding=5>
 * <tr>
 * 	<td> <img src="pics/pagGuideSlicedVolume_1.jpg"> </td>
 * 	<td> <img src="pics/pagGuideSlicedVolume_2.jpg"> </td>
 * 	<td> <img src="pics/pagGuideSlicedVolume_4.jpg"> </td>
 * </tr>
 * <tr>
 * 	<td> <img src="pics/pagGuideSlicedVolume_5.jpg"> </td>
 * 	<td> <img src="pics/pagGuideSlicedVolume_6.jpg"> </td>
 * 	<td> <img src="pics/pagGuideSlicedVolume_7.jpg"> </td>
 * </tr>
 * </table>
 * </center>
 *
 * Using the SlicedVolume class is very simple, see \ref pagGuideSlicedVolume for a complete example.
 *
 * Basically all SlicedVolume does is to compute the correct texture coordinates and generates on the fly N viewport
 * aligned slices, where N can be specified by the user with setSliceCount(). Such slices then can be rendered using
 * a GLSLProgram specified by the user, thus you can achieve virtually any kind of visual effect with it.
 *
 * The user can reimplement the updateUniforms() method to update his own uniform variables before the volume is rendered.
 * By default updateUniforms() updates the position of up to 4 lights in object space. Such positions are stored in the
 * \p "uniform vec3 light_position[4]" variable.
 * The updateUniforms() method also fills the \p "uniform bool light_enable[4]" variable with a flag marking if the Nth 
 * light is active or not. These light values are computed based on the lights bound to the current Shader.
 * The updateUniforms() method also fills the \p "uniform vec3 eye_position" variable which contains the camera position in
 * object space, useful to compute specular highlights etc.
 *
 * \sa 
 * - \ref pagGuideSlicedVolume
 * - \ref pagGuideRaycastVolume
 * - RaycastVolume
 */
//-----------------------------------------------------------------------------
//! Constructor.
SlicedVolume::SlicedVolume()
{
  VL_DEBUG_SET_OBJECT_NAME()
  mSliceCount = 1024;
  mGeometry = new Geometry;
  mGeometry->setObjectName("vl::SlicedVolume");
  
  fvec3 texc[] = 
  {
    fvec3(0,0,0), fvec3(1,0,0), fvec3(1,1,0), fvec3(0,1,0),
    fvec3(0,0,1), fvec3(1,0,1), fvec3(1,1,1), fvec3(0,1,1)
  };
  memcpy(mTexCoord, texc, sizeof(texc));
}
//-----------------------------------------------------------------------------
/** Reimplement this method to update the uniform variables of your GLSL program before the volume is rendered.
 * By default updateUniforms() updates the position of up to 4 lights in object space. Such positions are stored in the
 * \p "uniform vec3 light_position[4]" variable.
 * The updateUniforms() method also fills the \p "uniform bool light_enable[4]" variable with a flag marking if the Nth 
 * light is active or not. These light values are computed based on the lights bound to the current Shader.
 * The updateUniforms() method also fills the \p "uniform vec3 eye_position" variable which contains the camera position in
 * object space, useful to compute specular highlights etc.
 */
void SlicedVolume::updateUniforms(Actor*actor, real, const Camera* camera, Renderable*, const Shader* shader)
{
  const GLSLProgram* glsl = shader->getGLSLProgram();

  if (glsl->getUniformLocation("light_position") != -1 && glsl->getUniformLocation("light_enable") != -1)
  {
    // computes up to 4 light positions (in object space) and enables

    int light_enable[4] = { 0,0,0,0 };
    fvec3 light_position[4];

    for(int i=0; i<4; ++i)
    {
      const Light* light = shader->getLight(i);
      light_enable[i] = light != NULL;
      if (light)
      {
        // light position following transform
        if (light->boundTransform())
          light_position[i] = (fmat4)light->boundTransform()->worldMatrix() * light->position().xyz();
        // light position following camera
        else
          light_position[i] = ((fmat4)camera->modelingMatrix() * light->position()).xyz();

        // light position in object space
        if (actor->transform())
          light_position[i] = (fmat4)actor->transform()->worldMatrix().getInverse() * light_position[i];
      }
    }

    actor->gocUniform("light_position")->setUniform(4, light_position);
    actor->gocUniform("light_enable")->setUniform1i(4, light_enable);
  }

  if (glsl->getUniformLocation("eye_position") != -1)
  {
    // pass the eye position in object space

    // eye postion
    fvec3 eye = (fvec3)camera->modelingMatrix().getT();
    // world to object space
    if (actor->transform())
      eye = (fmat4)actor->transform()->worldMatrix().getInverse() * eye;
    actor->gocUniform("eye_position")->setUniform(eye);
  }
}
//-----------------------------------------------------------------------------
namespace
{
  class Edge
  {
  public:
    int v0, v1, intersection, flags;
    bool operator<(const Edge& other) const
    {
      return intersection > other.intersection;
    }
  };
}
//-----------------------------------------------------------------------------
void SlicedVolume::bindActor(Actor* actor)
{
  actor->actorEventCallbacks()->push_back( this );
  actor->setLod(0, mGeometry.get());
}
//-----------------------------------------------------------------------------
void SlicedVolume::onActorRenderStarted(Actor* actor, real clock, const Camera* camera, Renderable* rend, const Shader* shader, int pass)
{
  if (pass>0)
    return;

  // setup uniform variables

  if (shader->getGLSLProgram())
    updateUniforms(actor, clock, camera, rend, shader);

  // setup geometry: generate viewport aligned slices

  // skip generation is actor and camera did not move
  fmat4 mat;
  if (actor->transform())
    mat = (fmat4)(camera->viewMatrix() * actor->transform()->worldMatrix());
  else
    mat = (fmat4)camera->viewMatrix();

  if (mCache == mat)
    return;
  else
    mCache = mat;

  fmat4 imat = mat.getInverse();

  fvec3 cube_verts[] =
  {
    fvec3((float)box().minCorner().x(), (float)box().minCorner().y(), (float)box().minCorner().z()),
    fvec3((float)box().maxCorner().x(), (float)box().minCorner().y(), (float)box().minCorner().z()),
    fvec3((float)box().maxCorner().x(), (float)box().maxCorner().y(), (float)box().minCorner().z()),
    fvec3((float)box().minCorner().x(), (float)box().maxCorner().y(), (float)box().minCorner().z()),
    fvec3((float)box().minCorner().x(), (float)box().minCorner().y(), (float)box().maxCorner().z()),
    fvec3((float)box().maxCorner().x(), (float)box().minCorner().y(), (float)box().maxCorner().z()),
    fvec3((float)box().maxCorner().x(), (float)box().maxCorner().y(), (float)box().maxCorner().z()),
    fvec3((float)box().minCorner().x(), (float)box().maxCorner().y(), (float)box().maxCorner().z())
  };

  int min_idx = 0;
  int max_idx = 0;
  for(int i=0; i<8; ++i)
  {
    cube_verts[i] = mat * cube_verts[i];
    if (fabs(cube_verts[i].z()) < fabs(cube_verts[min_idx].z())) min_idx = i;
    if (fabs(cube_verts[i].z()) > fabs(cube_verts[max_idx].z())) max_idx = i;
  }

  if (cube_verts[min_idx].z() > 0)
  {
    // fixme?
    // the actor is not visible: remove the geometry or disable the actor?
    // return;
  }

  const int TOP    = 1;
  const int BOTTOM = 2;
  const int LEFT   = 4;
  const int RIGHT  = 8;
  const int FRONT  = 16;
  const int BACK   = 32;

  Edge edges[] =
  {
    {0,1,-1,FRONT |BOTTOM}, {1,2,-1,FRONT|RIGHT}, {2,3,-1,FRONT|TOP},  {3,0,-1,FRONT |LEFT},
    {4,5,-1,BACK  |BOTTOM}, {5,6,-1,BACK |RIGHT}, {6,7,-1,BACK |TOP},  {7,4,-1,BACK  |LEFT},
    {1,5,-1,BOTTOM|RIGHT},  {2,6,-1,TOP  |RIGHT}, {3,7,-1,TOP  |LEFT}, {0,4,-1,BOTTOM|LEFT}
  };

  std::vector<fvec3> points;
  std::vector<fvec3> points_t;
  std::vector<fvec3> polygons;
  std::vector<fvec3> polygons_t;

  polygons.reserve(sliceCount()*5);
  polygons_t.reserve(sliceCount()*5);
  float zrange = cube_verts[max_idx].z() - cube_verts[min_idx].z();
  float zstep  = zrange/(sliceCount()+1);
  int vert_idx[12];
  for(int islice=0; islice<sliceCount(); ++islice)
  {
    float z = cube_verts[max_idx].z() - zstep*(islice+1);
    fvec3 plane_o(0,0,z);
    fvec3 plane_n(0,0,1.0f);
    points.clear();
    points_t.clear();
    for(int iedge=0; iedge<12; ++iedge)
    {
      edges[iedge].intersection = -1;
      fvec3 vi  = cube_verts[ edges[iedge].v0 ];
      fvec3 eij = cube_verts[ edges[iedge].v1 ] - cube_verts[ edges[iedge].v0 ];
      float denom = dot(plane_n,eij);
      if (denom == 0)
        continue;
      float lambda = (z - dot(plane_n,vi))/denom;
      if (lambda<0 || lambda>1)
        continue;
      fvec3 v = vi + eij*lambda;
      edges[iedge].intersection = (int)points.size();
      points.push_back(v);
      fvec3 a = texCoords()[ edges[iedge].v0 ];
      fvec3 b = texCoords()[ edges[iedge].v1 ] - texCoords()[ edges[iedge].v0 ];
      fvec3 vt = a + b*lambda;
      points_t.push_back(vt);
    }
    std::sort(edges, edges+12);
    int vert_idx_c = 0;
    for(int ie0=0; ie0<12-1; ++ie0)
    {
      if (edges[ie0].intersection == -1)
        break;
      vert_idx[vert_idx_c++] = edges[ie0].intersection;
      for(int ie1=ie0+1; ie1<12; ++ie1)
      {
        if (edges[ie1].intersection == -1)
          continue;
        if( (edges[ie0].flags & edges[ie1].flags) )
        {
          Edge t       = edges[ie0+1];
          edges[ie0+1] = edges[ie1];
          edges[ie1]   = t;
          break;
        }
      }
    }
    for(int vc=0; vc<vert_idx_c-2; ++vc)
    {
      polygons.push_back(imat*points  [vert_idx[0]]);
      polygons.push_back(imat*points  [vert_idx[vc+1]]);
      polygons.push_back(imat*points  [vert_idx[vc+2]]);
      polygons_t.push_back(points_t[vert_idx[0]]);
      polygons_t.push_back(points_t[vert_idx[vc+1]]);
      polygons_t.push_back(points_t[vert_idx[vc+2]]);
    }
    #ifndef NDEBUG
      for(int ie0=0; ie0<12-1; ++ie0)
      {
        if (edges[ie0].intersection == -1)
          break;
        if (edges[ie0+1].intersection == -1)
          break;
        VL_CHECK(edges[ie0].flags & edges[ie0+1].flags)
      }
    #endif
  }

  mGeometry->drawCalls()->clear();
  ref<DrawArrays> da = new DrawArrays(PT_TRIANGLES, 0, (int)polygons.size());
  mGeometry->drawCalls()->push_back( da.get() );
  ref<ArrayFloat3> vertex_array = new ArrayFloat3;
  ref<ArrayFloat3> texcoo_array = new ArrayFloat3;
  vertex_array->resize(polygons.size());
  texcoo_array->resize(polygons_t.size());
  VL_CHECK((size_t)vertex_array->bufferObject()->bytesUsed() == sizeof(polygons  [0])*polygons.  size());
  VL_CHECK((size_t)texcoo_array->bufferObject()->bytesUsed() == sizeof(polygons_t[0])*polygons_t.size());
  memcpy(vertex_array->ptr(), &polygons  [0], vertex_array->bufferObject()->bytesUsed());
  memcpy(texcoo_array->ptr(), &polygons_t[0], texcoo_array->bufferObject()->bytesUsed());
  mGeometry->setVertexArray(vertex_array.get());
  mGeometry->setTexCoordArray(0,texcoo_array.get());

  mGeometry->setDisplayListDirty(true);
  mGeometry->setBufferObjectDirty(true);

  // fixme: 
  // it seems we have some problems with camera clipping/culling when the camera is close to the volume: the slices disappear or degenerate.
  // it does not seem to depend from camera clipping plane optimization.
}
//-----------------------------------------------------------------------------
void SlicedVolume::generateTextureCoordinates(const ivec3& img_size)
{
  if (!img_size.x() || !img_size.y() || !img_size.z())
  {
    Log::error("SlicedVolume::generateTextureCoordinates(): failed! The img_size passed does not represent a 3D image.\n");
    return;
  }

  float dx = 0.5f/img_size.x();
  float dy = 0.5f/img_size.y();
  float dz = 0.5f/img_size.z();

  float x0 = 0.0f + dx;
  float x1 = 1.0f - dx;
  float y0 = 0.0f + dy;
  float y1 = 1.0f - dy;
  float z0 = 0.0f + dz;
  float z1 = 1.0f - dz;

  fvec3 texc[] = 
  {
    fvec3(x0,y0,z0), fvec3(x1,y0,z0), fvec3(x1,y1,z0), fvec3(x0,y1,z0),
    fvec3(x0,y0,z1), fvec3(x1,y0,z1), fvec3(x1,y1,z1), fvec3(x0,y1,z1),
  };
  memcpy(mTexCoord, texc, sizeof(texc));
}
//-----------------------------------------------------------------------------
void SlicedVolume::generateTextureCoordinates(const ivec3& img_size, const ivec3& min_corner, const ivec3& max_corner)
{
    if (!img_size.x() || !img_size.y() || !img_size.z())
    {
        Log::error("SlicedVolume::setDisplayRegion(): failed! The size passed does not represent a 3D image.\n");
        return;
    }

    float dx = 0.5f/img_size.x();
    float dy = 0.5f/img_size.y();
    float dz = 0.5f/img_size.z();

    float x0 = min_corner.x()/(float)img_size.x() + dx;
    float x1 = max_corner.x()/(float)img_size.x() - dx;
    float y0 = min_corner.y()/(float)img_size.y() + dy;
    float y1 = max_corner.y()/(float)img_size.y() - dy;
    float z0 = min_corner.z()/(float)img_size.z() + dz;
    float z1 = max_corner.z()/(float)img_size.z() - dz;

    fvec3 texc[] = 
    {
        fvec3(x0,y0,z0), fvec3(x1,y0,z0), fvec3(x1,y1,z0), fvec3(x0,y1,z0),
        fvec3(x0,y0,z1), fvec3(x1,y0,z1), fvec3(x1,y1,z1), fvec3(x0,y1,z1)
    };
    memcpy(mTexCoord, texc, sizeof(texc));
}
//-----------------------------------------------------------------------------
void SlicedVolume::setBox(const AABB& box) 
{
  mBox = box; 
  mCache.fill(0);
  mGeometry->setBoundingBox( box );
  mGeometry->setBoundingSphere( box );
  mGeometry->setBoundsDirty(true);
}
//-----------------------------------------------------------------------------
