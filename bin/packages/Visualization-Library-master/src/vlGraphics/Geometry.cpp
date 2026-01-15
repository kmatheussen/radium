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

#include <vlGraphics/Geometry.hpp>
#include <vlGraphics/OpenGLContext.hpp>
#include <vlGraphics/DoubleVertexRemover.hpp>
#include <vlGraphics/MultiDrawElements.hpp>
#include <vlGraphics/DrawRangeElements.hpp>
#include <cmath>
#include <algorithm>

using namespace vl;

//-----------------------------------------------------------------------------
// Geometry
//-----------------------------------------------------------------------------
Geometry::Geometry()
{
  VL_DEBUG_SET_OBJECT_NAME()
  mVertexAttribArrays.setAutomaticDelete(false);
  mTexCoordArrays.setAutomaticDelete(false);
  mDrawCalls.setAutomaticDelete(false);
}
//-----------------------------------------------------------------------------
Geometry::~Geometry()
{
}
//-----------------------------------------------------------------------------
void Geometry::computeBounds_Implementation()
{
  const ArrayAbstract* coords = vertexArray() ? vertexArray() : vertexAttribArray(vl::VA_Position) ? vertexAttribArray(vl::VA_Position)->data() : NULL;

  if (coords == NULL)
  {
    Log::debug("Geometry::computeBounds_Implementation() failed! No vertex buffer present!\n");
    return;
  }

  if (coords->size() == 0)
  {
    Log::debug("Geometry::computeBounds_Implementation() failed! No vertices present in the local buffer! Did you forget to call setBoundingBox() and setBoundingSphere()?\n");
    return;
  }

  AABB aabb;
  for(int i=0; i<drawCalls()->size(); ++i)
  {
    for(IndexIterator iit = drawCalls()->at(i)->indexIterator(); iit.hasNext(); iit.next())
    {
      aabb += coords->getAsVec3( iit.index() );
    }
  }

  real radius = 0, r = 0;
  vec3 center = aabb.center();
  for(int i=0; i<drawCalls()->size(); ++i)
  {
    for(IndexIterator iit = drawCalls()->at(i)->indexIterator(); iit.hasNext(); iit.next())
    {
      r = (coords->getAsVec3(iit.index()) - center).lengthSquared();
      if (r > radius)
        radius = r;
    }
  }

  setBoundingBox( aabb );
  setBoundingSphere( Sphere(center, radius) );
}
//-----------------------------------------------------------------------------
ref<Geometry> Geometry::deepCopy() const
{
  ref<Geometry> geom = new Geometry;
  geom->deepCopyFrom(*this);
  return geom;
}
//-----------------------------------------------------------------------------
Geometry& Geometry::deepCopyFrom(const Geometry& other)
{
  // copy the base class Renderable
  super::operator=(other);

  // copy Geometry
  mVertexArray         = other.mVertexArray         ? other.mVertexArray->clone().get()         : NULL;
  mNormalArray         = other.mNormalArray         ? other.mNormalArray->clone().get()         : NULL;
  mColorArray          = other.mColorArray          ? other.mColorArray->clone().get()          : NULL;
  mSecondaryColorArray = other.mSecondaryColorArray ? other.mSecondaryColorArray->clone().get() : NULL;
  mFogCoordArray       = other.mFogCoordArray       ? other.mFogCoordArray->clone().get()       : NULL;

  mTexCoordArrays.resize( other.mTexCoordArrays.size() );
  for(int i=0; i<mTexCoordArrays.size(); ++i)
    mTexCoordArrays[i] = new TextureArray(other.mTexCoordArrays[i]->mTextureSampler, other.mTexCoordArrays[i]->mTexCoordArray ? other.mTexCoordArrays[i]->mTexCoordArray->clone().get() : NULL);

  // custom arrays
  mVertexAttribArrays.resize( other.mVertexAttribArrays.size() );
  for(int i=0; i<mVertexAttribArrays.size(); ++i)
  {
    mVertexAttribArrays[i] = new VertexAttribInfo;
    mVertexAttribArrays[i]->setNormalize( other.mVertexAttribArrays[i]->normalize() );
    mVertexAttribArrays[i]->setInterpretation( other.mVertexAttribArrays[i]->interpretation() );
    mVertexAttribArrays[i]->setAttribLocation( other.mVertexAttribArrays[i]->attribLocation() );
    mVertexAttribArrays[i]->setData( other.mVertexAttribArrays[i]->data() ? other.mVertexAttribArrays[i]->data()->clone().get() : NULL );
  }

  // primitives
  mDrawCalls.clear();
  for(int i=0; i<other.mDrawCalls.size(); ++i)
    mDrawCalls.push_back( other.mDrawCalls[i]->clone().get() );

  return *this;
}
//-----------------------------------------------------------------------------
ref<Geometry> Geometry::shallowCopy() const
{
  ref<Geometry> geom = new Geometry;
  geom->shallowCopyFrom(*this);
  return geom;
}
//-----------------------------------------------------------------------------
Geometry& Geometry::shallowCopyFrom(const Geometry& other)
{
  // copy the base class Renderable
  super::operator=(other);

  // copy Geometry attributes
  mVertexArray = other.mVertexArray;
  mNormalArray = other.mNormalArray;
  mColorArray = other.mColorArray;
  mSecondaryColorArray = other.mSecondaryColorArray;
  mFogCoordArray = other.mFogCoordArray;
  mTexCoordArrays = other.mTexCoordArrays;
  mVertexAttribArrays = other.mVertexAttribArrays;
  mDrawCalls = other.mDrawCalls;

  return *this;
}
//-----------------------------------------------------------------------------
void Geometry::setVertexArray(ArrayAbstract* data)
{
  // if one of this checks fail read the OpenGL Programmers Guide or the Reference Manual 
  // to see what "size" and "type" are allowed for glVertexPointer
  VL_CHECK( !data || (data->glSize() >=2 && data->glSize()<=4) )

  mVertexArray = data;
}
//-----------------------------------------------------------------------------
void Geometry::setNormalArray(ArrayAbstract* data)
{
  // if one of this checks fail read the OpenGL Programmers Guide or the Reference Manual 
  // to see what "size" and "type" are allowed for glNormalPointer
  VL_CHECK( !data || data->glSize() == 3 )
  VL_CHECK( !data || (data->glType() == GL_BYTE||
                      data->glType() == GL_SHORT ||
                      data->glType() == GL_INT ||
                      data->glType() == GL_FLOAT ||
                      data->glType() == GL_DOUBLE) );

  mNormalArray = data;
}
//-----------------------------------------------------------------------------
void Geometry::setColorArray(ArrayAbstract* data)
{
  // if one of this checks fail read the OpenGL Programmers Guide or the Reference Manual 
  // to see what "size" and "type" are allowed for glColorPointer
  VL_CHECK( !data || (data->glSize() >=3 && data->glSize()<=4) )
  VL_CHECK( !data || (data->glType() == GL_BYTE ||
                      data->glType() == GL_SHORT ||
                      data->glType() == GL_INT ||
                      data->glType() == GL_UNSIGNED_BYTE ||
                      data->glType() == GL_UNSIGNED_SHORT ||
                      data->glType() == GL_UNSIGNED_INT ||
                      data->glType() == GL_FLOAT ||
                      data->glType() == GL_DOUBLE) );

  mColorArray = data;
}
//-----------------------------------------------------------------------------
void Geometry::setSecondaryColorArray(ArrayAbstract* data)
{
  // if one of this checks fail read the OpenGL Programmers Guide or the Reference Manual 
  // to see what "size" and "type" are allowed for glSecondaryColorPointer
  VL_CHECK( !data || (data->glSize() >=3 && data->glSize()<=4) )
  VL_CHECK( !data || (data->glType() == GL_BYTE ||
                      data->glType() == GL_SHORT ||
                      data->glType() == GL_INT ||
                      data->glType() == GL_UNSIGNED_BYTE ||
                      data->glType() == GL_UNSIGNED_SHORT ||
                      data->glType() == GL_UNSIGNED_INT ||
                      data->glType() == GL_FLOAT ||
                      data->glType() == GL_DOUBLE) );

  mSecondaryColorArray = data;
}
//-----------------------------------------------------------------------------
void Geometry::setFogCoordArray(ArrayAbstract* data)
{
  // if one of this checks fail read the OpenGL Programmers Guide or the Reference Manual 
  // to see what "size" and "type" are allowed for glFogCoordPointer
  VL_CHECK( !data || (data->glSize() == 1) )
  VL_CHECK( !data || (data->glType() == GL_FLOAT || data->glType() == GL_DOUBLE) );
  
  mFogCoordArray = data;
}
//-----------------------------------------------------------------------------
void Geometry::setTexCoordArray(int tex_unit, ArrayAbstract* data)
{
  // if one of this checks fail read the OpenGL Programmers Guide or the Reference Manual 
  // to see what "size" and "type" are allowed for glTexCoordPointer
  VL_CHECK( !data || (data->glSize() == 1 || data->glSize() == 2 || data->glSize() == 3 || data->glSize() == 4) )
  VL_CHECK( !data || (data->glType() == GL_FLOAT  || 
                      data->glType() == GL_DOUBLE ||
                      data->glType() == GL_SHORT  ||
                      data->glType() == GL_INT) );

  VL_CHECK(tex_unit<VL_MAX_TEXTURE_UNITS);

  for(int i=0; i<mTexCoordArrays.size(); ++i)
  {
    if (mTexCoordArrays.at(i)->mTextureSampler == tex_unit)
    {
      if (data)
        mTexCoordArrays.at(i)->mTexCoordArray = data;
      else
        mTexCoordArrays.erase(i,1); // removes if NULL
      return;
    }
  }
  if (data)
    mTexCoordArrays.push_back(new TextureArray(tex_unit,data));
}
//-----------------------------------------------------------------------------
void Geometry::clearArrays(bool clear_draw_calls)
{
  setBufferObjectDirty(true);
  mVertexArray = NULL;
  mNormalArray = NULL;
  mColorArray = NULL;
  mSecondaryColorArray = NULL;
  mFogCoordArray = NULL;
  mTexCoordArrays.clear();
  mVertexAttribArrays.clear();
  if (clear_draw_calls)
    mDrawCalls.clear();
}
//-----------------------------------------------------------------------------
bool Geometry::flipNormals()
{
  ArrayAbstract* normarr = normalArray() ? normalArray() : vertexAttribArray(vl::VA_Normal) ? vertexAttribArray(vl::VA_Normal)->data() : NULL;

  if (normarr)
  {
    ArrayFloat3* norm3f = normarr->as<ArrayFloat3>();
    if (norm3f)
    {
      for(u32 i=0; i<norm3f->size(); ++i)
      {
        norm3f->at(i) = -norm3f->at(i);
      }
      return true;
    }
  }
  return false;
}
//-----------------------------------------------------------------------------
void Geometry::convertToVertexAttribs()
{
  std::map<int, ref<ArrayAbstract> > attrib_map;

  if (vertexArray())
  {
    attrib_map[VA_Position] = vertexArray();
    setVertexArray(NULL);
  }
  
  if (normalArray())
  {
    attrib_map[VA_Normal] = normalArray();
    setNormalArray(NULL);
  }
  
  if (colorArray())
  {
    attrib_map[VA_Color] = colorArray();
    setColorArray(NULL);
  }

  // Texture coordinates starting from VA_TexCoord0
  for(int i=0; i<mTexCoordArrays.size(); i++)
  {
    attrib_map[VA_TexCoord0+i] = mTexCoordArrays[i]->mTexCoordArray;
  }
  mTexCoordArrays.clear();
  
  // Secondary color and fog are packed right after the texture coordinates
  int index = VA_TexCoord0 + mTexCoordArrays.size();
  if (secondaryColorArray())
  {
    attrib_map[index++] = secondaryColorArray();
    setSecondaryColorArray(NULL);
  }

  if (fogCoordArray())
  {
    attrib_map[index++] = fogCoordArray();
    setFogCoordArray(NULL);
  }

  // copy over the collected attributes
  // note: we override eventual existing vertex attributes if they are in busy positions, the other are left where they are
  for(std::map<int, ref<ArrayAbstract> >::iterator it=attrib_map.begin(); it != attrib_map.end(); ++it)
  {
    if (vertexAttribArray(it->first) != NULL)
      Log::warning( Say("Geometry::convertToVertexAttribs(): vertex attrib index #%n is already in use, it will be overwritten.\n") << it->first );
    setVertexAttribArray(it->first, it->second.get());
  }

}
//-----------------------------------------------------------------------------
void Geometry::computeNormals(bool verbose)
{
  // Retrieve vertex position array
  ArrayAbstract* posarr = vertexArray() ? vertexArray() : vertexAttribArray(vl::VA_Position) ? vertexAttribArray(vl::VA_Position)->data() : NULL;
  if (!posarr || posarr->size() == 0)
  {
    Log::warning("Geometry::computeNormals() failed: no vertices found!\n");
    return;
  }

  ref<ArrayFloat3> norm3f = new ArrayFloat3;
  norm3f->resize( posarr->size() );

  // Install the normal array
  if (vertexArray())
    setNormalArray( norm3f.get() );
  else
    setVertexAttribArray(VA_Normal, norm3f.get());

  // zero the normals
  for(u32 i=0; i<norm3f->size(); ++i)
    (*norm3f)[i] = 0;

  // iterate all draw calls
  for(int prim=0; prim<(int)drawCalls()->size(); prim++)
  {
    // iterate all triangles, if present
    for(TriangleIterator trit = mDrawCalls[prim]->triangleIterator(); trit.hasNext(); trit.next())
    {
      u32 a = trit.a();
      u32 b = trit.b();
      u32 c = trit.c();

      if (verbose)
      if (a == b || b == c || c == a)
      {
        Log::warning( Say("Geometry::computeNormals(): skipping degenerate triangle %n %n %n\n") << a << b << c );
        continue;
      }

      VL_CHECK( a < posarr->size() )
      VL_CHECK( b < posarr->size() )
      VL_CHECK( c < posarr->size() )

      vec3 n, v0, v1, v2;

      v0 = posarr->getAsVec3(a);
      v1 = posarr->getAsVec3(b);
      v2 = posarr->getAsVec3(c);

      if (verbose)
      if (v0 == v1 || v1 == v2 || v2 == v0)
      {
        Log::warning("Geometry::computeNormals(): skipping degenerate triangle (same vertex coodinate).\n");
        continue;
      }

      v1 -= v0;
      v2 -= v0;

      n = cross(v1, v2);
      n.normalize();
      if (verbose)
      if ( fabs(1.0f - n.length()) > 0.1f )
      {
        Log::warning("Geometry::computeNormals(): skipping degenerate triangle (normalization failed).\n");
        continue;
      }

      (*norm3f)[a] += (fvec3)n;
      (*norm3f)[b] += (fvec3)n;
      (*norm3f)[c] += (fvec3)n;
    }
  }

  // normalize the normals
  for(int i=0; i<(int)norm3f->size(); ++i)
    (*norm3f)[i].normalize();
}
//-----------------------------------------------------------------------------
void Geometry::deleteBufferObject()
{
  if (!Has_BufferObject)
    return;

  for(int i=0; i<(int)drawCalls()->size(); ++i)
    drawCalls()->at(i)->deleteBufferObject();

  if (mVertexArray)
    mVertexArray->bufferObject()->deleteBufferObject();
  
  if (mNormalArray)
    mNormalArray->bufferObject()->deleteBufferObject();
  
  if (mColorArray)
    mColorArray->bufferObject()->deleteBufferObject();
  
  if (mSecondaryColorArray)
    mSecondaryColorArray->bufferObject()->deleteBufferObject();
  
  if (mFogCoordArray)
    mFogCoordArray->bufferObject()->deleteBufferObject();
  
  for (int i=0; i<mTexCoordArrays.size(); ++i)
    mTexCoordArrays[i]->mTexCoordArray->bufferObject()->deleteBufferObject();

  for(int i=0; i<vertexAttribArrays()->size(); ++i)
    if ( vertexAttribArrays()->at(i)->data() )
      vertexAttribArrays()->at(i)->data()->bufferObject()->deleteBufferObject();
}
//-----------------------------------------------------------------------------
void Geometry::updateDirtyBufferObject(EBufferObjectUpdateMode mode)
{
  if (!Has_BufferObject)
    return;

  bool force_update = (mode & BUF_ForceUpdate) != 0;

  if ( mVertexArray && (mVertexArray->isBufferObjectDirty() || force_update) )
    mVertexArray->updateBufferObject(mode);
  
  if ( mNormalArray && (mNormalArray->isBufferObjectDirty() || force_update) )
    mNormalArray->updateBufferObject(mode);
  
  if ( mColorArray && (mColorArray->isBufferObjectDirty() || force_update) )
    mColorArray->updateBufferObject(mode);
  
  if ( mSecondaryColorArray && (mSecondaryColorArray->isBufferObjectDirty() || force_update) )
    mSecondaryColorArray->updateBufferObject(mode);
  
  if ( mFogCoordArray && (mFogCoordArray->isBufferObjectDirty() || force_update) )
    mFogCoordArray->updateBufferObject(mode);
  
  for(int i=0; i<mTexCoordArrays.size(); ++i)
  {
    if ( mTexCoordArrays[i]->mTexCoordArray->isBufferObjectDirty() || force_update )
      mTexCoordArrays[i]->mTexCoordArray->updateBufferObject(mode);
  }
  
  for(int i=0; i<vertexAttribArrays()->size(); ++i)
    if ( vertexAttribArrays()->at(i)->data() && (vertexAttribArrays()->at(i)->data()->isBufferObjectDirty() || force_update) )
      vertexAttribArrays()->at(i)->data()->updateBufferObject(mode);

  for(int i=0; i<drawCalls()->size(); ++i)
    drawCalls()->at(i)->updateDirtyBufferObject(mode);
}
//-----------------------------------------------------------------------------
void Geometry::render_Implementation(const Actor*, const Shader*, const Camera*, OpenGLContext* gl_context) const
{
  VL_CHECK_OGL()

  // bind Vertex Attrib Set

  bool vbo_on = Has_BufferObject && isBufferObjectEnabled() && !isDisplayListEnabled();
  gl_context->bindVAS(this, vbo_on, false);

  // actual draw

  for(int i=0; i<(int)drawCalls()->size(); i++)
    if (drawCalls()->at(i)->isEnabled())
      drawCalls()->at(i)->render( vbo_on );

  VL_CHECK_OGL()
}
//-----------------------------------------------------------------------------
void Geometry::transform(const mat4& m, bool normalize)
{
  ArrayAbstract* posarr = vertexArray() ? vertexArray() : vertexAttribArray(vl::VA_Position) ? vertexAttribArray(vl::VA_Position)->data() : NULL;
  if (posarr)
    posarr->transform(m);

  ArrayAbstract* normarr = normalArray() ? normalArray() : vertexAttribArray(vl::VA_Normal) ? vertexAttribArray(vl::VA_Normal)->data() : NULL;
  if (normarr)
  {
    mat4 nmat = m.as3x3().invert().transpose();
    normarr->transform(nmat);
    if (normalize)
      normarr->normalize();
  }
}
//-----------------------------------------------------------------------------
void Geometry::setVertexAttribArray(const VertexAttribInfo& info)
{
  for(int i=0; i<vertexAttribArrays()->size(); ++i)
  {
    VL_CHECK(vertexAttribArrays()->at(i))
    if (vertexAttribArrays()->at(i)->attribLocation() == info.attribLocation())
    {
      *vertexAttribArrays()->at(i) = info;
      return;
    }
  }
  mVertexAttribArrays.push_back( new VertexAttribInfo(info) );
}
//-----------------------------------------------------------------------------
const VertexAttribInfo* Geometry::vertexAttribArray(unsigned int attrib_location) const
{
  for(int i=0; i<vertexAttribArrays()->size(); ++i)
    if (vertexAttribArrays()->at(i)->attribLocation() == attrib_location)
      return vertexAttribArrays()->at(i);
  return NULL;
}
//-----------------------------------------------------------------------------
VertexAttribInfo* Geometry::vertexAttribArray(unsigned int attrib_location)
{
  for(int i=0; i<vertexAttribArrays()->size(); ++i)
    if (vertexAttribArrays()->at(i)->attribLocation() == attrib_location)
      return vertexAttribArrays()->at(i);
  return NULL;
}
//-----------------------------------------------------------------------------
DrawCall* Geometry::mergeTriangleStrips()
{
  ArrayAbstract* posarr = vertexArray() ? vertexArray() : vertexAttribArray(vl::VA_Position) ? vertexAttribArray(vl::VA_Position)->data() : NULL;

  if (!posarr)
    return NULL;

  std::vector< ref<DrawElementsBase> > de_vector;
  std::vector<u32> indices;

  // collect DrawElementsUInt
  for(int i=drawCalls()->size(); i--; )
  {
    ref<DrawElementsBase> deb = cast<DrawElementsBase>( drawCalls()->at(i) );
    if (deb && deb->primitiveType() == PT_TRIANGLE_STRIP)
    {
      // preserve order
      de_vector.push_back( deb );
      drawCalls()->eraseAt(i);
    }
  }
  // preseve rendering order
  std::reverse(de_vector.begin(), de_vector.end());

  // generate new strip
  indices.reserve( posarr->size()*2 );
  for(u32 i=0; i<de_vector.size(); ++i)
  {
    u32 index_count = 0;
    for(IndexIterator it=de_vector[i]->indexIterator(); it.hasNext(); it.next(), ++index_count)
      indices.push_back(it.index());

    if (index_count == 0)
      continue;
    
    // odd -> even
    if ( index_count % 2 )
      indices.push_back( indices.back() );

    // concatenate next strip inserting degenerate triangles
    if ( i != de_vector.size()-1 )
    {
      // grab the first two indices of the next draw call
      IndexIterator it = de_vector[i+1]->indexIterator();
      int A = it.index();
      it.next();
      int B = it.index();

      if (A == -1 || B == -1)
        continue;

      indices.push_back( indices.back() );
      indices.push_back(A);
      indices.push_back(A);
      indices.push_back(B);
    }
  }

  if (indices.size())
  {
    ref<DrawElementsUInt> draw_elems = new DrawElementsUInt(PT_TRIANGLE_STRIP);
    draw_elems->indexBuffer()->resize(indices.size());
    memcpy(draw_elems->indexBuffer()->ptr(), &indices[0], sizeof(indices[0])*indices.size());
    drawCalls()->push_back(draw_elems.get());
    return draw_elems.get();
  }
  else
    return NULL;
}
//-----------------------------------------------------------------------------
void Geometry::mergeDrawCallsWithPrimitiveRestart(EPrimitiveType primitive_type)
{
  u32 total_index_count = 0;
  std::vector< ref<DrawCall> > mergendo_calls;
  for( u32 i=drawCalls()->size(); i--; )
  {
    if (drawCalls()->at(i)->primitiveType() == primitive_type)
    {
      int index_count = drawCalls()->at(i)->countIndices();
      VL_CHECK(index_count >= 0);
      total_index_count += index_count;
      // insert at the head to preserve the primitive rendering order
      mergendo_calls.push_back( drawCalls()->at(i) );
      drawCalls()->eraseAt(i);
    }
  }
  // preseve rendering order
  std::reverse(mergendo_calls.begin(), mergendo_calls.end());

  Log::debug( Say("%n draw calls will be merged using primitive restart.\n") << mergendo_calls.size() );

  if (mergendo_calls.empty())
    return;

#ifndef NDEBUG
  ArrayAbstract* posarr = vertexArray() ? vertexArray() : vertexAttribArray(vl::VA_Position) ? vertexAttribArray(vl::VA_Position)->data() : NULL;
#endif

  ref<DrawElementsUInt> de_prim_restart = new DrawElementsUInt(primitive_type);
  // make space for all the indices plus the primitive restart markers.
  de_prim_restart->indexBuffer()->resize(total_index_count + mergendo_calls.size()-1);
  GLuint* index = de_prim_restart->indexBuffer()->begin();
  // merge draw calls using primitive restart!
  for( u32 i=0; i<mergendo_calls.size(); ++i )
  {
    for( IndexIterator it = mergendo_calls[i]->indexIterator(); it.hasNext(); it.next(), ++index )
    {
      *index = it.index();
      VL_CHECK(*index < posarr->size());
    }
    if ( i != mergendo_calls.size() -1 )
    {
      *index = DrawElementsUInt::primitive_restart_index;
      ++index;
    }
  }
  VL_CHECK( index == de_prim_restart->indexBuffer()->end() )

  // enable primitive restart!
  de_prim_restart->setPrimitiveRestartEnabled(true);

  drawCalls()->push_back( de_prim_restart.get() );
}
//-----------------------------------------------------------------------------
void Geometry::mergeDrawCallsWithMultiDrawElements(EPrimitiveType primitive_type)
{
  u32 total_index_count = 0;
  std::vector< ref<DrawCall> > mergendo_calls;
  std::vector<GLsizei> count_vector;
  for( u32 i=drawCalls()->size(); i--; )
  {
    if (drawCalls()->at(i)->primitiveType() == primitive_type)
    {
      int index_count = drawCalls()->at(i)->countIndices();
      VL_CHECK(index_count >= 0);
      total_index_count += index_count;
      count_vector.push_back( index_count );
      mergendo_calls.push_back( drawCalls()->at(i) );
      drawCalls()->eraseAt(i);
    }
  }
  // preseve rendering order
  std::reverse(mergendo_calls.begin(), mergendo_calls.end());
  std::reverse(count_vector.begin(), count_vector.end());

  Log::debug( Say("%n draw calls will be merged using MultiDrawElements.\n") << mergendo_calls.size() );

  if (mergendo_calls.empty())
    return;

#ifndef NDEBUG
  ArrayAbstract* posarr = vertexArray() ? vertexArray() : vertexAttribArray(vl::VA_Position) ? vertexAttribArray(vl::VA_Position)->data() : NULL;
#endif

  ref<MultiDrawElementsUInt> de_multi = new MultiDrawElementsUInt(primitive_type);
  // make space for all the indices plus the primitive restart markers.
  de_multi->indexBuffer()->resize(total_index_count);
  GLuint* index = de_multi->indexBuffer()->begin();
  // merge draw calls using primitive restart!
  for( u32 i=0; i<mergendo_calls.size(); ++i )
  {
    for( IndexIterator it = mergendo_calls[i]->indexIterator(); it.hasNext(); it.next(), ++index )
    {
      *index = it.index();
      VL_CHECK(*index < posarr->size());
    }
  }
  VL_CHECK( index == de_multi->indexBuffer()->end() )

  // Specify primitive boundaries. This must be done last!
  de_multi->setCountVector( count_vector );

  drawCalls()->push_back( de_multi.get() );
}
//-----------------------------------------------------------------------------
void Geometry::mergeDrawCallsWithTriangles(EPrimitiveType primitive_type)
{
  u32 triangle_count = 0;
  std::vector< ref<DrawCall> > mergendo_calls;
  for( u32 i=drawCalls()->size(); i--; )
  {
    const DrawCall& dc = *drawCalls()->at(i);

    // ignore primitives that cannot be triangulated
    switch(dc.primitiveType())
    {
    case PT_TRIANGLES:
    case PT_TRIANGLE_STRIP:
    case PT_TRIANGLE_FAN:
    case PT_QUADS:
    case PT_QUAD_STRIP:
    case PT_POLYGON:
      break;
    default:
      continue;
    }

    if (primitive_type == PT_UNKNOWN || dc.primitiveType() == primitive_type || dc.primitiveType() == PT_TRIANGLES)
    {
      triangle_count += dc.countTriangles();
      // insert at the head to preserve the primitive rendering order
      mergendo_calls.insert( mergendo_calls.begin(), drawCalls()->at(i) );
      drawCalls()->eraseAt(i);
    }
  }
  // preseve rendering order
  std::reverse(mergendo_calls.begin(), mergendo_calls.end());

  if (mergendo_calls.empty())
    return;

  // if there was one single PT_TRIANGLES draw calls then we are done.
  if ( mergendo_calls.size() == 1 && mergendo_calls[0]->primitiveType() == PT_TRIANGLES )
  {
    drawCalls()->push_back( mergendo_calls[0].get() );
    return;
  }

#ifndef NDEBUG
  ArrayAbstract* posarr = vertexArray() ? vertexArray() : vertexAttribArray(vl::VA_Position) ? vertexAttribArray(vl::VA_Position)->data() : NULL;
#endif

  ref<DrawElementsUInt> de = new DrawElementsUInt;
  ArrayUInt1& index_buffer = *de->indexBuffer();
  index_buffer.resize( triangle_count * 3 );
  u32 idx = 0;
  for(u32 i=0; i<mergendo_calls.size(); ++i)
  {
    for(TriangleIterator it = mergendo_calls[i]->triangleIterator(); it.hasNext(); it.next(), idx+=3)
    {
      VL_CHECK( idx+2 < index_buffer.size() );

      index_buffer[idx+0] = it.a();
      index_buffer[idx+1] = it.b();
      index_buffer[idx+2] = it.c();

      // some sanity checks since we are here...
      VL_CHECK( it.a() < (int)posarr->size() && it.b() < (int)posarr->size() && it.c() < (int)posarr->size() );
      VL_CHECK( it.a() >= 0 && it.b() >= 0 && it.c() >= 0 );
    }
  }
  VL_CHECK( idx == index_buffer.size() );
  drawCalls()->push_back(de.get());
}
//-----------------------------------------------------------------------------
void Geometry::fixTriangleWinding()
{
  ArrayAbstract* posarr = vertexArray() ? vertexArray() : vertexAttribArray(vl::VA_Position) ? vertexAttribArray(vl::VA_Position)->data() : NULL;

  ArrayAbstract* normarr = normalArray() ? normalArray() : vertexAttribArray(vl::VA_Normal) ? vertexAttribArray(vl::VA_Normal)->data() : NULL;

  // fixing the triangle winding requires normals
  if ( normarr == NULL || posarr == NULL )
    return;

  u32 triangle_count = 0;
  std::vector< ref<DrawCall> > mergendo_calls;
  for( u32 i=drawCalls()->size(); i--; )
  {
    const DrawCall& dc = *drawCalls()->at(i);

    // ignore primitives that cannot be triangulated
    switch(dc.primitiveType())
    {
    case PT_TRIANGLES:
    case PT_TRIANGLE_STRIP:
    case PT_TRIANGLE_FAN:
    case PT_QUADS:
    case PT_QUAD_STRIP:
    case PT_POLYGON:
      break;
    default:
      continue;
    }

    triangle_count += dc.countTriangles();
    // insert at the head to preserve the primitive rendering order
    mergendo_calls.insert( mergendo_calls.begin(), drawCalls()->at(i) );
    drawCalls()->eraseAt(i);
  }
  // preseve rendering order
  std::reverse(mergendo_calls.begin(), mergendo_calls.end());

  ref<DrawElementsUInt> de = new DrawElementsUInt;
  ArrayUInt1& index_buffer = *de->indexBuffer();
  index_buffer.resize( triangle_count * 3 );
  u32 idx = 0;
  for(u32 i=0; i<mergendo_calls.size(); ++i)
  {
    for(TriangleIterator it = mergendo_calls[i]->triangleIterator(); it.hasNext(); it.next(), idx+=3)
    {
      VL_CHECK( idx+2 < index_buffer.size() );

      vec3 p0 = posarr->getAsVec3(it.a());
      vec3 p1 = posarr->getAsVec3(it.b());
      vec3 p2 = posarr->getAsVec3(it.c());
      p1 = (p1 - p0).normalize();
      p2 = (p2 - p0).normalize();
      vec3 n1 = vl::cross(p1, p2);

      vec3 v0 = normarr->getAsVec3(it.a());
      vec3 v1 = normarr->getAsVec3(it.b());
      vec3 v2 = normarr->getAsVec3(it.c());
      vec3 n2 = (v0+v1+v2).normalize();

      if (dot(n1, n2) > 0)
      {
        index_buffer[idx+0] = it.a();
        index_buffer[idx+1] = it.b();
        index_buffer[idx+2] = it.c();
      }
      else
      {
        index_buffer[idx+0] = it.a();
        index_buffer[idx+1] = it.c();
        index_buffer[idx+2] = it.b();
      }

      // some sanity checks since we are here...
      VL_CHECK( it.a() < (int)posarr->size() && it.b() < (int)posarr->size() && it.c() < (int)posarr->size() );
      VL_CHECK( it.a() >= 0 && it.b() >= 0 && it.c() >= 0 );
    }
  }
  VL_CHECK( idx == index_buffer.size() );
  drawCalls()->push_back(de.get());
}
//-----------------------------------------------------------------------------
void Geometry::regenerateVertices(const std::vector<u32>& map_new_to_old)
{
  VertexMapper mapper;

  if (vertexArray())
    setVertexArray( mapper.regenerate( vertexArray(), map_new_to_old ).get() );

  if (normalArray())
    setNormalArray( mapper.regenerate( normalArray(), map_new_to_old ).get() );

  if (colorArray())
    setColorArray( mapper.regenerate( colorArray(), map_new_to_old ).get() );

  if (secondaryColorArray())
    setSecondaryColorArray( mapper.regenerate( secondaryColorArray(), map_new_to_old ).get() );

  if (fogCoordArray())
    setFogCoordArray( mapper.regenerate( fogCoordArray(), map_new_to_old ).get() );

  for(int itex=0; itex<VL_MAX_TEXTURE_UNITS; ++itex)
    if (texCoordArray(itex))
      setTexCoordArray( itex, mapper.regenerate( texCoordArray(itex), map_new_to_old ).get() );

  for(int i=0; i<vertexAttribArrays()->size(); ++i)
    vertexAttribArrays()->at(i)->setData( mapper.regenerate(vertexAttribArrays()->at(i)->data(), map_new_to_old ).get() );
}
//-----------------------------------------------------------------------------
void Geometry::convertDrawCallToDrawArrays()
{
  ArrayAbstract* posarr = vertexArray() ? vertexArray() : vertexAttribArray(vl::VA_Position) ? vertexAttribArray(vl::VA_Position)->data() : NULL;

  // generate mapping 
  std::vector<u32> map_new_to_old;
  map_new_to_old.reserve( posarr ? (posarr->size() * 3) : (1024 * 64) );

  for(int i=drawCalls()->size(); i--; )
  {
    int start = (int)map_new_to_old.size();
    for(IndexIterator it=drawCalls()->at(i)->indexIterator(); it.hasNext(); it.next())
      map_new_to_old.push_back(it.index());
    int count = (int)map_new_to_old.size() - start;

    // substitute with DrawArrays
    ref<DrawArrays> da = new vl::DrawArrays( drawCalls()->at(i)->primitiveType(), start, count, drawCalls()->at(i)->instances() );
    drawCalls()->erase(i,1);
    drawCalls()->push_back(da.get());
  }

  regenerateVertices(map_new_to_old);
}
//-----------------------------------------------------------------------------
void Geometry::triangulateDrawCalls()
{
  // converts PT_QUADS, PT_QUADS_STRIP and PT_POLYGON into PT_TRIANGLES
  for( int idraw=this->drawCalls()->size(); idraw--; )
  {
    DrawCall* dc = this->drawCalls()->at(idraw);
    switch(dc->primitiveType())
    {
    case PT_QUADS:
    case PT_QUAD_STRIP:
    case PT_POLYGON:
      break;
    default:
      continue;
    }

    u32 tri_count = dc->countTriangles();

    ref<DrawElementsUInt> triangles = new DrawElementsUInt(PT_TRIANGLES, dc->instances());
    triangles->indexBuffer()->resize( tri_count*3 );
    unsigned int* ptr = triangles->indexBuffer()->begin();
    for( TriangleIterator it = dc->triangleIterator(); it.hasNext(); ++it, ptr+=3 )
    {
      ptr[0] = it.a();
      ptr[1] = it.b();
      ptr[2] = it.c();
    }
    VL_CHECK( ptr == triangles->indexBuffer()->end() )
    // substitute the draw call
    (*drawCalls())[idraw] = triangles;
  }
}
//-----------------------------------------------------------------------------
void Geometry::shrinkDrawCalls()
{
#ifndef NDEBUG
  ArrayAbstract* posarr = vertexArray() ? vertexArray() : vertexAttribArray(vl::VA_Position) ? vertexAttribArray(vl::VA_Position)->data() : NULL;
  VL_CHECK(posarr);
#endif

  for( int idraw=this->drawCalls()->size(); idraw--; )
  {
    ref<DrawCall> dc = this->drawCalls()->at(idraw);

    unsigned int restart_idx = dc->primitiveRestartIndex();
    bool restart_on = dc->primitiveRestartEnabled();

    // find max index
    int max_idx = -1;
    int idx_count = 0;
    for( vl::IndexIterator it = dc->indexIterator(); it.hasNext(); it.next(), ++idx_count )
    {
      // skip primitive restart indices
      if (restart_on && it.index() == (int)restart_idx)
        continue;
      else
        max_idx = it.index() > max_idx ? it.index() : max_idx;
    }
    
    // can use UByte
    if ( max_idx < 0xFF || (max_idx == 0xFF && !restart_on) )
    {
      if (dc->isOfType(DrawElementsBase::Type()))
      {
        ref<DrawElementsUByte> de = new DrawElementsUByte( dc->primitiveType(), dc->instances() );
        // prim restart
        de->setPrimitiveRestartEnabled( dc->primitiveRestartEnabled() );
        // base vertex
        de->setBaseVertex( dc->as<DrawElementsBase>()->baseVertex() );
        // regenerate indices
        de->indexBuffer()->resize( idx_count );
        u32 i=0;
        for( vl::IndexIterator it = dc->indexIterator(); it.hasNext(); ++it, ++i )
        {
          // skip primitive restart indices
          if (restart_on && it.index() == (int)restart_idx)
            de->indexBuffer()->at(i) = DrawElementsUByte::primitive_restart_index;
          else
          {
            VL_CHECK( it.index() >= 0 && it.index() < (int)posarr->size() );
            de->indexBuffer()->at(i) = (DrawElementsUByte::index_type)it.index();
          }
        }
        VL_CHECK( i == de->indexBuffer()->size() );
        // substitute new draw call
        (*drawCalls())[idraw] = de;
      }
      else
      if (dc->isOfType(DrawRangeElementsBase::Type()))
      {
        ref<DrawRangeElementsUByte> de = new DrawRangeElementsUByte( dc->primitiveType(), dc->instances() );
        // prim restart
        de->setPrimitiveRestartEnabled( dc->primitiveRestartEnabled() );
        // base vertex
        de->setBaseVertex( dc->as<DrawRangeElementsBase>()->baseVertex() );
        // range
        de->setRangeStart( dc->as<DrawRangeElementsBase>()->rangeStart() );
        de->setRangeEnd( dc->as<DrawRangeElementsBase>()->rangeEnd() );
        // regenerate indices
        de->indexBuffer()->resize( idx_count );
        u32 i=0;
        for( vl::IndexIterator it = dc->indexIterator(); it.hasNext(); ++it, ++i )
        {
          // skip primitive restart indices
          if (restart_on && it.index() == (int)restart_idx)
            de->indexBuffer()->at(i) = DrawRangeElementsUByte::primitive_restart_index;
          else
            de->indexBuffer()->at(i) = (DrawRangeElementsUByte::index_type)it.index();
        }
        VL_CHECK( i == de->indexBuffer()->size() );
        // substitute new draw call
        (*drawCalls())[idraw] = de;
      }
      else
      if (dc->isOfType(MultiDrawElementsBase::Type()))
      {
        ref<MultiDrawElementsUByte> de = new MultiDrawElementsUByte( dc->primitiveType() );
        // regenerate indices
        de->indexBuffer()->resize( idx_count );
        u32 i=0;
        for( vl::IndexIterator it = dc->indexIterator(); it.hasNext(); ++it, ++i )
        {
          // skip primitive restart indices
          if (restart_on && it.index() == (int)restart_idx)
            de->indexBuffer()->at(i) = DrawElementsUByte::primitive_restart_index;
          else
            de->indexBuffer()->at(i) = (MultiDrawElementsUByte::index_type)it.index();
        }
        VL_CHECK( i == de->indexBuffer()->size() );
        // prim restart
        de->setPrimitiveRestartEnabled( dc->primitiveRestartEnabled() );
        // base vertex
        de->setBaseVertices( dc->as<MultiDrawElementsBase>()->baseVertices() );
        // count vector
        de->setCountVector( dc->as<MultiDrawElementsBase>()->countVector() );
        // substitute new draw call
        (*drawCalls())[idraw] = de;
      }
    } // can use UByte
    else
    // can use UShort
    if ( max_idx < 0xFFFF || (max_idx == 0xFFFF && !restart_on) )
    {
      if (dc->isOfType(DrawElementsBase::Type()))
      {
        ref<DrawElementsUShort> de = new DrawElementsUShort( dc->primitiveType(), dc->instances() );
        // prim restart
        de->setPrimitiveRestartEnabled( dc->primitiveRestartEnabled() );
        // base vertex
        de->setBaseVertex( dc->as<DrawElementsBase>()->baseVertex() );
        // regenerate indices
        de->indexBuffer()->resize( idx_count );
        u32 i=0;
        for( vl::IndexIterator it = dc->indexIterator(); it.hasNext(); ++it, ++i )
        {
          // skip primitive restart indices
          if (restart_on && it.index() == (int)restart_idx)
            de->indexBuffer()->at(i) = DrawElementsUShort::primitive_restart_index;
          else
          {
            VL_CHECK( it.index() >= 0 && it.index() < (int)posarr->size() );
            de->indexBuffer()->at(i) = (DrawElementsUShort::index_type)it.index();
          }
        }
        VL_CHECK( i == de->indexBuffer()->size() );
        // substitute new draw call
        (*drawCalls())[idraw] = de;
      }
      else
      if (dc->isOfType(DrawRangeElementsBase::Type()))
      {
        ref<DrawRangeElementsUShort> de = new DrawRangeElementsUShort( dc->primitiveType(), dc->instances() );
        // prim restart
        de->setPrimitiveRestartEnabled( dc->primitiveRestartEnabled() );
        // base vertex
        de->setBaseVertex( dc->as<DrawRangeElementsBase>()->baseVertex() );
        // range
        de->setRangeStart( dc->as<DrawRangeElementsBase>()->rangeStart() );
        de->setRangeEnd( dc->as<DrawRangeElementsBase>()->rangeEnd() );
        // regenerate indices
        de->indexBuffer()->resize( idx_count );
        u32 i=0;
        for( vl::IndexIterator it = dc->indexIterator(); it.hasNext(); ++it, ++i )
        {
          // skip primitive restart indices
          if (restart_on && it.index() == (int)restart_idx)
            de->indexBuffer()->at(i) = DrawRangeElementsUShort::primitive_restart_index;
          else
            de->indexBuffer()->at(i) = (DrawRangeElementsUShort::index_type)it.index();
        }
        VL_CHECK( i == de->indexBuffer()->size() );
        // substitute new draw call
        (*drawCalls())[idraw] = de;
      }
      else
      if (dc->isOfType(MultiDrawElementsBase::Type()))
      {
        ref<MultiDrawElementsUShort> de = new MultiDrawElementsUShort( dc->primitiveType() );
        // regenerate indices
        de->indexBuffer()->resize( idx_count );
        u32 i=0;
        for( vl::IndexIterator it = dc->indexIterator(); it.hasNext(); ++it, ++i )
        {
          // skip primitive restart indices
          if (restart_on && it.index() == (int)restart_idx)
            de->indexBuffer()->at(i) = DrawElementsUShort::primitive_restart_index;
          else
            de->indexBuffer()->at(i) = (MultiDrawElementsUShort::index_type)it.index();
        }
        VL_CHECK( i == de->indexBuffer()->size() );
        // prim restart
        de->setPrimitiveRestartEnabled( dc->primitiveRestartEnabled() );
        // base vertex
        de->setBaseVertices( dc->as<MultiDrawElementsBase>()->baseVertices() );
        // count vector
        de->setCountVector( dc->as<MultiDrawElementsBase>()->countVector() );
        // substitute new draw call
        (*drawCalls())[idraw] = de;
      }
    } // can use UShort

  } // for()
}
//-----------------------------------------------------------------------------
void Geometry::makeGLESFriendly()
{
  // converts legacy vertex arrays into generic vertex attributes
#if defined(VL_OPENGL_ES2)
  convertToVertexAttribs();
#endif

  // converts quads and polygons into triangles
  triangulateDrawCalls();
  
  // use short or byte instead of int
  shrinkDrawCalls();

  // check primitive type is supported by OpenGL ES
  for(int i=0; i<drawCalls()->size(); ++i)
  {
    DrawCall* dc = drawCalls()->at(i);
    // check supported primitive types
    switch(dc->primitiveType())
    {
    case GL_POINTS:
    case GL_LINE_STRIP:
    case GL_LINE_LOOP:
    case GL_LINES:
    case GL_TRIANGLE_STRIP:
    case GL_TRIANGLE_FAN:
    case GL_TRIANGLES:
      break;

    case PT_QUADS:
    case PT_QUAD_STRIP:
    case PT_POLYGON:
    case PT_LINES_ADJACENCY:
    case PT_LINE_STRIP_ADJACENCY:
    case PT_TRIANGLES_ADJACENCY:
    case PT_TRIANGLE_STRIP_ADJACENCY:
    case PT_PATCHES:
      dc->setEnabled(false);
      Log::error("Geometry::makeGLESFriendly(): primitive type illegal under GLES, draw call disabled.\n");
      break;

    default:
      VL_TRAP();
      break;
    }
  }
}
//-----------------------------------------------------------------------------
bool Geometry::sortVertices()
{
  ArrayAbstract* posarr = vertexArray() ? vertexArray() : vertexAttribArray(vl::VA_Position) ? vertexAttribArray(vl::VA_Position)->data() : NULL;

  if (!posarr)
  {
    Log::warning("Geometry::sortVertices() failed. No vertices found.\n");
    return false;
  }

  // supports only DrawElements* and generates DrawElementsUInt

  std::vector< ref<DrawElementsUInt> > de_u32_set;

  // collect DrawElements
  for(int i=0; i<drawCalls()->size(); ++i)
  {
    DrawCall* dc = drawCalls()->at(i);
    if (dc->primitiveRestartEnabled())
    {
      Log::error("Geometry::sortVertices() does not support DrawCalls with primitive restart enabled.\n");
      return false;
    }

    DrawElementsUInt*   de_u32 = dc->as<DrawElementsUInt>();
    DrawElementsUShort* de_u16 = dc->as<DrawElementsUShort>();
    DrawElementsUByte*  de_u8  = dc->as<DrawElementsUByte>();
    if (de_u32)
    {
      ref<DrawElementsUInt> de = new DrawElementsUInt(de_u32->primitiveType(), de_u32->instances());
      de_u32_set.push_back(de);
      de->indexBuffer()->resize( de_u32->indexBuffer()->size() );
      for(unsigned int j=0; j<de_u32->indexBuffer()->size(); ++j)
        de->indexBuffer()->at(j) = de_u32->indexBuffer()->at(j) + de_u32->baseVertex(); // bake base vertex
    }
    else
    if(de_u16)
    {
      ref<DrawElementsUInt> de = new DrawElementsUInt(de_u16->primitiveType(), de_u16->instances());
      de_u32_set.push_back(de);
      de->indexBuffer()->resize( de_u16->indexBuffer()->size() );
      for(unsigned int j=0; j<de_u16->indexBuffer()->size(); ++j)
        de->indexBuffer()->at(j) = de_u16->indexBuffer()->at(j) + de_u16->baseVertex(); // bake base vertex
    }
    else
    if(de_u8)
    {
      ref<DrawElementsUInt> de = new DrawElementsUInt(de_u8->primitiveType(), de_u8->instances());
      de_u32_set.push_back(de);
      de->indexBuffer()->resize( de_u8->indexBuffer()->size() );
      for(unsigned int j=0; j<de_u8->indexBuffer()->size(); ++j)
        de->indexBuffer()->at(j) = de_u8->indexBuffer()->at(j) + de_u8->baseVertex(); // bake base vertex
    }
    else
    {
      Log::error("Geometry::sortVertices() supports only DrawElements* draw calls.\n");
      return false;
    }
  }

  // erase all draw calls
  drawCalls()->clear();

  // reset tables
  std::vector<u32> map_new_to_old;
  map_new_to_old.resize( posarr->size() );
  memset(&map_new_to_old[0], 0xFF, map_new_to_old.size()*sizeof(map_new_to_old[0])); // fill with 0xFF for debugging

  std::vector<u32> map_old_to_new;
  map_old_to_new.resize( posarr->size() );
  memset(&map_old_to_new[0], 0xFF, map_old_to_new.size()*sizeof(map_old_to_new[0])); // fill with 0xFF for debugging

  std::vector<u32> used;
  used.resize( posarr->size() );
  memset(&used[0], 0, used.size()*sizeof(used[0]));

  // assign new vertex indices in order of appearence
  u32 new_idx = 0;
  for(u32 i=0; i<de_u32_set.size(); ++i)
  {
    ArrayUInt1* index_buffer = de_u32_set[i]->indexBuffer();
    for(u32 idx=0; idx<index_buffer->size(); ++idx)
    {
      if (!used[index_buffer->at(idx)])
      {
        const DrawElementsUInt::index_type& old_idx = index_buffer->at(idx);
        map_new_to_old[new_idx] = old_idx;
        map_old_to_new[old_idx] = new_idx;
        used[old_idx] = 1;
        ++new_idx;
      }
    }
  }

  // regenerate vertices
  regenerateVertices(map_new_to_old);

  // regenerate draw calls
  for(u32 i=0; i<de_u32_set.size(); ++i)
  {
    drawCalls()->push_back(de_u32_set[i].get());
    ArrayUInt1* index_buffer = de_u32_set[i]->indexBuffer();
    for(u32 j=0; j<index_buffer->size(); ++j)
    {
      index_buffer->at(j) = map_old_to_new[index_buffer->at(j)];
    }
  }

  return true;
}
//-----------------------------------------------------------------------------
void Geometry::colorizePrimitives()
{
  ArrayAbstract* posarr = vertexArray() ? vertexArray() : vertexAttribArray(vl::VA_Position) ? vertexAttribArray(vl::VA_Position)->data() : NULL;

  if (!posarr)
    return;

  ref<ArrayFloat4> col = new vl::ArrayFloat4;
  col->resize( posarr->size() );

  if (vertexArray())
    setColorArray( col.get() );
  else
    setVertexAttribArray( vl::VA_Color, col.get() );

  for(int i=0; i<drawCalls()->size(); ++i)
  {
    fvec4 c;
    c.r() = rand()%100 / 99.0f;
    c.g() = rand()%100 / 99.0f;
    c.b() = rand()%100 / 99.0f;
    c.a() = 1.0f;

    for(IndexIterator it=drawCalls()->at(i)->indexIterator(); it.hasNext(); it.next())
      col->at( it.index() ) = c;
  }
}
//-----------------------------------------------------------------------------
void Geometry::computeTangentSpace(
  u32 vert_count, 
  const fvec3 *vertex, 
  const fvec3* normal,
  const fvec2 *texcoord, 
  const DrawCall* prim,
  fvec3 *tangent, 
  fvec3 *bitangent )
{
  std::vector<fvec3> tan1;
  std::vector<fvec3> tan2;
  tan1.resize(vert_count);
  tan2.resize(vert_count);
  
  for ( TriangleIterator trit = prim->triangleIterator(); trit.hasNext(); trit.next() )
  {
    int tri[] = { trit.a(), trit.b(), trit.c() };

    VL_CHECK(tri[0] < (int)vert_count );
    VL_CHECK(tri[1] < (int)vert_count );
    VL_CHECK(tri[2] < (int)vert_count );
    
    const fvec3& v1 = vertex[tri[0]];
    const fvec3& v2 = vertex[tri[1]];
    const fvec3& v3 = vertex[tri[2]];
    
    const fvec2& w1 = texcoord[tri[0]];
    const fvec2& w2 = texcoord[tri[1]];
    const fvec2& w3 = texcoord[tri[2]];
    
    float x1 = v2.x() - v1.x();
    float x2 = v3.x() - v1.x();
    float y1 = v2.y() - v1.y();
    float y2 = v3.y() - v1.y();
    float z1 = v2.z() - v1.z();
    float z2 = v3.z() - v1.z();
    
    float s1 = w2.x() - w1.x();
    float s2 = w3.x() - w1.x();
    float t1 = w2.y() - w1.y();
    float t2 = w3.y() - w1.y();
    
    float r = 1.0F / (s1 * t2 - s2 * t1);
    fvec3 sdir((t2 * x1 - t1 * x2) * r, (t2 * y1 - t1 * y2) * r, (t2 * z1 - t1 * z2) * r);
    fvec3 tdir((s1 * x2 - s2 * x1) * r, (s1 * y2 - s2 * y1) * r, (s1 * z2 - s2 * z1) * r);

    tan1[tri[0]] += sdir;
    tan1[tri[1]] += sdir;
    tan1[tri[2]] += sdir;

    tan2[tri[0]] += tdir;
    tan2[tri[1]] += tdir;
    tan2[tri[2]] += tdir;
  }

  for ( u32 a = 0; a < vert_count; a++)
  {
    const fvec3& n = normal[a];
    const fvec3& t = tan1[a];

    // Gram-Schmidt orthogonalize
    tangent[a] = (t - n * dot(n, t)).normalize();

    if ( bitangent )
    {
      // Calculate handedness
      float w = (dot(cross(n, t), tan2[a]) < 0.0F) ? -1.0F : 1.0F;
      bitangent[a] = cross( n, tangent[a] ) * w;
    }
  }
}
//-----------------------------------------------------------------------------
