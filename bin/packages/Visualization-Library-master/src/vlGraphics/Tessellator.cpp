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

#include <vlGraphics/Tessellator.hpp>

using namespace vl;

//-----------------------------------------------------------------------------
Tessellator::Tessellator()
{
  VL_DEBUG_SET_OBJECT_NAME()
  mTessNormal = fvec3(0,0,0);
  mBoundaryOnly = false;
  mTolerance = 0.0;
  mWindingRule = TW_TESS_WINDING_ODD;
  mTessellateIntoSinglePolygon = true;
}
//-----------------------------------------------------------------------------
Tessellator::~Tessellator()
{
  freeCombinedVertices();
}
//-----------------------------------------------------------------------------
bool Tessellator::tessellate(bool append_tessellated_tris)
{
  if (!append_tessellated_tris)
    mTessellatedTris.clear();
  mFans.clear();
  mTriStrips.clear();
  mLineLoops.clear();
  mPrimitiveType = 0;
  freeCombinedVertices();
  if (mContours.empty() || mContourVerts.empty())
  {
    vl::Log::error("Tessellator::tessellate(): no contours specified.\n");
    return false;
  }

  GLUtesselator* tobj = gluNewTess();
  // callbacks
  gluTessCallback(tobj, GLU_TESS_BEGIN_DATA,   (callback_type)tessBeginData);
  gluTessCallback(tobj, GLU_TESS_VERTEX_DATA,  (callback_type)tessVertexData);
  gluTessCallback(tobj, GLU_TESS_COMBINE_DATA, (callback_type)tessCombineData);
  gluTessCallback(tobj, GLU_TESS_END,     (callback_type)tessEnd);
  gluTessCallback(tobj, GLU_TESS_ERROR,        (callback_type)tessError);
  // normal
   gluTessNormal( tobj, tessNormal().x(), tessNormal().y(), tessNormal().z() );
  // properties
   gluTessProperty(tobj, GLU_TESS_BOUNDARY_ONLY, boundaryOnly() ? GL_TRUE : GL_FALSE);
   gluTessProperty(tobj, GLU_TESS_TOLERANCE, tolerance());
   gluTessProperty(tobj, GLU_TESS_WINDING_RULE, windingRule());
  // tessellation
  if (tessellateIntoSinglePolygon())
  {
    gluTessBeginPolygon(tobj, this);
    for(unsigned cont=0, idx=0; cont<mContours.size(); ++cont)
    {
      gluTessBeginContour(tobj);
      for(int i=0; i<mContours[cont]; ++i, ++idx)
        gluTessVertex(tobj, mContourVerts[idx].ptr(), mContourVerts[idx].ptr());
      gluTessEndContour(tobj);
    }
    gluTessEndPolygon(tobj);
  }
  else
  {
    for(unsigned cont=0, idx=0; cont<mContours.size(); ++cont)
    {
      gluTessBeginPolygon(tobj, this);
      gluTessBeginContour(tobj);
      for(int i=0; i<mContours[cont]; ++i, ++idx)
        gluTessVertex(tobj, mContourVerts[idx].ptr(), mContourVerts[idx].ptr());
      gluTessEndContour(tobj);
      gluTessEndPolygon(tobj);
    }
  }
  gluDeleteTess(tobj);

  // triangulate fans
  for(unsigned fan=0; fan<mFans.size();    ++fan)
  for(unsigned iv =1; iv<mFans[fan].size()-1; ++iv)
  {
    mTessellatedTris.push_back(mFans[fan][0]);
    mTessellatedTris.push_back(mFans[fan][iv]);
    mTessellatedTris.push_back(mFans[fan][iv+1]);
  }

  // triangulate strips
  for(unsigned strip=0; strip<mTriStrips.size(); ++strip)
  for(unsigned iv=0; iv<mTriStrips[strip].size()-2; ++iv)
  {
    if (iv % 2)
    {
      mTessellatedTris.push_back(mTriStrips[strip][iv+0]);
      mTessellatedTris.push_back(mTriStrips[strip][iv+2]);
      mTessellatedTris.push_back(mTriStrips[strip][iv+1]);
    }
    else
    {
      mTessellatedTris.push_back(mTriStrips[strip][iv+0]);
      mTessellatedTris.push_back(mTriStrips[strip][iv+1]);
      mTessellatedTris.push_back(mTriStrips[strip][iv+2]);
    }
  }

  mContours.clear();
  mContourVerts.clear();

  #if 0
    if (tessellatedTris().empty())
    {
      vl::Log::warning("Tessellator::tessellate(): no triangles generated.\n");
      return false;
    }
  #endif

  return true;
}
//-----------------------------------------------------------------------------
void Tessellator::freeCombinedVertices()
{
  for(unsigned i=0; i<mCombinedVertices.size(); ++i)
    delete mCombinedVertices[i];
  mCombinedVertices.clear();
}
//-----------------------------------------------------------------------------
ref<Geometry> Tessellator::tessellateGeometry(bool append_tessellated_tris)
{
  tessellate(append_tessellated_tris);

  if (mTessellatedTris.empty())
    return NULL;

  ref<Geometry> geom = new Geometry;
  ref<ArrayFloat3> vert_array = new ArrayFloat3;
  
  vert_array->initFrom(mTessellatedTris);

  geom->setVertexArray(vert_array.get());
  geom->drawCalls()->push_back( new vl::DrawArrays(PT_TRIANGLES, 0, vert_array->size()) );
  geom->computeNormals();
  return geom;
}
//-----------------------------------------------------------------------------
// Tessellation callbacks
//-----------------------------------------------------------------------------
void CALLBACK Tessellator::tessBeginData( GLenum type, Tessellator* tessellator )
{
  tessellator->mPrimitiveType = type;
  if(type == GL_TRIANGLES)
  {
    // do nothing
  }
  else
  if(type == GL_TRIANGLE_FAN)
    tessellator->mFans.resize( tessellator->mFans.size() + 1 );
  else
  if(type == GL_TRIANGLE_STRIP)
    tessellator->mTriStrips.resize( tessellator->mTriStrips.size() + 1 );
  else
  if(type == GL_LINE_LOOP)
    tessellator->mLineLoops.resize( tessellator->mLineLoops.size() + 1 );
  else
  {
    Log::error("Tessellator::beginData() unknown primitive.\n");
  }
}
//-----------------------------------------------------------------------------
void CALLBACK Tessellator::tessVertexData( dvec3* vec, Tessellator* tessellator )
{
  if(tessellator->mPrimitiveType == GL_TRIANGLES)
    tessellator->mTessellatedTris.push_back( (fvec3)(*vec) );
  else
  if(tessellator->mPrimitiveType == GL_TRIANGLE_FAN)
    tessellator->mFans.back().push_back( (fvec3)(*vec) );
  else
  if(tessellator->mPrimitiveType == GL_TRIANGLE_STRIP)
    tessellator->mTriStrips.back().push_back( (fvec3)(*vec) );
  else
  if(tessellator->mPrimitiveType == GL_LINE_LOOP)
    tessellator->mLineLoops.back().push_back( (fvec3)(*vec) );
  else
  {
    Log::error("Tessellator::vertexData() unknown primitive.\n");
  }
}
//-----------------------------------------------------------------------------
void CALLBACK Tessellator::tessCombineData( GLdouble coords[3], dvec3*[4], GLfloat[4], dvec3 **dataOut, Tessellator* tessellator )
{
  dvec3 *vec = new dvec3;
  vec->x() = coords[0];
  vec->y() = coords[1];
  vec->z() = coords[2];
  *dataOut = vec;
  tessellator->mCombinedVertices.push_back( vec );
}
//-----------------------------------------------------------------------------
void CALLBACK Tessellator::tessEnd(void)
{
}
//-----------------------------------------------------------------------------
void CALLBACK Tessellator::tessError( GLenum errno )
{
  const GLubyte* estring = gluErrorString(errno);
  Log::error( Say("Tessellator error: %s.\n") << estring );
}
//-----------------------------------------------------------------------------
