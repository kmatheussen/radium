/**************************************************************************************/
/*                                                                                    */
/*  Visualization Library                                                             */
/*  http://www.visualizationlibrary.org                                               */
/*                                                                                    */
/*  Copyright (c) 2005-2011, Michele Bosi                                             */
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

#include <vlGraphics/GeometryPrimitives.hpp>
#include <vlGraphics/Geometry.hpp>
#include <vlGraphics/DoubleVertexRemover.hpp>
#include <vlGraphics/BezierSurface.hpp>

using namespace vl;

//-----------------------------------------------------------------------------
//! \p detail can be between 0 ( = icosahedron) and 8 (extremely detailed sphere)
//! a value of 2 yelds already very good results.
ref<Geometry> vl::makeIcosphere(const vec3& pos, real diameter, int detail, bool remove_doubles)
{
  ref<Geometry> geom = new Geometry;
  geom->setObjectName("Icosphere");

  ref<ArrayFloat3> coords = new ArrayFloat3;
  ref<ArrayFloat3> norms = new ArrayFloat3;

  ref<DrawElementsUInt> polys = new DrawElementsUInt(PT_TRIANGLES);

  const real X = (real)0.525731112119133606;
  const real Z = (real)0.850650808352039932;
  std::vector< vec3 > verts;
  verts.push_back( vec3(-X, 0, Z) );
  verts.push_back( vec3(X, 0, Z) );
  verts.push_back( vec3(-X, 0, -Z) );
  verts.push_back( vec3(X, 0, -Z) );
  verts.push_back( vec3(0, Z, X) );
  verts.push_back( vec3(0, Z, -X) );
  verts.push_back( vec3(0, -Z, X) );
  verts.push_back( vec3(0, -Z, -X) );
  verts.push_back( vec3(Z, X, 0) );
  verts.push_back( vec3(-Z, X, 0) );
  verts.push_back( vec3(Z, -X, 0) );
  verts.push_back( vec3(-Z, -X, 0) );

  int idxs[] = { 
    1,4,0, 4,9,0, 4,5,9, 8,5,4, 1,8,4, 
    1,10,8, 10,3,8, 8,3,5, 3,2,5, 3,7,2, 
    3,10,7, 10,6,7, 6,11,7, 6,0,11, 6,1,0,
    10,1,6, 11,0,9, 2,11,9, 5,2,9, 11,2,7
  };

  std::vector<int> indices;
  for(int i=0; i<4*5*3; ++i)
    indices.push_back(idxs[i]);

  // triangulate the icosahedron
  if (detail>8)
    detail = 8;
  if (detail<0)
    detail = 0;
  for(int i=0; i<detail; ++i)
  {
    std::vector<int> indices2;
    std::vector< vec3 > verts2;
    for( int j=0, idx=0; j<(int)indices.size(); j+=3)
    {
      indices2.push_back(idx++); indices2.push_back(idx++); indices2.push_back(idx++);
      indices2.push_back(idx++); indices2.push_back(idx++); indices2.push_back(idx++);
      indices2.push_back(idx++); indices2.push_back(idx++); indices2.push_back(idx++);
      indices2.push_back(idx++); indices2.push_back(idx++); indices2.push_back(idx++);

      vec3 v1 = verts[ indices[j+0] ]; v1.normalize();
      vec3 v2 = verts[ indices[j+1] ]; v2.normalize();
      vec3 v3 = verts[ indices[j+2] ]; v3.normalize();
      vec3 a = (v1 + v2) * 0.5f; a.normalize();
      vec3 b = (v2 + v3) * 0.5f; b.normalize();
      vec3 c = (v3 + v1) * 0.5f; c.normalize();
      verts2.push_back(v1); verts2.push_back( a); verts2.push_back(c);
      verts2.push_back( a); verts2.push_back(v2); verts2.push_back(b);
      verts2.push_back( a); verts2.push_back( b); verts2.push_back(c);
      verts2.push_back( c); verts2.push_back( b); verts2.push_back(v3);
    }
    verts = verts2;
    indices = indices2;
  }

  // generate sphere vertices and connection information

  real radius = diameter / 2;

  coords->resize( (int)verts.size() );
  norms->resize( (int)verts.size() );
  for( int i=0; i<(int)verts.size(); ++i )
  {
    coords->at(i) = (fvec3)(verts[i]*radius + pos);
    vec3 n = verts[i];
    n.normalize();
    norms->at(i) = (fvec3)n;
  }

  polys->indexBuffer()->resize( (int)indices.size() );
  for(int i=0; i<(int)indices.size(); ++i)
  {
    VL_CHECK( indices[i] < (int)coords->size() )
    polys->indexBuffer()->at(i) = indices[i];
  }

  geom->setVertexArray(coords.get());
  geom->setNormalArray(norms.get());
  geom->drawCalls()->push_back(polys.get());

  if (remove_doubles)
  {
    DoubleVertexRemover dvr;
    dvr.removeDoubles(geom.get());
  }

#if defined(VL_OPENGL_ES1) || defined(VL_OPENGL_ES2)
  geom->makeGLESFriendly();
#endif

  return geom;
}
//-----------------------------------------------------------------------------
ref<Geometry> vl::makeTeapot( const vec3& origin, real diameter, int detail)
{
  // 32 patches 4x4
  static const int patch_idx[] = { 
    1,   2,   3,   4,   5,   6,   7,   8,   9,   10,  11,  12,  13,  14,  15,  16, 
    4,   17,  18,  19,  8,   20,  21,  22,  12,  23,  24,  25,  16,  26,  27,  28, 
    19,  29,  30,  31,  22,  32,  33,  34,  25,  35,  36,  37,  28,  38,  39,  40, 
    31,  41,  42,  1,   34,  43,  44,  5,   37,  45,  46,  9,   40,  47,  48,  13, 
    13,  14,  15,  16,  49,  50,  51,  52,  53,  54,  55,  56,  57,  58,  59,  60, 
    16,  26,  27,  28,  52,  61,  62,  63,  56,  64,  65,  66,  60,  67,  68,  69, 
    28,  38,  39,  40,  63,  70,  71,  72,  66,  73,  74,  75,  69,  76,  77,  78, 
    40,  47,  48,  13,  72,  79,  80,  49,  75,  81,  82,  53,  78,  83,  84,  57, 
    57,  58,  59,  60,  85,  86,  87,  88,  89,  90,  91,  92,  93,  94,  95,  96, 
    60,  67,  68,  69,  88,  97,  98,  99,  92,  100, 101, 102, 96,  103, 104, 105, 
    69,  76,  77,  78,  99,  106, 107, 108, 102, 109, 110, 111, 105, 112, 113, 114, 
    78,  83,  84,  57,  108, 115, 116, 85,  111, 117, 118, 89,  114, 119, 120, 93, 
    121, 122, 123, 124, 125, 126, 127, 128, 129, 130, 131, 132, 133, 134, 135, 136, 
    124, 137, 138, 121, 128, 139, 140, 125, 132, 141, 142, 129, 136, 143, 144, 133, 
    133, 134, 135, 136, 145, 146, 147, 148, 149, 150, 151, 152, 69,  153, 154, 155, 
    136, 143, 144, 133, 148, 156, 157, 145, 152, 158, 159, 149, 155, 160, 161, 69, 
    162, 163, 164, 165, 166, 167, 168, 169, 170, 171, 172, 173, 174, 175, 176, 177, 
    165, 178, 179, 162, 169, 180, 181, 166, 173, 182, 183, 170, 177, 184, 185, 174, 
    174, 175, 176, 177, 186, 187, 188, 189, 190, 191, 192, 193, 194, 195, 196, 197, 
    177, 184, 185, 174, 189, 198, 199, 186, 193, 200, 201, 190, 197, 202, 203, 194, 
    204, 204, 204, 204, 207, 208, 209, 210, 211, 211, 211, 211, 212, 213, 214, 215, 
    204, 204, 204, 204, 210, 217, 218, 219, 211, 211, 211, 211, 215, 220, 221, 222, 
    204, 204, 204, 204, 219, 224, 225, 226, 211, 211, 211, 211, 222, 227, 228, 229, 
    204, 204, 204, 204, 226, 230, 231, 207, 211, 211, 211, 211, 229, 232, 233, 212, 
    212, 213, 214, 215, 234, 235, 236, 237, 238, 239, 240, 241, 242, 243, 244, 245, 
    215, 220, 221, 222, 237, 246, 247, 248, 241, 249, 250, 251, 245, 252, 253, 254, 
    222, 227, 228, 229, 248, 255, 256, 257, 251, 258, 259, 260, 254, 261, 262, 263, 
    229, 232, 233, 212, 257, 264, 265, 234, 260, 266, 267, 238, 263, 268, 269, 242, 
    270, 270, 270, 270, 279, 280, 281, 282, 275, 276, 277, 278, 271, 272, 273, 274, 
    270, 270, 270, 270, 282, 289, 290, 291, 278, 286, 287, 288, 274, 283, 284, 285, 
    270, 270, 270, 270, 291, 298, 299, 300, 288, 295, 296, 297, 285, 292, 293, 294, 
    270, 270, 270, 270, 300, 305, 306, 279, 297, 303, 304, 275, 294, 301, 302, 271 
  };

  // 306 vertex coordinates
  static const float coords[] = {
    1.4f, 0.0f, 2.4f, 1.4f, -0.784f, 2.4f, 0.784f, -1.4f, 2.4f, 0.0f, -1.4f, 2.4f, 1.3375f, 0.0f, 2.53125f, 
    1.3375f, -0.749f, 2.53125f, 0.749f, -1.3375f, 2.53125f, 0.0f, -1.3375f, 2.53125f, 1.4375f, 0.0f, 2.53125f, 
    1.4375f, -0.805f, 2.53125f, 0.805f, -1.4375f, 2.53125f, 0.0f, -1.4375f, 2.53125f, 1.5f, 0.0f, 2.4f, 1.5f, 
    -0.84f, 2.4f, 0.84f, -1.5f, 2.4f, 0.0f, -1.5f, 2.4f, -0.784f, -1.4f, 2.4f, -1.4f, -0.784f, 2.4f, -1.4f, 
    0.0f, 2.4f, -0.749f, -1.3375f, 2.53125f, -1.3375f, -0.749f, 2.53125f, -1.3375f, 0.0f, 2.53125f, -0.805f, 
    -1.4375f, 2.53125f, -1.4375f, -0.805f, 2.53125f, -1.4375f, 0.0f, 2.53125f, -0.84f, -1.5f, 2.4f, -1.5f, 
    -0.84f, 2.4f, -1.5f, 0.0f, 2.4f, -1.4f, 0.784f, 2.4f, -0.784f, 1.4f, 2.4f, 0.0f, 1.4f, 2.4f, -1.3375f, 
    0.749f, 2.53125f, -0.749f, 1.3375f, 2.53125f, 0.0f, 1.3375f, 2.53125f, -1.4375f, 0.805f, 2.53125f, -0.805f, 
    1.4375f, 2.53125f, 0.0f, 1.4375f, 2.53125f, -1.5f, 0.84f, 2.4f, -0.84f, 1.5f, 2.4f, 0.0f, 1.5f, 2.4f, 
    0.784f, 1.4f, 2.4f, 1.4f, 0.784f, 2.4f, 0.749f, 1.3375f, 2.53125f, 1.3375f, 0.749f, 2.53125f, 0.805f, 
    1.4375f, 2.53125f, 1.4375f, 0.805f, 2.53125f, 0.84f, 1.5f, 2.4f, 1.5f, 0.84f, 2.4f, 1.75f, 0.0f, 1.875f, 
    1.75f, -0.98f, 1.875f, 0.98f, -1.75f, 1.875f, 0.0f, -1.75f, 1.875f, 2.0f, 0.0f, 1.35f, 2.0f, -1.12f, 1.35f, 
    1.12f, -2.0f, 1.35f, 0.0f, -2.0f, 1.35f, 2.0f, 0.0f, 0.9f, 2.0f, -1.12f, 0.9f, 1.12f, -2.0f, 0.9f, 0.0f, 
    -2.0f, 0.9f, -0.98f, -1.75f, 1.875f, -1.75f, -0.98f, 1.875f, -1.75f, 0.0f, 1.875f, -1.12f, -2.0f, 1.35f, 
    -2.0f, -1.12f, 1.35f, -2.0f, 0.0f, 1.35f, -1.12f, -2.0f, 0.9f, -2.0f, -1.12f, 0.9f, -2.0f, 0.0f, 0.9f, 
    -1.75f, 0.98f, 1.875f, -0.98f, 1.75f, 1.875f, 0.0f, 1.75f, 1.875f, -2.0f, 1.12f, 1.35f, -1.12f, 2.0f, 
    1.35f, 0.0f, 2.0f, 1.35f, -2.0f, 1.12f, 0.9f, -1.12f, 2.0f, 0.9f, 0.0f, 2.0f, 0.9f, 0.98f, 1.75f, 
    1.875f, 1.75f, 0.98f, 1.875f, 1.12f, 2.0f, 1.35f, 2.0f, 1.12f, 1.35f, 1.12f, 2.0f, 0.9f, 2.0f, 1.12f, 0.9f, 
    2.0f, 0.0f, 0.45f, 2.0f, -1.12f, 0.45f, 1.12f, -2.0f, 0.45f, 0.0f, -2.0f, 0.45f, 1.5f, 0.0f, 0.225f, 1.5f, 
    -0.84f, 0.225f, 0.84f, -1.5f, 0.225f, 0.0f, -1.5f, 0.225f, 1.5f, 0.0f, 0.15f, 1.5f, -0.84f, 0.15f, 0.84f, 
    -1.5f, 0.15f, 0.0f, -1.5f, 0.15f, -1.12f, -2.0f, 0.45f, -2.0f, -1.12f, 0.45f, -2.0f, 0.0f, 0.45f, -0.84f, 
    -1.5f, 0.225f, -1.5f, -0.84f, 0.225f, -1.5f, 0.0f, 0.225f, -0.84f, -1.5f, 0.15f, -1.5f, -0.84f, 0.15f, 
    -1.5f, 0.0f, 0.15f, -2.0f, 1.12f, 0.45f, -1.12f, 2.0f, 0.45f, 0.0f, 2.0f, 0.45f, -1.5f, 0.84f, 0.225f, 
    -0.84f, 1.5f, 0.225f, 0.0f, 1.5f, 0.225f, -1.5f, 0.84f, 0.15f, -0.84f, 1.5f, 0.15f, 0.0f, 1.5f, 0.15f, 
    1.12f, 2.0f, 0.45f, 2.0f, 1.12f, 0.45f, 0.84f, 1.5f, 0.225f, 1.5f, 0.84f, 0.225f, 0.84f, 1.5f, 0.15f, 1.5f, 
    0.84f, 0.15f, -1.6f, 0.0f, 2.025f, -1.6f, -0.3f, 2.025f, -1.5f, -0.3f, 2.25f, -1.5f, 0.0f, 2.25f, -2.3f, 
    0.0f, 2.025f, -2.3f, -0.3f, 2.025f, -2.5f, -0.3f, 2.25f, -2.5f, 0.0f, 2.25f, -2.7f, 0.0f, 2.025f, -2.7f, 
    -0.3f, 2.025f, -3.0f, -0.3f, 2.25f, -3.0f, 0.0f, 2.25f, -2.7f, 0.0f, 1.8f, -2.7f, -0.3f, 1.8f, -3.0f, -0.3f, 
    1.8f, -3.0f, 0.0f, 1.8f, -1.5f, 0.3f, 2.25f, -1.6f, 0.3f, 2.025f, -2.5f, 0.3f, 2.25f, -2.3f, 0.3f, 2.025f, 
    -3.0f, 0.3f, 2.25f, -2.7f, 0.3f, 2.025f, -3.0f, 0.3f, 1.8f, -2.7f, 0.3f, 1.8f, -2.7f, 0.0f, 1.575f, -2.7f, 
    -0.3f, 1.575f, -3.0f, -0.3f, 1.35f, -3.0f, 0.0f, 1.35f, -2.5f, 0.0f, 1.125f, -2.5f, -0.3f, 1.125f, -2.65f, 
    -0.3f, 0.9375f, -2.65f, 0.0f, 0.9375f, -2.0f, -0.3f, 0.9f, -1.9f, -0.3f, 0.6f, -1.9f, 0.0f, 0.6f, -3.0f, 0.3f, 
    1.35f, -2.7f, 0.3f, 1.575f, -2.65f, 0.3f, 0.9375f, -2.5f, 0.3f, 1.125f, -1.9f, 0.3f, 0.6f, -2.0f, 0.3f, 0.9f, 
    1.7f, 0.0f, 1.425f, 1.7f, -0.66f, 1.425f, 1.7f, -0.66f, 0.6f, 1.7f, 0.0f, 0.6f, 2.6f, 0.0f, 1.425f, 2.6f, 
    -0.66f, 1.425f, 3.1f, -0.66f, 0.825f, 3.1f, 0.0f, 0.825f, 2.3f, 0.0f, 2.1f, 2.3f, -0.25f, 2.1f, 2.4f, -0.25f, 
    2.025f, 2.4f, 0.0f, 2.025f, 2.7f, 0.0f, 2.4f, 2.7f, -0.25f, 2.4f, 3.3f, -0.25f, 2.4f, 3.3f, 0.0f, 2.4f, 1.7f, 
    0.66f, 0.6f, 1.7f, 0.66f, 1.425f, 3.1f, 0.66f, 0.825f, 2.6f, 0.66f, 1.425f, 2.4f, 0.25f, 2.025f, 2.3f, 0.25f, 
    2.1f, 3.3f, 0.25f, 2.4f, 2.7f, 0.25f, 2.4f, 2.8f, 0.0f, 2.475f, 2.8f, -0.25f, 2.475f, 3.525f, -0.25f, 2.49375f, 
    3.525f, 0.0f, 2.49375f, 2.9f, 0.0f, 2.475f, 2.9f, -0.15f, 2.475f, 3.45f, -0.15f, 2.5125f, 3.45f, 0.0f, 2.5125f, 
    2.8f, 0.0f, 2.4f, 2.8f, -0.15f, 2.4f, 3.2f, -0.15f, 2.4f, 3.2f, 0.0f, 2.4f, 3.525f, 0.25f, 2.49375f, 2.8f, 
    0.25f, 2.475f, 3.45f, 0.15f, 2.5125f, 2.9f, 0.15f, 2.475f, 3.2f, 0.15f, 2.4f, 2.8f, 0.15f, 2.4f, 0.0f, 0.0f, 
    3.15f, 0.0f, -0.002f, 3.15f, 0.002f, 0.0f, 3.15f, 0.8f, 0.0f, 3.15f, 0.8f, -0.45f, 3.15f, 0.45f, -0.8f, 3.15f, 
    0.0f, -0.8f, 3.15f, 0.0f, 0.0f, 2.85f, 0.2f, 0.0f, 2.7f, 0.2f, -0.112f, 2.7f, 0.112f, -0.2f, 2.7f, 0.0f, -0.2f, 
    2.7f, -0.002f, 0.0f, 3.15f, -0.45f, -0.8f, 3.15f, -0.8f, -0.45f, 3.15f, -0.8f, 0.0f, 3.15f, -0.112f, -0.2f, 2.7f, 
    -0.2f, -0.112f, 2.7f, -0.2f, 0.0f, 2.7f, 0.0f, 0.002f, 3.15f, -0.8f, 0.45f, 3.15f, -0.45f, 0.8f, 3.15f, 0.0f, 
    0.8f, 3.15f, -0.2f, 0.112f, 2.7f, -0.112f, 0.2f, 2.7f, 0.0f, 0.2f, 2.7f, 0.45f, 0.8f, 3.15f, 0.8f, 0.45f, 3.15f, 
    0.112f, 0.2f, 2.7f, 0.2f, 0.112f, 2.7f, 0.4f, 0.0f, 2.55f, 0.4f, -0.224f, 2.55f, 0.224f, -0.4f, 2.55f, 0.0f, 
    -0.4f, 2.55f, 1.3f, 0.0f, 2.55f, 1.3f, -0.728f, 2.55f, 0.728f, -1.3f, 2.55f, 0.0f, -1.3f, 2.55f, 1.3f, 0.0f, 
    2.4f, 1.3f, -0.728f, 2.4f, 0.728f, -1.3f, 2.4f, 0.0f, -1.3f, 2.4f, -0.224f, -0.4f, 2.55f, -0.4f, -0.224f, 2.55f, 
    -0.4f, 0.0f, 2.55f, -0.728f, -1.3f, 2.55f, -1.3f, -0.728f, 2.55f, -1.3f, 0.0f, 2.55f, -0.728f, -1.3f, 2.4f, -1.3f, 
    -0.728f, 2.4f, -1.3f, 0.0f, 2.4f, -0.4f, 0.224f, 2.55f, -0.224f, 0.4f, 2.55f, 0.0f, 0.4f, 2.55f, -1.3f, 0.728f, 
    2.55f, -0.728f, 1.3f, 2.55f, 0.0f, 1.3f, 2.55f, -1.3f, 0.728f, 2.4f, -0.728f, 1.3f, 2.4f, 0.0f, 1.3f, 2.4f, 
    0.224f, 0.4f, 2.55f, 0.4f, 0.224f, 2.55f, 0.728f, 1.3f, 2.55f, 1.3f, 0.728f, 2.55f, 0.728f, 1.3f, 2.4f, 1.3f, 
    0.728f, 2.4f, 0.0f, 0.0f, 0.0f, 1.5f, 0.0f, 0.15f, 1.5f, 0.84f, 0.15f, 0.84f, 1.5f, 0.15f, 0.0f, 1.5f, 0.15f, 
    1.5f, 0.0f, 0.075f, 1.5f, 0.84f, 0.075f, 0.84f, 1.5f, 0.075f, 0.0f, 1.5f, 0.075f, 1.425f, 0.0f, 0.0f, 1.425f, 
    0.798f, 0.0f, 0.798f, 1.425f, 0.0f, 0.0f, 1.425f, 0.0f, -0.84f, 1.5f, 0.15f, -1.5f, 0.84f, 0.15f, -1.5f, 0.0f, 
    0.15f, -0.84f, 1.5f, 0.075f, -1.5f, 0.84f, 0.075f, -1.5f, 0.0f, 0.075f, -0.798f, 1.425f, 0.0f, -1.425f, 0.798f, 
    0.0f, -1.425f, 0.0f, 0.0f, -1.5f, -0.84f, 0.15f, -0.84f, -1.5f, 0.15f, 0.0f, -1.5f, 0.15f, -1.5f, -0.84f, 0.075f, 
    -0.84f, -1.5f, 0.075f, 0.0f, -1.5f, 0.075f, -1.425f, -0.798f, 0.0f, -0.798f, -1.425f, 0.0f, 0.0f, -1.425f, 0.0f, 
    0.84f, -1.5f, 0.15f, 1.5f, -0.84f, 0.15f, 0.84f, -1.5f, 0.075f, 1.5f, -0.84f, 0.075f, 0.798f, -1.425f, 0.0f, 1.425f, 
    -0.798f, 0.0f
  };

  ref<BezierSurface> teapot = new BezierSurface;
  const fvec3* verts = (const fvec3*)coords;

  for(int i=0; i<32; ++i)
  {
    ref<BezierPatch> patch = new BezierPatch(4,4);
    for(int j=0; j<16; ++j)
    {
      int idx = patch_idx[j+16*i]-1;
      VL_CHECK(idx < sizeof(coords) / coords[0])
      patch->points()[j] = (vec3)verts[ idx ];
    }
    teapot->patches().push_back(patch.get());
  }
  teapot->setDetail(detail);
  teapot->updateBezierSurface(false);
  real s = real(1.0) / teapot->boundingBox().width() * diameter;
  mat4 m = mat4::getTranslation( origin )  *
           mat4::getRotation(-90, 1, 0, 0) * 
           mat4::getScaling(s, s, s)       * 
           mat4::getTranslation(-teapot->boundingBox().center());
  teapot->transform( m );

  DoubleVertexRemover dvr;
  dvr.removeDoubles(teapot.get());

#if defined(VL_OPENGL_ES1) || defined(VL_OPENGL_ES2)
  teapot->makeGLESFriendly();
#endif

  return teapot;
}
//-----------------------------------------------------------------------------
ref<Geometry> vl::makeUVSphere( const vec3& origin, real diameter, int phi, int theta)
{
  ref<Geometry> geom = new Geometry;
  geom->setObjectName("UVSphere");

  diameter = diameter / 2.0f;
  ref<ArrayFloat3> vert3 = new ArrayFloat3;
  geom->setVertexArray(vert3.get());

  // create vertices
  vert3->resize( theta * phi + 2 );
  int vert_idx=0;
  vert3->at(vert_idx++) = (fvec3)(vec3(0,1*diameter,0) + origin);
  for(int i=0; i<theta; ++i)
  {
    for(int j=0; j<phi; ++j)
    {
      // vec3 v(1*radius,radius - radius*2*((real)i/(theta-1)),0);
      vec3 v(0,1*diameter,0);
      v = mat4::getRotation(180.0f/(theta+1)*(i+1),0,0,1) * v;
      v = mat4::getRotation(360.0f/phi*j,0,1,0)  * v;
      vert3->at(vert_idx++) = (fvec3)(v+origin);
    }
  }
  vert3->at(vert_idx++) = (fvec3)(vec3(0,-1*diameter,0) + origin);

  // side quads

  ref<DrawElementsUInt> quads = new DrawElementsUInt( PT_QUADS );
  quads->indexBuffer()->resize( (theta-1)*phi*4 );
  geom->drawCalls()->push_back(quads.get());
  int idx = 0;
  for(int i=0; i<theta-1; ++i)
  {
    for(int j=0; j<phi; ++j)
    {
      quads->indexBuffer()->at(idx++) = 1+phi*(i+1)+(j+0)%phi;
      quads->indexBuffer()->at(idx++) = 1+phi*(i+1)+(j+1)%phi;
      quads->indexBuffer()->at(idx++) = 1+phi*(i+0)+(j+1)%phi;
      quads->indexBuffer()->at(idx++) = 1+phi*(i+0)+(j+0)%phi;
    }
  }

  // top/bottom triangles

  ref<DrawElementsUInt> tris = new DrawElementsUInt( PT_TRIANGLES );

  tris->indexBuffer()->resize( phi*3 + phi*3 );
  geom->drawCalls()->push_back(tris.get());
  idx = 0;
  // top fan
  for(int j=0; j<phi; ++j)
  {
    tris->indexBuffer()->at(idx++) = 0;
    tris->indexBuffer()->at(idx++) = 1+(j+0)%phi;
    tris->indexBuffer()->at(idx++) = 1+(j+1)%phi;
  }
  // bottom fan
  for(int j=0; j<phi; ++j)
  {
    tris->indexBuffer()->at(idx++) = (int)geom->vertexArray()->size()-1;
    tris->indexBuffer()->at(idx++) = 1+phi*(theta-1)+(j+1)%phi;
    tris->indexBuffer()->at(idx++) = 1+phi*(theta-1)+(j+0)%phi;
  }

#if defined(VL_OPENGL_ES1) || defined(VL_OPENGL_ES2)
  geom->makeGLESFriendly();
#endif

  return geom;
}
//-----------------------------------------------------------------------------
ref<Geometry> vl::makeCylinder( const vec3& origin, real diameter, real height, int phi, int theta, bool top, bool bottom)
{
  ref<Geometry> geom = new Geometry;
  geom->setObjectName("Cylinder");

  diameter = diameter / 2;
  height = height / 2;
  ref<ArrayFloat3> vert3 = new ArrayFloat3;
  geom->setVertexArray(vert3.get());

  // create vertices
  vert3->resize( theta * phi + (top?phi+1:0) + (bottom?phi+1:0) );
  int vert_idx=0;
  for(int i=0; i<theta; ++i)
  {
    for(int j=0; j<phi; ++j)
    {
      vec3 v(1*diameter, 1*height - 2*height*((real)i/(theta-1)), 0);
      v = mat4::getRotation(360.0f/phi*j,0,1,0) * v;
      vert3->at(vert_idx++) = (fvec3)(v + origin);
    }
  }

  // side quads

  ref<DrawElementsUInt> quads = new DrawElementsUInt( PT_QUADS );
  quads->indexBuffer()->resize( (theta-1)*phi*4 );
  geom->drawCalls()->push_back(quads.get());
  int idx = 0;
  for(int i=0; i<theta-1; ++i)
  {
    for(int j=0; j<phi; ++j)
    {
      quads->indexBuffer()->at(idx++) = phi*(i+1)+(j+0)%phi;
      quads->indexBuffer()->at(idx++) = phi*(i+1)+(j+1)%phi;
      quads->indexBuffer()->at(idx++) = phi*(i+0)+(j+1)%phi;
      quads->indexBuffer()->at(idx++) = phi*(i+0)+(j+0)%phi;
    }
  }

  // top/bottom triangles

  if (top)
  {
    ref<DrawElementsUInt> tris = new DrawElementsUInt( PT_TRIANGLE_FAN );
    tris->indexBuffer()->resize( phi+2 );
    geom->drawCalls()->push_back(tris.get());
    idx = 0;

    int fan_center = vert_idx;
    vert3->at(vert_idx++) = (fvec3)(vec3(0, height, 0)  + origin);
    for(int j=0; j<phi; ++j)
    {
      vec3 v(1*diameter, height, 0);
      v = mat4::getRotation(360.0f/phi*j,0,1,0) * v;
      vert3->at(vert_idx++) = (fvec3)(v + origin);
    }

    // top fan
    tris->indexBuffer()->at(idx++) = fan_center;
    for(int j=0; j<phi+1; ++j)
      tris->indexBuffer()->at(idx++) = 1+fan_center+j%phi;
  }

  if (bottom)
  {
    ref<DrawElementsUInt> tris = new DrawElementsUInt( PT_TRIANGLE_FAN );
    tris->indexBuffer()->resize( phi+2 );
    geom->drawCalls()->push_back(tris.get());
    idx = 0;

    int fan_center = vert_idx;
    vert3->at(vert_idx++) = (fvec3)(vec3(0, -height, 0) + origin);
    for(int j=0; j<phi; ++j)
    {
      vec3 v(1*diameter, - height, 0);
      v = mat4::getRotation(360.0f/phi*j,0,1,0) * v;
      vert3->at(vert_idx++) = (fvec3)(v + origin);
    }

    // bottom fan
    tris->indexBuffer()->at(idx++) = fan_center;
    for(int j=0; j<phi+1; ++j)
      tris->indexBuffer()->at(idx++) = 1+fan_center+(phi -1 - j%phi);
  }

#if defined(VL_OPENGL_ES1) || defined(VL_OPENGL_ES2)
  geom->makeGLESFriendly();
#endif

  return geom;
}
//-----------------------------------------------------------------------------
ref<Geometry> vl::makeTorus( const vec3& origin, real diameter, real thickness, int phi, int theta, float tex_coords )
{
  ref<Geometry> geom = new Geometry;
  geom->setObjectName("Torus");

  // create vertices
  thickness /= 2.0f;
  const real radius = diameter / 2.0f - thickness;

  // vertices
  ref<ArrayFloat3> vert3 = new ArrayFloat3;
  geom->setVertexArray(vert3.get());
  vert3->resize( (phi+1) * (theta+1) );

  // normals
  ref<ArrayFloat3> norm3 = new ArrayFloat3;
  geom->setNormalArray(norm3.get());
  norm3->resize( (phi+1) * (theta+1) );

  // texture coordinates
  ref<ArrayFloat2> texc2 = new ArrayFloat2;
  if (tex_coords)
  {
    geom->setTexCoordArray(0,texc2.get());
    texc2->resize( (phi+1) * (theta+1) );
  }

  int vect_idx = 0;
  for(int i=0; i<theta+1; ++i)
  {
    for(int j=0; j<phi+1; ++j)
    {
      vec3 v(thickness, 0, 0);
      vec3 o(radius, 0, 0);
      v = mat4::getRotation(360.0f/phi*j,0,1,0) * v;
      v = mat4::getRotation(360.0f/theta*i,0,0,1) * v;
      o = mat4::getRotation(360.0f/theta*i,0,0,1) * o;

      if (tex_coords)
        texc2->at(vect_idx) = fvec2((float)i/theta,(float)j/phi) * tex_coords;

      vert3->at(vect_idx) = (fvec3)(v + o + origin);

      norm3->at(vect_idx) = (fvec3)v.normalize();

      ++vect_idx;
    }
  }

  ref<DrawElementsUInt> polys = new DrawElementsUInt( PT_QUADS );
  geom->drawCalls()->push_back(polys.get());
  int idx = 0;
  polys->indexBuffer()->resize( theta * phi * 4 );
  // create indices
  for(int i=0; i<theta; ++i)
  {
    for(int j=0; j<phi; ++j)
    {
      int i1 = i+1;
      polys->indexBuffer()->at(idx++) = (phi+1)*i +(j+0);
      polys->indexBuffer()->at(idx++) = (phi+1)*i +(j+1);
      polys->indexBuffer()->at(idx++) = (phi+1)*i1+(j+1);
      polys->indexBuffer()->at(idx++) = (phi+1)*i1+(j+0);
    }
  }

#if defined(VL_OPENGL_ES1) || defined(VL_OPENGL_ES2)
  geom->makeGLESFriendly();
#endif

  return geom;
}
//-----------------------------------------------------------------------------
ref<Geometry> vl::makeBox( const AABB& aabb, bool tex_coords )
{
  return makeBox( aabb.minCorner(), aabb.maxCorner(), tex_coords );
}
//-----------------------------------------------------------------------------
ref<Geometry> vl::makeBox( const vec3& min, const vec3& max, bool tex_coords )
{
  return makeBox( (min+max)*0.5, max.x()-min.x(), max.y()-min.y(), max.z()-min.z(), tex_coords );
}
//-----------------------------------------------------------------------------
ref<Geometry> vl::makeBox( const vec3& origin, real xside, real yside, real zside, bool tex_coords)
{
  /*
  1--------0 
  |\       |\
  | 5------|-4
  2--------3 |
   \|       \| 
    6------- 7 
  */

  ref<Geometry> geom = new Geometry;
  geom->setObjectName("Box");

  ref<ArrayFloat3> vert3 = new ArrayFloat3;
  geom->setVertexArray(vert3.get());

  real x=xside/2.0f;
  real y=yside/2.0f;
  real z=zside/2.0f;

  fvec3 a0( (fvec3)(vec3(+x,+y,+z) + origin) );
  fvec3 a1( (fvec3)(vec3(-x,+y,+z) + origin) );
  fvec3 a2( (fvec3)(vec3(-x,-y,+z) + origin) );
  fvec3 a3( (fvec3)(vec3(+x,-y,+z) + origin) );
  fvec3 a4( (fvec3)(vec3(+x,+y,-z) + origin) );
  fvec3 a5( (fvec3)(vec3(-x,+y,-z) + origin) );
  fvec3 a6( (fvec3)(vec3(-x,-y,-z) + origin) );
  fvec3 a7( (fvec3)(vec3(+x,-y,-z) + origin) );

#if defined(VL_OPENGL)

  fvec3 verts[] = {
   a1, a2, a3, a0,
   a2, a6, a7, a3,
   a6, a5, a4, a7,
   a5, a1, a0, a4,
   a0, a3, a7, a4,
   a5, a6, a2, a1
  };

  ref<DrawArrays> polys = new DrawArrays(PT_QUADS, 0, 24);
  geom->drawCalls()->push_back( polys.get() );
  vert3->resize( 24  );
  memcpy(vert3->ptr(), verts, sizeof(verts));

  if(tex_coords)
  {
    fvec2 texc[] = {
      fvec2(0,1), fvec2(0,0), fvec2(1,0), fvec2(1,1),
      fvec2(0,1), fvec2(0,0), fvec2(1,0), fvec2(1,1),
      fvec2(1,0), fvec2(1,1), fvec2(0,1), fvec2(0,0),
      fvec2(0,1), fvec2(0,0), fvec2(1,0), fvec2(1,1),
      fvec2(0,0), fvec2(1,0), fvec2(1,1), fvec2(0,1),
      fvec2(1,1), fvec2(0,1), fvec2(0,0), fvec2(1,0)
    };
    ref<ArrayFloat2> tex_array = new ArrayFloat2;
    geom->setTexCoordArray(0, tex_array.get());
    tex_array->resize( vert3->size() );
    memcpy(tex_array->ptr(), texc, sizeof(texc));
  }

#else
  
  fvec3 verts[] = {
    a1, a2, a3, a3, a0, a1,
    a2, a6, a7, a7, a3, a2,
    a6, a5, a4, a4, a7, a6,
    a5, a1, a0, a0, a4, a5,
    a0, a3, a7, a7, a4, a0,
    a5, a6, a2, a2, a1, a5
  };

  ref<DrawArrays> polys = new DrawArrays(PT_TRIANGLES, 0, 36);
  geom->drawCalls()->push_back( polys.get() );
  vert3->resize( 36 );
  memcpy(vert3->ptr(), verts, sizeof(verts));

  if(tex_coords)
  {
    fvec2 texc[] = {
      fvec2(0,1), fvec2(0,0), fvec2(1,0), fvec2(1,0), fvec2(1,1), fvec2(0,1), 
      fvec2(0,1), fvec2(0,0), fvec2(1,0), fvec2(1,0), fvec2(1,1), fvec2(0,1), 
      fvec2(1,0), fvec2(1,1), fvec2(0,1), fvec2(0,1), fvec2(0,0), fvec2(1,0), 
      fvec2(0,1), fvec2(0,0), fvec2(1,0), fvec2(1,0), fvec2(1,1), fvec2(0,1), 
      fvec2(0,0), fvec2(1,0), fvec2(1,1), fvec2(1,1), fvec2(0,1), fvec2(0,0), 
      fvec2(1,1), fvec2(0,1), fvec2(0,0), fvec2(0,0), fvec2(1,0), fvec2(1,1), 
    };
    ref<ArrayFloat2> tex_array = new ArrayFloat2;
    geom->setTexCoordArray(0, tex_array.get());
    tex_array->resize( vert3->size() );
    memcpy(tex_array->ptr(), texc, sizeof(texc));
  }

#endif

#if defined(VL_OPENGL_ES1) || defined(VL_OPENGL_ES2)
  geom->makeGLESFriendly();
#endif

  return geom;
}
//-----------------------------------------------------------------------------
ref<Geometry> vl::makePyramid( const vec3& origin, real side, real height)
{
  ref<Geometry> geom = new Geometry;
  geom->setObjectName("Pyramid");

  ref<ArrayFloat3> vert3 = new ArrayFloat3;
  geom->setVertexArray(vert3.get());

  real x = side   / 2.0f;
  real y = height;
  real z = side   / 2.0f;

  fvec3 a0( (fvec3)(vec3(+0,+y,+0) + origin) );
  fvec3 a1( (fvec3)(vec3(-x,+0,-z) + origin) );
  fvec3 a2( (fvec3)(vec3(-x,-0,+z) + origin) );
  fvec3 a3( (fvec3)(vec3(+x,-0,+z) + origin) );
  fvec3 a4( (fvec3)(vec3(+x,+0,-z) + origin) );

  ref<DrawArrays> polys = new DrawArrays(PT_TRIANGLES, 0, 6*3);
  geom->drawCalls()->push_back( polys.get() );

  vert3->resize(6*3);

  vert3->at(0)  = a4; vert3->at(1)  = a2; vert3->at(2)  = a1; 
  vert3->at(3)  = a2; vert3->at(4)  = a4; vert3->at(5)  = a3; 
  vert3->at(6)  = a4; vert3->at(7)  = a1; vert3->at(8)  = a0; 
  vert3->at(9)  = a1; vert3->at(10) = a2; vert3->at(11) = a0;
  vert3->at(12) = a2; vert3->at(13) = a3; vert3->at(14) = a0;
  vert3->at(15) = a3; vert3->at(16) = a4; vert3->at(17) = a0;

#if defined(VL_OPENGL_ES1) || defined(VL_OPENGL_ES2)
  geom->makeGLESFriendly();
#endif

  return geom;
}
//-----------------------------------------------------------------------------
ref<Geometry> vl::makeCone( const vec3& origin, real diameter, real height, int phi, bool bottom)
{
  ref<Geometry> geom = new Geometry;
  geom->setObjectName("Cone");

  ref<ArrayFloat3> vert3 = new ArrayFloat3;
  geom->setVertexArray( vert3.get() );

  diameter = diameter / 2;

  vert3->resize( phi+1 + (bottom?phi+1:0) );
  // create vertices
  int vert_idx = 0;
  vert3->at(vert_idx++) = (fvec3)(vec3(0, height/2.0f, 0) + origin);
  for(int j=0; j<phi; ++j)
  {
    vec3 v(1*diameter, -height/2.0f, 0);
    v = mat4::getRotation(360.0f/phi*j,0,1,0) * v;
    vert3->at(vert_idx++) = (fvec3)(v + origin);
  }

  // top fan
  ref<DrawElementsUInt> top_fan = new DrawElementsUInt(PT_TRIANGLE_FAN);
  top_fan->indexBuffer()->resize(phi+2);
  geom->drawCalls()->push_back(top_fan.get());
  int idx = 0;
  top_fan->indexBuffer()->at(idx++) = 0;
  for(int j=0; j<phi+1; ++j)
    top_fan->indexBuffer()->at(idx++) = 1+j%phi;

  // bottom fan
  if (bottom)
  {
    int fan_center = vert_idx;
    vert3->at(vert_idx++) = (fvec3)(vec3(0, -height/2.0f, 0) + origin);
    for(int j=0; j<phi; ++j)
    {
      vec3 v(1*diameter, -height/2.0f, 0);
      v = mat4::getRotation(360.0f/phi*j,0,1,0) * v;
      vert3->at(vert_idx++) = (fvec3)(v + origin);
    }

    ref<DrawElementsUInt> bottom_fan = new DrawElementsUInt(PT_TRIANGLE_FAN);
    bottom_fan->indexBuffer()->resize(phi+2);
    geom->drawCalls()->push_back(bottom_fan.get());
    idx = 0;
    bottom_fan->indexBuffer()->at(idx++) = fan_center;
    for(int j=0; j<phi+1; ++j)
      bottom_fan->indexBuffer()->at(idx++) = fan_center+1+(phi-1-j%phi);
  }

#if defined(VL_OPENGL_ES1) || defined(VL_OPENGL_ES2)
  geom->makeGLESFriendly();
#endif

  return geom;
}
//-----------------------------------------------------------------------------
//! \note if tex_coord_scale_u and tex_coord_scale_v are both == 0 no texture coordinate is generated
ref<Geometry> vl::makeGrid( const vec3& origin, real xside, real zside, int x, int z, bool gen_texcoords, fvec2 uv0, fvec2 uv1)
{
  ref<Geometry> geom = new Geometry;
  geom->setObjectName("Grid");

  ref<ArrayFloat3> vert3 = new ArrayFloat3;
  ref<ArrayFloat2> text2 = new ArrayFloat2;
  geom->setVertexArray( vert3.get() );

  VL_CHECK(x>=2)
  VL_CHECK(z>=2)
  real dx = xside / (x-1);
  real dz = zside / (z-1);
  xside /= 2.0f;
  zside /= 2.0f;

  vert3->resize( x * z );
  if (gen_texcoords)
  {
    geom->setTexCoordArray( 0, text2.get() );
    text2->resize( x * z );
  }

  // create vertices
  int vert_idx = 0;
  for(int i=0; i<z; ++i)
    for(int j=0; j<x; ++j, ++vert_idx)
    {
      vert3->at(vert_idx) = (fvec3)(vec3(-xside+j*dx, 0, -zside+i*dz) + origin);
      if (gen_texcoords)
      {
        float tu = (float)j/(x-1); // 0 .. 1
        float tv = (float)i/(z-1); // 0 .. 1
        text2->at(vert_idx).s() = (1.0f-tu) * uv0.s() + tu * uv1.s();
        text2->at(vert_idx).t() = (1.0f-tv) * uv0.t() + tv * uv1.t();
      }
    }

  // create indices
  ref<DrawElementsUInt> polys = new DrawElementsUInt(PT_QUADS);
  geom->drawCalls()->push_back(polys.get());
  int idx = 0;
  polys->indexBuffer()->resize( (z-1)*(x-1)*4 );
  for(int i=0; i<z-1; ++i)
  {
    for(int j=0; j<x-1; ++j)
    {
      polys->indexBuffer()->at(idx++) = j+0 + x*(i+1);
      polys->indexBuffer()->at(idx++) = j+1 + x*(i+1);
      polys->indexBuffer()->at(idx++) = j+1 + x*(i+0);
      polys->indexBuffer()->at(idx++) = j+0 + x*(i+0);
    }
  }

#if defined(VL_OPENGL_ES1) || defined(VL_OPENGL_ES2)
  geom->makeGLESFriendly();
#endif

  return geom;
}
//-----------------------------------------------------------------------------
ref<Geometry> vl::makePoints( const std::vector< vec3>& pos, const fvec4& color )
{
  ref<Geometry> geom = new Geometry;
  geom->setObjectName("Points");

  ref<ArrayFloat3> vert3 = new ArrayFloat3;
  ref<ArrayFloat4> col4 = new ArrayFloat4;
  geom->setVertexArray( vert3.get() );
  geom->setColorArray( col4.get() );
  vert3->resize( (int)pos.size() );
  col4->resize( (int)pos.size() );

  for(unsigned i=0; i<pos.size(); ++i)
  {
    vert3->at(i) = (fvec3)pos[i];
    col4->at(i)  = color;
  }

  geom->drawCalls()->push_back( new DrawArrays(PT_POINTS, 0, vert3->size() ));

#if defined(VL_OPENGL_ES1) || defined(VL_OPENGL_ES2)
  geom->makeGLESFriendly();
#endif

  return geom;
}
//-----------------------------------------------------------------------------
ref<Geometry> vl::makeIcosahedron( const vec3& origin, real diameter )
{
  ref<Geometry> geom = new Geometry;
  geom->setObjectName("Icosahedron");

  ref<ArrayFloat3> vert3 = new ArrayFloat3;
  geom->setVertexArray(vert3.get());

  // red book 1.4 p89

  const real x = 0.525731112119133606f / 1.0f;
  const real z = 0.850650808352039932f / 1.0f;
  const real radius = diameter / 2.0f;

  vert3->resize( 12 );

  vert3->at(0) = (fvec3)(origin + vec3(-x, 0.0, +z)*radius);
  vert3->at(1) = (fvec3)(origin + vec3(+x, 0.0, +z)*radius);
  vert3->at(2) = (fvec3)(origin + vec3(-x, 0.0, -z)*radius);
  vert3->at(3) = (fvec3)(origin + vec3(+x, 0.0, -z)*radius);

  vert3->at(4) = (fvec3)(origin + vec3(0.0, +z, +x)*radius);
  vert3->at(5) = (fvec3)(origin + vec3(0.0, +z, -x)*radius);
  vert3->at(6) = (fvec3)(origin + vec3(0.0, -z, +x)*radius);
  vert3->at(7) = (fvec3)(origin + vec3(0.0, -z, -x)*radius);

  vert3->at(8)  = (fvec3)(origin + vec3(+z, +x, 0.0)*radius);
  vert3->at(9)  = (fvec3)(origin + vec3(-z, +x, 0.0)*radius);
  vert3->at(10) = (fvec3)(origin + vec3(+z, -x, 0.0)*radius);
  vert3->at(11) = (fvec3)(origin + vec3(-z, -x, 0.0)*radius);

  unsigned short faces[20][3] = 
  {
    {1,4,0},  {4,9,0},  {4,5,9},  {8,5,4},  {1,8,4}, 
    {1,10,8}, {10,3,8}, {8,3,5},  {3,2,5},  {3,7,2}, 
    {3,10,7}, {10,6,7}, {6,11,7}, {6,0,11}, {6,1,0}, 
    {10,1,6}, {11,0,9}, {2,11,9}, {5,2,9},  {11,2,7}
  };

  ref<DrawElementsUShort> polys = new DrawElementsUShort(PT_TRIANGLES);
  geom->drawCalls()->push_back(polys.get());
  polys->indexBuffer()->resize(20*3);
  memcpy(polys->indexBuffer()->ptr(), faces, sizeof(faces));

#if defined(VL_OPENGL_ES1) || defined(VL_OPENGL_ES2)
  geom->makeGLESFriendly();
#endif

  return geom;
}
//-----------------------------------------------------------------------------
ref<Geometry> vl::makeCircle( vec3 origin, real radius, int slices )
{
  ref< Geometry > geom = new Geometry;
  geom->setObjectName("Circle");

  ref< ArrayFloat3 > points = new ArrayFloat3;
  geom->setVertexArray(points.get());
  points->resize( slices );
  for(int i=0; i<slices; ++i)
  {
    real t = 360.0f * i / slices;
    vec3 v = mat4::getRotation(t,0,1,0) * vec3(radius,0,0) + origin;
    points->at(i) = (fvec3)v;
  }
  geom->drawCalls()->push_back( new DrawArrays(PT_LINE_LOOP, 0, points->size()) );

#if defined(VL_OPENGL_ES1) || defined(VL_OPENGL_ES2)
  geom->makeGLESFriendly();
#endif

  return geom;
}
//-----------------------------------------------------------------------------
ref<Geometry> vl::makeCapsule(float radius, float height, int segments, ECapsuleCap top_cap, ECapsuleCap bottom_cap, const fvec4& top_col, const fvec4& bottom_col)
{
  float height2 = height / 2.0f;

  ref<Geometry> geom = new Geometry;
  geom->setObjectName("Capsule");

  ref<ArrayFloat3> vert_array = new ArrayFloat3;
  ref<ArrayFloat4> colr_array = new ArrayFloat4;
  geom->setVertexArray(vert_array.get());
  geom->setColorArray (colr_array.get());
  std::vector<fvec3> verts;
  std::vector<fvec4> cols;

  // upper
  for(int i=0; i<segments; ++i)
  {
    float a = (float)i/segments*fPi*2.0f;
    fvec3 v(::cos(a)*radius,+height2,::sin(a)*radius);
    verts.push_back(v);
    cols.push_back(top_col);
  }
  if (top_col != bottom_col)
  {
    // mid-upper
    for(int i=0; i<segments; ++i)
    {
      float a = (float)i/segments*fPi*2.0f;
      fvec3 v(::cos(a)*radius,0,::sin(a)*radius);
      verts.push_back(v);
      cols.push_back(top_col);
    }
    // mid-lower
    for(int i=0; i<segments; ++i)
    {
      float a = (float)i/segments*fPi*2.0f;
      fvec3 v(::cos(a)*radius,0,::sin(a)*radius);
      verts.push_back(v);
      cols.push_back(bottom_col);
    }
    ref<DrawElementsUInt> de_up = new DrawElementsUInt(PT_QUADS);
    ref<DrawElementsUInt> de_lo = new DrawElementsUInt(PT_QUADS);
    geom->drawCalls()->push_back(de_up.get());
    geom->drawCalls()->push_back(de_lo.get());
    de_up->indexBuffer()->resize(segments*4);
    de_lo->indexBuffer()->resize(segments*4);
    int upup = segments*0;
    int uplo = segments*1;
    int loup = segments*2;
    int lolo = segments*3;
    for(int i=0; i<segments; ++i)
    {
      int i1 = (i+1) % segments;
      de_up->indexBuffer()->at(i*4+3) = uplo + i;
      de_up->indexBuffer()->at(i*4+2) = uplo + i1;
      de_up->indexBuffer()->at(i*4+1) = upup + i1;
      de_up->indexBuffer()->at(i*4+0) = upup + i;

      de_lo->indexBuffer()->at(i*4+3) = lolo + i;
      de_lo->indexBuffer()->at(i*4+2) = lolo + i1;
      de_lo->indexBuffer()->at(i*4+1) = loup + i1;
      de_lo->indexBuffer()->at(i*4+0) = loup + i;
    }
  }
  else
  {
    ref<DrawElementsUInt> de_up = new DrawElementsUInt(PT_QUADS);
    geom->drawCalls()->push_back(de_up.get());
    de_up->indexBuffer()->resize(segments*4);
    int upup = segments*0;
    int uplo = segments*1;
    for(int i=0; i<segments; ++i)
    {
      int i1 = (i+1) % segments;
      de_up->indexBuffer()->at(i*4+3) = uplo + i;
      de_up->indexBuffer()->at(i*4+2) = uplo + i1;
      de_up->indexBuffer()->at(i*4+1) = upup + i1;
      de_up->indexBuffer()->at(i*4+0) = upup + i;
    }
  }
  // lower
  for(int i=0; i<segments; ++i)
  {
    float a = (float)i/segments*fPi*2.0f;
    fvec3 v(::cos(a)*radius,-height2,::sin(a)*radius);
    verts.push_back(v);
    cols.push_back(bottom_col);
  }
  // caps
  if (top_cap == CC_FlatCap)
  {
    int start = verts.size();
    for(int i=0; i<segments; ++i)
    {
      float a = (float)i/segments*fPi*2.0f;
      fvec3 v(::cos(a)*radius,+height2,::sin(a)*radius);
      verts.push_back(v);
      cols.push_back(top_col);
    }
    ref<DrawElementsUInt> de = new DrawElementsUInt(PT_TRIANGLE_FAN);
    geom->drawCalls()->push_back(de.get());
    de->indexBuffer()->resize(segments);
    for(int i=0,j=segments; j--; ++i)
      de->indexBuffer()->at(j) = start + i;
  }
  if (bottom_cap == CC_FlatCap)
  {
    int start = verts.size();
    for(int i=0; i<segments; ++i)
    {
      float a = (float)i/segments*fPi*2.0f;
      fvec3 v(::cos(a)*radius,-height2,::sin(a)*radius);
      verts.push_back(v);
      cols.push_back(bottom_col);
    }
    ref<DrawElementsUInt> de = new DrawElementsUInt(PT_TRIANGLE_FAN);
    geom->drawCalls()->push_back(de.get());
    de->indexBuffer()->resize(segments);
    for(int i=0; i<segments; ++i)
      de->indexBuffer()->at(i) = start + i;
  }
  int segments2 = segments/3; if (segments2<2) segments2=2;  
  if (top_cap == CC_RoundedCap)
  {
    int start = verts.size();
    for(int j=0; j<segments2; ++j)
    {
      float aj = (float)j/segments2*fPi/2.0f;
      for(int i=0; i<segments; ++i)
      {
        float a = (float)i/segments*360;
        fvec3 v(::cos(aj)*radius,::sin(aj)*radius,0);
        verts.push_back(fmat4::getRotation(a,0,1,0) * v + fvec3(0,height2,0));
        cols.push_back(top_col);
      }
    }
    // top point
    verts.push_back(fvec3(0,+height2+radius,0));
    cols.push_back(top_col);

    ref<DrawElementsUInt> de_quads = new DrawElementsUInt(PT_QUADS);
    geom->drawCalls()->push_back(de_quads.get());
    de_quads->indexBuffer()->resize(segments*(segments2-1)*4);
    for(int j=0,idx=0; j<segments2-1; ++j)
    {
      int uplo = start+segments*j;
      int upup = start+segments*(j+1);
      for(int i=0; i<segments; ++i)
      {
        int i1 = (i+1) % segments;
        de_quads->indexBuffer()->at(idx++) = uplo + i;
        de_quads->indexBuffer()->at(idx++) = uplo + i1;
        de_quads->indexBuffer()->at(idx++) = upup + i1;
        de_quads->indexBuffer()->at(idx++) = upup + i;
      }
    }

    ref<DrawElementsUInt> de = new DrawElementsUInt(PT_TRIANGLE_FAN);
    geom->drawCalls()->push_back(de.get());
    de->indexBuffer()->resize(segments+2);
    de->indexBuffer()->at(0) = (GLuint)verts.size()-1;
    for(int i=0; i<segments+1; ++i)
      de->indexBuffer()->at(i+1) = (GLuint)verts.size()-1-segments+i%segments;
  }
  if (bottom_cap == CC_RoundedCap)
  {
    int start = verts.size();
    for(int j=0; j<segments2; ++j)
    {
      float aj = (float)j/segments2*fPi/2.0f;
      for(int i=0; i<segments; ++i)
      {
        float a = -(float)i/segments*360;
        fvec3 v(::cos(aj)*radius,-::sin(aj)*radius,0);
        verts.push_back(fmat4::getRotation(a,0,1,0) * v + fvec3(0,-height2,0));
        cols.push_back(bottom_col);
      }
    }
    // bottom point
    verts.push_back(fvec3(0,-height2-radius,0));
    cols.push_back(bottom_col);

    ref<DrawElementsUInt> de_quads = new DrawElementsUInt(PT_QUADS);
    geom->drawCalls()->push_back(de_quads.get());
    de_quads->indexBuffer()->resize(segments*(segments2-1)*4);
    for(int j=0,idx=0; j<segments2-1; ++j)
    {
      int uplo = start+segments*j;
      int upup = start+segments*(j+1);
      for(int i=0; i<segments; ++i)
      {
        int i1 = (i+1) % segments;
        de_quads->indexBuffer()->at(idx++) = uplo + i;
        de_quads->indexBuffer()->at(idx++) = uplo + i1;
        de_quads->indexBuffer()->at(idx++) = upup + i1;
        de_quads->indexBuffer()->at(idx++) = upup + i;
      }
    }

    ref<DrawElementsUInt> de = new DrawElementsUInt(PT_TRIANGLE_FAN);
    geom->drawCalls()->push_back(de.get());
    de->indexBuffer()->resize(segments+2);
    de->indexBuffer()->at(0) = (GLuint)verts.size()-1;
    for(int i=0; i<segments+1; ++i)
      de->indexBuffer()->at(i+1) = (GLuint)verts.size()-1-segments+i%segments;
  }

  vert_array->initFrom(verts);
  colr_array->initFrom(cols);

#if defined(VL_OPENGL_ES1) || defined(VL_OPENGL_ES2)
  geom->makeGLESFriendly();
#endif

  return geom;
}
//-----------------------------------------------------------------------------
