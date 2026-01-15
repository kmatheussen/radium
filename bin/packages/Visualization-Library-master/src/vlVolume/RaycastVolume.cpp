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

#include <vlVolume/RaycastVolume.hpp>
#include <vlGraphics/GLSL.hpp>
#include <vlGraphics/Light.hpp>
#include <vlGraphics/Camera.hpp>

using namespace vl;

/** \class vl::RaycastVolume
 * A ActorEventCallback used to render a volume using GPU raycasting.
 *
 * Pictures from: \ref pagGuideRaycastVolume tutorial.
 *
 * <center>
 * <table border=0 cellspacing=0 cellpadding=5>
 * <tr>
 * 	<td> <img src="pics/pagGuideRaycastVolume_1.jpg"> </td>
 * 	<td> <img src="pics/pagGuideRaycastVolume_2.jpg"> </td>
 * 	<td> <img src="pics/pagGuideRaycastVolume_3.jpg"> </td>
 * </tr>
 * <tr>
 * 	<td> <img src="pics/pagGuideRaycastVolume_4.jpg"> </td>
 * 	<td> <img src="pics/pagGuideRaycastVolume_5.jpg"> </td>
 * 	<td> <img src="pics/pagGuideRaycastVolume_6.jpg"> </td>
 * </tr>
 * </table>
 * </center>
 *
 * \sa 
 * - \ref pagGuideRaycastVolume
 * - \ref pagGuideSlicedVolume
 * - SlicedVolume
 *
 */
RaycastVolume::RaycastVolume()
{
  VL_DEBUG_SET_OBJECT_NAME()
  // box geometry
  mGeometry = new Geometry;

  // install vertex coords array
  mVertCoord = new ArrayFloat3;
  mVertCoord->resize( 8 );
  mGeometry->setVertexArray( mVertCoord.get() );

  // install texture coords array
  mTexCoord = new ArrayFloat3;
  mTexCoord->resize( 8 );
  mGeometry->setTexCoordArray( 0, mTexCoord.get() );

  // install index array
  ref<DrawElementsUInt> de = new DrawElementsUInt( PT_QUADS );
  mGeometry->drawCalls()->push_back( de.get() );
  unsigned int de_indices[] = 
  {
    0,1,2,3, 1,5,6,2, 5,4,7,6, 4,0,3,7, 3,2,6,7, 4,5,1,0
  };
  de->indexBuffer()->resize( 4*6 );
  memcpy( de->indexBuffer()->ptr(), de_indices, sizeof( de_indices ) );

  // generate default texture coordinates
  fvec3 texc[] = 
  {
    fvec3( 0,0,0 ), fvec3( 1,0,0 ), fvec3( 1,1,0 ), fvec3( 0,1,0 ),
    fvec3( 0,0,1 ), fvec3( 1,0,1 ), fvec3( 1,1,1 ), fvec3( 0,1,1 )
  };
  memcpy( mTexCoord->ptr(), texc, sizeof( texc ) );

  // default box dimensions and geometry
  setBox( AABB( vec3( 0,0,0 ), vec3( 1,1,1 ) ) );
}
//-----------------------------------------------------------------------------
/** Reimplement this method to update the uniform variables of your GLSL program before the volume is rendered.
 * - By default updateUniforms() updates the position of up to 4 lights in object space. Such positions are stored in the
 *   \p "uniform vec3 light_position[4]" variable. The updateUniforms() method also fills the 
 *   \p "uniform bool light_enable[4]" variable with a flag marking if the Nth light is active or not. 
 *   These light values are computed based on the lights bound to the current Shader.
 * - The \p "uniform vec3 eye_position" variable contains the camera position in object space, useful to compute 
 *   specular highlights, raycast direction etc. 
 * - The \p "uniform vec3 eye_look" variable contains the camera look vector in object space. */
void RaycastVolume::updateUniforms( vl::Actor*actor, vl::real, const vl::Camera* camera, vl::Renderable*, const vl::Shader* shader )
{
  const GLSLProgram* glsl = shader->getGLSLProgram();
  VL_CHECK( glsl );

  // used later
  fmat4 inv_mat;
  if (actor->transform())
    inv_mat = ( fmat4 )actor->transform()->worldMatrix().getInverse();

  if ( glsl->getUniformLocation( "light_position" ) != -1 && glsl->getUniformLocation( "light_enable" ) != -1 )
  {
    // computes up to 4 light positions ( in object space ) and enables

    int light_enable[4] = { 0,0,0,0 };
    fvec3 light_position[4];

    for( int i=0; i<4; ++i )
    {
      const vl::Light* light = shader->getLight( i );
      light_enable[i] = light != NULL;
      if ( light )
      {
        // light position following transform
        if ( light->boundTransform() )
          light_position[i] = ( fmat4 )light->boundTransform()->worldMatrix() * light->position().xyz();
        // light position following camera
        else
          light_position[i] = ( ( fmat4 )camera->modelingMatrix() * light->position() ).xyz();

        // light position in object space
        if ( actor->transform() )
          light_position[i] = inv_mat * light_position[i];
      }
    }

    actor->gocUniform( "light_position" )->setUniform( 4, light_position );
    actor->gocUniform( "light_enable" )->setUniform1i( 4, light_enable );
  }

  if ( glsl->getUniformLocation( "eye_position" ) != -1 )
  {
    // pass the eye position in object space

    // eye postion
    fvec3 eye = ( fvec3 )camera->modelingMatrix().getT();
    // world to object space
    if ( actor->transform() )
      eye = inv_mat * eye;
    actor->gocUniform( "eye_position" )->setUniform( eye );
  }

  if ( glsl->getUniformLocation( "eye_look" ) != -1 )
  {
    // pass the eye look direction in object space

    // eye postion
    fvec3 look = -( fvec3 )camera->modelingMatrix().getZ();
    // world to object space
    if ( actor->transform() )
    {
      // look = inv_mat * look;
      look = ( fmat4 )actor->transform()->worldMatrix().getInverse().getTransposed() * look;
    }
    actor->gocUniform( "eye_look" )->setUniform( look );
  }
}
//-----------------------------------------------------------------------------
void RaycastVolume::bindActor( Actor* actor )
{
  actor->actorEventCallbacks()->erase( this );
  actor->actorEventCallbacks()->push_back( this );
  actor->setLod( 0, mGeometry.get() );
}
//-----------------------------------------------------------------------------
void RaycastVolume::onActorRenderStarted( Actor* actor, real clock, const Camera* camera, Renderable* rend, const Shader* shader, int pass )
{
  if ( pass>0 )
    return;

  // setup uniform variables

  if ( shader->getGLSLProgram() )
    updateUniforms( actor, clock, camera, rend, shader );
}
//-----------------------------------------------------------------------------
void RaycastVolume::generateTextureCoordinates( const ivec3& size )
{
  if ( !size.x() || !size.y() || !size.z() )
  {
    Log::error( "RaycastVolume::generateTextureCoordinates(): failed! The size passed does not represent a 3D image.\n" );
    return;
  }

  float dx = 0.5f/size.x();
  float dy = 0.5f/size.y();
  float dz = 0.5f/size.z();

  float x0 = 0.0f + dx;
  float x1 = 1.0f - dx;
  float y0 = 0.0f + dy;
  float y1 = 1.0f - dy;
  float z0 = 0.0f + dz;
  float z1 = 1.0f - dz;

  fvec3 texc[] = 
  {
    fvec3( x0,y0,z1 ), fvec3( x1,y0,z1 ), fvec3( x1,y1,z1 ), fvec3( x0,y1,z1 ), // mic fixme: i don't remember why we need z1 here and z0 below...
    fvec3( x0,y0,z0 ), fvec3( x1,y0,z0 ), fvec3( x1,y1,z0 ), fvec3( x0,y1,z0 ), 
  };
  memcpy( mTexCoord->ptr(), texc, sizeof( texc ) );
}
//-----------------------------------------------------------------------------
void RaycastVolume::generateTextureCoordinates(const ivec3& img_size, const ivec3& min_corner, const ivec3& max_corner)
{
    if (!img_size.x() || !img_size.y() || !img_size.z())
    {
        Log::error("RaycastVolume::setDisplayRegion(): failed! The size passed does not represent a 3D image.\n");
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
      fvec3( x0,y0,z1 ), fvec3( x1,y0,z1 ), fvec3( x1,y1,z1 ), fvec3( x0,y1,z1 ), 
      fvec3( x0,y0,z0 ), fvec3( x1,y0,z0 ), fvec3( x1,y1,z0 ), fvec3( x0,y1,z0 ), 
    };
    memcpy( mTexCoord->ptr(), texc, sizeof(texc) );
}
//-----------------------------------------------------------------------------
void RaycastVolume::setBox( const AABB& box ) 
{
  mBox = box;
  // generate the box geometry
  float x0 = box.minCorner().x();
  float y0 = box.minCorner().y();
  float z0 = box.minCorner().z();
  float x1 = box.maxCorner().x();
  float y1 = box.maxCorner().y();
  float z1 = box.maxCorner().z();
  fvec3 box_verts[] = 
  {
    fvec3( x0,y0,z1 ), fvec3( x1,y0,z1 ), fvec3( x1,y1,z1 ), fvec3( x0,y1,z1 ), 
    fvec3( x0,y0,z0 ), fvec3( x1,y0,z0 ), fvec3( x1,y1,z0 ), fvec3( x0,y1,z0 ), 
  };
  memcpy( mVertCoord->ptr(), box_verts, sizeof( box_verts ) );
  mGeometry->setBoundsDirty( true );
}
//-----------------------------------------------------------------------------
