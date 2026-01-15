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

#include <vlGraphics/Billboard.hpp>
#include <vlGraphics/Camera.hpp>

using namespace vl;

//-----------------------------------------------------------------------------
Billboard::Billboard()
{
  setAxis(vec3(0,1,0));
  setNormal(vec3(0,0,1));
  mType = BT_SphericalBillboard;
  VL_DEBUG_SET_OBJECT_NAME()
}
//-----------------------------------------------------------------------------
vec3 Billboard::position()
{
  return localMatrix().getT();
}
//-----------------------------------------------------------------------------
void Billboard::setPosition(const vec3& pos)
{
  setLocalMatrix( mat4::getTranslation(pos) );
}
//-----------------------------------------------------------------------------
void Billboard::computeWorldMatrix(Camera* camera)
{
  if( assumeIdentityWorldMatrix() )
  {
    setWorldMatrix( mat4() ); 
  }
  else
  if ( !camera )
  {
    Transform::computeWorldMatrix(NULL);
  }
  else
  {
    mat4 world_mat;
    vec3 pos = position();
    if (parent() && !parent()->assumeIdentityWorldMatrix())
      pos = parent()->worldMatrix() * pos;
    if ( type() == BT_SphericalBillboard )
    {
      // eye positional
      world_mat.setZ( (camera->modelingMatrix().getT() - pos).normalize() );
      world_mat.setY( camera->modelingMatrix().getY() );
      world_mat.setX( cross(world_mat.getY(), world_mat.getZ()) );
      world_mat = vl::mat4::getTranslation(pos) * world_mat;

      // eye directional
      //world_mat.setZ( camera->modelingMatrix().getZ() );
      //world_mat.setY( camera->modelingMatrix().getY() );
      //world_mat.setX( camera->modelingMatrix().getX() );
      //world_mat = vl::mat4::getTranslation(pos) * world_mat;
    }
    else
    if ( type() == BT_AxisAlignedBillboard )
    {
      vec3 normal = mNormal;
      vec3 bill_to_eye = (camera->modelingMatrix().getT() - pos).normalize();
      // flatten on the plane defined by the axis()
      normal      = normal      - axis() * dot(normal,      axis());
      bill_to_eye = bill_to_eye - axis() * dot(bill_to_eye, axis());
      normal.normalize();
      bill_to_eye.normalize();
      world_mat = vl::mat4::getTranslation(pos) * mat4::getRotation(normal,bill_to_eye);
    }

    setWorldMatrix( world_mat ); 
  }
}
//-----------------------------------------------------------------------------
