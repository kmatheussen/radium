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

#include <vlGraphics/Light.hpp>
#include <vlCore/Transform.hpp>
#include <vlGraphics/Camera.hpp>
#include <vlCore/Log.hpp>
#include <vlCore/Say.hpp>

using namespace vl;

//------------------------------------------------------------------------------
// Light
//------------------------------------------------------------------------------
Light::Light()
{
  VL_DEBUG_SET_OBJECT_NAME()
  mAmbient = fvec4(0,0,0,1);
  mDiffuse = fvec4(1,1,1,1);
  mSpecular = fvec4(1,1,1,1);
  mPosition = fvec4(0,0,0,1);
  mSpotDirection = fvec3(0,0,-1);
  mSpotExponent = 0;
  mSpotCutoff = 180.0f;
  mConstantAttenuation  = 1.0f;
  mLinearAttenuation    = 0.0f;
  mQuadraticAttenuation = 0.0f;
  mBoundTransform = NULL;
  mEnabled = true;
}
//------------------------------------------------------------------------------
void Light::apply(int index, const Camera* camera, OpenGLContext*) const
{
  VL_CHECK_OGL()

  if (enabled())
  {
    glEnable (GL_LIGHT0 + index); VL_CHECK_OGL()

    glMatrixMode(GL_MODELVIEW);
    glPushMatrix();

    // follows the given node
    if ( boundTransform() )
      camera->applyModelViewMatrix( boundTransform()->worldMatrix() );
    else
    {
      // follows the camera
      /*glMatrixMode(GL_MODELVIEW);*/
      glLoadIdentity();
    }

    glLightfv(GL_LIGHT0+index, GL_AMBIENT,  mAmbient.ptr());
    glLightfv(GL_LIGHT0+index, GL_DIFFUSE,  mDiffuse.ptr());
    glLightfv(GL_LIGHT0+index, GL_SPECULAR, mSpecular.ptr());
    glLightfv(GL_LIGHT0+index, GL_POSITION, mPosition.ptr());

    glLightf(GL_LIGHT0+index, GL_SPOT_CUTOFF, mSpotCutoff);

    // if its a spot light
    if (mSpotCutoff != 180.0f) 
    {
      VL_CHECK(mSpotCutoff>=0.0f && mSpotCutoff<=90.0f);
      glLightfv(GL_LIGHT0+index, GL_SPOT_DIRECTION, mSpotDirection.ptr());
      glLightf(GL_LIGHT0+index, GL_SPOT_EXPONENT, mSpotExponent);
    }

    // if positional or spot light compute the attenuation factors, that is
    // attenuation is useless of directional lights.
    if (mSpotCutoff != 180.0f || mPosition.w() != 0)
    {
      glLightf(GL_LIGHT0+index, GL_CONSTANT_ATTENUATION, mConstantAttenuation);
      glLightf(GL_LIGHT0+index, GL_LINEAR_ATTENUATION, mLinearAttenuation);
      glLightf(GL_LIGHT0+index, GL_QUADRATIC_ATTENUATION, mQuadraticAttenuation);
    }

    /*glMatrixMode(GL_MODELVIEW);*/
    glPopMatrix();
  }
  else
  {
    glDisable(GL_LIGHT0 + index);
  }
}
//------------------------------------------------------------------------------
void Light::bindTransform(Transform* transform) 
{ 
  mBoundTransform = transform; 
}
//------------------------------------------------------------------------------
Transform* Light::boundTransform()
{ 
  return mBoundTransform.get(); 
}
//------------------------------------------------------------------------------
const Transform* Light::boundTransform() const
{ 
  return mBoundTransform.get(); 
}
//------------------------------------------------------------------------------
