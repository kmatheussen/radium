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

#ifndef Light_INCLUDE_ONCE
#define Light_INCLUDE_ONCE

#include <vlCore/Vector4.hpp>
#include <vlCore/Transform.hpp>
#include <vlGraphics/link_config.hpp>
#include <vlGraphics/RenderState.hpp>

namespace vl
{
  //------------------------------------------------------------------------------
  // Light
  //------------------------------------------------------------------------------
  /**
   * Wraps the OpenGL function glLight(). See also http://www.opengl.org/sdk/docs/man/xhtml/glLight.xml for more information.
   *
   * See also the \ref pagGuideLights "Lights Tutorial" for a practical example of how to use OpenGL lights.
   * \sa Shader, Effect, Actor, vl::EN_LIGHTING
  */
  class VLGRAPHICS_EXPORT Light: public RenderStateIndexed
  {
    VL_INSTRUMENT_CLASS(vl::Light, RenderStateIndexed)

  public:
    Light();

    virtual ERenderState type() const { return RS_Light; }

    virtual void apply(int index, const Camera*, OpenGLContext* ctx) const;

    void setAmbient(const fvec4& ambientcolor) { mAmbient = ambientcolor; }
    const fvec4& ambient() const { return mAmbient; }

    void setDiffuse(const fvec4& diffusecolor) { mDiffuse = diffusecolor; }
    const fvec4& diffuse() const { return mDiffuse; }

    void setSpecular(const fvec4& specularcolor) { mSpecular = specularcolor; }
    const fvec4& specular() const { return mSpecular; }

    /** The position or direction of a light. 
     * - If the fourth component of position equals 0.0f then the light is considered a directional light and position points in the direction of the light.
     * - If the fourth component of position equals 1.0f then the light is considered a positional light and position contains the position of the light.
     *
     * \b Note. The position or direction of a light is relative to the followed Transform or to the camera if the light does not follow any transform.
     */
    void setPosition(const fvec4& position) { mPosition = position; }
    //! The position or direction of a light. 
    const fvec4& position() const { return mPosition; }

    void setSpotDirection(const fvec3& spotdirection) { mSpotDirection = spotdirection; }
    const fvec3& spotDirection() const { return mSpotDirection; }

    void setSpotExponent(float spotexponent) { mSpotExponent = spotexponent; }
    float spotExponent() const { return mSpotExponent; }

    //! Valid values are from 0.0f to 90.0f plus the special value 180.0f (default) which disables the spot lighting.
    void setSpotCutoff(float spotcutoff) { mSpotCutoff = spotcutoff; }
    //! Valid values are from 0.0f to 90.0f plus the special value 180.0f (default) which disables the spot lighting.
    float spotCutoff() const { return mSpotCutoff; }

    //! If the light is positional, rather than directional, its intensity is attenuated by the reciprocal 
    //! of the sum of the constant attenuation, the linear attenuation times the distance between the light and the 
    //! vertex being lighted, and the quadratic attenuation times the square of the same distance.
    void setLinearAttenuation(float linearattenuation) { mLinearAttenuation = linearattenuation; }
    float linearAttenuation() const { return mLinearAttenuation; }

    //! If the light is positional, rather than directional, its intensity is attenuated by the reciprocal 
    //! of the sum of the constant attenuation, the linear attenuation times the distance between the light and the 
    //! vertex being lighted, and the quadratic attenuation times the square of the same distance.
    void setQuadraticAttenuation(float quadraticattenuation) { mQuadraticAttenuation = quadraticattenuation; }
    float quadraticAttenuation() const { return mQuadraticAttenuation; }

    //! If the light is positional, rather than directional, its intensity is attenuated by the reciprocal 
    //! of the sum of the constant attenuation, the linear attenuation times the distance between the light and the 
    //! vertex being lighted, and the quadratic attenuation times the square of the same distance.
    void setConstantAttenuation(float constantattenuation) { mConstantAttenuation = constantattenuation; }
    float constantAttenuation() const { return mConstantAttenuation; }

    //! If NULL follows the camera otherwise the given transformation node
    void bindTransform(Transform* transform);

    Transform* boundTransform();

    const Transform* boundTransform() const;
    
    virtual ref<RenderState> clone() const
    {
      ref<Light> rs = new Light;
      *rs = *this;
      return rs;
    }

    void setEnabled(bool enabled) { mEnabled = enabled; }
    
    bool enabled() const { return mEnabled; }

  protected:
    fvec4 mAmbient;
    fvec4 mDiffuse;
    fvec4 mSpecular;
    fvec4 mPosition;
    fvec3 mSpotDirection;
    float mSpotExponent;
    float mSpotCutoff;
    float mConstantAttenuation;
    float mLinearAttenuation;
    float mQuadraticAttenuation;
    ref<Transform> mBoundTransform;
    bool mEnabled;
  };
}

#endif
