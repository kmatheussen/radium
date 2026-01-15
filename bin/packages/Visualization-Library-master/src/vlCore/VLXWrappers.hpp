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

#ifndef VLXWrapper_Core_INCLUDE_ONCE
#define VLXWrapper_Core_INCLUDE_ONCE

#include <vlCore/VLXClassWrapper.hpp>
#include <vlCore/VLXRegistry.hpp>
#include <vlCore/VLXSerializer.hpp>
#include <vlCore/VLXValue.hpp>
#include <vlCore/vlxutils.hpp>

#define VL_SERIALIZER_VERSION 100

#define VLX_IMPORT_CHECK_RETURN(Condition, Obj)                                                          \
  if (!(Condition))                                                                                      \
  {                                                                                                      \
    s.signalImportError( Say("Line %n : condition failed : %s\n\tsee %s : %n\n") << (Obj).lineNumber() << #Condition <<  __FILE__ << __LINE__ ); \
    return;                                                                                              \
  }

#define VLX_IMPORT_CHECK_RETURN_NULL(Condition, Obj)                                                     \
  if (!(Condition))                                                                                      \
  {                                                                                                      \
    s.signalImportError( Say("Line %n : condition failed : %s\n\tsee %s : %n\n") << (Obj).lineNumber() << #Condition <<  __FILE__ << __LINE__ ); \
    return NULL;                                                                                         \
  }

namespace vl
{
  inline VLXValue export_AABB(const AABB& aabb)
  {
    VLXValue value ( new VLXStructure("<vl::AABB>") );
    *value.getStructure() << "MinCorner" << vlx_toValue(aabb.minCorner());
    *value.getStructure() << "MaxCorner" << vlx_toValue(aabb.maxCorner());
    return value;
  }

  inline AABB import_AABB(const VLXStructure* vlx)
  {
    AABB aabb;

    VL_CHECK( vlx->tag() == "<vl::AABB>" )

    for(size_t i=0; i<vlx->value().size(); ++i)
    {
      const std::string& key = vlx->value()[i].key();
      const VLXValue& value = vlx->value()[i].value();
      if (key == "MinCorner")
      {
        VL_CHECK(value.type() == VLXValue::ArrayReal)
        aabb.setMinCorner( vlx_vec3(value.getArrayReal()) );
      }
      else
      if (key == "MaxCorner")
      {
        VL_CHECK(value.type() == VLXValue::ArrayReal)
        aabb.setMaxCorner( vlx_vec3(value.getArrayReal()) );
      }
    }

    return aabb;
  }

  inline VLXValue export_Sphere(const Sphere& sphere)
  {
    VLXValue value ( new VLXStructure("<vl::Sphere>") );
    *value.getStructure() << "Center" << vlx_toValue(sphere.center());
    *value.getStructure() << "Radius" << sphere.radius();
    return value;
  }

  inline Sphere import_Sphere(const VLXStructure* vlx)
  {
    Sphere sphere;

    VL_CHECK( vlx->tag() == "<vl::Sphere>" )

    for(size_t i=0; i<vlx->value().size(); ++i)
    {
      const std::string& key = vlx->value()[i].key();
      const VLXValue& value = vlx->value()[i].value();
      if (key == "Center")
      {
        VL_CHECK(value.type() == VLXValue::ArrayReal)
        sphere.setCenter( vlx_vec3(value.getArrayReal()) );
      }
      else
      if (key == "Radius")
      {
        VL_CHECK(value.type() == VLXValue::Real)
        sphere.setRadius( (real)value.getReal() );
      }
    }

    return sphere;
  }
}

#endif
