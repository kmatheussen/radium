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

#include <vlCore/Transform.hpp>
#include <vlCore/GlobalSettings.hpp>
#include <vlCore/VisualizationLibrary.hpp>
#include <vlCore/Log.hpp>
#include <algorithm>
#include <set>

using namespace vl;

//-----------------------------------------------------------------------------
// Transform
//-----------------------------------------------------------------------------
Transform::~Transform()
{
#if 0
  if (!mChildren.empty())
    Log::warning("Transform::~Transform(): a Transform with children is being destroyed! One or more Transforms will be orphaned.\n");
#endif 

  for(size_t i=0; i<mChildren.size(); ++i)
  {
    mChildren[i]->mParent = NULL;
    mChildren[i]->setLocalMatrix( mChildren[i]->worldMatrix() );
  }
}
//-----------------------------------------------------------------------------
void Transform::translate(real x, real y, real z)
{
  setLocalMatrix( mat4::getTranslation(x,y,z)*localMatrix() );
}
//-----------------------------------------------------------------------------
void Transform::translate(const vec3& t)
{
  setLocalMatrix( mat4::getTranslation(t)*localMatrix() );
}
//-----------------------------------------------------------------------------
void Transform::scale(real x, real y, real z)
{
  setLocalMatrix( mat4::getScaling(x,y,z)*localMatrix() );
}
//-----------------------------------------------------------------------------
void Transform::rotate(real degrees, real x, real y, real z)
{
  setLocalMatrix( mat4::getRotation(degrees,x,y,z)*localMatrix() );
}
//-----------------------------------------------------------------------------
void Transform::rotate(const vec3& from, const vec3& to)
{
  setLocalMatrix( mat4::getRotation(from,to)*localMatrix() );
}
//-----------------------------------------------------------------------------
void Transform::preMultiply(const mat4& m)
{
  setLocalMatrix( m*localMatrix() );
}
//-----------------------------------------------------------------------------
void Transform::postMultiply(const mat4& m)
{
  setLocalMatrix( localMatrix()*m );
}
//-----------------------------------------------------------------------------
