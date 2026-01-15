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

#include <vlGraphics/SceneManager.hpp>
#include <vlGraphics/Actor.hpp>
#include <vlGraphics/Camera.hpp>
#include <vlGraphics/Effect.hpp>
#include <vlGraphics/Scissor.hpp>
#include <vlGraphics/Texture.hpp>
#include <vlCore/Image.hpp>

using namespace vl;

//-----------------------------------------------------------------------------
SceneManager::SceneManager()
{
  VL_DEBUG_SET_OBJECT_NAME()
  // mActors = new ActorCollection;
  mBoundsDirty = true;
  mCullingEnabled = true;
  mEnableMask = 0xFFFFFFFF;
}
//-----------------------------------------------------------------------------
void SceneManager::computeBounds()
{
  ActorCollection actors;
  extractActors(actors);

  AABB bbox;
  bbox.setNull();
  for(int i=0; i<actors.size(); ++i)
  {
    actors.at(i)->computeBounds();
    bbox += actors.at(i)->boundingBox();
  }
  setBoundingBox(bbox);

  // better method
  Sphere sphere;
  real radius = real(-1.0);
  for(int i=0; i<actors.size(); ++i)
  {
    if (actors.at(i)->boundingSphere().isNull())
      continue;
    real r = (actors.at(i)->boundingSphere().center() - boundingBox().center()).length() + actors.at(i)->boundingSphere().radius();
    if (r > radius)
      radius = r;
  }
  sphere.setCenter(boundingBox().center());
  sphere.setRadius(radius);
  setBoundingSphere(sphere);

  setBoundsDirty(false);
}
//-----------------------------------------------------------------------------
bool SceneManager::isEnabled(Actor*a) const 
{ 
  return (a->enableMask() & enableMask()) != 0; 
}
//-----------------------------------------------------------------------------
