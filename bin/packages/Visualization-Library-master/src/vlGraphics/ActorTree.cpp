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

#include <vlGraphics/ActorTree.hpp>
#include <vlCore/Log.hpp>

using namespace vl;

//-----------------------------------------------------------------------------
void ActorTree::eraseAllChildren()
{
  for(unsigned i=0;i<mChildren.size(); ++i)
    mChildren[i]->setParent(NULL);
  mChildren.clear();
}
//-----------------------------------------------------------------------------
void ActorTree::addChild(ActorTreeAbstract* node)
{ 
  if (node->parent())
  {
    vl::Log::error("ActorTreeAbstract::addChild(node): 'node' already has a parent.\n");
    return;
  }
  node->setParent(this);
  mChildren.push_back(node);
}
//-----------------------------------------------------------------------------
void ActorTree::setChild(int i, ActorTreeAbstract* node) 
{ 
  if (node->parent())
  {
    vl::Log::error("ActorTreeAbstract::setChild(int,node): 'node' already has a parent.\n");
    return;
  }
  mChildren[i]->setParent(NULL);
  mChildren[i] = node;
  mChildren[i]->setParent(this);
}
//-----------------------------------------------------------------------------
void ActorTree::eraseChild(int i, int count)
{
  for(int j=i; j<i+count; ++j)
    mChildren[j]->setParent(NULL);
  mChildren.erase(mChildren.begin()+i,mChildren.begin()+i+count);
}
//-----------------------------------------------------------------------------
