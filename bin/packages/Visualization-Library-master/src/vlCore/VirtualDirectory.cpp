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

#include <vlCore/VirtualDirectory.hpp>

using namespace vl;

//-----------------------------------------------------------------------------
bool VirtualDirectory::setPath(const String& name)
{
  String root = name;
  root.trim();
  root.normalizeSlashes();
  if (root.empty())
  {
    Log::error("VirtualDirectory::setPath() given an empty path.\n");
    return false;
  }
  if (!root.endsWith('/'))
  {
    // Log::warning( Say("VirtualDirectory::setPath() : path (%s) must end with a '/'.\n") << root );
    root += '/';
  }

  mPath = root;
  return true;
}
//-----------------------------------------------------------------------------
String VirtualDirectory::translatePath(const String& p) const
{
  // strip trailing '/'
  String t = p;
  /*while(t.endsWith('/'))
    t = t.left(-1);*/
  while(t.startsWith('/'))
    t = t.right(-1);

  // make sure the full path is present
  if (t.startsWith(path()))
    return t.normalizeSlashes();
  else
  {
    return (path() + t).normalizeSlashes();
  }
}
//-----------------------------------------------------------------------------
