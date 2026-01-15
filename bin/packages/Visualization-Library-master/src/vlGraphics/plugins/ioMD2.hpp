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

#if !defined(LoadMD2_INCLUDE_ONCE)
#define LoadMD2_INCLUDE_ONCE

#include <vlGraphics/Actor.hpp>
#include <vlGraphics/Geometry.hpp>
#include <vlCore/VirtualFile.hpp>
#include <vlCore/ResourceLoadWriter.hpp>
#include <vlCore/ResourceDatabase.hpp>

namespace vl
{
//-----------------------------------------------------------------------------
  VLGRAPHICS_EXPORT ref<ResourceDatabase> loadMD2(const String& path);
  VLGRAPHICS_EXPORT ref<ResourceDatabase> loadMD2(VirtualFile* file);
//---------------------------------------------------------------------------
// LoadWriterMD2
//---------------------------------------------------------------------------
  /**
   * The LoadWriterMD2 class is a ResourceLoadWriter capable of reading MD2 files.
   */
  class LoadWriterMD2: public ResourceLoadWriter
  {
    VL_INSTRUMENT_CLASS(vl::LoadWriterMD2, ResourceLoadWriter)

  public:
    LoadWriterMD2(): ResourceLoadWriter("|md2|", "|md2|") {}

    ref<ResourceDatabase> loadResource(const String& path) const 
    {
      return loadMD2(path);
    }

    ref<ResourceDatabase> loadResource(VirtualFile* file) const
    {
      return loadMD2(file);
    }

    //! Not supported yet.
    bool writeResource(const String& /*path*/, ResourceDatabase* /*resource*/) const
    {
      return false;
    }

    //! Not supported yet.
    bool writeResource(VirtualFile* /*file*/, ResourceDatabase* /*resource*/) const
    {
      return false;
    }
  };
//---------------------------------------------------------------------------
}

#endif
