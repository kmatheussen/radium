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

#if !defined(LoadSTL_INCLUDE_ONCE)
#define LoadSTL_INCLUDE_ONCE

#include <vlGraphics/Geometry.hpp>
#include <vlCore/ResourceLoadWriter.hpp>
#include <vlCore/ResourceDatabase.hpp>

namespace vl
{
  class VirtualFile;
  class TextStream;
}

namespace vl
{
//-----------------------------------------------------------------------------
  VLGRAPHICS_EXPORT ref<ResourceDatabase> loadSTL(VirtualFile* file);
  VLGRAPHICS_EXPORT ref<ResourceDatabase> loadSTL(const String& path);
//---------------------------------------------------------------------------
// LoadWriterSTL
//---------------------------------------------------------------------------
  /**
   * The LoadWriterSTL class is a ResourceLoadWriter capable of reading STL files.
   */
  class LoadWriterSTL: public ResourceLoadWriter
  {
    VL_INSTRUMENT_CLASS(vl::LoadWriterSTL, ResourceLoadWriter)

  public:
    LoadWriterSTL(): ResourceLoadWriter("|stl|", "|stl|") {}

    ref<ResourceDatabase> loadResource(const String& path) const 
    {
      return loadSTL(path);
    }

    ref<ResourceDatabase> loadResource(VirtualFile* file) const
    {
      return loadSTL(file);
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
//-----------------------------------------------------------------------------
// STLLoader
//-----------------------------------------------------------------------------
  /**
   * Loads an STL file.
   */
  class VLGRAPHICS_EXPORT STLLoader
  {
  public:
    //! Loads a STL file.
    ref<ResourceDatabase> loadSTL(VirtualFile* file);
    ref<ResourceDatabase> loadAscii(VirtualFile* file);
    ref<ResourceDatabase> loadBinary(VirtualFile* file);
  };
};

#endif

