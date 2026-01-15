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

#if !defined(LoadAC3D_INCLUDE_ONCE)
#define LoadAC3D_INCLUDE_ONCE

#include <vlCore/Vector3.hpp>
#include <vlGraphics/Actor.hpp>
#include <vlCore/ResourceLoadWriter.hpp>
#include <vlCore/ResourceDatabase.hpp>

namespace vl
{
  class VirtualFile;
}

namespace vl
{
  //! Loads and AC3D file (.ac)
  //!
  //! \note The importer supports only very simple objects, this are the limitations:
  //!   - No hierarchical transforms
  //!   - Imports only the polygons, no lines
  //!   - Only one material per object
  //!   - Face flags supported: only "two-sided" flag
  //!   - The objects are all flat shaded
  VLGRAPHICS_EXPORT ref<ResourceDatabase> loadAC3D( VirtualFile* file );

  //! Loads and AC3D file (.ac)
  //!
  //! \note The importer supports only very simple objects, this are the limitations:
  //!   - No hierarchical transforms
  //!   - Imports only the polygons, no lines
  //!   - Only one material per object
  //!   - Face flags supported: only "two-sided" flag
  //!   - The objects are all flat shaded
  VLGRAPHICS_EXPORT ref<ResourceDatabase> loadAC3D( const String& path );

//---------------------------------------------------------------------------
// LoadWriterAC3D
//---------------------------------------------------------------------------
  /**
   * The LoadWriterAC3D class is a ResourceLoadWriter capable of reading AC3D files.
   */
  class LoadWriterAC3D: public ResourceLoadWriter
  {
    VL_INSTRUMENT_CLASS(vl::LoadWriterAC3D, ResourceLoadWriter)

  public:
    LoadWriterAC3D(): ResourceLoadWriter("|ac|", "|ac|") {}

    ref<ResourceDatabase> loadResource(const String& path) const 
    {
      return loadAC3D(path);
    }

    ref<ResourceDatabase> loadResource(VirtualFile* file) const
    {
      return loadAC3D(file);
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
}

#endif
