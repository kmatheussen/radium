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

#ifndef LoadWriterVLX_INCLUDE_ONCE
#define LoadWriterVLX_INCLUDE_ONCE

#include <vlCore/VLXClassWrapper.hpp>
#include <vlCore/VLXRegistry.hpp>
#include <vlCore/VLXSerializer.hpp>
#include <vlCore/VLXValue.hpp>
#include <vlCore/vlxutils.hpp>
#include <vlCore/LoadWriterManager.hpp>
#include <vlGraphics/Actor.hpp>
#include <vlGraphics/Effect.hpp>
#include <vlGraphics/Shader.hpp>
#include <vlGraphics/Geometry.hpp>
#include <vlGraphics/Light.hpp>
#include <vlGraphics/ClipPlane.hpp>
#include <vlGraphics/Camera.hpp>
#include <vlGraphics/DrawElements.hpp>
#include <vlGraphics/MultiDrawElements.hpp>
#include <vlGraphics/DrawArrays.hpp>
#include <vlGraphics/SceneManagerActorTree.hpp>
#include <vlGraphics/DistanceLODEvaluator.hpp>
#include <vlGraphics/PixelLODEvaluator.hpp>
#include <vlGraphics/DepthSortCallback.hpp>
#include <vlGraphics/GLSL.hpp>
#include <vlCore/ResourceDatabase.hpp>
#include <vlCore/DiskFile.hpp>

namespace vl
{
  //-----------------------------------------------------------------------------

  VLGRAPHICS_EXPORT ref<ResourceDatabase> loadVLT(VirtualFile* file);
  VLGRAPHICS_EXPORT ref<ResourceDatabase> loadVLT(const String& path);
  VLGRAPHICS_EXPORT ref<ResourceDatabase> loadVLB(VirtualFile* file);
  VLGRAPHICS_EXPORT ref<ResourceDatabase> loadVLB(const String& path);
  VLGRAPHICS_EXPORT bool saveVLT(VirtualFile* file, const ResourceDatabase*);
  VLGRAPHICS_EXPORT bool saveVLT(const String& file, const ResourceDatabase*);
  VLGRAPHICS_EXPORT bool saveVLB(VirtualFile* file, const ResourceDatabase*);
  VLGRAPHICS_EXPORT bool saveVLB(const String& file, const ResourceDatabase*);
  VLGRAPHICS_EXPORT bool isVLT(VirtualFile* file);
  VLGRAPHICS_EXPORT bool isVLT(const String& file);
  VLGRAPHICS_EXPORT bool isVLB(VirtualFile* file);
  VLGRAPHICS_EXPORT bool isVLB(const String& file);

  //---------------------------------------------------------------------------
  // LoadWriterVLX
  //---------------------------------------------------------------------------
  /**
   * A ResourceLoadWriter capable of reading Visualization Library's VLT and VLB files.
   */
  class LoadWriterVLX: public ResourceLoadWriter
  {
    VL_INSTRUMENT_CLASS(vl::LoadWriterVLX, ResourceLoadWriter)

  public:
    LoadWriterVLX(): ResourceLoadWriter("|vlt|vlb|", "|vlt|vlb|") {}

    ref<ResourceDatabase> loadResource(const String& path) const 
    {
      if (isVLT(path))
        return loadVLT(path);
      else
      if (isVLB(path))
        return loadVLB(path);
      else
        return NULL;
    }

    ref<ResourceDatabase> loadResource(VirtualFile* file) const
    {
      if (isVLT(file))
        return loadVLT(file);
      else
      if (isVLB(file))
        return loadVLB(file);
      else
        return NULL;
    }

    bool writeResource(const String& path, ResourceDatabase* res_db) const
    {
      if (path.extractFileExtension().toLowerCase() == "vlt")
        return saveVLT(path, res_db);
      else
      if (path.extractFileExtension().toLowerCase() == "vlb")
        return saveVLB(path, res_db);
      else
        return false;
    }

    bool writeResource(VirtualFile* file, ResourceDatabase* res_db) const
    {
      if (file->path().extractFileExtension().toLowerCase() == "vlt")
        return saveVLT(file, res_db);
      else
      if (file->path().extractFileExtension().toLowerCase() == "vlb")
        return saveVLB(file, res_db);
      else
        return false;
    }
  };
//-----------------------------------------------------------------------------
}

#endif
