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

#include <vlCore/VisualizationLibrary.hpp>
#include <vlCore/LoadWriterManager.hpp>
#include <vlGraphics/Rendering.hpp>
#include <vlGraphics/BezierSurface.hpp>
#include <vlGraphics/FontManager.hpp>

using namespace vl;

#if defined(VL_IO_3D_VLX)
  #include "plugins/ioVLX.hpp"
#include <vlGraphics/VLXWrappers.hpp>
#endif
#if defined(VL_IO_3D_3DS)
  #include "plugins/io3DS.hpp"
#endif
#if defined(VL_IO_3D_OBJ)
  #include "plugins/ioOBJ.hpp"
#endif
#if defined(VL_IO_3D_AC3D)
  #include "plugins/ioAC3D.hpp"
#endif
#if defined(VL_IO_3D_PLY)
  #include "plugins/ioPLY.hpp"
#endif
#if defined(VL_IO_3D_STL)
  #include "plugins/ioSTL.hpp"
#endif
#if defined(VL_IO_3D_MD2)
  #include "plugins/ioMD2.hpp"
#endif
#if defined(VL_IO_3D_COLLADA)
  #include "plugins/COLLADA/ioDae.hpp"
#endif

//------------------------------------------------------------------------------
// VL misc
//------------------------------------------------------------------------------
namespace
{
  bool gInitializedGraphics = false;
};
//-----------------------------------------------------------------------------
// Default FontManager
//-----------------------------------------------------------------------------
namespace
{
  ref<FontManager> gDefaultFontManager = NULL;
}
FontManager* vl::defFontManager()
{
  return gDefaultFontManager.get();
}
void vl::setDefFontManager(FontManager* fm)
{
  gDefaultFontManager = fm;
}
//-----------------------------------------------------------------------------
#if defined(VL_IO_3D_VLX)
namespace
{
  void registerVLXWrappers()
  {
    // Geometry serializer
    defVLXRegistry()->registerClassWrapper( Geometry::Type(), new VLXClassWrapper_Geometry );

    // VertexAttribInfo
    defVLXRegistry()->registerClassWrapper( VertexAttribInfo::Type(), new VLXClassWrapper_VertexAttribInfo );

    // BezierSurface
    defVLXRegistry()->registerClassWrapper( BezierSurface::Type(), new VLXClassWrapper_Geometry );

    // PatchParameter
    defVLXRegistry()->registerClassWrapper( PatchParameter::Type(), new VLXClassWrapper_PatchParameter );

    // DrawCall
    ref<VLXClassWrapper_DrawCall> drawcall_serializer = new VLXClassWrapper_DrawCall;
    defVLXRegistry()->registerClassWrapper( DrawArrays::Type(), drawcall_serializer.get() );
    defVLXRegistry()->registerClassWrapper( DrawElementsUInt::Type(), drawcall_serializer.get() );
    defVLXRegistry()->registerClassWrapper( DrawElementsUShort::Type(), drawcall_serializer.get() );
    defVLXRegistry()->registerClassWrapper( DrawElementsUByte::Type(), drawcall_serializer.get() );
    defVLXRegistry()->registerClassWrapper( MultiDrawElementsUInt::Type(), drawcall_serializer.get() );
    defVLXRegistry()->registerClassWrapper( MultiDrawElementsUShort::Type(), drawcall_serializer.get() );
    defVLXRegistry()->registerClassWrapper( MultiDrawElementsUByte::Type(), drawcall_serializer.get() );

    // ResourceDatabase
    defVLXRegistry()->registerClassWrapper( ResourceDatabase::Type(), new VLXClassWrapper_ResourceDatabase );

    // Uniform
    defVLXRegistry()->registerClassWrapper( Uniform::Type(), new VLXClassWrapper_Uniform );

    // LODEvaluator
    defVLXRegistry()->registerClassWrapper( LODEvaluator::Type(), new VLXClassWrapper_LODEvaluator );

    // Transform
    defVLXRegistry()->registerClassWrapper( Transform::Type(), new VLXClassWrapper_Transform );

    // Material
    defVLXRegistry()->registerClassWrapper( Material::Type(), new VLXClassWrapper_Material );

    // Texture
    defVLXRegistry()->registerClassWrapper( Texture::Type(), new VLXClassWrapper_Texture );

    // TextureSampler
    defVLXRegistry()->registerClassWrapper( TextureSampler::Type(), new VLXClassWrapper_TextureSampler );

    // TexParameter
    defVLXRegistry()->registerClassWrapper( TexParameter::Type(), new VLXClassWrapper_TexParameter );

    // ActorEventCallback
    defVLXRegistry()->registerClassWrapper( DepthSortCallback::Type(), new VLXClassWrapper_ActorEventCallback );

    // LODEvaluator
    ref<VLXClassWrapper_LODEvaluator> lod_evaluator = new VLXClassWrapper_LODEvaluator;
    defVLXRegistry()->registerClassWrapper( PixelLODEvaluator::Type(), lod_evaluator.get() );
    defVLXRegistry()->registerClassWrapper( DistanceLODEvaluator::Type(), lod_evaluator.get() );

    // Actor
    defVLXRegistry()->registerClassWrapper( Actor::Type(), new VLXClassWrapper_Actor );

    // Effect
    defVLXRegistry()->registerClassWrapper( Effect::Type(), new VLXClassWrapper_Effect );

    // Shader
    defVLXRegistry()->registerClassWrapper( Shader::Type(), new VLXClassWrapper_Shader );

    // Camera
    defVLXRegistry()->registerClassWrapper( Camera::Type(), new VLXClassWrapper_Camera );

    // Light
    defVLXRegistry()->registerClassWrapper( Light::Type(), new VLXClassWrapper_Light );

    // ClipPlane
    defVLXRegistry()->registerClassWrapper( ClipPlane::Type(), new VLXClassWrapper_ClipPlane );

    // Color
    defVLXRegistry()->registerClassWrapper( Color::Type(), new VLXClassWrapper_Color );

    // SecondaryColor
    defVLXRegistry()->registerClassWrapper( SecondaryColor::Type(), new VLXClassWrapper_SecondaryColor );

    // Normal
    defVLXRegistry()->registerClassWrapper( Normal::Type(), new VLXClassWrapper_Normal );

    // VertexAttrib
    defVLXRegistry()->registerClassWrapper( VertexAttrib::Type(), new VLXClassWrapper_VertexAttrib );

    // Viewport
    defVLXRegistry()->registerClassWrapper( Viewport::Type(), new VLXClassWrapper_Viewport );

    // GLSL
    defVLXRegistry()->registerClassWrapper( GLSLProgram::Type(), new VLXClassWrapper_GLSLProgram );
    ref<VLXClassWrapper_GLSLShader> sh_serializer = new VLXClassWrapper_GLSLShader;
    defVLXRegistry()->registerClassWrapper( GLSLVertexShader::Type(), sh_serializer.get() );
    defVLXRegistry()->registerClassWrapper( GLSLFragmentShader::Type(), sh_serializer.get() );
    defVLXRegistry()->registerClassWrapper( GLSLGeometryShader::Type(), sh_serializer.get() );
    defVLXRegistry()->registerClassWrapper( GLSLTessControlShader::Type(), sh_serializer.get() );
    defVLXRegistry()->registerClassWrapper( GLSLTessEvaluationShader::Type(), sh_serializer.get() );

    // GLSLShader
    defVLXRegistry()->registerClassWrapper( GLSLShader::Type(), new VLXClassWrapper_GLSLShader );

    // Array serializer
    ref<VLXClassWrapper_Array> array_serializer = new VLXClassWrapper_Array;

    defVLXRegistry()->registerClassWrapper( ArrayFloat1::Type(), array_serializer.get() );
    defVLXRegistry()->registerClassWrapper( ArrayFloat2::Type(), array_serializer.get() );
    defVLXRegistry()->registerClassWrapper( ArrayFloat3::Type(), array_serializer.get() );
    defVLXRegistry()->registerClassWrapper( ArrayFloat4::Type(), array_serializer.get() );

    defVLXRegistry()->registerClassWrapper( ArrayDouble1::Type(), array_serializer.get() );
    defVLXRegistry()->registerClassWrapper( ArrayDouble2::Type(), array_serializer.get() );
    defVLXRegistry()->registerClassWrapper( ArrayDouble3::Type(), array_serializer.get() );
    defVLXRegistry()->registerClassWrapper( ArrayDouble4::Type(), array_serializer.get() );

    defVLXRegistry()->registerClassWrapper( ArrayInt1::Type(), array_serializer.get() );
    defVLXRegistry()->registerClassWrapper( ArrayInt2::Type(), array_serializer.get() );
    defVLXRegistry()->registerClassWrapper( ArrayInt3::Type(), array_serializer.get() );
    defVLXRegistry()->registerClassWrapper( ArrayInt4::Type(), array_serializer.get() );

    defVLXRegistry()->registerClassWrapper( ArrayUInt1::Type(), array_serializer.get() );
    defVLXRegistry()->registerClassWrapper( ArrayUInt2::Type(), array_serializer.get() );
    defVLXRegistry()->registerClassWrapper( ArrayUInt3::Type(), array_serializer.get() );
    defVLXRegistry()->registerClassWrapper( ArrayUInt4::Type(), array_serializer.get() );

    defVLXRegistry()->registerClassWrapper( ArrayShort1::Type(), array_serializer.get() );
    defVLXRegistry()->registerClassWrapper( ArrayShort2::Type(), array_serializer.get() );
    defVLXRegistry()->registerClassWrapper( ArrayShort3::Type(), array_serializer.get() );
    defVLXRegistry()->registerClassWrapper( ArrayShort4::Type(), array_serializer.get() );

    defVLXRegistry()->registerClassWrapper( ArrayUShort1::Type(), array_serializer.get() );
    defVLXRegistry()->registerClassWrapper( ArrayUShort2::Type(), array_serializer.get() );
    defVLXRegistry()->registerClassWrapper( ArrayUShort3::Type(), array_serializer.get() );
    defVLXRegistry()->registerClassWrapper( ArrayUShort4::Type(), array_serializer.get() );

    defVLXRegistry()->registerClassWrapper( ArrayByte1::Type(), array_serializer.get() );
    defVLXRegistry()->registerClassWrapper( ArrayByte2::Type(), array_serializer.get() );
    defVLXRegistry()->registerClassWrapper( ArrayByte3::Type(), array_serializer.get() );
    defVLXRegistry()->registerClassWrapper( ArrayByte4::Type(), array_serializer.get() );

    defVLXRegistry()->registerClassWrapper( ArrayUByte1::Type(), array_serializer.get() );
    defVLXRegistry()->registerClassWrapper( ArrayUByte2::Type(), array_serializer.get() );
    defVLXRegistry()->registerClassWrapper( ArrayUByte3::Type(), array_serializer.get() );
    defVLXRegistry()->registerClassWrapper( ArrayUByte4::Type(), array_serializer.get() );
  }
}
#endif
//------------------------------------------------------------------------------
void VisualizationLibrary::initGraphics()
{
  VL_CHECK(!gInitializedGraphics);
  if (gInitializedGraphics)
  {
    Log::bug("VisualizationLibrary::initGraphics(): Visualization Library Graphics is already initialized!\n");
    return;
  }

  // --- Init Graphics ---

  // Install default FontManager
  gDefaultFontManager = new FontManager;

  // Register 3D modules
  #if defined(VL_IO_3D_VLX)
    registerVLXWrappers();
    registerLoadWriter(new LoadWriterVLX);
  #endif
  #if defined(VL_IO_3D_OBJ)
    registerLoadWriter(new LoadWriterOBJ);
  #endif
  #if defined(VL_IO_3D_3DS)
    registerLoadWriter(new LoadWriter3DS);
  #endif
  #if defined(VL_IO_3D_AC3D)
    registerLoadWriter(new LoadWriterAC3D);
  #endif
  #if defined(VL_IO_3D_PLY)
    registerLoadWriter(new LoadWriterPLY);
  #endif
  #if defined(VL_IO_3D_STL)
    registerLoadWriter(new LoadWriterSTL);
  #endif
  #if defined(VL_IO_3D_MD2)
    registerLoadWriter(new LoadWriterMD2);
  #endif
  #if defined(VL_IO_3D_COLLADA)
    registerLoadWriter(new LoadWriterDae);
  #endif

  // ---

  // Initialized = on
  gInitializedGraphics = true;
}
//------------------------------------------------------------------------------
void VisualizationLibrary::shutdownGraphics()
{
  if (gInitializedGraphics)
  {
    gInitializedGraphics = false;

    // --- Dispose Graphics ---

    // Dispose default FontManager
    gDefaultFontManager->releaseAllFonts();
    gDefaultFontManager = NULL;
  }
}
//------------------------------------------------------------------------------
void VisualizationLibrary::init(bool log_info)
{
  initCore(log_info);
  initGraphics();
}
//------------------------------------------------------------------------------
void VisualizationLibrary::shutdown()
{
  Log::debug("VisualizationLibrary::shutdown()\n");
  shutdownGraphics();
  shutdownCore();
}
//------------------------------------------------------------------------------
bool VisualizationLibrary::isGraphicsInitialized() { return gInitializedGraphics; }
//------------------------------------------------------------------------------
