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

#ifndef LoadWriterDae_INCLUDE_ONCE
#define LoadWriterDae_INCLUDE_ONCE

#include <vlGraphics/link_config.hpp>
#include <vlCore/VirtualFile.hpp>
#include <vlCore/ResourceLoadWriter.hpp>
#include <vlCore/ResourceDatabase.hpp>

namespace vl
{
//---------------------------------------------------------------------------
// LoadWriterDae
//---------------------------------------------------------------------------
  /**
   * The LoadWriterDae class is a ResourceLoadWriter capable of reading COLLADA files.
   */
  class LoadWriterDae: public ResourceLoadWriter
  {
    VL_INSTRUMENT_CLASS(vl::LoadWriterDae, ResourceLoadWriter)

  public:
    //! Loading options used by LoadWriterDae and DaeLoader
    class LoadOptions: public Object
    {
    public:
      enum TransparencyOption 
      { 
        TransparencyKeep,   //!<< Keep the <transparency> value as it is.
        TransparencyInvert, //!<< Transparency becomes 1.0 - <transparency>.
        TransparencyAuto    //!<< Transparency is inverted if <authoring_tool> contains the string "Google" or reports ColladaMax or ColladaMaya version less than 3.03.
      };

    public:
      LoadOptions()
      {
        mInvertTransparency = TransparencyAuto;
        mUseAlwaysMipmapping = true;
        mFlattenTransformHierarchy = true;
        mComputeMissingNormals = true;
        mFixBadNormals = true;
        mMergeDrawCalls = true;
        mExtractSkins = false;
        mLightMeshSize = 0;
        mExportLights = false;
      }

      //! If true the <node>'s transform hierachy is flattened and baked inside the Actor::transform(), otherwise the full transform tree is exported. Enabled by default.
      void setFlattenTransformHierarchy(bool flatten) { mFlattenTransformHierarchy = flatten; }

      //! If true the <node>'s transform hierachy is flattened and baked inside the Actor::transform(), otherwise the full transform tree is exported. Enabled by default.
      bool flattenTransformHierarchy() const { return mFlattenTransformHierarchy; }

      //! If true then TPF_LINEAR_MIPMAP_NEAREST filtering is used when a non-mipmapped filter is specified. Enabled by default.
      void setUseAlwaysMipmapping(bool use) { mUseAlwaysMipmapping = use; }

      //! If true then TPF_LINEAR_MIPMAP_NEAREST filtering is used when a non-mipmapped filter is specified. Enabled by default.
      bool useAlwaysMipmapping() const { return mUseAlwaysMipmapping; }

      //! Invert the value of the <transparency> tag. Set to TransparencyAuto by default.
      void setInvertTransparency(TransparencyOption invert) { mInvertTransparency = invert; }

      //! Invert the value of the <transparency> tag. Set to TransparencyAuto by default.
      TransparencyOption invertTransparency() const { return mInvertTransparency; }

      //! Compute normals for those objects that don't have. Enabled by default.
      void setComputeMissingNormals(bool compute) { mComputeMissingNormals = compute; }

      //! Compute normals for those objects that don't have. Enabled by default.
      bool computeMissingNormals() const { return mComputeMissingNormals; }

      //! Fix normals that are flipped compared to the polygon winding order. Enabled by default.
      void setFixBadNormals(bool fix) { mFixBadNormals = fix; }

      //! Fix normals that are flipped compared to the polygon winding order. Enabled by default.
      bool fixBadNormals() const { return mFixBadNormals; }

      //! If set to true the skinned geometries will be also exported. Enabled by default.
      void setExtractSkins(bool extract) { mExtractSkins = extract; }

      //! If set to true the skinned geometries will be also exported. Enabled by default.
      bool extractSkins() const { return mExtractSkins; }

      //! If set to true merges all the draw calls of each Geometry into one triangle and/or one triangle strip draw call. Enabled by default.
      void setMergeDrawCalls(bool merge) { mMergeDrawCalls = merge; }

      //! If set to true merges all the draw calls of each Geometry into one triangle and/or one triangle strip draw call. Enabled by default.
      bool mergeDrawCalls() const { return mMergeDrawCalls; }

      //! If size != 0 a mesh will be generated and exported for each light source. If size == 0 no light-mesh will be generated.
      //! The generated meshes will be a sphere for point lights, a pyramid for directional light, a cone for spotlights, a torus for ambient lights.
      //! Such meshes will be renamed "LightMesh-"+<name of the light object they represent>.
      void setLightMeshSize(float size) { mLightMeshSize = size; }

      //! If size != 0 a mesh will be generated and exported for each light source. If size == 0 no light-mesh will be generated.
      //! The generated meshes will be a sphere for point lights, a pyramid for directional light, a cone for spotlights, a torus for ambient lights.
      //! Such meshes will be renamed "LightMesh-"+<name of the light object they represent>.
      float lightMeshSize() const { return mLightMeshSize; }

      //! If true the lights contained in the COLLADA file will be exported otherwise one single dummy light will be used to lit the models.
      void setExportLights(bool exp_lights) { mExportLights = exp_lights; }

      //! If true the lights contained in the COLLADA file will be exported otherwise one single dummy light will be used to lit the models.
      bool exportLights() const { return mExportLights; }

    protected:
      TransparencyOption mInvertTransparency;
      bool mFlattenTransformHierarchy;
      bool mUseAlwaysMipmapping;
      bool mComputeMissingNormals;
      bool mFixBadNormals;
      bool mExtractSkins;
      bool mMergeDrawCalls;
      float mLightMeshSize;
      bool mExportLights;
    };

  public:
    static VLGRAPHICS_EXPORT ref<ResourceDatabase> load(const String& path, const LoadOptions* options);

    static VLGRAPHICS_EXPORT ref<ResourceDatabase> load(VirtualFile* file, const LoadOptions* options);

    LoadWriterDae(): ResourceLoadWriter("|dae|", "|dae|") 
    { 
      mLoadOptions = new LoadOptions;
    }

    ref<ResourceDatabase> loadResource(const String& path) const 
    {
      return load(path, loadOptions());
    }

    ref<ResourceDatabase> loadResource(VirtualFile* file) const
    {
      return load(file, loadOptions());
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

    // --- options ---

    const LoadOptions* loadOptions() const { return mLoadOptions.get(); }

    LoadOptions* loadOptions() { return mLoadOptions.get(); }

  protected:
    ref<LoadOptions> mLoadOptions;
  };
//---------------------------------------------------------------------------
}

#endif
