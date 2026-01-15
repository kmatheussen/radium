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

#ifndef DaeHelpers_INCLUDE_ONCE
#define DaeHelpers_INCLUDE_ONCE

#include <vlGraphics/plugins/COLLADA/DaeHelpers.hpp>
#include <vlGraphics/plugins/COLLADA/ioDae.hpp>

namespace vl
{
  //! COLLADA loader
  class DaeLoader
  {
  public:
    DaeLoader();

    bool load(VirtualFile* file);

    const ResourceDatabase* resources() const { return mResources.get(); }

    ResourceDatabase* resources() { return mResources.get(); }

    // --- options ---

    void setLoadOptions(const LoadWriterDae::LoadOptions* options) { mLoadOptions = options; }

    const LoadWriterDae::LoadOptions* loadOptions() const { return mLoadOptions; }

    // --- internal logic ---
  protected:

    void reset();

    void parseInputs(Dae::Primitive* dae_primitive, const domInputLocalOffset_Array& input_arr, const std::vector< ref<Dae::Input> >& vertex_inputs);

    ref<Dae::Mesh> parseGeometry(daeElement* geometry);

    Dae::Source* getSource(daeElement* source_el);

    void bindMaterials(Dae::Node* dae_node, Dae::Mesh* dae_mesh, domBind_materialRef bind_material);

    void parseNode(daeElement* el, Dae::Node* parent);

    void parseAsset(domElement* root);

    void loadImages(const domImage_Array& images);

    void parseImages(daeElement* library);

    void parseEffects(daeElement* library);

    void prepareTexture2D(Dae::Sampler2D* sampler2D);

    void parseMaterials(daeElement* library);

    ref<Light> parseLight(domLight*, Transform*);

    void setupLights();

    ref<Effect> setup_vl_Effect( Dae::Material* mat );

    static std::string percentDecode(const char* uri);

    static Dae::EInputSemantic getSemantic(const char* semantic);

    static const char* getSemanticString(Dae::EInputSemantic semantic);

    static ETexParamFilter translateSampleFilter(domFx_sampler_filter_common filter);

    static ETexParamWrap translateWrapMode(domFx_sampler_wrap_common wrap);
  
    // template required becase Transparent is implemented as something different from domCommon_color_or_texture_typeRef!!!
    template<class T_color_or_texture>
    void parseColor(const domProfile_COMMON* common, const T_color_or_texture& color_or_texture, Dae::ColorOrTexture* out_col);

    void generateGeometry(Dae::Primitive* primitive, const char* name);

  protected:
    const LoadWriterDae::LoadOptions* mLoadOptions;

  protected:
    ref<ResourceDatabase> mResources;
    std::vector< ref<Light> > mLights;
    std::map< daeElementRef, ref<Dae::Material> > mMaterials;
    std::map< daeElementRef, ref<Dae::Effect> > mEffects;
    std::map< daeElementRef, ref<Dae::Mesh> > mMeshes; // daeElement* -> <geometry>
    std::vector< ref<Dae::Node> > mNodes;
    std::map< daeElementRef, ref<Dae::Source> > mSources; // daeElement* -> <source>
    std::map< daeElementRef, ref<Image> > mImages;
    std::map< daeElementRef, ref<Dae::NewParam> > mDaeNewParams;
    ref<Effect> mDefaultFX;
    ref<Dae::Node> mScene;
    DAE mDAE;
    String mFilePath;
    mat4 mUpMatrix;
    bool mInvertTransparency;
    bool mAssumeOpaque;
  };
}

#endif
