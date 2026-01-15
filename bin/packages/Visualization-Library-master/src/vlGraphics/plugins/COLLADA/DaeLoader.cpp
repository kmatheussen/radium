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

#include <vlGraphics/plugins/COLLADA/DaeLoader.hpp>
#include <vlGraphics/GeometryPrimitives.hpp>

using namespace vl;

namespace
{
  //-----------------------------------------------------------------------------
  const char* VL_NO_MATERIAL_SPECIFIED = "<VL_NO_MATERIAL_SPECIFIED>";
  const char* VL_DEFAULT_LIGHT = "<VL_DEFAULT_LIGHT>";
  //-----------------------------------------------------------------------------
  struct 
  {
    Dae::EInputSemantic mSemantic;
    const char* mSemanticString;
  } SemanticTable[] = 
    {
      { Dae::IS_UNKNOWN,         "UNKNOWN"         },
      { Dae::IS_BINORMAL,        "BINORMAL"        },
      { Dae::IS_COLOR,           "COLOR"           },
      { Dae::IS_CONTINUITY,      "CONTINUITY"      },
      { Dae::IS_IMAGE,           "IMAGE"           },
      { Dae::IS_INPUT,           "INPUT"           },
      { Dae::IS_IN_TANGENT,      "IN_TANGENT"      },
      { Dae::IS_INTERPOLATION,   "INTERPOLATION"   },
      { Dae::IS_INV_BIND_MATRIX, "INV_BIND_MATRIX" },
      { Dae::IS_JOINT,           "JOINT"           },
      { Dae::IS_LINEAR_STEPS,    "LINEAR_STEPS"    },
      { Dae::IS_MORPHS_TARGET,   "MORPHS_TARGET"   },
      { Dae::IS_MORPH_WEIGHT,    "MORPH_WEIGHT"    },
      { Dae::IS_NORMAL,          "NORMAL"          },
      { Dae::IS_OUTPUT,          "OUTPUT"          },
      { Dae::IS_OUT_TANGENT,     "OUT_TANGENT"     },
      { Dae::IS_POSITION,        "POSITION"        },
      { Dae::IS_TANGENT,         "TANGENT"         },
      { Dae::IS_TEXBINORMAL,     "TEXBINORMAL"     },
      { Dae::IS_TEXCOORD,        "TEXCOORD"        },
      { Dae::IS_TEXTANGENT,      "TEXTANGENT"      },
      { Dae::IS_UV,              "UV"              },
      { Dae::IS_VERTEX,          "VERTEX"          },
      { Dae::IS_WEIGHT,          "WEIGHT"          },
      { Dae::IS_UNKNOWN,          NULL             }
    };
}
//-----------------------------------------------------------------------------
DaeLoader::DaeLoader()
{
  reset();

  // default material

  mDefaultFX = new Effect;
  mDefaultFX->setObjectName( VL_NO_MATERIAL_SPECIFIED );
  mDefaultFX->shader()->enable(EN_LIGHTING);
  mDefaultFX->shader()->setRenderState( new Light, 0 );
  mDefaultFX->shader()->gocMaterial()->setFlatColor( vl::fuchsia );
  mDefaultFX->shader()->gocPolygonMode()->set(PM_LINE, PM_LINE);
}
//-----------------------------------------------------------------------------
void DaeLoader::reset()
{
  mAssumeOpaque = false;
  mInvertTransparency = false;
  mScene = NULL;
  mResources = new ResourceDatabase;
}
//-----------------------------------------------------------------------------
void DaeLoader::parseInputs(Dae::Primitive* dae_primitive, const domInputLocalOffset_Array& input_arr, const std::vector< ref<Dae::Input> >& vertex_inputs)
{
  dae_primitive->mIndexStride = 0;

  for(size_t iinp=0; iinp<input_arr.getCount(); ++iinp)
  {
    domInputLocalOffsetRef input = input_arr.get(iinp);

    // copy over VERTEX inputs with the current offset and set
    if ( getSemantic(input->getSemantic()) ==  Dae::IS_VERTEX )
    {
      VL_CHECK(!vertex_inputs.empty())
      for(size_t ivert=0; ivert<vertex_inputs.size(); ++ivert)
      {
        ref<Dae::Input> dae_input = new Dae::Input;
        dae_input->mSemantic = vertex_inputs[ivert]->mSemantic;
        dae_input->mSource   = vertex_inputs[ivert]->mSource;
        dae_input->mOffset   = (size_t)input->getOffset();
        dae_input->mSet      = (size_t)input->getSet();
        dae_primitive->mChannels.push_back(dae_input);

        VL_CHECK(dae_input->mSource);
        VL_CHECK(dae_input->mSemantic != Dae::IS_UNKNOWN);

          
        dae_primitive->mIndexStride = std::max(dae_primitive->mIndexStride, dae_input->mOffset);
      }
    }
    else
    {
        ref<Dae::Input> dae_input = new Dae::Input;
        dae_input->mSemantic = getSemantic(input->getSemantic());
        dae_input->mSource   = getSource( input->getSource().getElement() );
        dae_input->mOffset   = (size_t)input->getOffset();
        dae_input->mSet      = (size_t)input->getSet();

        // if the source is NULL getSource() has already issued an error.
        if (dae_input->mSource)
          dae_primitive->mChannels.push_back( dae_input );

        VL_CHECK(dae_input->mSource);
        VL_CHECK(dae_input->mSemantic != Dae::IS_UNKNOWN);

        dae_primitive->mIndexStride = std::max(dae_primitive->mIndexStride, dae_input->mOffset);
    }
  }
    
  dae_primitive->mIndexStride += 1;
}
//-----------------------------------------------------------------------------
ref<Dae::Mesh> DaeLoader::parseGeometry(daeElement* geometry)
{
  // try to reuse the geometry in the library
  std::map< daeElementRef, ref<Dae::Mesh> >::iterator it = mMeshes.find( geometry );
  if (it != mMeshes.end())
    return it->second;

  if (!geometry->getChild("mesh"))
    return NULL;

  domMesh* mesh = static_cast<domMesh*>(geometry->getChild("mesh"));

  // add to dictionary
  ref<Dae::Mesh> dae_mesh = new Dae::Mesh;
  mMeshes[geometry] = dae_mesh;

  // vertices
  domVerticesRef vertices = mesh->getVertices();
  domInputLocal_Array input_array = vertices->getInput_array();
  for(size_t i=0; i<input_array.getCount(); ++i)
  {
    ref<Dae::Input> dae_input = new Dae::Input;

    dae_input->mSemantic = getSemantic(input_array[i]->getSemantic());
    if (dae_input->mSemantic == Dae::IS_UNKNOWN)
    {
      Log::error( Say("LoadWriterDae: the following semantic is unknown: %s\n") << input_array[i]->getSemantic() );
      continue;
    }

    dae_input->mSource = getSource( input_array[i]->getSource().getElement() );
    // if the source is NULL getSource() already issued an error.
    if (!dae_input->mSource)
      continue;

    dae_mesh->mVertexInputs.push_back(dae_input);
  }

  // --- --- ---- primitives ---- --- ---

  // NOTE: for the moment we generate one Geometry for each primitive but we should try to generate
  // one single set of vertex attribute array for each input semantic and recycle it if possible.
  // Unfortunately COLLADA makes this trivial task impossible to achieve.

  // --- ---- triangles ---- ---
  domTriangles_Array triangles_arr = mesh->getTriangles_array();
  for(size_t itri=0; itri< triangles_arr.getCount(); ++itri)
  {
    domTrianglesRef triangles = triangles_arr.get(itri);

    ref<Dae::Primitive> dae_primitive = new Dae::Primitive;
    dae_mesh->mPrimitives.push_back(dae_primitive);
    dae_primitive->mType = Dae::PT_TRIANGLES;
    dae_primitive->mCount = (size_t)triangles->getCount();

    // --- input ---
    domInputLocalOffset_Array input_arr = triangles->getInput_array();
    parseInputs(dae_primitive.get(), input_arr, dae_mesh->mVertexInputs);

    // --- ---- p ---- ---
    dae_primitive->mP.push_back( triangles->getP() );

    // --- ---- material ---- ---
    dae_primitive->mMaterial = triangles->getMaterial() ? triangles->getMaterial() : VL_NO_MATERIAL_SPECIFIED;
      
    // --- ---- generates the geometry ---- ---
    generateGeometry( dae_primitive.get(), geometry->getAttribute("id").c_str() );
  }

  // --- ---- triangles fan ---- ---
  domTrifans_Array trifan_arr = mesh->getTrifans_array();
  for(size_t itri=0; itri< trifan_arr.getCount(); ++itri)
  {
    domTrifansRef trifan = trifan_arr.get(itri);

    ref<Dae::Primitive> dae_primitive = new Dae::Primitive;
    dae_mesh->mPrimitives.push_back(dae_primitive);
    dae_primitive->mType = Dae::PT_TRIFANS;
    dae_primitive->mCount = (size_t)trifan->getCount();

    // --- input ---
    domInputLocalOffset_Array input_arr = trifan->getInput_array();
    parseInputs(dae_primitive.get(), input_arr, dae_mesh->mVertexInputs);

    // --- ---- p ---- ---
    for(size_t ip=0; ip<trifan->getP_array().getCount(); ++ip)
      dae_primitive->mP.push_back( trifan->getP_array().get(ip) );

    // --- ---- material ---- ---
    dae_primitive->mMaterial = trifan->getMaterial() ? trifan->getMaterial() : VL_NO_MATERIAL_SPECIFIED;

    // --- ---- generates the geometry ---- ---
    generateGeometry( dae_primitive.get(), geometry->getAttribute("id").c_str() );
  }

  // --- ---- triangle strip ---- ---
  domTristrips_Array tristrip_arr = mesh->getTristrips_array();
  for(size_t itri=0; itri< tristrip_arr.getCount(); ++itri)
  {
    domTristripsRef tristrip = tristrip_arr.get(itri);

    ref<Dae::Primitive> dae_primitive = new Dae::Primitive;
    dae_mesh->mPrimitives.push_back(dae_primitive);
    dae_primitive->mType = Dae::PT_TRISTRIPS;
    dae_primitive->mCount = (size_t)tristrip->getCount();

    // --- input ---
    domInputLocalOffset_Array input_arr = tristrip->getInput_array();
    parseInputs(dae_primitive.get(), input_arr, dae_mesh->mVertexInputs);

    // --- ---- p ---- ---
    for(size_t ip=0; ip<tristrip->getP_array().getCount(); ++ip)
      dae_primitive->mP.push_back( tristrip->getP_array().get(ip) );

    // --- ---- material ---- ---
    dae_primitive->mMaterial = tristrip->getMaterial() ? tristrip->getMaterial() : VL_NO_MATERIAL_SPECIFIED;
      
    // --- ---- generates the geometry ---- ---
    generateGeometry( dae_primitive.get(), geometry->getAttribute("id").c_str() );
  }

  // --- ---- polygons ---- ---
  domPolygons_Array polygon_arr = mesh->getPolygons_array();
  for(size_t itri=0; itri< polygon_arr.getCount(); ++itri)
  {
    domPolygonsRef polygon = polygon_arr.get(itri);

    ref<Dae::Primitive> dae_primitive = new Dae::Primitive;
    dae_mesh->mPrimitives.push_back(dae_primitive);
    dae_primitive->mType = Dae::PT_POLYGONS;
    dae_primitive->mCount = (size_t)polygon->getCount();

    // --- input ---
    domInputLocalOffset_Array input_arr = polygon->getInput_array();
    parseInputs(dae_primitive.get(), input_arr, dae_mesh->mVertexInputs);

    // --- ---- p ---- ---
    for(size_t ip=0; ip<polygon->getP_array().getCount(); ++ip)
      dae_primitive->mP.push_back( polygon->getP_array().get(ip) );

    // --- ---- material ---- ---
    dae_primitive->mMaterial = polygon->getMaterial() ? polygon->getMaterial() : VL_NO_MATERIAL_SPECIFIED;
      
    // --- ---- generates the geometry ---- ---
    generateGeometry( dae_primitive.get(), geometry->getAttribute("id").c_str() );
  }

  // --- ---- polylists ---- ---
  domPolylist_Array polylist_arr = mesh->getPolylist_array();
  for(size_t itri=0; itri< polylist_arr.getCount(); ++itri)
  {
    domPolylistRef polylist = polylist_arr.get(itri);

    ref<Dae::Primitive> dae_primitive = new Dae::Primitive;
    dae_mesh->mPrimitives.push_back(dae_primitive);
    dae_primitive->mType = Dae::PT_POLYGONS;
    dae_primitive->mCount = (size_t)polylist->getVcount()->getValue().getCount();

    // --- input ---
    domInputLocalOffset_Array input_arr = polylist->getInput_array();
    parseInputs(dae_primitive.get(), input_arr, dae_mesh->mVertexInputs);

    // --- ---- p ---- ---
    size_t ip=0;
    for(size_t ivc=0; ivc<polylist->getVcount()->getValue().getCount(); ++ivc)
    {
      domPRef p = static_cast<domP*>(domP::create(mDAE).cast());
      VL_CHECK(p->typeID() == domP::ID());
      dae_primitive->mP.push_back( p );
      size_t vcount = (size_t)polylist->getVcount()->getValue()[ivc];
      p->getValue().setCount(vcount * dae_primitive->mIndexStride);
      for(size_t i=0; i<p->getValue().getCount(); ++i)
        p->getValue().set(i, polylist->getP()->getValue()[ip++]);
    }

    // --- ---- material ---- ---
    dae_primitive->mMaterial = polylist->getMaterial() ? polylist->getMaterial() : VL_NO_MATERIAL_SPECIFIED;
      
    // --- ---- generates the geometry ---- ---
    generateGeometry( dae_primitive.get(), geometry->getAttribute("id").c_str() );
  }

  // --- ---- linestrips ---- ---
  domLinestrips_Array linestrip_arr = mesh->getLinestrips_array();
  for(size_t itri=0; itri< linestrip_arr.getCount(); ++itri)
  {
    domLinestripsRef linestrip = linestrip_arr.get(itri);

    ref<Dae::Primitive> dae_primitive = new Dae::Primitive;
    dae_mesh->mPrimitives.push_back(dae_primitive);
    dae_primitive->mType = Dae::PT_LINE_STRIP;
    dae_primitive->mCount = (size_t)linestrip->getCount();

    // --- input ---
    domInputLocalOffset_Array input_arr = linestrip->getInput_array();
    parseInputs(dae_primitive.get(), input_arr, dae_mesh->mVertexInputs);

    // --- ---- p ---- ---
    for(size_t ip=0; ip<linestrip->getP_array().getCount(); ++ip)
      dae_primitive->mP.push_back( linestrip->getP_array().get(ip) );

    // --- ---- material ---- ---
    dae_primitive->mMaterial = linestrip->getMaterial() ? linestrip->getMaterial() : VL_NO_MATERIAL_SPECIFIED;
      
    // --- ---- generates the geometry ---- ---
    generateGeometry( dae_primitive.get(), geometry->getAttribute("id").c_str() );
  }

  // --- ---- lines ---- ---
  domLines_Array line_arr = mesh->getLines_array();
  for(size_t itri=0; itri< line_arr.getCount(); ++itri)
  {
    domLinesRef line = line_arr.get(itri);

    ref<Dae::Primitive> dae_primitive = new Dae::Primitive;
    dae_mesh->mPrimitives.push_back(dae_primitive);
    dae_primitive->mType = Dae::PT_LINES;
    dae_primitive->mCount = (size_t)line->getCount();

    // --- input ---
    domInputLocalOffset_Array input_arr = line->getInput_array();
    parseInputs(dae_primitive.get(), input_arr, dae_mesh->mVertexInputs);

    // --- ---- p ---- ---
    dae_primitive->mP.push_back( line->getP() );

    // --- ---- material ---- ---
    dae_primitive->mMaterial = line->getMaterial() ? line->getMaterial() : VL_NO_MATERIAL_SPECIFIED;
      
    // --- ---- generates the geometry ---- ---
    generateGeometry( dae_primitive.get(), geometry->getAttribute("id").c_str() );
  }

  return dae_mesh;
}
//-----------------------------------------------------------------------------
Dae::Source* DaeLoader::getSource(daeElement* source_el)
{
  std::map< daeElementRef, ref<Dae::Source> >::iterator it = mSources.find(source_el);
  if (it != mSources.end())
    return it->second.get();
  else
  {
    VL_CHECK(source_el->typeID() == domSource::ID())
    domSourceRef source = static_cast<domSource*>(source_el);

    domSource::domTechnique_commonRef tech_common = source->getTechnique_common(); VL_CHECK(tech_common)
    domAccessorRef accessor = tech_common->getAccessor();
      
    size_t mask = 0;
    // we support up to 32 parameters for a single accessor
    domParam_Array param_array = accessor->getParam_array();
    size_t attr_count = param_array.getCount() <= 32 ? param_array.getCount() : 32;
    for(size_t ipar=0; ipar<attr_count; ++ipar)
    {
      if (param_array[ipar]->getName() && strlen(param_array[ipar]->getName()))
        mask |= 1<<ipar;
    }

    ref<Dae::Source> dae_source = new Dae::Source;

    if (source->getFloat_array())
      dae_source->init(source->getFloat_array(), accessor->getCount(), accessor->getStride(), accessor->getOffset(), mask);
    else
    if (source->getInt_array())
      dae_source->init(source->getInt_array(), accessor->getCount(), accessor->getStride(), accessor->getOffset(), mask);
    else
    if (source->getBool_array())
      dae_source->init(source->getBool_array(), accessor->getCount(), accessor->getStride(), accessor->getOffset(), mask);
    else
    {
      Log::error("LoadWriterDae: no supported source data found. Only Float_array, Int_array and Bool_array are supported as source data.\n");
      return NULL;
    }

    // add to source library for quick access later
    mSources[source] = dae_source;

    return dae_source.get();
  }
}
//-----------------------------------------------------------------------------
ref<Effect> DaeLoader::setup_vl_Effect( Dae::Material* mat )
{
  VL_CHECK(mat)
  VL_CHECK(mat->mDaeEffect)
  // VL_CHECK(mat->mDaeEffect->mDaeTechniqueCOMMON)

  ref<Effect> fx = new Effect;
  fx->shader()->enable(EN_DEPTH_TEST);

  // very basic material setup
  if (mat->mDaeEffect->mDaeTechniqueCOMMON)
  {
    Dae::TechniqueCOMMON* common_tech =mat->mDaeEffect->mDaeTechniqueCOMMON.get();

    // compute the actual tranparency
    float transparency = 0;
    if ( common_tech->mOpaqueMode == Dae::OM_A_ONE )
      transparency = common_tech->mTransparent.mColor.a() * common_tech->mTransparency;
    else
      transparency = (1.0f - dot( common_tech->mTransparent.mColor.rgb(), fvec3(0.2126f, 0.7152f, 0.0722f))) * common_tech->mTransparency;

    bool use_lighting = strstr(mat->mDaeEffect->objectName().c_str(), "blinn:") ||
                        strstr(mat->mDaeEffect->objectName().c_str(), "phong:") ||
                        strstr(mat->mDaeEffect->objectName().c_str(), "lambert:");

    // enable lighting only if required
    if ( use_lighting )
    {
      fx->shader()->enable(EN_LIGHTING);

      // mic fixme: most of .dae files I tested require this even if no double_sided flag is set.
      // fx->shader()->gocLightModel()->setTwoSide(true);

      // material sanity checks: these are needed only when using fixed function pipeline
      common_tech->mShininess = vl::clamp(common_tech->mShininess, 0.0f, 128.0f);

      // mic fixme: this vl::Effect can be put in mDaeTechniqueCOMMON and shared among all the materials that use it.
      fx->shader()->gocMaterial()->setDiffuse  ( common_tech->mDiffuse.mColor  ); // this is fuchsia by default
      fx->shader()->gocMaterial()->setAmbient  ( common_tech->mAmbient.mColor  );
      fx->shader()->gocMaterial()->setEmission ( common_tech->mEmission.mColor );
      fx->shader()->gocMaterial()->setSpecular ( common_tech->mSpecular.mColor );
      fx->shader()->gocMaterial()->setShininess( common_tech->mShininess );

      // if a texture is bound to the diffuse channel use it
      // mic fixme: for the moment we support only one texture, we also assume the texture coords #0 are the right ones...
      if ( common_tech->mDiffuse.mSampler && common_tech->mDiffuse.mSampler->mTexture )
      {
        fx->shader()->gocTextureSampler(0)->setTexture( common_tech->mDiffuse.mSampler->mTexture.get() );
        fx->shader()->gocMaterial()->setDiffuse( vl::white );
      }

      // alpha blending management

      // sets the alpha value of all material colors, front and back
      fx->shader()->gocMaterial()->multiplyTransparency( transparency );
    }
    else
    if ( strstr(mat->mDaeEffect->objectName().c_str(), "constant:") )
    {

      // constant can have only emission, check if there is a texture attached to it and use it
      // mic fixme: for the moment we support only one texture, we also assume the texture coords #0 are the right ones...
      if ( common_tech->mEmission.mSampler && common_tech->mEmission.mSampler->mTexture )
      {
        fx->shader()->gocTextureSampler(0)->setTexture( common_tech->mEmission.mSampler->mTexture.get() );
        // this is already the default: fx->shader()->gocColor()->setColor( vl::white );
      }
      else
        fx->shader()->gocColor()->setValue( common_tech->mEmission.mColor );
    }

    // enable alpha blending if material is transparent or alpha is coming from the diffuse texture
    // NOTE: to be pedantic with the specs we should enabled the alpha blending if common_tech->mBlendingOn == true however
    // applications and exporters have historically misused the <transparency> and <transparent> tags...
    if ( transparency < 1.0f || (common_tech->mTransparent.mSampler && common_tech->mTransparent.mSampler == common_tech->mDiffuse.mSampler) )
      if (!mAssumeOpaque)
        fx->shader()->enable(EN_BLEND);

    // to be correct we should do this but most models are not made to render correctly this way...
#if 0
    if (!mat->mDaeEffect->mDoubleSided)
      fx->shader()->enable(EN_CULL_FACE); // no two sidelighting, yes culling
    else
    if (use_lighting)
      fx->shader()->gocLightModel()->setTwoSide(true); // yes two side lighting, no culling
#endif
  }
  else
  {
    Log::error("LoadWriterDae: technique or profile not supported.\n");
    fx->shader()->gocMaterial()->setDiffuse( vl::fuchsia );
  }

  return fx;
}
//-----------------------------------------------------------------------------
void DaeLoader::bindMaterials(Dae::Node* dae_node, Dae::Mesh* dae_mesh, domBind_materialRef bind_material)
{
  // map symbols to actual materials
  std::map< std::string, Dae::Material* > material_map;

  if ( bind_material )
  {
    if (bind_material->getTechnique_common())
    {
      domInstance_material_Array& material_instances = bind_material->getTechnique_common()->getInstance_material_array();
      for(size_t i=0; i<material_instances.getCount(); ++i)
      {
        daeElement* material = material_instances[i]->getTarget().getElement();
        VL_CHECK(material)
        std::map< daeElementRef, ref<Dae::Material> >::iterator it = mMaterials.find( material );
        if (it != mMaterials.end())
        {
          // mic fixme: issue warning
          // VL_CHECK( material_map.find(material_instances[i]->getSymbol()) == material_map.end() )
          // VL_CHECK( material_instances[i]->getSymbol() )
          material_map[ material_instances[i]->getSymbol() ] = it->second.get();
        }
        else
        {
          VL_LOG_DEBUG << "- LoadWriterDae: material '" << material << "' not found!\n";
          continue;
        }
      }
    }
    else
    {
      VL_LOG_DEBUG << "- LoadWriterDae: technique_COMMON not found!\n";
    }
  }

  // now we need to instance the material
  for(size_t iprim=0; iprim<dae_mesh->mPrimitives.size(); ++iprim)
  {
    ref<Dae::Material> dae_material;

    if (!dae_mesh->mPrimitives[iprim]->mMaterial.empty())
    {
      std::map< std::string, Dae::Material* >::iterator it = material_map.find( dae_mesh->mPrimitives[iprim]->mMaterial );
      if (it != material_map.end())
      {
        dae_material = it->second;
      }
      else
      {
        if ( dae_mesh->mPrimitives[iprim]->mMaterial != VL_NO_MATERIAL_SPECIFIED)
        {
          VL_LOG_DEBUG << "- LoadWriterDae: material symbol " << dae_mesh->mPrimitives[iprim]->mMaterial << " could not be resolved.\n";
        }
      }
    }

    ref<Effect> fx = dae_material ? setup_vl_Effect(dae_material.get()) : mDefaultFX;

    ref<Actor> actor = new Actor( dae_mesh->mPrimitives[iprim]->mGeometry.get(), fx.get(), dae_node->mTransform.get() );
    dae_node->mActors.push_back( actor );
  }
}
//-----------------------------------------------------------------------------
void DaeLoader::parseNode(daeElement* el, Dae::Node* parent)
{
  if (el->typeID() == domNode::ID())
  {
    // --- --- --- parse this node --- --- ---

    // create new node and add it to the library
    ref<Dae::Node> this_node = new Dae::Node;
    mNodes.push_back(this_node);
    parent->mChildren.push_back( this_node );
    parent->mTransform->addChild( this_node->mTransform.get() );

    domNode* node = static_cast<domNode*>(el);

    // parse geometries
    domInstance_geometry_Array geometries = node->getInstance_geometry_array();
    for(size_t i=0; i<geometries.getCount(); ++i)
    {
      VL_CHECK(geometries[i]->getUrl().getElement()->typeID() == domGeometry::ID())
      daeElement* geometry = geometries[i]->getUrl().getElement();
      ref<Dae::Mesh> dae_mesh = parseGeometry(geometry);
      if (dae_mesh)
        this_node->mMesh.push_back(dae_mesh.get());
        
      // generate the Actors belonging to this node with their own material
      bindMaterials(this_node.get(), dae_mesh.get(), geometries[i]->getBind_material());
    }

    // parse controllers
    if (loadOptions()->extractSkins())
    {
      domInstance_controller_Array controllers = node->getInstance_controller_array();
      for(size_t i=0; i<controllers.getCount(); ++i)
      {
        VL_CHECK(controllers[i]->getUrl().getElement()->typeID() == domController::ID())
        daeElement* controller_el = controllers[i]->getUrl().getElement();
        VL_CHECK(controller_el)
        if (!controller_el)
          continue;

        domController* controller = static_cast<domController*>(controller_el);
        daeElement* geometry = controller->getSkin()->getSource().getElement();
        VL_CHECK(geometry)
        if (!geometry)
          continue;

        ref<Dae::Mesh> dae_mesh = parseGeometry(geometry);
        if (dae_mesh)
          this_node->mMesh.push_back(dae_mesh.get());
        
        // generate the Actors belonging to this node with their own material
        bindMaterials(this_node.get(), dae_mesh.get(), controllers[i]->getBind_material());
      }
    }

    // note: transforms are post-multiplied in the order in which they are specified (as if they were sub-nodes)
    for(size_t ichild=0; ichild<node->getChildren().getCount(); ++ichild)
    {
      daeElement* child = node->getChildren()[ichild];
      
      if ( 0 == strcmp(child->getElementName(), "matrix") )
      {
        domMatrix* matrix = static_cast<domMatrix*>(child);
        mat4 local_matrix;
        for(int i=0; i<16; ++i)
          local_matrix.ptr()[i] = (real)matrix->getValue().get(i);
        local_matrix.transpose();
        this_node->mTransform->postMultiply(local_matrix);
      }
      else
      if ( 0 == strcmp(child->getElementName(), "translate") )
      {
        domTranslate* tr = static_cast<domTranslate*>(child);
        mat4 m = mat4::getTranslation((real)tr->getValue()[0], (real)tr->getValue()[1], (real)tr->getValue()[2]);
        this_node->mTransform->postMultiply( m );
      }
      else
      if ( 0 == strcmp(child->getElementName(), "rotate") )
      {
        domRotate* rot = static_cast<domRotate*>(child);
        mat4 m = mat4::getRotation((real)rot->getValue()[3], (real)rot->getValue()[0], (real)rot->getValue()[1], (real)rot->getValue()[2]);
        this_node->mTransform->postMultiply( m );
      }
      else
      if ( 0 == strcmp(child->getElementName(), "scale") )
      {
        domScale* sc = static_cast<domScale*>(child);
        mat4 m = mat4::getScaling((real)sc->getValue()[0], (real)sc->getValue()[1], (real)sc->getValue()[2]);
        this_node->mTransform->postMultiply( m );
      }
      else
      if ( 0 == strcmp(child->getElementName(), "lookat") )
      {
        domLookat* lookat = static_cast<domLookat*>(child);
        vec3 eye ((real)lookat->getValue()[0], (real)lookat->getValue()[1], (real)lookat->getValue()[2]);
        vec3 look((real)lookat->getValue()[3], (real)lookat->getValue()[4], (real)lookat->getValue()[5]);
        vec3 up  ((real)lookat->getValue()[6], (real)lookat->getValue()[7], (real)lookat->getValue()[8]);
        this_node->mTransform->preMultiply( mat4::getLookAt(eye, look, up) );
      }
      else
      if ( 0 == strcmp(child->getElementName(), "skew") )
      {
        // mic fixme: support skew
        // domSkew* skew = static_cast<domSkew*>(child);
        Log::error("LoadWriterDae: <skew> transform not supported yet. Call me if you know how to compute it.\n");
      }
    }

    // parse lights
    domInstance_light_Array lights = node->getInstance_light_array();
    for(size_t i=0; i<lights.getCount(); ++i)
    {
      daeElementRef dae_light = lights[i]->getUrl().getElement();
      domLight* dom_light = dynamic_cast<domLight*>(dae_light.cast());
      ref<Light> light = parseLight(dom_light, this_node->mTransform.get());
      mLights.push_back( light );
    }

    // --- --- --- parse children --- --- ---

    // parse instance nodes
    domInstance_node_Array nodes = node->getInstance_node_array();
    for(size_t i=0; i<nodes.getCount(); ++i)
    {
      daeElement* node = nodes[i]->getUrl().getElement();
      VL_CHECK(node->typeID() == domNode::ID())
      parseNode(node, this_node.get());
    }

    // parse proper children
    daeTArray< daeSmartRef<daeElement> > children = node->getChildren();
    for(size_t i=0; i<children.getCount(); ++i)
      parseNode(children[i], this_node.get());
  }
}
//-----------------------------------------------------------------------------
bool DaeLoader::load(VirtualFile* file)
{
  reset();

  mFilePath = file->path();

  // load COLLADA file as a string.
  std::vector<char> buffer;
  file->load(buffer);
  if (buffer.empty())
    return false;
  buffer.push_back(0);

  daeElement* root = mDAE.openFromMemory(file->path().toStdString(), (char*)&buffer[0]);
  if (!root)
  {
    Log::error( "LoadWriterDae: failed to open COLLADA document.\n" );
    return false;
  }

  parseAsset(root);

  parseImages(root->getDescendant("library_images"));

  parseEffects(root->getDescendant("library_effects"));

  parseMaterials(root->getDescendant("library_materials"));

  daeElement* visual_scene = root->getDescendant("visual_scene");
  if (!visual_scene)
  {
    Log::error( "LoadWriterDae: <visual_scene> not found!\n" );
    return false;
  }

  // --- parse the visual scene ---

  mScene = new Dae::Node;
  daeTArray< daeSmartRef<daeElement> > children = visual_scene->getChildren();
  for(size_t i=0; i<children.getCount(); ++i)
    parseNode(children[i], mScene.get());

  // --- fill the resource database and final setup ---

  // --- transform setup ---
  // Up vector
  // Note that we don't touch the local space vertices and the intermediate matrices,
  // for proper reorientation of matrices and geometry the up-vector conditioner in
  // Refinery should do the job.
  mScene->mTransform->preMultiply( mUpMatrix );
  // Setup world matrices
  mScene->mTransform->computeWorldMatrixRecursive();

  // --- light setup ---
  // Computes position and direction of lights, sorts them (direction -> spot -> point), 
  // adds them to the resource database.
  setupLights();

  // return the Actors
  for( size_t inode=0; inode<mNodes.size(); ++inode )
  {
    for(size_t i=0; i<mNodes[inode]->mActors.size(); ++i)
    {
      Actor* actor = mNodes[inode]->mActors[i].get();

      // add actor to the resources
      mResources->resources().push_back( actor );

      // *** flatten transform hierarchy ***
      if ( loadOptions()->flattenTransformHierarchy() )
        actor->transform()->removeFromParent();

      // *** merge draw calls ***
      if (loadOptions()->mergeDrawCalls())
      {
        Geometry* geom = actor->lod(0)->as<Geometry>();
        if (geom)
        {
          // first merge all tristrips
          ref<DrawCall> tristrips = geom->mergeTriangleStrips();
          // keep it for later
          geom->drawCalls()->erase( tristrips.get() );

          // merge all non-tristrips
          geom->mergeDrawCallsWithTriangles(PT_UNKNOWN);

          // put back the tristrips
          if (tristrips.get())
            geom->drawCalls()->push_back( tristrips.get() );
        }
      }

      // *** check for transforms that require normal rescaling ***
      mat4 nmatrix = actor->transform()->worldMatrix().as3x3().invert().transpose();
      real len_x = nmatrix.getX().length();
      real len_y = nmatrix.getY().length();
      real len_z = nmatrix.getZ().length();
      if ( fabs(len_x - 1) > 0.05f || fabs(len_y - 1) > 0.05f || fabs(len_z - 1) > 0.05f )
      {
        // Log::warning("Detected mesh with scaled transform: enabled normal renormalization.\n");
        if ( actor->effect()->shader()->isEnabled(vl::EN_LIGHTING) )
          actor->effect()->shader()->enable(vl::EN_NORMALIZE); // or vl::EN_RESCALE_NORMAL
      }

      // *** light association & normal computation (only if lighting is on!) ***
      if ( actor->effect()->shader()->isEnabled(EN_LIGHTING) )
      {
        // *** light association ***
        // crete new effect/shader with it's own light set
        ref<Effect> fx = new Effect;
        fx->setObjectName( actor->effect()->objectName().c_str() );
        fx->shader()->setEnableSet( actor->effect()->shader()->getEnableSet() );
        fx->shader()->setRenderStateSet( actor->effect()->shader()->getRenderStateSet() );
        actor->setEffect( fx.get() );
        for(size_t ilight=0; ilight<mLights.size() && ilight<8; ++ilight)
          fx->shader()->setRenderState( mLights[ilight].get(), ilight );

       // *** compute missing normals ***
        Geometry* geom = actor->lod(0)->as<Geometry>();
       if ( loadOptions()->computeMissingNormals() && geom && !geom->normalArray() )
         geom->computeNormals();
      }
    }
  }

  if ( loadOptions()->flattenTransformHierarchy() )
    mScene->mTransform->flattenHierarchy();
  else
    mResources->resources().push_back( mScene->mTransform );

  return true;
}
//-----------------------------------------------------------------------------
std::string DaeLoader::percentDecode(const char* uri)
{
  std::string str;
  for(int i=0; uri[i]; ++i)
  {
    // process encoded character
    if ( uri[i] == '%' && uri[i+1] && uri[i+2] )
    {
      ++i;
      char hex1 = uri[i];
      if (hex1 >= '0' && hex1 <= '9')
        hex1 -= '0';
      else
      if (hex1 >= 'A' && hex1 <= 'F')
        hex1 -= 'A';
      else
      if (hex1 >= 'a' && hex1 <= 'f')
        hex1 -= 'a';
      else
        hex1 = -1;

      ++i;
      char hex2 = uri[i];
      if (hex2 >= '0' && hex2 <= '9')
        hex2 -= '0';
      else
      if (hex2 >= 'A' && hex2 <= 'F')
        hex2 -= 'A';
      else
      if (hex2 >= 'a' && hex2 <= 'f')
        hex2 -= 'a';
      else
        hex2 = -1;

      // encoding error
      if (hex1 == -1 || hex2 == -1)
      {
        // insert percent code as it is
        str.push_back('%');
        i -= 2;
      }

      char ch = (hex1 << 4) + (hex2);
      str.push_back(ch);
    }
    else
      str.push_back(uri[i]);
  }
  return str;
}
//-----------------------------------------------------------------------------
void DaeLoader::loadImages(const domImage_Array& images)
{
  for(size_t i=0; i<images.getCount(); ++i)
  {
    if ( strstr( images[i]->getInit_from()->getValue().getProtocol(), "file") == 0 )
    {
      Log::error( Say("LoadWriterDae: protocol not supported: %s\n") << images[i]->getInit_from()->getValue().getURI() );
      continue;
    }

    std::string full_path = percentDecode( images[i]->getInit_from()->getValue().getURI() + 6 );
    ref<Image> image = loadImage( full_path.c_str() );
      
    mImages[ images[i].cast() ] = image;
  }
}
//-----------------------------------------------------------------------------
void DaeLoader::parseImages(daeElement* library)
{
  if (!library)
    return;
  domLibrary_images* library_images = static_cast<domLibrary_images*>(library);
  const domImage_Array& images = library_images->getImage_array();
  loadImages(images);
}
//-----------------------------------------------------------------------------
void DaeLoader::parseEffects(daeElement* library)
{
  if (!library)
    return;
  domLibrary_effects* library_effects = static_cast<domLibrary_effects*>(library);
  const domEffect_Array& effects = library_effects->getEffect_array();
  for(size_t i=0; i<effects.getCount(); ++i)
  {
    domEffect* effect = effects[i].cast();

    ref<Dae::Effect> dae_effect = new Dae::Effect;

    std::string effect_name;
    if (effect->getName())
      effect_name = effect->getName();

    mEffects[effect] = dae_effect;

    // load images
    loadImages(effect->getImage_array());

    const domFx_profile_abstract_Array& profiles = effect->getFx_profile_abstract_array();
    for(size_t i=0; i<profiles.getCount(); ++i)
    {
      // <profile_COMMON>
      if ( profiles[i]->typeID() == domProfile_COMMON::ID() )
      {
        domProfile_COMMON* common = static_cast<domProfile_COMMON*>(profiles[i].cast());

        // --- parse <newparam> ---
        for(size_t ipar=0; ipar<common->getNewparam_array().getCount(); ++ipar)
        {
          domCommon_newparam_typeRef newparam = common->getNewparam_array()[ipar];

          ref<Dae::NewParam> dae_newparam = new Dae::NewParam;
          dae_effect->mNewParams.push_back( dae_newparam );

          // insert in the map se can resolve references to <sampler2D> and <surface>
          mDaeNewParams[newparam.cast()] = dae_newparam;

          // --- <surface> ---
          // mic fixme: for the moment we support only single image 2D surfaces
          if (newparam->getSurface())
          {
            domFx_surface_commonRef surface = newparam->getSurface();

            if ( !surface->getFx_surface_init_common()->getInit_from_array().getCount() )
            {
              VL_LOG_DEBUG << "- 'surface->getFx_surface_init_common()->getInit_from_array().getCount()' is 0: " << __FILE__ << ":" << __LINE__ << "\n";
              continue;
            }

            dae_newparam->mDaeSurface = new Dae::Surface;
            daeElement* ref_image = surface->getFx_surface_init_common()->getInit_from_array()[0]->getValue().getElement();
            if (!ref_image)
            {
              VL_LOG_DEBUG << "- 'surface->getFx_surface_init_common()->getInit_from_array()[0]->getValue().getElement()' FAILED: " << __FILE__ << ":" << __LINE__ << "\n";
              continue;
            }

            std::map< daeElementRef, ref<Image> >::iterator it = mImages.find( ref_image );
            if (it != mImages.end())
              dae_newparam->mDaeSurface->mImage = it->second.get();
            else
            {
              VL_LOG_DEBUG << "- 'mImages.find( ref_image )' FAILED: " << __FILE__ << ":" << __LINE__ << "\n";
              continue;
            }
          }

          // --- <sampler2D> ---
          if (newparam->getSampler2D())
          {
            domFx_sampler2D_commonRef sampler2D = newparam->getSampler2D();
              
            dae_newparam->mDaeSampler2D = new Dae::Sampler2D;

            // --- <source> ---
            daeSIDResolver sid_res( effect, sampler2D->getSource()->getValue() );
            domElement* surface_newparam = sid_res.getElement();
            if(!surface_newparam)
            {
              VL_LOG_DEBUG << (Say("- <surface> '%s' referenced by <sampler2D> '%s' not found!\n") << sampler2D->getSource()->getValue() << newparam->getSid() );
              continue;
            }

            std::map< daeElementRef, ref<Dae::NewParam> >::iterator it = mDaeNewParams.find(surface_newparam);
            if ( it != mDaeNewParams.end() )
            {
              dae_newparam->mDaeSampler2D->mDaeSurface = it->second->mDaeSurface;
            }
            else
            {
              VL_LOG_DEBUG << "- 'mDaeNewParams.find(surface_newparam)' FAILED: " << __FILE__ << ":" << __LINE__ << "\n";
              continue;
            }
              
            // --- <minfilter> ---
            if( sampler2D->getMinfilter() )
            {
              dae_newparam->mDaeSampler2D->mMinFilter = translateSampleFilter( sampler2D->getMinfilter()->getValue() );
            }
                

            // --- <magfilter> ---
            if( sampler2D->getMagfilter() )
            {
              dae_newparam->mDaeSampler2D->mMagFilter = translateSampleFilter( sampler2D->getMagfilter()->getValue() );
            }

            // --- <wrap_s> ---
            if (sampler2D->getWrap_s())
            {
              dae_newparam->mDaeSampler2D->mWrapS = translateWrapMode( sampler2D->getWrap_s()->getValue() );
            }

            // --- <wrap_t> ---
            if (sampler2D->getWrap_t())
            {
              dae_newparam->mDaeSampler2D->mWrapT = translateWrapMode( sampler2D->getWrap_t()->getValue() );
            }

            // prepare vl::Texture for creation with the given parameters
            prepareTexture2D(dae_newparam->mDaeSampler2D.get());
          }

          // --- <float>, <float2>, <float3>, <floa4> ---
          if ( newparam->getFloat() )
          {
            dae_newparam->mFloat4 = fvec4((float)newparam->getFloat()->getValue(), 0, 0, 0);
          }
          else
          if ( newparam->getFloat2() )
          {
            daeDouble* fptr = &newparam->getFloat2()->getValue()[0];
            dae_newparam->mFloat4 = fvec4((float)fptr[0], (float)fptr[1], 0, 0);
          }
          else
          if ( newparam->getFloat3() )
          {
            daeDouble* fptr = &newparam->getFloat3()->getValue()[0];
            dae_newparam->mFloat4 = fvec4((float)fptr[0], (float)fptr[1], (float)fptr[2], 0);
          }
          else
          if ( newparam->getFloat4() )
          {
            daeDouble* fptr = &newparam->getFloat4()->getValue()[0];
            dae_newparam->mFloat4 = fvec4((float)fptr[0], (float)fptr[1], (float)fptr[2], (float)fptr[3]);
          }
        }

        // <technique sid="COMMON">

        // --- parse technique ---
        if (common->getTechnique()->getBlinn())
        {
          // track effect name
          effect_name = "blinn:"+effect_name;

          domProfile_COMMON::domTechnique::domBlinnRef blinn = common->getTechnique()->getBlinn();

          dae_effect->mDaeTechniqueCOMMON = new Dae::TechniqueCOMMON;
          parseColor( common, blinn->getEmission(), &dae_effect->mDaeTechniqueCOMMON->mEmission );
          parseColor( common, blinn->getAmbient(),  &dae_effect->mDaeTechniqueCOMMON->mAmbient );
          parseColor( common, blinn->getDiffuse(),  &dae_effect->mDaeTechniqueCOMMON->mDiffuse );
          parseColor( common, blinn->getSpecular(), &dae_effect->mDaeTechniqueCOMMON->mSpecular );
          if (blinn->getShininess())
            dae_effect->mDaeTechniqueCOMMON->mShininess = (float)blinn->getShininess()->getFloat()->getValue();

          parseColor( common, blinn->getReflective(), &dae_effect->mDaeTechniqueCOMMON->mReflective );
          if (blinn->getReflectivity())
            dae_effect->mDaeTechniqueCOMMON->mReflectivity = (float)blinn->getReflectivity()->getFloat()->getValue();

          if (blinn->getTransparent())
          {
            dae_effect->mDaeTechniqueCOMMON->mBlendingOn = true;
            parseColor( common, blinn->getTransparent(), &dae_effect->mDaeTechniqueCOMMON->mTransparent );
            dae_effect->mDaeTechniqueCOMMON->mOpaqueMode = blinn->getTransparent()->getOpaque() == FX_OPAQUE_ENUM_A_ONE ? Dae::OM_A_ONE : Dae::OM_RGB_ZERO;
          }
          if (blinn->getTransparency())
          {
            dae_effect->mDaeTechniqueCOMMON->mBlendingOn = true;
            dae_effect->mDaeTechniqueCOMMON->mTransparency = (float)blinn->getTransparency()->getFloat()->getValue();
          }
        }
        else
        if (common->getTechnique()->getPhong())
        {
          // track effect name
          effect_name = "phong:"+effect_name;

          domProfile_COMMON::domTechnique::domPhongRef phong = common->getTechnique()->getPhong();

          dae_effect->mDaeTechniqueCOMMON = new Dae::TechniqueCOMMON;
          parseColor( common, phong->getEmission(), &dae_effect->mDaeTechniqueCOMMON->mEmission );
          parseColor( common, phong->getAmbient(),  &dae_effect->mDaeTechniqueCOMMON->mAmbient );
          parseColor( common, phong->getDiffuse(),  &dae_effect->mDaeTechniqueCOMMON->mDiffuse );
          parseColor( common, phong->getSpecular(), &dae_effect->mDaeTechniqueCOMMON->mSpecular );
          if (phong->getShininess())
          {
            dae_effect->mDaeTechniqueCOMMON->mShininess = (float)phong->getShininess()->getFloat()->getValue();
          }

          parseColor( common, phong->getReflective(), &dae_effect->mDaeTechniqueCOMMON->mReflective );
          if (phong->getReflectivity())
            dae_effect->mDaeTechniqueCOMMON->mReflectivity = (float)phong->getReflectivity()->getFloat()->getValue();

          if (phong->getTransparent())
          {
            dae_effect->mDaeTechniqueCOMMON->mBlendingOn = true;
            parseColor( common, phong->getTransparent(), &dae_effect->mDaeTechniqueCOMMON->mTransparent );
            dae_effect->mDaeTechniqueCOMMON->mOpaqueMode = phong->getTransparent()->getOpaque() == FX_OPAQUE_ENUM_A_ONE ? Dae::OM_A_ONE : Dae::OM_RGB_ZERO;
          }
          if (phong->getTransparency())
          {
            dae_effect->mDaeTechniqueCOMMON->mBlendingOn = true;
            dae_effect->mDaeTechniqueCOMMON->mTransparency = (float)phong->getTransparency()->getFloat()->getValue();
          }
        }
        else
        if (common->getTechnique()->getLambert())
        {
          // track effect name
          effect_name = "lambert:"+effect_name;

          domProfile_COMMON::domTechnique::domLambertRef lambert = common->getTechnique()->getLambert();

          dae_effect->mDaeTechniqueCOMMON = new Dae::TechniqueCOMMON;
          parseColor( common, lambert->getEmission(), &dae_effect->mDaeTechniqueCOMMON->mEmission );
          parseColor( common, lambert->getAmbient(),  &dae_effect->mDaeTechniqueCOMMON->mAmbient );
          parseColor( common, lambert->getDiffuse(),  &dae_effect->mDaeTechniqueCOMMON->mDiffuse );
          dae_effect->mDaeTechniqueCOMMON->mSpecular.mColor = fvec4(0,0,0,1);
          dae_effect->mDaeTechniqueCOMMON->mShininess = 0;

          parseColor( common, lambert->getReflective(), &dae_effect->mDaeTechniqueCOMMON->mReflective );
          if (lambert->getReflectivity())
            dae_effect->mDaeTechniqueCOMMON->mReflectivity = (float)lambert->getReflectivity()->getFloat()->getValue();

          if (lambert->getTransparent())
          {
            dae_effect->mDaeTechniqueCOMMON->mBlendingOn = true;
            parseColor( common, lambert->getTransparent(), &dae_effect->mDaeTechniqueCOMMON->mTransparent );
            dae_effect->mDaeTechniqueCOMMON->mOpaqueMode = lambert->getTransparent()->getOpaque() == FX_OPAQUE_ENUM_A_ONE ? Dae::OM_A_ONE : Dae::OM_RGB_ZERO;
          }
          if (lambert->getTransparency())
          {
            dae_effect->mDaeTechniqueCOMMON->mBlendingOn = true;
            dae_effect->mDaeTechniqueCOMMON->mTransparency = (float)lambert->getTransparency()->getFloat()->getValue();
          }
        }
        else
        if (common->getTechnique()->getConstant())
        {
          // track effect name
          effect_name = "constant:"+effect_name;

          domProfile_COMMON::domTechnique::domConstantRef constant = common->getTechnique()->getConstant();

          dae_effect->mDaeTechniqueCOMMON = new Dae::TechniqueCOMMON;
          parseColor( common, constant->getEmission(), &dae_effect->mDaeTechniqueCOMMON->mEmission );
          dae_effect->mDaeTechniqueCOMMON->mAmbient.mColor  = fvec4(0,0,0,1);
          dae_effect->mDaeTechniqueCOMMON->mDiffuse.mColor  = fvec4(0,0,0,1);
          dae_effect->mDaeTechniqueCOMMON->mSpecular.mColor = fvec4(0,0,0,1);
          dae_effect->mDaeTechniqueCOMMON->mShininess = 0;

          parseColor( common, constant->getReflective(), &dae_effect->mDaeTechniqueCOMMON->mReflective );
          if (constant->getReflectivity())
            dae_effect->mDaeTechniqueCOMMON->mReflectivity = (float)constant->getReflectivity()->getFloat()->getValue();

          if (constant->getTransparent())
          {
            dae_effect->mDaeTechniqueCOMMON->mBlendingOn = true;
            parseColor( common, constant->getTransparent(), &dae_effect->mDaeTechniqueCOMMON->mTransparent );
            dae_effect->mDaeTechniqueCOMMON->mOpaqueMode = constant->getTransparent()->getOpaque() == FX_OPAQUE_ENUM_A_ONE ? Dae::OM_A_ONE : Dae::OM_RGB_ZERO;
          }
          if (constant->getTransparency())
          {
            dae_effect->mDaeTechniqueCOMMON->mBlendingOn = true;
            dae_effect->mDaeTechniqueCOMMON->mTransparency = (float)constant->getTransparency()->getFloat()->getValue();
          }
        }
        else
        {
          Log::error("LoadWriterDae: technique not supported.\n");
        }

        dae_effect->setObjectName( effect_name.c_str() );

        // trasparency override options
        if (mAssumeOpaque)
        {
          dae_effect->mDaeTechniqueCOMMON->mTransparency = 1.0f;
          dae_effect->mDaeTechniqueCOMMON->mTransparent.mColor = fvec4(0, 0, 0, 1);
          dae_effect->mDaeTechniqueCOMMON->mOpaqueMode = Dae::OM_A_ONE; // mic fixme: metti questo dentro Dae:: namespace
        }
        else
        if(mInvertTransparency)
        {
          dae_effect->mDaeTechniqueCOMMON->mTransparency = 1.0f - dae_effect->mDaeTechniqueCOMMON->mTransparency;
          // and don't trust <transparent> values...
          dae_effect->mDaeTechniqueCOMMON->mTransparent.mColor = fvec4(0, 0, 0, 1);
          dae_effect->mDaeTechniqueCOMMON->mOpaqueMode = Dae::OM_A_ONE;
        }

        // <extra>

        for(size_t iextra=0; iextra<common->getExtra_array().getCount(); ++iextra)
        {
          domExtraRef extra = common->getExtra_array()[iextra];
          for(size_t itech=0; itech<extra->getTechnique_array().getCount(); ++itech)
          {
            domTechniqueRef tech = extra->getTechnique_array()[itech];
            if ( strstr(tech->getProfile(), "GOOGLEEARTH") )
            {
              domAny* double_sided = (domAny*)tech->getChild("double_sided");
              if (double_sided)
              {
                const char* ptr = double_sided->getValue();
                if(strcmp(ptr, "1") == 0)
                  dae_effect->mDoubleSided = true;
              }
            }
          }
        }

      }

    }
  }
}
//-----------------------------------------------------------------------------
void DaeLoader::prepareTexture2D(Dae::Sampler2D* sampler2D)
{
  if (sampler2D->mDaeSurface && sampler2D->mDaeSurface->mImage)
  {
    bool use_mipmaps = true;
    switch(sampler2D->mMinFilter)
    {
    case TPF_LINEAR:
    case TPF_NEAREST:
      if ( loadOptions()->useAlwaysMipmapping() )
        sampler2D->mMinFilter = TPF_LINEAR_MIPMAP_NEAREST;
      else
        use_mipmaps = false;
    default:
      break;
    }

    sampler2D->mTexture = new Texture;
    sampler2D->mTexture->prepareTexture2D(sampler2D->mDaeSurface->mImage.get(), TF_UNKNOWN, use_mipmaps, false);
    sampler2D->mTexture->getTexParameter()->setWrapS(sampler2D->mWrapS);
    sampler2D->mTexture->getTexParameter()->setWrapT(sampler2D->mWrapT);
    sampler2D->mTexture->getTexParameter()->setMinFilter(sampler2D->mMinFilter);
    sampler2D->mTexture->getTexParameter()->setMagFilter(sampler2D->mMagFilter);
  }
}
//-----------------------------------------------------------------------------
void DaeLoader::parseMaterials(daeElement* library)
{
  if (!library)
    return;
  domLibrary_materials* library_materials = static_cast<domLibrary_materials*>(library);
  const domMaterial_Array& materials = library_materials->getMaterial_array();
  for(size_t i=0; i<materials.getCount(); ++i)
  {
    domElement* effect = materials[i]->getInstance_effect()->getUrl().getElement();
    if (!effect)
    {
      VL_LOG_DEBUG << "- 'materials[i]->getInstance_effect()->getUrl().getElement()' FAILED: " << __FILE__ << ":" << __LINE__ << "\n";
      continue;
    }

    std::map< daeElementRef, ref<Dae::Effect> >::iterator it = mEffects.find(effect);
    if (it != mEffects.end())
    {
      domMaterial* material = materials[i].cast();
      ref<Dae::Material> dae_material = new Dae::Material;
      dae_material->mDaeEffect = it->second;
      mMaterials[ material ] = dae_material;
    }
    else
    {
      VL_LOG_DEBUG << "- 'mEffects.find(effect)' FAILED: " << __FILE__ << ":" << __LINE__ << "\n";
      continue;
    }
  }
}
//-----------------------------------------------------------------------------
ref<Light> DaeLoader::parseLight(domLight* dom_light, Transform* transform)
{
  domLight::domTechnique_commonRef light_common = dom_light->getTechnique_common();

  ref<Light> light = new Light;
  if (dom_light->getName())
    light->setObjectName( dom_light->getName() );
  else
  if (dom_light->getID())
    light->setObjectName( dom_light->getID() );

  light->bindTransform(transform);

  if (light_common->getPoint())
  {
    domLight::domTechnique_common::domPointRef point = light_common->getPoint();

    if (point->getColor())
    {
      domFloat3& c = point->getColor()->getValue();
      fvec4 color((float)c[0], (float)c[1], (float)c[2], 1);
      light->setAmbient( fvec4(0,0,0,1) );
      light->setDiffuse(color);
      light->setSpecular(color);
    }

    if (point->getConstant_attenuation())
      light->setConstantAttenuation( (float)point->getConstant_attenuation()->getValue() );
    if (point->getLinear_attenuation())
      light->setLinearAttenuation( (float)point->getLinear_attenuation()->getValue() );
    if (point->getQuadratic_attenuation())
      light->setQuadraticAttenuation( (float)point->getQuadratic_attenuation()->getValue() );

    light->setPosition( fvec4(0,0,0,1) );
  }
  else
  if (light_common->getDirectional())
  {
    domLight::domTechnique_common::domDirectionalRef directional = light_common->getDirectional();

    if (directional->getColor())
    {
      domFloat3& c = directional->getColor()->getValue();
      fvec4 color((float)c[0], (float)c[1], (float)c[2], 1);
      light->setAmbient( fvec4(0,0,0,1) );
      light->setDiffuse(color);
      light->setSpecular(color);
    }

    light->setPosition( fvec4( 0, 0, 1, 0) );
  }
  else
  if (light_common->getSpot())
  {
    domLight::domTechnique_common::domSpotRef spot= light_common->getSpot();

    if (spot->getColor())
    {
      domFloat3& c = spot->getColor()->getValue();
      fvec4 color((float)c[0], (float)c[1], (float)c[2], 1);
      light->setAmbient( fvec4(0,0,0,1) );
      light->setDiffuse(color);
      light->setSpecular(color);
    }

    if (spot->getConstant_attenuation())
      light->setConstantAttenuation( (float)spot->getConstant_attenuation()->getValue() );
    if (spot->getLinear_attenuation())
      light->setLinearAttenuation( (float)spot->getLinear_attenuation()->getValue() );
    if (spot->getQuadratic_attenuation())
      light->setQuadraticAttenuation( (float)spot->getQuadratic_attenuation()->getValue() );

    if (spot->getFalloff_angle())
      light->setSpotCutoff( (float)spot->getFalloff_angle()->getValue() );
    if (spot->getFalloff_exponent())
      light->setSpotExponent( (float)spot->getFalloff_exponent()->getValue() );

    light->setSpotDirection( fvec3(0,0,-1) );
    light->setPosition( fvec4( 0, 0, 0, 1) );
  }
  else
  if (light_common->getAmbient())
  {
    domLight::domTechnique_common::domAmbientRef ambient = light_common->getAmbient();

    if (ambient->getColor())
    {
      domFloat3& c = ambient->getColor()->getValue();
      fvec4 color((float)c[0], (float)c[1], (float)c[2], 1);
      light->setAmbient( color );
      light->setDiffuse( fvec4(0,0,0,1) );
      light->setSpecular( fvec4(0,0,0,1) );
    }

    // this is actually irrelevant since the diffuse and specular colors are zeroed.
    light->setPosition( fvec4( 0, 0, 0, 1) );

    // just for clarity, no attenuation
    light->setConstantAttenuation( 1 );
    light->setLinearAttenuation( 0 );
    light->setQuadraticAttenuation( 0 );
  }

  return light;
}
//-----------------------------------------------------------------------------
void DaeLoader::setupLights()
{
  // generate light meshes
  if (loadOptions()->lightMeshSize())
  {
    for(size_t i=0; i<mLights.size(); ++i)
    {
      // spot light
      if (mLights[i]->spotCutoff() != 180)
      {
        ref<Geometry> light_mesh = vl::makeCone( vec3(0,0,0), loadOptions()->lightMeshSize(), loadOptions()->lightMeshSize(), 10 );
        light_mesh->transform( mat4::getTranslation(0,loadOptions()->lightMeshSize(),0) );
        light_mesh->transform( mat4::getRotation(90, +1,0,0) );
        light_mesh->setObjectName( ("LightMesh-" + mLights[i]->objectName()).c_str() );
        ref<Effect> fx = new Effect;
        fx->shader()->enable(EN_DEPTH_TEST);
        fx->shader()->gocPolygonMode()->set(PM_LINE, PM_LINE);
        fx->shader()->gocColor()->setValue(vl::fuchsia);
        mResources->resources().push_back( new Actor( light_mesh.get(), fx.get(), mLights[i]->boundTransform() ) );
      }
      else
      // directional light
      if (mLights[i]->position().w() == 0)
      {
        ref<Geometry> light_mesh = vl::makePyramid( vec3(0,0,0), loadOptions()->lightMeshSize() / 2, loadOptions()->lightMeshSize() );
        light_mesh->transform( mat4::getRotation(90, -1,0,0) );
        light_mesh->setObjectName( ("LightMesh-" + mLights[i]->objectName()).c_str() );
        ref<Effect> fx = new Effect;
        fx->shader()->enable(EN_DEPTH_TEST);
        fx->shader()->gocPolygonMode()->set(PM_LINE, PM_LINE);
        fx->shader()->gocColor()->setValue(vl::fuchsia);
        mResources->resources().push_back( new Actor( light_mesh.get(), fx.get(), mLights[i]->boundTransform() ) );
      }
      else
      // point light
      if( mLights[i]->ambient() == fvec4(0,0,0,1) )
      {
        ref<Geometry> light_mesh = vl::makeUVSphere( vec3(0,0,0), loadOptions()->lightMeshSize(), 10, 5);
        light_mesh->setObjectName( ("LightMesh-" + mLights[i]->objectName()).c_str() );
        ref<Effect> fx = new Effect;
        fx->shader()->enable(EN_DEPTH_TEST);
        fx->shader()->gocPolygonMode()->set(PM_LINE, PM_LINE);
        fx->shader()->gocColor()->setValue(vl::fuchsia);
        mResources->resources().push_back( new Actor( light_mesh.get(), fx.get(), mLights[i]->boundTransform() ) );
      }
      else
      // ambient light
      {
        ref<Geometry> light_mesh = vl::makeTorus( vec3(0,0,0), loadOptions()->lightMeshSize(), loadOptions()->lightMeshSize()/4, 8, 14);
        light_mesh->setNormalArray(NULL); // remove normals
        light_mesh->setObjectName( ("LightMesh-" + mLights[i]->objectName()).c_str() );
        ref<Effect> fx = new Effect;
        fx->shader()->enable(EN_DEPTH_TEST);
        fx->shader()->gocPolygonMode()->set(PM_LINE, PM_LINE);
        fx->shader()->gocColor()->setValue(vl::fuchsia);
        mResources->resources().push_back( new Actor( light_mesh.get(), fx.get(), mLights[i]->boundTransform() ) );
      }
    }
  }

  struct dummy
  {
    static bool light_sorter(const ref<Light>& a, const ref<Light>& b)
    {
      // ambient lights first
      if (a->ambient() != b->ambient())
        return b->ambient() < a->ambient();
      else
      // directional lights first
      if (a->position().w() != b->position().w())
        return a->position().w() < b->position().w();
      else
      // point lights first, spotlights last
        return a->spotCutoff() > b->spotCutoff();
    }
  };

  std::sort(mLights.begin(), mLights.end(), dummy::light_sorter);

  // set light indices and adds to the resource database
  for(size_t i=0; i<mLights.size(); ++i)
  {
    mResources->resources().push_back( mLights[i].get() );
  }

  if (loadOptions()->exportLights() == false)
    mLights.clear();

  // default light if no lights were present in the scene or exportLights() == false
  if (mLights.empty())
  {
    mLights.push_back( new Light );
    mLights[0]->setObjectName(VL_DEFAULT_LIGHT);
  }
}
//-----------------------------------------------------------------------------
Dae::EInputSemantic DaeLoader::getSemantic(const char* semantic)
{
  for(int i=0; SemanticTable[i].mSemanticString; ++i)
  {
    if (strcmp(semantic, SemanticTable[i].mSemanticString) == 0)
      return SemanticTable[i].mSemantic;
  }

  return Dae::IS_UNKNOWN;
}
//-----------------------------------------------------------------------------
const char* DaeLoader::getSemanticString(Dae::EInputSemantic semantic)
{
  for(int i=0; SemanticTable[i].mSemanticString; ++i)
  {
    if ( semantic == SemanticTable[i].mSemantic )
      return SemanticTable[i].mSemanticString;
  }

  return NULL;
}
//-----------------------------------------------------------------------------
ETexParamFilter DaeLoader::translateSampleFilter(domFx_sampler_filter_common filter)
{
  switch(filter)
  {
	  case FX_SAMPLER_FILTER_COMMON_NEAREST:                return TPF_NEAREST;
	  case FX_SAMPLER_FILTER_COMMON_LINEAR:                 return TPF_LINEAR;
	  case FX_SAMPLER_FILTER_COMMON_NEAREST_MIPMAP_NEAREST: return TPF_NEAREST_MIPMAP_NEAREST;
	  case FX_SAMPLER_FILTER_COMMON_LINEAR_MIPMAP_NEAREST:  return TPF_LINEAR_MIPMAP_NEAREST;
	  case FX_SAMPLER_FILTER_COMMON_NEAREST_MIPMAP_LINEAR:  return TPF_NEAREST_MIPMAP_LINEAR;
	  case FX_SAMPLER_FILTER_COMMON_LINEAR_MIPMAP_LINEAR:   return TPF_LINEAR_MIPMAP_LINEAR;
	  default:                                              return (ETexParamFilter)0;
  }
}
//-----------------------------------------------------------------------------
ETexParamWrap DaeLoader::translateWrapMode(domFx_sampler_wrap_common wrap)
{
  switch(wrap)
  {
	  case FX_SAMPLER_WRAP_COMMON_WRAP:   return TPW_REPEAT;
	  case FX_SAMPLER_WRAP_COMMON_MIRROR: return TPW_MIRRORED_REPEAT;
	  case FX_SAMPLER_WRAP_COMMON_CLAMP:  return TPW_CLAMP;
	  case FX_SAMPLER_WRAP_COMMON_BORDER: return TPW_CLAMP_TO_BORDER;
	  default:                            return (ETexParamWrap)0;
  }
}
//-----------------------------------------------------------------------------
template<class T_color_or_texture>
void DaeLoader::parseColor(const domProfile_COMMON* common, const T_color_or_texture& color_or_texture, Dae::ColorOrTexture* out_col)
{
  if (!color_or_texture)
    return;

  if (color_or_texture->getColor())
  {
    domFx_color_common& col = color_or_texture->getColor()->getValue();
    out_col->mColor = fvec4( (float)col[0], (float)col[1], (float)col[2], (float)col[3] );
  }

  if (color_or_texture->getTexture())
  {
    // <texture texture="...">
    daeSIDResolver sid_res( const_cast<domProfile_COMMON*>(common), color_or_texture->getTexture()->getTexture() );
    domElement* sampler2D_newparam = sid_res.getElement();

    std::map< daeElementRef, ref<Dae::NewParam> >::iterator it = mDaeNewParams.find(sampler2D_newparam);
    if ( it != mDaeNewParams.end() )
    {
      VL_CHECK(it->second->mDaeSampler2D)
      out_col->mSampler = it->second->mDaeSampler2D;
      if ( it->second->mDaeSampler2D.get() == NULL)
      {
        VL_LOG_DEBUG << "- LoadWriterDae: malformed file: <texture texture=..> points to a <newparam> that does not contain <sampler2D>!\n";
      }
    }
    else
    {
      std::map< daeElementRef, ref<Image> >::iterator it = mImages.find(sampler2D_newparam);
      if ( it != mImages.end() )
      {
        // create dummy sampler
        out_col->mSampler = new Dae::Sampler2D;
        out_col->mSampler->mDaeSurface = new Dae::Surface;
        out_col->mSampler->mDaeSurface->mImage = it->second;
        prepareTexture2D( out_col->mSampler.get() );
        VL_LOG_DEBUG << "- LoadWriterDae: malformed file: <texture texture=..> parameter points to an <image> instead of a <sampler2D>!\n"
                          "VL will create a dummy sampler with the specified image.\n";
      }
      else
      {
        VL_LOG_DEBUG << "- LoadWriterDae: malformed file: <texture texture=..> could not be resolved to anything!\n";
      }
    }

    // <texture texcoord="...">
    out_col->mTexCoord = color_or_texture->getTexture()->getTexcoord();
  }
}
//-----------------------------------------------------------------------------
void DaeLoader::generateGeometry(Dae::Primitive* prim, const char* name)
{
  VL_CHECK(prim->mIndexStride);

  prim->mGeometry = new Geometry;
  if (name)
    prim->mGeometry->setObjectName(name);

  // no primitives where specified so we treat it as a cloud of points simulating a single increasing <p>
  if(prim->mP.size() == 0 && prim->mType == Dae::PT_UNKNOWN && prim->mChannels.size())
  {
    // some sanity checks
    for(size_t i=0; i<prim->mChannels.size(); ++i)
    {
      if ( prim->mChannels[i]->mSource->count() != prim->mChannels[0]->mSource->count() )
      {
        VL_LOG_DEBUG << "- LoadWriterDae: cannot generate point cloud: channels have different sizes!\n";
        return;
      }
      if ( prim->mChannels[i]->mOffset != 0 )
      {
        VL_LOG_DEBUG << "- LoadWriterDae: cannot generate point cloud: channels must have offset == 0!\n";
        return;
      }
    }

    // generate dummy <p>
    prim->mP.resize(1);
    prim->mP[0] = static_cast<domP*>(domP::create(mDAE).cast());
    prim->mP[0]->getValue().setCount( prim->mChannels[0]->mSource->count() );
    for (size_t i=0; i<prim->mChannels[0]->mSource->count(); ++i)
      prim->mP[0]->getValue()[i] = i;
  }

  size_t total_index_count = 0;
  for(size_t ip=0; ip<prim->mP.size(); ++ip)
    total_index_count += prim->mP[ip]->getValue().getCount();

  ref<ArrayUInt1> index_buffer = new ArrayUInt1;
  index_buffer->resize( total_index_count / prim->mIndexStride );

  std::vector<GLint> vcount;

  // generate index buffer for DrawElements or MultiDrawElements.
  std::set<Dae::Vert> vert_set;
  for(size_t ip=0, iidx=0; ip<prim->mP.size(); ++ip)
  {
    const domListOfUInts& p = prim->mP[ip]->getValue();

    vcount.push_back( p.getCount() / prim->mIndexStride );

    for(size_t ivert=0; ivert<p.getCount(); ivert+=prim->mIndexStride, ++iidx)
    {
      Dae::Vert vert;

      // fill vertex info
      for(size_t ichannel=0; ichannel<prim->mChannels.size(); ++ichannel)
        vert.mAttribIndex[ichannel] = (size_t)p[ivert + prim->mChannels[ichannel]->mOffset];

      size_t final_index = 0xFFFFFFFF;
      // retrieve/insert the vertex
      std::set<Dae::Vert>::iterator it = vert_set.find(vert);
      if (it == vert_set.end())
      {
        vert.mIndex = final_index = vert_set.size();
        vert_set.insert(vert);
      }
      else
        final_index = it->mIndex;
        
      // this is the actual index
      (*index_buffer)[iidx] = final_index;
    }
  }

  if (vcount.size() == 1)
  {
    // use DrawElements
    ref<DrawElementsUInt> de;
    switch(prim->mType)
    {
      case Dae::PT_UNKNOWN:    de = new DrawElementsUInt( PT_POINTS ); break; // of course we can do better than this but we would need a lot of extra logic
      case Dae::PT_LINES:      de = new DrawElementsUInt( PT_LINES ); break;
      case Dae::PT_LINE_STRIP: de = new DrawElementsUInt( PT_LINE_STRIP ); break;
      case Dae::PT_POLYGONS:   de = new DrawElementsUInt( PT_POLYGON ); break;
      case Dae::PT_TRIFANS:    de = new DrawElementsUInt( PT_TRIANGLE_FAN ); break;
      case Dae::PT_TRIANGLES:  de = new DrawElementsUInt( PT_TRIANGLES ); break;
      case Dae::PT_TRISTRIPS:  de = new DrawElementsUInt( PT_TRIANGLE_STRIP ); break;
      default:
        VL_TRAP()
    }

    de->setIndexBuffer( index_buffer.get() );
    prim->mGeometry->drawCalls()->push_back( de.get() );
  }
  else
  {
    // use MultiDrawElements
    ref<MultiDrawElementsUInt> mde;
    switch(prim->mType)
    {
      case Dae::PT_UNKNOWN:    mde = new MultiDrawElementsUInt( PT_POINTS ); break; // of course we can do better than this but we would need a lot of extra logic
      case Dae::PT_LINES:      mde = new MultiDrawElementsUInt( PT_LINES ); break;
      case Dae::PT_LINE_STRIP: mde = new MultiDrawElementsUInt( PT_LINE_STRIP ); break;
      case Dae::PT_POLYGONS:   mde = new MultiDrawElementsUInt( PT_POLYGON ); break;
      case Dae::PT_TRIFANS:    mde = new MultiDrawElementsUInt( PT_TRIANGLE_FAN ); break;
      case Dae::PT_TRIANGLES:  mde = new MultiDrawElementsUInt( PT_TRIANGLES ); break;
      case Dae::PT_TRISTRIPS:  mde = new MultiDrawElementsUInt( PT_TRIANGLE_STRIP ); break;
      default:
        VL_TRAP()
    }

    mde->setIndexBuffer( index_buffer.get() );
    mde->setCountVector( vcount );
    prim->mGeometry->drawCalls()->push_back( mde.get() );
  }

  // generate new vertex attrib info and install data
  size_t tex_unit = 0;
  for( size_t ich=0; ich<prim->mChannels.size(); ++ich )
  {
    // init data storage for this channel
    ref<ArrayAbstract> vert_attrib;
    float* ptr = NULL;
    float* ptr_end = NULL;
    switch(prim->mChannels[ich]->mSource->dataSize())
    {
      case 1:
      {
        ref<ArrayFloat1> array_f1 = new ArrayFloat1;
        vert_attrib = array_f1;
        array_f1->resize( vert_set.size() );
        ptr = array_f1->begin();
        // debug
        ptr_end = ptr + vert_set.size() * 1;
        break;
      }
      case 2:
      {
        ref<ArrayFloat2> array_f2 = new ArrayFloat2;
        vert_attrib = array_f2;
        array_f2->resize( vert_set.size() );
        ptr = array_f2->at(0).ptr();
        // debug
        ptr_end = ptr + vert_set.size() * 2;
        break;
      }
      case 3:
      {
        ref<ArrayFloat3> array_f3 = new ArrayFloat3;
        vert_attrib = array_f3;
        array_f3->resize( vert_set.size() );
        ptr = array_f3->at(0).ptr();
        // debug
        ptr_end = ptr + vert_set.size() * 3;
        break;
      }
      case 4:
      {
        ref<ArrayFloat4> array_f4 = new ArrayFloat4;
        vert_attrib = array_f4;
        array_f4->resize( vert_set.size() );
        ptr = array_f4->at(0).ptr();
        // debug
        ptr_end = ptr + vert_set.size() * 4;
        break;
      }
      default:
        Log::warning( Say("LoadWriterDae: input '%s' skipped because parameter count is more than 4.\n") << getSemanticString(prim->mChannels[ich]->mSemantic) );
        continue;
    }

    // install vertex attribute
    switch(prim->mChannels[ich]->mSemantic)
    {
    case Dae::IS_POSITION: prim->mGeometry->setVertexArray( vert_attrib.get() ); break;
    case Dae::IS_NORMAL:   prim->mGeometry->setNormalArray( vert_attrib.get() ); break;
    case Dae::IS_COLOR:    prim->mGeometry->setColorArray( vert_attrib.get() ); break;
    case Dae::IS_TEXCOORD: prim->mGeometry->setTexCoordArray( tex_unit++, vert_attrib.get() ); break;
    default:
      VL_LOG_DEBUG << ( Say("- LoadWriterDae: input semantic '%s' not supported.\n") << getSemanticString(prim->mChannels[ich]->mSemantic) );
      continue;
    }

    // name it as TEXCOORD@SET0 etc. to be recognized when binding (not used yet)
    vert_attrib->setObjectName( String(Say("%s@SET%n") << getSemanticString(prim->mChannels[ich]->mSemantic) << prim->mChannels[ich]->mSet).toStdString().c_str() );

    // fill the vertex attribute array
    for(std::set<Dae::Vert>::iterator it = vert_set.begin(); it != vert_set.end(); ++it)
    {
      const Dae::Vert& vert = *it;
      size_t idx = vert.mAttribIndex[ich];
      VL_CHECK(ptr + prim->mChannels[ich]->mSource->dataSize()*vert.mIndex < ptr_end);
      prim->mChannels[ich]->mSource->readData(idx, ptr +prim-> mChannels[ich]->mSource->dataSize()*vert.mIndex);
    }
  }

  // --- fix bad normals ---
  if ( loadOptions()->fixBadNormals() && prim->mGeometry->normalArray() )
  {
    ref<ArrayFloat3> norm_old = vl::cast<ArrayFloat3>(prim->mGeometry->normalArray());
    VL_CHECK(norm_old);

    // recompute normals
    prim->mGeometry->computeNormals();
    ref<ArrayFloat3> norm_new = vl::cast<ArrayFloat3>(prim->mGeometry->normalArray());
    VL_CHECK(norm_new);

    size_t flipped = 0;
    size_t degenerate = 0;
    for(size_t i=0; i<norm_new->size(); ++i)
    {
      // compare VL normals with original ones
      float l = norm_old->at(i).length();
      if ( l < 0.5f ) 
      {
        norm_old->at(i) = norm_new->at(i);
        ++degenerate;
      }

      if ( l < 0.9f || l > 1.1f ) 
      {
        norm_old->at(i).normalize();
        ++degenerate;
      }

      if ( dot(norm_new->at(i), norm_old->at(i)) < -0.1f )
      {
        norm_old->at(i) = -norm_old->at(i);
        ++flipped;
      }
    }

    // mic fixme: issue these things as debug once things got stable
    if (degenerate || flipped) 
      VL_LOG_DEBUG << ( Say("- LoadWriterDae: fixed bad normals in \"%s\": degenerate=%n, flipped=%n (out of %n).\n")  << prim->mGeometry->objectName() << degenerate << flipped << norm_old->size() );

    // reinstall fixed normals
    prim->mGeometry->setNormalArray(norm_old.get());
  }

   // disabled: we transform the root matrix instead
   // --- orient geometry based on up vector ---
   // prim->mGeometry->transform((mat4)mUpMatrix);
}
//-----------------------------------------------------------------------------
void DaeLoader::parseAsset(domElement* root)
{
  domElement* asset_el = root->getChild("asset");
  if (asset_el)
  {
    domAsset* asset = static_cast<domAsset*>(asset_el);

    // up vector
    if (asset->getUp_axis())
    {
      if( asset->getUp_axis()->getValue() == UPAXISTYPE_X_UP )
      {
        // X_UP Negative y Positive x Positive z
        mUpMatrix.setX( vec3( 0, 1, 0) );
        mUpMatrix.setY( vec3(-1, 0, 0) );
        mUpMatrix.setZ( vec3( 0, 0, 1) );
      }
      else
      if( asset->getUp_axis()->getValue() == UPAXISTYPE_Z_UP )
      {
        // Z_UP Positive x Positive z Negative y
        mUpMatrix.setX( vec3(1, 0, 0) );
        mUpMatrix.setY( vec3(0, 0,-1) );
        mUpMatrix.setZ( vec3(0, 1, 0) );
      }
    }

    // try to fix the transparency written by crappy tools
    mInvertTransparency = false;
    mAssumeOpaque = false;
    if (loadOptions()->invertTransparency() == LoadWriterDae::LoadOptions::TransparencyInvert)
      mInvertTransparency = true;
    else
    if (loadOptions()->invertTransparency() == LoadWriterDae::LoadOptions::TransparencyAuto)
    {
      for(size_t i=0; i<asset->getContributor_array().getCount(); ++i)
      {
        const char* tool = asset->getContributor_array()[i]->getAuthoring_tool()->getValue();
        
        if (!tool)
          continue;

        VL_LOG_DEBUG << "- Authoring tool = " << tool << "\n";

        // Google SketchUp before 7.1 requires <transparency> inversion.
        // see http://www.collada.org/public_forum/viewtopic.php?f=12&t=1667
        const char* google_str = strstr(tool, "Google SketchUp");
        size_t google_str_len = strlen("Google SketchUp");
        if ( google_str )
        {
          if ( strlen(google_str) > google_str_len )
          {
            float version = 1000;
            if ( sscanf( google_str + google_str_len, "%f", &version) )
            {
              version = version * 100 + 0.5f;
              if (version < 710)
                mInvertTransparency = true;
            }
            else
            {
              // don't trust Google SketchUp if we cannot read the version
              mAssumeOpaque = true;
            }
          }
          else
          {
            // don't trust Google SketchUp if we cannot read the version
            mAssumeOpaque = true;
          }
          break;
        }

        // See https://collada.org/mediawiki/index.php/ColladaMaya#ColladaMaya_3.03
        // - "Data exported with previous versions of our COLLADA tools may import with inverted transparency in ColladaMax 3.03 and ColladaMaya 3.03."
        // - Actually ColladaMax/ColladaMaya before 3.03 use unpredictable combinations of <transparent> and <transparency>, so... we assume opaque.

        const char* colladamaya_str = strstr(tool, "ColladaMaya v");
        size_t colladamaya_str_len = strlen("ColladaMaya v");
        if ( colladamaya_str )
        {
          float version = 1000;
          if ( strlen(colladamaya_str) > colladamaya_str_len  )
          {
            if ( sscanf( colladamaya_str + colladamaya_str_len, "%f", &version) )
            {
              version = version * 100 + 0.5f;
              if (version < 303)
                mAssumeOpaque = true;
            }
            else
            {
              // don't trust ColladaMaya if we cannot read the version
              mAssumeOpaque = true;
            }
          }
          else
          {
            // don't trust ColladaMaya if we cannot read the version
            mAssumeOpaque = true;
          }
        }

        const char* colladamax_str = strstr(tool, "ColladaMax v");
        size_t colladamax_str_len = strlen("ColladaMax v");
        if ( colladamax_str )
        {
          float version = 1000;
          if ( strlen(colladamax_str) > colladamax_str_len )
          {
            if ( sscanf( colladamax_str + colladamax_str_len, "%f", &version) )
            {
              version = version * 100 + 0.5f;
              if (version < 303)
                mAssumeOpaque = true;
            }
            else
            {
              // don't trust ColladaMax if we cannot read the version
              mAssumeOpaque = true;
            }
          }
          else
          {
            // don't trust ColladaMax if we cannot read the version
            mAssumeOpaque = true;
          }
        }

        // MeshLab seem to flip the transparency also now.
        if ( strstr(tool, "VCGLib | MeshLab") )
        {
          mInvertTransparency = true;
        }

        VL_LOG_DEBUG << "- Invert transparency = " << (mInvertTransparency ? "yes" : "no.") << "\n";
        VL_LOG_DEBUG << "- Assume opaque = " << (mAssumeOpaque? "yes" : "no.") << "\n";

        // stop at the first contributor
        break;
      }
    }
  }
}
