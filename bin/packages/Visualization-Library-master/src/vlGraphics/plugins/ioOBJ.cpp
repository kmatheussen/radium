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

#include "ioOBJ.hpp"
#include <vlCore/TextStream.hpp>
#include <vlCore/VirtualFile.hpp>
#include <vlCore/VirtualDirectory.hpp>
#include <vlCore/VisualizationLibrary.hpp>
#include <vlCore/FileSystem.hpp>
#include <vlGraphics/DoubleVertexRemover.hpp>
#include <vlCore/LoadWriterManager.hpp>
#include <vlGraphics/Effect.hpp>
#include <vlGraphics/Actor.hpp>
#include <string>
#include <vector>
#include <stdio.h>

using namespace vl;

namespace 
{
  /*
   * Utility function
   */
  template <class T>
  void append(std::vector<T>& vec, const T& data, const int& alloc_step = 1024*10)
  {
    if (vec.size() == vec.capacity())
      vec.reserve( vec.size() + alloc_step );
    vec.push_back(data);
  }
}
//-----------------------------------------------------------------------------
// ObjTexture
//-----------------------------------------------------------------------------
ObjTexture::ObjTexture()
{
  mBlendU   = false;
  mBlendV   = false;
  mCC       = false;
  mClamp    = false;
  mMM_Base  = 0.0f;
  mMM_Gain  = 0.0f;
  mO_UVW[0] = 0.0f; mO_UVW[1] = 0.0f; mO_UVW[2] = 0.0f;
  mS_UVW[0] = 0.0f; mS_UVW[1] = 0.0f; mS_UVW[2] = 0.0f;
  mT_UVW[0] = 0.0f; mT_UVW[1] = 0.0f; mT_UVW[2] = 0.0f;
  mTexres_Value = 0.0f;
  mImfchan  = 0;
  mBM       = 0.0f;
}
//-----------------------------------------------------------------------------
const ObjTexture& ObjTexture::parseLine(const String& line, const String& file)
{
  std::vector<String> tokens;
  line.split(" \t",tokens,true);

  if (!tokens.empty())
  {
    tokens.erase(tokens.begin());
    mFileName = tokens.back();
  }
  for(int i=0; i<(int)tokens.size(); ++i)
  {
    if (tokens[i] == "-blendu")
    {
      mBlendU = tokens[i+1] == "on";
      ++i;
    }
    else
    if (tokens[i] == "-blendv")
    {
      mBlendV = tokens[i+1] == "on";
      ++i;
    }
    else
    if (tokens[i] == "-cc")
    {
      mCC = tokens[i+1] == "on";
      ++i;
    }
    else
    if (tokens[i] == "-clamp")
    {
      mClamp = tokens[i+1] == "on";
      ++i;
    }
    else
    if (tokens[i] == "-mm")
    {
      mMM_Base = (float)tokens[i+1].toDouble();
      mMM_Gain = (float)tokens[i+2].toDouble();
      i+=2;
    }
    else
    if (tokens[i] == "-o")
    {
      mO_UVW[0] = (float)tokens[i+1].toDouble();
      mO_UVW[1] = (float)tokens[i+2].toDouble();
      mO_UVW[2] = (float)tokens[i+3].toDouble();
      i+=3;
    }
    else
    if (tokens[i] == "-s")
    {
      mS_UVW[0] = (float)tokens[i+1].toDouble();
      mS_UVW[1] = (float)tokens[i+2].toDouble();
      mS_UVW[2] = (float)tokens[i+3].toDouble();
      i+=3;
    }
    else
    if (tokens[i] == "-t")
    {
      mT_UVW[0] = (float)tokens[i+1].toDouble();
      mT_UVW[1] = (float)tokens[i+2].toDouble();
      mT_UVW[2] = (float)tokens[i+3].toDouble();
      i+=3;
    }
    else
    if (tokens[i] == "-texres")
    {
      mTexres_Value = (float)tokens[i+1].toDouble();
      ++i;
    }
    else
    if (tokens[i] == "-imfchan")
    {
      mImfchan = (unsigned char)tokens[i+1][0];
      ++i;
    }
    else
    if (tokens[i] == "-bm")
    {
      mBM = (float)tokens[i+1].toDouble();
      ++i;
    }
    else
    {
      if ( i != (int)tokens.size()-1 )
        Log::error( Say("Unknown map option '%s' in file '%s'.\n") << tokens[i] << file );
    }
  }
  return *this;
}
//-----------------------------------------------------------------------------
void ObjTexture::print()
{
  Log::print(
    Say("-blendu %s -blendv %s -cc %s -clamp %s -mm %n %n -o %n %n %n -s %n %n %n -t %n %n %n -texres %n -imfchan '%c' -bm %n %s\n")
    << (mBlendU ? "on" : "off")
    << (mBlendV ? "on" : "off")
    << (mCC     ? "on" : "off")
    << (mClamp  ? "on" : "off")
    << mMM_Base << mMM_Gain
    << mO_UVW[0] << mO_UVW[1] << mO_UVW[2]
    << mS_UVW[0] << mS_UVW[1] << mS_UVW[2]
    << mT_UVW[0] << mT_UVW[1] << mT_UVW[2]
    << mTexres_Value
    << mImfchan
    << mBM
    << mFileName.toStdString().c_str()
    );
}
//-----------------------------------------------------------------------------
// loadObjMaterials
//-----------------------------------------------------------------------------
void ObjLoader::loadObjMaterials(VirtualFile* input, std::vector<ObjMaterial>& materials )
{
  ref<TextStream> line_reader = new TextStream(input);

  String line;
  String file = input->path();
  while(line_reader->readLine(line))
  {
    line = line.trim();
    if (line.empty() || line[0] == '#')
      continue;
    else
    {
      if (line.startsWith("newmtl"))
      {
        materials.push_back(ObjMaterial());
        materials.back().setObjectName( line.field(' ', 1).toStdString().c_str() );
      }
      else
      if (line.startsWith("Ns"))
      {
        materials.back().setNs (line.field(' ', 1).toFloat());
      }
      else
      if (line.startsWith("Ka"))
      {
        fvec3 col;
        col.r() = (float)line.field(' ', 1).toDouble();
        col.g() = (float)line.field(' ', 2).toDouble();
        col.b() = (float)line.field(' ', 3).toDouble();
        materials.back().setKa(col);
      }
      else
      if (line.startsWith("Kd"))
      {
        fvec3 col;
        col.r() = (float)line.field(' ', 1).toDouble();
        col.g() = (float)line.field(' ', 2).toDouble();
        col.b() = (float)line.field(' ', 3).toDouble();
        materials.back().setKd(col);
      }
      else
      if (line.startsWith("Ks"))
      {
        fvec3 col;
        col.r() = (float)line.field(' ', 1).toDouble();
        col.g() = (float)line.field(' ', 2).toDouble();
        col.b() = (float)line.field(' ', 3).toDouble();
        materials.back().setKs(col);
      }
      else
      if (line.startsWith("Ke"))
      {
        fvec3 col;
        col.r() = (float)line.field(' ', 1).toDouble();
        col.g() = (float)line.field(' ', 2).toDouble();
        col.b() = (float)line.field(' ', 3).toDouble();
        materials.back().setKe(col);
      }
      else
      if (line.startsWith("Tf"))
      {
        // skip transmission filter
      }
      else
      if (line.startsWith("Ni"))
      {
        materials.back().setNi(line.field(' ', 1).toFloat());
      }
      else
      if (line.startsWith("d") || line.startsWith("Tr"))
      {
        materials.back().setTr(line.field(' ', 1).toFloat());
      }
      else
      if (line.startsWith("illum"))
      {
        materials.back().setIllum(line.field(' ', 1).toInt());
      }
      else
      if (line.startsWith("map_Kd"))
        materials.back().setMap_Kd(ObjTexture().parseLine(line,file));
      else
      if (line.startsWith("map_Ks"))
        materials.back().setMap_Ks(ObjTexture().parseLine(line,file));
      else
      if (line.startsWith("map_Ka"))
        materials.back().setMap_Ka(ObjTexture().parseLine(line,file));
      else
      if (line.startsWith("map_Ns"))
        materials.back().setMap_Ns(ObjTexture().parseLine(line,file));
      else
      if (line.startsWith("map_d"))
        materials.back().setMap_d(ObjTexture().parseLine(line,file));
      else
      if (line.startsWith("decal"))
        materials.back().setMap_Decal(ObjTexture().parseLine(line,file));
      else
      if (line.startsWith("disp"))
        materials.back().setMap_Disp(ObjTexture().parseLine(line,file));
      else
      if (line.startsWith("bump") || line.startsWith("map_bump"))
        materials.back().setMap_Bump(ObjTexture().parseLine(line,file));
      else
        Log::error( Say("Unknown field '%s' in file %s'.\n") << line << file );
    }
  }
}
//-----------------------------------------------------------------------------
ref<ResourceDatabase> ObjLoader::loadOBJ( VirtualFile* file )
{
  if (!file)
  {
    Log::error("loadOBJ() called with NULL argument.\n");
    return NULL;
  }
  ref<TextStream> stream = new TextStream(file);
  if ( !stream->inputFile()->open(OM_ReadOnly) )
  {
    Log::error( Say("loadOBJ(): could not open source file.\n") );
    return NULL;
  }

  mCoords.clear();
  // std::vector<float> mNormals;
  // std::vector<float> mTexCoords;
  std::map< std::string, ref<ObjMaterial> > mMaterials;
  std::vector< ref<ObjMesh> > mMeshes;

  ref<ObjMaterial> cur_material;
  ref<ObjMesh> cur_mesh;

  const int BUF_SIZE = 1024;
  char cmd[BUF_SIZE];

  int line_count = 0;
  int f_format_type = 0;
  std::string object_name;
  bool starts_new_geom = true;
  #if 0
    bool smoothing_group = false;
  #endif

  std::string stdstr_line;
  while( stream->readLine(stdstr_line) )
  {
    ++line_count;
    std::string line = String::trimStdString(stdstr_line);
    if (line.empty() || line[0] == '#')
      continue;

    // note: comments cannot be multiline
    while( line[line.length()-1] == '\\' && stream->readLine(stdstr_line) )
    {
      ++line_count;
      // remove "\"
      line[line.length()-1] = ' ';
      // remove spaces before \ and insert a single ' ' space
      line = String::trimStdString(line) + ' ';
      // appends new line
      line += String::trimStdString(stdstr_line);
    }

    cmd[0] = 0;
    sscanf(line.c_str(), "%s ", cmd);
    if ( !cmd[0] )
      continue;
// ----------------------------------------------------------------------------
    // Vertex data:
    if (strcmp(cmd,"v") == 0) // Geometric vertices
    {
      float x=0,y=0,z=0,w=1.0f;
      sscanf(line.c_str()+2,"%f %f %f", &x, &y, &z);
      append(mCoords,fvec4(x,y,z,w));
      /*append(mCoords,x);
      append(mCoords,y);
      append(mCoords,z);
      append(mCoords,w);*/
    }
    else
    if (strcmp(cmd,"vt") == 0) // Texture vertices
    {
      // note, this might have less than 3 mCoords
      float x=0,y=0,z=0;
      sscanf(line.c_str()+3,"%f %f %f", &x, &y, &z);
      append(mTexCoords,fvec3(x,y,z));
      /*append(mTexCoords,x);
      append(mTexCoords,y);
      append(mTexCoords,z);*/
    }
    else
    if (strcmp(cmd,"vn") == 0) // Vertex mNormals
    {
      float x=0,y=0,z=0;
      sscanf(line.c_str()+3,"%f %f %f", &x, &y, &z);
      append(mNormals,fvec3(x,y,z));
      /*append(mNormals,x);
      append(mNormals,y);
      append(mNormals,z);*/
    }
    else
    /*if (strcmp(cmd,"vp") == 0) // Parameter space vertices
    {
    }
    else*/
// ----------------------------------------------------------------------------
    // Elements:
    if (strcmp(cmd,"f") == 0) // Face
    {
      // starts new geometry if necessary
      if (starts_new_geom)
      {
        cur_mesh = new ObjMesh;
        cur_mesh->setObjectName(object_name.c_str());
        mMeshes.push_back( cur_mesh );
        starts_new_geom = false;
        cur_mesh->setMaterial(cur_material.get());

        // detect vertex format

        int i=1;
        while( line[i] == ' ' || line[i] == '\t' ) ++i;
        int slash1 = 0;
        int slash2 = 0;
        while( i < (int)line.size() && line[i] != ' ' && line[i] != '\t' ) 
        {
          if (line[i] == '/')
          {
            if (!slash1)
              slash1 = i;
            else
            if (!slash2)
            {
              slash2 = i;
              break;
            }
          }
          ++i;
        }
        if (!slash1 && !slash2)
          f_format_type = 0;
        else
        if (slash1 && !slash2)
          f_format_type = 1;
        else
        {
          VL_CHECK(slash1)
          VL_CHECK(slash2)
          if (slash2 == slash1+1)
            f_format_type = 2;
          else
            f_format_type = 3;
        }
      }

      int face_type = 0;
      // divide into tokens
      for(size_t i=0; i < line.size(); ++i)
      {
        if (line[i] == ' ')
        {
          ++face_type;

          // eat all the spaces
          while( line[i] == ' ' ) 
            ++i;

          if (line[i] == 0)
            break;

          int iv=-1,ivt=-1,ivn=-1;
          // 0 = f v       v       v
          // 1 = f v/vt    v/vt    v/vt
          // 2 = f v//vn   v//vn   v//vn
          // 3 = f v/vt/vn v/vt/vn v/vt/vn
          switch(f_format_type)
          {
          case 0:
            sscanf(line.c_str()+i, "%d", &iv); 
            if (iv>0)  --iv; else  iv  = (int)mCoords.size()    - iv;
            append(cur_mesh->facePositionIndex(), iv);
            break;
          case 1:
            sscanf(line.c_str()+i, "%d/%d", &iv,&ivt); 
            if (iv>0)  --iv; else  iv  = (int)mCoords.size()    - iv;
            if (ivt>0) --ivt; else ivt = (int)mTexCoords.size() - ivt;
            append(cur_mesh->facePositionIndex(), iv);
            append(cur_mesh->faceTexCoordIndex(), ivt);
            break;
          case 2:
            sscanf(line.c_str()+i, "%d//%d", &iv,&ivn); 
            if (iv>0)  --iv; else  iv  = (int)mCoords.size()    - iv;
            if (ivn>0) --ivn; else ivn = (int)mNormals.size()   - ivn;
            append(cur_mesh->facePositionIndex(), iv);
            append(cur_mesh->faceNormalIndex(), ivn);
            break;
          case 3:
            sscanf(line.c_str()+i, "%d/%d/%d", &iv,&ivt,&ivn); 
            if (iv>0)  --iv; else  iv  = (int)mCoords.size()    - iv;
            if (ivt>0) --ivt; else ivt = (int)mTexCoords.size() - ivt;
            if (ivn>0) --ivn; else ivn = (int)mNormals.size()   - ivn;
            append(cur_mesh->facePositionIndex(), iv);
            append(cur_mesh->faceTexCoordIndex(), ivt);
            append(cur_mesh->faceNormalIndex(), ivn);
            break;
          default:
            break;
          }
        }
      }
      VL_CHECK(face_type > 2)
      // track the face type in order to triangulate it later
      append(cur_mesh->face_type(), face_type);
    }
    else
    /*if (strcmp(cmd,"p") == 0) // Point 
    {
    }
    else
    if (strcmp(cmd,"l") == 0) // Line
    {
    }
    else
    if (strcmp(cmd,"curv") == 0) // Curve
    {
    }
    else
    if (strcmp(cmd,"curv2") == 0) // 2D Curve
    {
    }
    else
    if (strcmp(cmd,"surf") == 0) // Surface
    {
    }
    else
// ----------------------------------------------------------------------------
    // Free-form curve/surface attributes:
    if (strcmp(cmd,"deg") == 0) // Degree
    {
    }
    else
    if (strcmp(cmd,"bmat") == 0) // Basis matrix
    {
    }
    else
    if (strcmp(cmd,"step") == 0) // Step size
    {
    }
    else
    if (strcmp(cmd,"cstype") == 0) // Curve or surface type
    {
    }
    else
// ----------------------------------------------------------------------------
    // Free-form curve/surface body statements:
    if (strcmp(cmd,"parm") == 0) // Parameter values
    {
    }
    else
    if (strcmp(cmd,"trim") == 0) // Outer trimming loop
    {
    }
    else
    if (strcmp(cmd,"hole") == 0) // Inner trimming loop
    {
    }
    else
    if (strcmp(cmd,"scrv") == 0) // Special curve
    {
    }
    else
    if (strcmp(cmd,"sp") == 0) // Special point
    {
    }
    else
    if (strcmp(cmd,"end") == 0) // End statement
    {
    }
    else
// ----------------------------------------------------------------------------
    // Connectivity between free-form surfaces:
    if (strcmp(cmd,"con") == 0) // Connect
    {
    }
    else
    // Grouping:
    if (strcmp(cmd,"g") == 0) // Group name
    {
    }
    else*/
    if (strcmp(cmd,"s") == 0) // Smoothing group
    {
      // not used
      #if 0
        if(String::trim(line+1) == "off" || String::trim(line+1) == "0")
          smoothing_group = false;
        else
          smoothing_group = true;
      #endif
    }
    else
    /*if (strcmp(cmd,"mg") == 0) // Merging group
    {
    }
    else*/
    if (strcmp(cmd,"o") == 0) // Object name
    {
      starts_new_geom = true;
      object_name = String::trimStdString(line.c_str()+1);
    }
    else
// ----------------------------------------------------------------------------
    // Display/render attributes:
    /*if (strcmp(cmd,"bevel") == 0) // Bevel interpolation
    {
    }
    else
    if (strcmp(cmd,"c_interp") == 0) // Color interpolation
    {
    }
    else
    if (strcmp(cmd,"d_interp") == 0) // Dissolve interpolation
    {
    }
    else
    if (strcmp(cmd,"lod") == 0) // Level of detail
    {
    }
    else*/
    if (strcmp(cmd,"usemtl") == 0) // Material name
    {
      starts_new_geom = true;
      std::string mat_name = String(line.c_str()+6).trim().toStdString();
      // can also become NULL
      cur_material = mMaterials[mat_name];
    }
    else
    if (strcmp(cmd,"mtllib") == 0) // Material library
    {
      // creates the path for the mtl
      String path = file->path().extractPath() + String(line.c_str()+7).trim();
      ref<VirtualFile> vfile = defFileSystem()->locateFile(path, file->path().extractPath());
      std::string str_file = file->path().toStdString();
      std::string str_path = path.toStdString();
      if (vfile)
      {
        // reads the material
        std::vector<ObjMaterial> mats;
        loadObjMaterials(vfile.get(), mats);
        // updates the material library
        for(size_t i=0; i < mats.size(); ++i)
          mMaterials[mats[i].objectName()] = new ObjMaterial(mats[i]);
      }
      else
      {
        Log::error( Say("Could not find OBJ material file '%s'.\n") << path );
      }
    }
    /*else
    if (strcmp(cmd,"shadow_obj") == 0) // Shadow casting
    {
    }
    else
    if (strcmp(cmd,"trace_obj") == 0) // Ray tracing
    {
    }
    else
    if (strcmp(cmd,"ctech") == 0) // Curve approximation technique
    {
    }
    else
    if (strcmp(cmd,"stech") == 0) // Surface approximation technique
    {
    }*/
  }

  ref<ResourceDatabase> res_db = new ResourceDatabase;

  // compile the material/effect library

  std::map< ObjMaterial*, ref<Effect> > material_map;
  for (std::map< std::string, ref<ObjMaterial> >::iterator it = mMaterials.begin();
      it != mMaterials.end();
      ++it)
  {
    ref<Effect> effect = new Effect;
    res_db->resources().push_back(effect.get());

    ref<ObjMaterial> obj_mat = it->second;
    material_map[it->second.get()] = effect;
    res_db->resources().push_back(effect);

    effect->shader()->enable(EN_DEPTH_TEST);
    effect->shader()->disable(EN_CULL_FACE);
    effect->shader()->enable(EN_LIGHTING);
    effect->shader()->gocLightModel()->setTwoSide(true);

    if (obj_mat)
    {
      // sets the name
      effect->shader()->gocMaterial()->setObjectName(obj_mat->objectName().c_str());
      // add the Material to the ResourceDatabase
      res_db->resources().push_back(effect->shader()->gocMaterial());
      // setup the material
      fvec4 diffuse  = fvec4( obj_mat->kd(), 1.0f - obj_mat->tr() );
      fvec4 ambient  = fvec4( obj_mat->ka(), 1.0f - obj_mat->tr() );
      fvec4 specular = fvec4( obj_mat->ks(), 1.0f - obj_mat->tr() );
      fvec4 emission = fvec4( obj_mat->ke(), 1.0f - obj_mat->tr() );
      effect->shader()->gocMaterial()->setDiffuse( diffuse );
      effect->shader()->gocMaterial()->setAmbient( ambient );
      effect->shader()->gocMaterial()->setSpecular( specular );
      effect->shader()->gocMaterial()->setEmission( emission );
      effect->shader()->gocMaterial()->setShininess( obj_mat->ns());

      // setup transparency
      if (obj_mat->tr() > 0 || obj_mat->map_d().valid())
      {
        effect->shader()->enable(EN_BLEND);

        // if it has a mask map use alpha testing (typically for vegetation).
        if ( obj_mat->map_d().valid() )
        {
          effect->shader()->gocAlphaFunc()->set(FU_GEQUAL, 0.5);
          effect->shader()->enable(EN_ALPHA_TEST);
          // disable cull face
          // enable two side
        }
        else
        {
          effect->shader()->enable(EN_CULL_FACE);
          effect->shader()->gocLightModel()->setTwoSide(false);
        }
      }

      // setup texture
      if (obj_mat->map_Kd().valid())
      {
        // diffuse
        ref<VirtualFile> diff_file = defFileSystem()->locateFile(obj_mat->map_Kd().path(), file->path().extractPath());
        ref<Image> diff_img;
        if (diff_file)
          diff_img = loadImage( diff_file.get() );

        // mask
        if (obj_mat->map_d().valid())
        {
          ref<VirtualFile> mask_file = defFileSystem()->locateFile(obj_mat->map_d().path(), file->path().extractPath());
          ref<Image> mask_img;
          if (mask_file)
            mask_img = loadImage( mask_file.get() );
          if (mask_img)
          {
            if (mask_img->width() == diff_img->width() && mask_img->height() == diff_img->height())
            {
              diff_img = diff_img->convertFormat(IF_RGBA); VL_CHECK(diff_img)
              diff_img = diff_img->convertType(IT_UNSIGNED_BYTE); VL_CHECK(diff_img)
              mask_img = mask_img->convertType(IT_UNSIGNED_BYTE);
              int bpp = mask_img->bitsPerPixel() / 8;
              unsigned char* mask_px  = mask_img->pixels();
              unsigned char* mask_end = mask_img->pixels() + mask_img->requiredMemory();
              unsigned char* diff_px  = diff_img->pixels() + 3;
              while( mask_px != mask_end )
              {
                diff_px[0] = mask_px[0];
                mask_px += bpp;
                diff_px += 4;
              }
            }
          }
        }

        if ( diff_img )
        {
          ref<Texture> texture = new Texture;
          texture->getTexParameter()->setMinFilter(TPF_LINEAR_MIPMAP_LINEAR);
          texture->getTexParameter()->setMagFilter(TPF_LINEAR);
          texture->prepareTexture2D( diff_img.get(), TF_RGBA, true );
          effect->shader()->gocTextureSampler(0)->setTexture( texture.get() );
        }
      }
    }
  }

  for(int imesh=0; imesh<(int)mMeshes.size(); ++imesh)
  {
    if ( mMeshes[imesh]->facePositionIndex().empty() )
    {
      Log::warning("OBJ mesh empty.\n");
      VL_TRAP()
      continue;
    }

    #ifndef NDEBUG
      int sum = 0;
      for(int k=0; k<(int)mMeshes[imesh]->face_type().size(); ++k)
        sum += mMeshes[imesh]->face_type()[k];
      VL_CHECK( sum == (int)mMeshes[imesh]->facePositionIndex().size() )
    #endif

    ref< ArrayFloat3 >   v_coords  = new ArrayFloat3;
    ref< ArrayFloat3 >   n_coords  = new ArrayFloat3;
    // we support only 2d textures
    ref< ArrayFloat2 >   t_coords2 = new ArrayFloat2;

    if ( !mMeshes[imesh]->faceNormalIndex().empty() && mMeshes[imesh]->faceNormalIndex().size() != mMeshes[imesh]->facePositionIndex().size() )
    {
      Log::print("OBJ mesh corrupted.\n");
      continue;
    }

    if ( !mMeshes[imesh]->faceTexCoordIndex().empty() && mMeshes[imesh]->faceTexCoordIndex().size() != mMeshes[imesh]->facePositionIndex().size() )
    {
      Log::print("OBJ mesh corrupted.\n");
      continue;
    }

    // allocate vertex buffer

    int tri_verts_count = 0;
    for(int k=0; k<(int)mMeshes[imesh]->face_type().size(); ++k)
      tri_verts_count += (mMeshes[imesh]->face_type()[k] - 2) * 3;

    v_coords->resize( tri_verts_count );
    if ( !mMeshes[imesh]->faceNormalIndex().empty() )
      n_coords->resize( tri_verts_count );
    if ( !mMeshes[imesh]->faceTexCoordIndex().empty() )
      t_coords2->resize( tri_verts_count );

    // fill geometry

    int src_base_idx = 0;
    int dst_base_idx = 0;
    for(int iface=0; iface<(int)mMeshes[imesh]->face_type().size(); ++iface)
    {
      int type = mMeshes[imesh]->face_type()[iface];
      for( int ivert=2; ivert < type; ++ivert )
      {
        int a = mMeshes[imesh]->facePositionIndex()[src_base_idx+0];
        int b = mMeshes[imesh]->facePositionIndex()[src_base_idx+ivert-1];
        int c = mMeshes[imesh]->facePositionIndex()[src_base_idx+ivert];

        VL_CHECK( a>= 0)
        VL_CHECK( b>= 0)
        VL_CHECK( c>= 0)
        VL_CHECK( a<(int)mCoords.size() )
        VL_CHECK( b<(int)mCoords.size() )
        VL_CHECK( c<(int)mCoords.size() )

        v_coords->at(dst_base_idx+0) = mCoords[a].xyz();
        v_coords->at(dst_base_idx+1) = mCoords[b].xyz();
        v_coords->at(dst_base_idx+2) = mCoords[c].xyz();

        if (!mMeshes[imesh]->faceNormalIndex().empty())
        {
          int na = mMeshes[imesh]->faceNormalIndex()[src_base_idx+0];
          int nb = mMeshes[imesh]->faceNormalIndex()[src_base_idx+ivert-1];
          int nc = mMeshes[imesh]->faceNormalIndex()[src_base_idx+ivert];

          VL_CHECK( na>= 0)
          VL_CHECK( nb>= 0)
          VL_CHECK( nc>= 0)
          VL_CHECK( na<(int)mNormals.size() )
          VL_CHECK( nb<(int)mNormals.size() )
          VL_CHECK( nc<(int)mNormals.size() )

          n_coords->at(dst_base_idx+0) = mNormals[na];
          n_coords->at(dst_base_idx+1) = mNormals[nb];
          n_coords->at(dst_base_idx+2) = mNormals[nc];
        }

        // we consider all the texture coords as 2d
        if (!mMeshes[imesh]->faceTexCoordIndex().empty())
        {
          int na = mMeshes[imesh]->faceTexCoordIndex()[src_base_idx+0];
          int nb = mMeshes[imesh]->faceTexCoordIndex()[src_base_idx+ivert-1];
          int nc = mMeshes[imesh]->faceTexCoordIndex()[src_base_idx+ivert];

          VL_CHECK( na>= 0)
          VL_CHECK( nb>= 0)
          VL_CHECK( nc>= 0)
          VL_CHECK( na<(int)mTexCoords.size() )
          VL_CHECK( nb<(int)mTexCoords.size() )
          VL_CHECK( nc<(int)mTexCoords.size() )

          t_coords2->at(dst_base_idx+0) = mTexCoords[na].st();
          t_coords2->at(dst_base_idx+1) = mTexCoords[nb].st();
          t_coords2->at(dst_base_idx+2) = mTexCoords[nc].st();
        }

        dst_base_idx+=3;
      }
      src_base_idx += type;
    }

    // Geometry

    ref<Geometry> geom = new Geometry;
    geom->setObjectName(mMeshes[imesh]->objectName().c_str());
    geom->setVertexArray( v_coords.get() );
    if ( mMeshes[imesh]->faceNormalIndex().size() )
      geom->setNormalArray( n_coords.get() );
    if ( mMeshes[imesh]->faceTexCoordIndex().size() )
      geom->setTexCoordArray(0, t_coords2.get() );
    geom->drawCalls()->push_back( new DrawArrays(PT_TRIANGLES, 0, tri_verts_count) );

    // Material/Effect

    ref<Effect> effect = material_map[ mMeshes[imesh]->material() ];
    if (!effect)
    {
      effect = material_map[ mMeshes[imesh]->material() ] = new Effect;
      res_db->resources().push_back(effect.get());

      effect->shader()->enable(EN_DEPTH_TEST);
      effect->shader()->disable(EN_CULL_FACE);
      effect->shader()->enable(EN_LIGHTING);
      effect->shader()->gocLightModel()->setTwoSide(true);
    }
    VL_CHECK(effect)

    // Actor
    ref<Actor> actor = new Actor(geom.get(), NULL);
    res_db->resources().push_back(actor);
    actor->setObjectName(mMeshes[imesh]->objectName().c_str());
    actor->setEffect(effect.get());
  }

  stream->inputFile()->close();

  return res_db;
}
//-----------------------------------------------------------------------------
ref<ResourceDatabase> vl::loadOBJ( const String& path )
{
  ref<VirtualFile> file = defFileSystem()->locateFile( path );
  if (file)
    return loadOBJ( file.get() );
  else
  {
    Log::error( Say("Could not locate '%s'.\n") << path );
    return NULL;
  }
}
//-----------------------------------------------------------------------------
ref<ResourceDatabase> vl::loadOBJ( VirtualFile* file )
{
  return ObjLoader().loadOBJ(file);
}
//-----------------------------------------------------------------------------
