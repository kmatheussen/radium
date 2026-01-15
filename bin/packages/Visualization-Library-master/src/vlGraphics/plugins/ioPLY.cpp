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

#include "ioPLY.hpp"
#include <vlCore/Log.hpp>
#include <vlCore/Say.hpp>
#include <vlCore/VisualizationLibrary.hpp>
#include <vlCore/FileSystem.hpp>
#include <vlCore/TextStream.hpp>
#include <vlCore/VirtualFile.hpp>
#include <vlGraphics/Effect.hpp>
#include <vlGraphics/Actor.hpp>
#include <vlCore/LoadWriterManager.hpp>

using namespace vl;
//-----------------------------------------------------------------------------
ref<ResourceDatabase> vl::loadPLY(const String& path)
{
  ref<VirtualFile> file = defFileSystem()->locateFile(path);

  if (file)
    return loadPLY( file.get() );
  else
  {
    Log::error( Say("Could not locate '%s'.\n") << path );
    return NULL;
  }
}
//-----------------------------------------------------------------------------
ref<ResourceDatabase> vl::loadPLY(VirtualFile* file)
{
  PlyLoader ply;
  ref<ResourceDatabase> res_db = ply.loadPly(file);
  return res_db;
}
//-----------------------------------------------------------------------------
void PlyLoader::PlyScalar::read(VirtualFile* file, bool le)
{
  switch(scalarType()) 
  {
    case PlyChar:   mData.mChar   = file->readSInt8(); break;
    case PlyUChar:  mData.mUChar  = file->readUInt8(); break;
    case PlyShort:  mData.mShort  = file->readSInt16(le); break;
    case PlyUShort: mData.mUShort = file->readUInt16(le); break;
    case PlyInt:    mData.mInt    = file->readSInt32(le); break;
    case PlyUInt:   mData.mUInt   = file->readUInt32(le); break;
    case PlyFloat:  mData.mFloat  = file->readFloat(le); break;
    case PlyDouble: mData.mDouble = file->readDouble(le); break;
    default:
      Log::error("PlyLoader: scalar read error.\n");
  }
}
void PlyLoader::PlyScalar::read(TextStream* text)
{
  int idata;
  double ddata;
  switch(scalarType())
  {
    case PlyChar:   text->readInt(idata); mData.mChar   = (char)idata; break;
    case PlyUChar:  text->readInt(idata); mData.mUChar  = (unsigned char)idata; break;
    case PlyShort:  text->readInt(idata); mData.mShort  = (short)idata; break;
    case PlyUShort: text->readInt(idata); mData.mUShort = (unsigned short)idata; break;
    case PlyInt:    text->readInt(idata); mData.mInt    = (int)idata; break;
    case PlyUInt:   text->readInt(idata); mData.mUInt   = (unsigned int)idata; break;
    case PlyFloat:  text->readDouble(ddata); mData.mFloat  = (float)ddata; break;
    case PlyDouble: text->readDouble(ddata); mData.mDouble = (double)ddata; break;
    default:
      Log::error("PlyLoader: scalar read error.\n");
  }
}
float PlyLoader::PlyScalar::getAsFloat() const
{
  switch(scalarType())
  {
    case PlyChar:   return (float)mData.mChar;
    case PlyUChar:  return (float)mData.mUChar;
    case PlyShort:  return (float)mData.mShort;
    case PlyUShort: return (float)mData.mUShort;
    case PlyInt:    return (float)mData.mInt;
    case PlyUInt:   return (float)mData.mUInt;
    case PlyFloat:  return mData.mFloat;
    case PlyDouble: return (float)mData.mDouble;
    default:
      Log::error("PlyLoader: getAsFloat() error.\n");
      return 0.0f;
  }
}
int PlyLoader::PlyScalar::getAsInt() const
{
  switch(scalarType())
  {
    case PlyChar:   return (int)mData.mChar;
    case PlyUChar:  return (int)mData.mUChar;
    case PlyShort:  return (int)mData.mShort;
    case PlyUShort: return (int)mData.mUShort;
    case PlyInt:    return (int)mData.mInt;
    case PlyUInt:   return (int)mData.mUInt;
    case PlyFloat:  return (int)mData.mFloat;
    case PlyDouble: return (int)mData.mDouble;
    default:
      Log::error("PlyLoader: getAsFloat() error.\n");
      return 0;
  }
}
void PlyLoader::PlyElement::analyze()
{
  mVertex.resize(3);
  mNormal.resize(3);
  mColor.resize(4);
  if (name() == "vertex")
  {
    for(unsigned int j=0; j<properties().size(); ++j)
    {
      PlyScalar* scalar = cast<PlyScalar>(properties()[j].get()); VL_CHECK(scalar)
      if (scalar)
      {
        if (scalar->name() == "x")
          mVertex[0] = scalar;
        else
        if (scalar->name() == "y")
          mVertex[1] = scalar;
        else
        if (scalar->name() == "z")
          mVertex[2] = scalar;
        else
        if (scalar->name() == "nx")
          mNormal[0] = scalar;
        else
        if (scalar->name() == "ny")
          mNormal[1] = scalar;
        else
        if (scalar->name() == "nz")
          mNormal[2] = scalar;
        else
        if (scalar->name() == "red")
          mColor[0] = scalar;
        else
        if (scalar->name() == "green")
          mColor[1] = scalar;
        else
        if (scalar->name() == "blue")
          mColor[2] = scalar;
        else
        if (scalar->name() == "alpha")
          mColor[3] = scalar;
      }
    }
  }
}
void PlyLoader::readElements(VirtualFile* file)
{
  // reposition to the correct location
  file->seekSet(0);
  std::string str;
  do
  {
    unsigned char ch = file->readUInt8();
    if (ch == '\n' && str == "end_header")
      break;
    if (ch == '\n')
    {
      str.clear();
      continue;
    }
    str.push_back( ch );
  } while(true);

  for(unsigned i=0; i<mElements.size(); ++i)
    for(int j=0; j<mElements[i]->elemCount(); ++j)
    {
      mElements[i]->read(file, littleEndian());
      newElement(mElements[i].get());
    }
}
void PlyLoader::readElements(TextStream* text)
{
  for(unsigned i=0; i<mElements.size(); ++i)
    for(int j=0; j<mElements[i]->elemCount(); ++j)
    {
      mElements[i]->read(text);
      newElement(mElements[i].get());
    }
}
void PlyLoader::newElement(PlyElement*el)
{
  if (el->name() == "vertex")
  {
    for(unsigned int j=0; j<el->properties().size(); ++j)
    {
      if (mVerts)
        mVerts->at(mVertexIndex)   = el->getVertex();
      if (mNormals)
        mNormals->at(mVertexIndex) = el->getNormal();
      if (mColors)
        mColors->at(mVertexIndex)  = el->getColor();
    }

    ++mVertexIndex;
  }
  else
  if (el->name() == "face")
  {
    for(unsigned int j=0; j<el->properties().size(); ++j)
    {
      PlyScalarList* list = cast<PlyScalarList>(el->properties()[j].get()); VL_CHECK(list)
      if (list && list->name() == "vertex_indices")
      {
        for(int i=1; i<(int)list->scalars().size()-1; ++i)
        {
          if (mIndices.capacity() - mIndices.size() == 0)
            mIndices.reserve( mIndices.capacity() * 2 );
          mIndices.push_back( list->scalars()[0].getAsInt());
          mIndices.push_back( list->scalars()[i].getAsInt());
          mIndices.push_back( list->scalars()[i+1].getAsInt());
        }
      }
    }
  }
}
PlyLoader::EType PlyLoader::translateType(const String& type)
{
  if (type == "int8")   return PlyChar; else
  if (type == "char")   return PlyChar; else
  if (type == "uint8")   return PlyChar; else
  if (type == "uchar")  return PlyUChar; else
  if (type == "ushort") return PlyUShort; else
  if (type == "uint16") return PlyUShort; else
  if (type == "short")  return PlyShort; else
  if (type == "int16")  return PlyShort; else
  if (type == "int")    return PlyInt; else
  if (type == "int32")    return PlyInt; else
  if (type == "uint")   return PlyUInt; else
  if (type == "uint32")   return PlyUInt; else
  if (type == "float")  return PlyFloat; else
  if (type == "float32")  return PlyFloat; else
  if (type == "float64")  return PlyDouble; else
  if (type == "double") return PlyDouble; 
  else
  {
    Log::error("PlyLoader: type parse error.\n");
    return PlyError;
  }
}
void PlyLoader::analyzeHeader()
{
  // allocate space for vertices, normals and colors.
  for(unsigned int i=0; i<elements().size(); ++i)
  {
    if (elements()[i]->name() == "vertex")
    {
      for(unsigned int j=0; j<elements()[i]->properties().size(); ++j)
      {
        if (elements()[i]->properties()[j]->name() == "x")
        {
          mVerts = new ArrayFloat3;
          mVerts->resize( elements()[i]->elemCount() );
        }
        if (elements()[i]->properties()[j]->name() == "nx")
        {
          mNormals = new ArrayFloat3;
          mNormals->resize( elements()[i]->elemCount() );
        }
        if (elements()[i]->properties()[j]->name() == "red")
        {
          mColors = new ArrayUByte4;
          mColors->resize( elements()[i]->elemCount() );
          memset(mColors->ptr(), 0xFF, sizeof(mColors->at(0))*mColors->size());
        }
      }
      elements()[i]->analyze();
    }
  }
  if (mVerts->size() == 0)
  {
    Log::error("PlyLoader: no vertices found.\n");
  }
}
bool PlyLoader::readHeader(TextStream* line_reader)
{
  mVerts   = NULL;
  mNormals = NULL;
  mColors  = NULL;
  mIndices.reserve(100);
  bool ok = true;
  elements().clear();
  String str;
  // ply loader
  line_reader->readLineLF(str);
  if (str.trim() != "ply")
    return false;
  // format
  line_reader->readLineLF(str);
  if (str.trim() == "format ascii 1.0")
  {
    mBinary = false;
  }
  else
  if (str.trim() == "format binary_little_endian 1.0")
  {
    mBinary = true;
    mLittleEndian = true;
  }
  else
  if (str.trim() == "format binary_big_endian 1.0")
  {
    mBinary = true;
    mLittleEndian = false;
  }
  else
    return false;
  // read elements
  while ( (line_reader->readLineLF(str)) && str.trim() != "end_header")
  {
    if (str.startsWith("comment"))
      continue;
    else
    if (str.startsWith("element"))
    {
      elements().push_back(new PlyElement);
      elements().back()->setName( str.field(' ',1) );
      elements().back()->setElemCount( str.field(' ',2).toInt() );
    }
    else
    if (str.startsWith("property"))
    {
      String prop_type = str.field(' ',1);
      ref<PlyScalar> prop;
      ref<PlyScalarList> scalar_list;
      if (prop_type == "list")
      {
        String num_type = str.field(' ',2);
        String idx_type = str.field(' ',3);
        String name     = str.field(' ',4);
        if (name.empty())
        {
          Log::error("PLY parse error #5.\n");
          return false;
        }

        scalar_list = new PlyScalarList;
        elements().back()->properties().push_back(scalar_list);

        scalar_list->setCountType(translateType(num_type));
        scalar_list->setScalarType(translateType(idx_type));
        scalar_list->setName(name);
      }
      else
      {
        prop = new PlyScalar;
        elements().back()->properties().push_back(prop);
        prop->setName( str.field(' ',2) );
        prop->setScalarType(translateType(prop_type));
      }
    }
    else
    {
      Log::error("PLY parse error #2.\n");
      return false;
    }
  }
  analyzeHeader();
  mVertexIndex = 0;
  return ok;
}
ref<ResourceDatabase> PlyLoader::loadPly(VirtualFile* file)
{
  ref<TextStream> line_reader = new TextStream;

  line_reader->setInputFile(file);

  readHeader(line_reader.get());

  if (binary())
    readElements(file);
  else
    readElements(line_reader.get());
  file->close();

  if (mIndices.empty() || !mVerts)
    return NULL;

  ref<Geometry> geom = new Geometry;
  geom->setVertexArray(mVerts.get());
  geom->setNormalArray(mNormals.get());
  geom->setColorArray(mColors.get());
  ref<DrawElementsUInt> de = new DrawElementsUInt(PT_TRIANGLES);
  geom->drawCalls()->push_back(de.get());
  de->indexBuffer()->resize(mIndices.size());
  memcpy(de->indexBuffer()->ptr(), &mIndices[0], sizeof(unsigned int)*mIndices.size());

  // Effect
  ref<Effect> effect = new Effect;

  // Detph test
  effect->shader()->enable(EN_DEPTH_TEST);

  // Lighting
  if( mNormals )
    effect->shader()->enable(EN_LIGHTING);

  // note: we don't insert any light
  if (mColors)
    effect->shader()->gocMaterial()->setColorMaterialEnabled(true);

  ref<ResourceDatabase> res_db = new ResourceDatabase;
  res_db->resources().push_back( new Actor(geom.get(), effect.get(), NULL ) );
  return res_db;
}
//-----------------------------------------------------------------------------
