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

#if !defined(LoadOBJ_INCLUDE_ONCE)
#define LoadOBJ_INCLUDE_ONCE

#include <vlCore/String.hpp>
#include <vlGraphics/Geometry.hpp>
#include <vlCore/ResourceLoadWriter.hpp>
#include <vlCore/ResourceDatabase.hpp>
#include <map>

namespace vl
{
  class VirtualFile;

//-----------------------------------------------------------------------------
  //! Loads a Wavefront OBJ file. See also ObjLoader.
  VLGRAPHICS_EXPORT ref<ResourceDatabase> loadOBJ( const String& path );
//-----------------------------------------------------------------------------
  //! Loads a Wavefront OBJ file. See also ObjLoader.
  VLGRAPHICS_EXPORT ref<ResourceDatabase> loadOBJ( VirtualFile* file );
//---------------------------------------------------------------------------
// LoadWriterOBJ
//---------------------------------------------------------------------------
  /**
   * The LoadWriterOBJ class is a ResourceLoadWriter capable of reading OBJ files.
   */
  class LoadWriterOBJ: public ResourceLoadWriter
  {
    VL_INSTRUMENT_CLASS(vl::LoadWriterOBJ, ResourceLoadWriter)

  public:
    LoadWriterOBJ(): ResourceLoadWriter("|obj|", "|obj|") {}

    void registerLoadWriter();

    ref<ResourceDatabase> loadResource(const String& path) const 
    {
      return loadOBJ(path);
    }

    ref<ResourceDatabase> loadResource(VirtualFile* file) const
    {
      return loadOBJ(file);
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
// ObjTexture
//-----------------------------------------------------------------------------
  //! Represents a Wavefront OBJ texture. See also ObjMaterial and ObjLoader.
  class VLGRAPHICS_EXPORT ObjTexture
  {
  public:
    ObjTexture();
    const ObjTexture& parseLine(const String& line, const String& file);
    bool valid() const { return !mFileName.empty(); }
    //! Prints the content of the material. Used for debugging purposes.
    void print();

    //! Texture file name
    const String& path() const { return mFileName; }
    //! -blendu on | off
    bool blendU() const { return mBlendU; }
    //! -blendv on | off
    bool blendV() const { return mBlendV; }
    //! -cc on | off
    bool cc() const { return mCC; }
    //! -clamp on | off
    bool clamp() const { return mClamp; }
    //! -mm base gain
    float mm_Base() const { return mMM_Base; }
    //! -mm base gain
    float mm_Gain() const { return mMM_Gain; }
    //! -o u v w
    const fvec3& o_UVW() const { return mO_UVW; }
    //! -s u v w
    const fvec3& s_UVW() const { return mS_UVW; }
    //! -t u v w
    const fvec3& t_UVW() const { return mT_UVW; }
    //! -texres value
    float texres_Value() const { return mTexres_Value; }
    //! -imfchan r | g | b | m | l | z
    char imfchan() const { return mImfchan; }
    //! -bm mult
    float bm() const { return mBM; }

    //! Texture file name
    void setPath(const String& filename) { mFileName = filename; }
    //! -blendu on | off
    void setBlendU(bool on) { mBlendU = on; }
    //! -blendv on | off
    void setBlendV(bool on) { mBlendV = on; }
    //! -cc on | off
    void setCC(bool on) { mCC = on; }
    //! -clamp on | off
    void setClamp(bool on) { mClamp = on; }
    //! -mm base gain
    void setMM_Base(float base) { mMM_Base = base; }
    //! -mm base gain
    void setMM_Gain(float gain) { mMM_Gain = gain; }
    //! -o u v w
    void setO_UVW(const fvec3& o_uvw) { mO_UVW = o_uvw; }
    //! -s u v w
    void setS_UVW(const fvec3& s_uvw) { mS_UVW = s_uvw; }
    //! -t u v w
    void setT_UVW(const fvec3& t_uvw) { mT_UVW = t_uvw; }
    //! -texres value
    void setTexres_Value(float value) { mTexres_Value = value; }
    //! -imfchan r | g | b | m | l | z
    void setImfchan(char val) { mImfchan = val; }
    //! -bm mult
    void setBM(float bm) { mBM = bm; }

  protected:
    //! Texture file name
    String mFileName;
    //! -blendu on | off
    bool   mBlendU; 
    //! -blendv on | off
    bool   mBlendV; 
    //! -cc on | off
    bool   mCC; 
    //! -clamp on | off
    bool   mClamp; 
    //! -mm base gain
    float  mMM_Base;
    //! -mm base gain
    float  mMM_Gain;
    //! -o u v w
    fvec3 mO_UVW;
    //! -s u v w
    fvec3 mS_UVW;
    //! -t u v w
    fvec3 mT_UVW;
    //! -texres value
    float  mTexres_Value;
    //! -imfchan r | g | b | m | l | z
    char   mImfchan;
    //! -bm mult
    float  mBM;
  };
//-----------------------------------------------------------------------------
// ObjMaterial
//-----------------------------------------------------------------------------
  //! Represents a Wavefront OBJ material as loaded from an MTL file. See also ObjLoader.
  class ObjMaterial: public Object
  {
    VL_INSTRUMENT_CLASS(vl::ObjMaterial, Object)

  public:
    ObjMaterial(): mTr(1.0f), mNs(0.0f), mIllum(0), mNi(1.0f) {}

    //! Ka - ambient color
    const fvec3& ka() const { return mKa; }
    //! Kd - diffuse color
    const fvec3& kd() const { return mKd; }
    //! Ks - specular color
    const fvec3& ks() const { return mKs; }
    //! Ke - emissive color
    const fvec3& ke() const { return mKe; }
    //! Tr/d - transparency
    float tr() const { return mTr; }
    //! Ns - specular exponent
    float ns() const { return mNs; }
    //! illum - illumination model
    int illum() const { return mIllum; }
    //! Ni - optical density / index of refraction
    float ni() const { return mNi; }
    //! map_Kd - ambient diffuse
    const ObjTexture& map_Kd() const { return mMap_Kd; }
    //! map_Ka - ambient color map
    const ObjTexture& map_Ka() const { return mMap_Ka; }
    //! map_Ks - specular color map
    const ObjTexture& map_Ks() const { return mMap_Ks; }
    //! map_Ns - specular exponent map
    const ObjTexture& map_Ns() const { return mMap_Ns; }
    //! map_d - transparency map
    const ObjTexture& map_d() const { return mMap_d; }
    //! decal - decal map
    const ObjTexture& map_Decal() const { return mMap_Decal; }
    //! disp - displace map
    const ObjTexture& map_Disp() const { return mMap_Disp; }
    //! bump - bump map
    const ObjTexture& map_Bump() const { return mMap_Bump; }

    //! Ka - ambient color
    void setKa(const fvec3& ka) { mKa = ka; }
    //! Kd - diffuse color
    void setKd(const fvec3& kd) { mKd = kd; }
    //! Ks - specular color
    void setKs(const fvec3& ks) { mKs = ks; }
    //! Ke - emissive color
    void setKe(const fvec3& ke) { mKe = ke; }
    //! Tr/d - transparency
    void setTr(float tr) { mTr = tr; }
    //! Ns - specular exponent
    void setNs(float ns) { mNs = ns; }
    //! illum - illumination model
    void setIllum(int illum) { mIllum = illum; }
    //! Ni - optical density / index of refraction
    void setNi(float ni) { mNi = ni; }
    //! map_Kd - ambient diffuse
    void setMap_Kd(const ObjTexture& map_kd) { mMap_Kd = map_kd; }
    //! map_Ka - ambient color map
    void setMap_Ka(const ObjTexture& map_ka) { mMap_Ka = map_ka; }
    //! map_Ks - specular color map
    void setMap_Ks(const ObjTexture& map_ks) { mMap_Ks = map_ks; }
    //! map_Ns - specular exponent map
    void setMap_Ns(const ObjTexture& map_ns) { mMap_Ns = map_ns; }
    //! map_d - transparency map
    void setMap_d(const ObjTexture& map_d) { mMap_d = map_d; }
    //! decal - decal map
    void setMap_Decal(const ObjTexture& map_decal) { mMap_Decal = map_decal; }
    //! disp - displace map
    void setMap_Disp(const ObjTexture& map_disp) { mMap_Disp = map_disp; }
    //! bump - bump map
    void setMap_Bump(const ObjTexture& map_bump) { mMap_Bump = map_bump; }

  protected:
    //! Ka - ambient color
    fvec3 mKa;
    //! Kd - diffuse color
    fvec3 mKd;
    //! Ks - specular color
    fvec3 mKs;
    //! Ke - emissive color
    fvec3 mKe;
    //! Tr/d - transparency
    float mTr;
    //! Ns - specular exponent
    float mNs;
    //! illum - illumination model
    int mIllum;
    //! Ni - optical density / index of refraction
    float mNi;
    //! map_Kd - ambient diffuse
    ObjTexture mMap_Kd; 
    //! map_Ka - ambient color map
    ObjTexture mMap_Ka; 
    //! map_Ks - specular color map
    ObjTexture mMap_Ks; 
    //! map_Ns - specular exponent map
    ObjTexture mMap_Ns; 
    //! map_d - transparency map
    ObjTexture mMap_d;  
    //! decal - decal map
    ObjTexture mMap_Decal; 
    //! disp - displace map
    ObjTexture mMap_Disp;  
    //! bump - bump map
    ObjTexture mMap_Bump;  
  };
//-----------------------------------------------------------------------------
// ObjMesh
//-----------------------------------------------------------------------------
  //! Represents a Wavefront OBJ mesh. See also ObjLoader.
  class ObjMesh: public Object
  {
    VL_INSTRUMENT_CLASS(vl::ObjMesh, Object)

  public:
    //! The material associated to this mesh
    void setMaterial(ObjMaterial* mat)  { mMaterial = mat; }

    //! The material associated to this mesh
    const ObjMaterial* material() const { return mMaterial.get(); }
    //! Index into ObjLoader::vertexArray() vector
    const std::vector<int>& facePositionIndex() const { return mFace_icoords; }
    //! Index into ObjLoader::normalArray() vector
    const std::vector<int>& faceNormalIndex() const { return mFace_inormals; }
    //! Index into ObjLoader::texCoordsArray() vector
    const std::vector<int>& faceTexCoordIndex() const { return mFace_itexcoords; }
    //! Each entry represents a face, the number represents how many vertices the face has.
    //! The for each vertex of each face there is an index in facePositionIndex(), faceNormalIndex(), faceTexCoordIndex()
    const std::vector<int>& face_type() const { return mFace_type; }

    //! The material associated to this mesh
    ObjMaterial* material() { return mMaterial.get(); }
    //! Index into ObjLoader::vertexArray() vector
    std::vector<int>& facePositionIndex()  { return mFace_icoords; }
    //! Index into ObjLoader::normalArray() vector
    std::vector<int>& faceNormalIndex()  { return mFace_inormals; }
    //! Index into ObjLoader::texCoordsArray() vector
    std::vector<int>& faceTexCoordIndex()  { return mFace_itexcoords; }
    //! Each entry represents a face, the number represents how many vertices the face has.
    //! The for each vertex of each face there is an index in facePositionIndex(), faceNormalIndex(), faceTexCoordIndex()
    std::vector<int>& face_type()  { return mFace_type; }

  protected:
    ref<ObjMaterial> mMaterial;
    std::vector<int> mFace_icoords;
    std::vector<int> mFace_inormals;
    std::vector<int> mFace_itexcoords;
    std::vector<int> mFace_type;
  };
//-----------------------------------------------------------------------------
// ObjLoader
//-----------------------------------------------------------------------------
  //! Loads a Wavefront OBJ file
  class ObjLoader
  {
  public:
    const std::vector<fvec4>& vertexArray() const { return mCoords; }
    const std::vector<fvec3>& normalArray() const { return mNormals; }
    const std::vector<fvec3>& texCoordsArray() const { return mTexCoords; }
    const std::map< std::string, ref<ObjMaterial> >& materials() const { return mMaterials; }
    const std::vector< ref<ObjMesh> >& meshes() const { return mMeshes; }

    std::vector<fvec4>& vertexArray() { return mCoords; }
    std::vector<fvec3>& normalArray() { return mNormals; }
    std::vector<fvec3>& texCoordsArray() { return mTexCoords; }
    std::map< std::string, ref<ObjMaterial> >& materials() { return mMaterials; }
    std::vector< ref<ObjMesh> >& meshes() { return mMeshes; }

    //! Loads a Wavefront OBJ file.
    //! \param file The OBJ file to be loaded
    ref<ResourceDatabase> loadOBJ( VirtualFile* file );

    //! Loads a Wavefront MTL file.
    //! \param file The MTL file to be loaded
    //! \param materials Is filled with the loaded materials
    void loadObjMaterials(VirtualFile* file, std::vector<ObjMaterial>& materials );

  protected:
    std::vector<fvec4> mCoords;
    std::vector<fvec3> mNormals;
    std::vector<fvec3> mTexCoords;
    std::map< std::string, ref<ObjMaterial> > mMaterials;
    std::vector< ref<ObjMesh> > mMeshes;
  };
//-----------------------------------------------------------------------------
}

#endif
