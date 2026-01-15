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

#ifndef Terrain_INCLUDE_ONCE
#define Terrain_INCLUDE_ONCE

#include <vlGraphics/SceneManagerActorKdTree.hpp>
#include <vlGraphics/ShaderNode.hpp>

namespace vl
{
  /**
   * The Terrain class implements a ActorKdTree-based terrain scene manager.
   *
   * Some applications do not need complex LOD or CLOD management algorithms to display height fields or terrains, 
   * either because the data displayed has a limited size or because particular constraints are applied to the camera 
   * (like in RTS games for example) for which only a limited portion of the terrain is visible at a given time. 
   
   * This applications can greatly benefit from the vl::Terrain scene manager which implements a fool-proof, high-precision 
   * (8, 16 and 32 bits heightmaps are supported) and efficient terrain/heightfield generation and management system. 
   * All the user has to provide is a texture for the terrain, a heightfield image and a detail texture following very 
   * simple and precise guidelines. 
   
   * The vl::Terrain class takes care of all the rest, like dividing the provided images 
   * into chunks, generating the geometry, generating the kd-tree, computing the appropriate texture coordinates in order 
   * to prevent seams and so on. 
   
   * If the OpenGL Shading Language is available, the vl::Terrain class can also store the 
   * heightmap directly on the GPU and can generate the geometry on the fly using a technique called "vertex texture 
   * fetch" (http://developer.nvidia.com/object/using_vertex_textures.html). This technique allows the application to 
   * save GPU memory and to manage even greater terrain databases at a higher speed.
   *
   * \sa setTerrainTexture(), setHeightmapTexture(), setDetailTexture()
   */
  class VLGRAPHICS_EXPORT Terrain: public SceneManagerActorKdTree
  {
    VL_INSTRUMENT_CLASS(vl::Terrain, SceneManagerActorKdTree)

  public:
    Terrain(): 
        mShaderNode(new ShaderNode), mWidth(0), mHeight(0), mDepth(0), mDetailRepetitionCount(0), 
        mHeightmapTextureFormat(TF_LUMINANCE16F), mTerrainTextureFormat(TF_RGB), mDetailTextureFormat(TF_RGB),
        mUseGLSL(true) 
    {
      mChunks.setAutomaticDelete(false);
    }

    void init();

    bool useGLSL() const { return mUseGLSL; }
    int detailRepetitionMode() const { return mDetailRepetitionCount; }
    double width() const { return mWidth; }
    double depth() const { return mDepth; }
    double height() const { return mHeight; }
    const vec3& origin() const { return mOrigin; }
    const String& detailTexture() const { return mDetailTexture; }
    const String& terrainTexture() const { return mTerrainTexture; }
    const String& heightmapTexture() const { return mHeightmapTexture; }
    ETextureFormat heightmapTextureFormat() const { return mHeightmapTextureFormat; }
    ETextureFormat terrainTextureFormat() const { return mTerrainTextureFormat; }
    ETextureFormat detailTextureFormat() const { return mDetailTextureFormat; }

    void setUseGLSL(bool enable) { mUseGLSL = enable; }
    void setDetailRepetitionCount(int count) { mDetailRepetitionCount = count; }
    void setWidth(double w)  { mWidth = w; }
    void setDepth(double d)  { mDepth = d; }
    void setHeight(double h) { mHeight = h; }
    void setOrigin(const vec3& origin) { mOrigin = origin; }
    void setHeightmapTextureFormat(ETextureFormat format) { mHeightmapTextureFormat = format; }
    void setTerrainTextureFormat(ETextureFormat format) { mTerrainTextureFormat = format; }
    void setDetailTextureFormat(ETextureFormat format) { mDetailTextureFormat = format; }

    void setVertexShader(const String& vs_path) { mVertexShader = vs_path; }
    const String& vertexShader() const { return mVertexShader; }

    void setFragmentShader(const String& fs_path) { mFragmentShader = fs_path; }
    const String& fragmentShader() const { return mFragmentShader; }

    const ActorCollection& chunks() const { return mChunks; }
    int chunkCount() const { return mChunks.size(); }
    Actor* chunk(int i) { return mChunks[i].get(); }
    const Actor* chunk(int i) const { return mChunks[i].get(); }

    /**
     * Sets the texture to be used as detail texture, it can be of any size.
    */
    void setDetailTexture(const String& texture) { mDetailTexture = texture; }
    /**
     * Sets the texture to be used as the base terrain texture.
     * Its size must be of the form: n * d - n + 1 where:
     * - 'n' is any positive integer. 'n' is the value passed to setChunkSubdivision().
     * - 'd' is a power of 2 and defines the dimension of the texture applied to each terrain chunk.
     * For example: for a terrain with 8x8 chunks each of which with a 256x256 texture we have: 8*256-8+1 = 2041 = x and y size of the texture image.
    */
    void setTerrainTexture(const String& texture) { mTerrainTexture = texture; }
    /**
     * Sets the texture to be used as the heightmap.
     * Its size must be of the form: n * d - n + 1 where:
     * - 'n' is any positive integer. 'n' is the value passed to setChunkSubdivision().
     * - 'd' is a power of 2 and defines the dimension of the heightmap portion used to build each terrain chunk.
     * For example: for a terrain with 8x8 chunks each of which with a 256x256 heightmap we have: 8*256-8+1 = 2041 = x and y size of the heightmap image.
    */
    void setHeightmapTexture(const String& texture) { mHeightmapTexture = texture; }

    const ShaderNode* shaderNode() const { return mShaderNode.get(); }
    ShaderNode* shaderNode() { return mShaderNode.get(); }

  protected:
    ref<ShaderNode> mShaderNode;
    ActorCollection mChunks;
    double mWidth;
    double mHeight;
    double mDepth;
    vec3 mOrigin;
    String mDetailTexture;
    String mTerrainTexture;
    String mHeightmapTexture;
    String mVertexShader;
    String mFragmentShader;
    int mDetailRepetitionCount;
    ETextureFormat mHeightmapTextureFormat;
    ETextureFormat mTerrainTextureFormat;
    ETextureFormat mDetailTextureFormat;
    bool mUseGLSL;
  };
}

#endif
