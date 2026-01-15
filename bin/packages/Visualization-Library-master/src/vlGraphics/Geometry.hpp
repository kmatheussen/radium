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

#ifndef Geometry_INCLUDE_ONCE
#define Geometry_INCLUDE_ONCE

#include <vlGraphics/IVertexAttribSet.hpp>
#include <vlGraphics/BufferObject.hpp>
#include <vlCore/Vector2.hpp>
#include <vlCore/Vector4.hpp>
#include <vlGraphics/Renderable.hpp>
#include <vlCore/vlnamespace.hpp>
#include <vlCore/Log.hpp>
#include <vlCore/Say.hpp>
#include <vlCore/Colors.hpp>
#include <vlGraphics/DrawElements.hpp>
#include <vlGraphics/DrawArrays.hpp>
#include <vlCore/Collection.hpp>
#include <vlGraphics/VertexAttribInfo.hpp>

namespace vl
{
  class OpenGLContext;

  //------------------------------------------------------------------------------
  // Geometry
  //------------------------------------------------------------------------------
  /**
   * The Geometry class is a Renderable that implements a polygonal mesh made of
   * polygons, lines and points.
   *
   * @sa 
   * - ArrayAbstract
   * - Renderable
   * - Actor
   * - Transform
   * - Effect
  */
  class VLGRAPHICS_EXPORT Geometry: public Renderable, public IVertexAttribSet
  {
    VL_INSTRUMENT_CLASS(vl::Geometry, Renderable)

    // use deepCopy() and shallowCopy() instead
    // Geometry(const Geometry& other): Renderable(other) { }
    Geometry& operator=(const Geometry&) { return *this; }

  private:
    class TextureArray: public Object
    {
    public:
      TextureArray(int unit, ArrayAbstract* data): mTextureSampler(unit), mTexCoordArray(data) { }
      const BufferObject* bufferObject() const { return mTexCoordArray->bufferObject(); }
      BufferObject* bufferObject() { return mTexCoordArray->bufferObject(); }
      int mTextureSampler;
      ref<ArrayAbstract> mTexCoordArray;
    };

  public:
    /** Constructor. */
    Geometry();

    /** Destructor. */
    virtual ~Geometry();

    /**
     * Performs a shallow copy of a Geometry.
     * @sa deepCopy() */
    ref<Geometry> shallowCopy() const;

    /**
     * Performs a shallow copy of the specified Geometry.
     * @sa deepCopy() */
    Geometry& shallowCopyFrom(const Geometry&);

    /**
     * Performs a deep copy of a Geometry.
     * @sa shallowCopy() */
    ref<Geometry> deepCopy() const;

    /**
     * Performs a deep copy of the specified Geometry.
     * @sa shallowCopy() */
    Geometry& deepCopyFrom(const Geometry&);

    //! Returns the list of DrawCall objects bound to a Geometry
    Collection<DrawCall>* drawCalls() { return &mDrawCalls; }

    //! Returns the list of DrawCall objects bound to a Geometry
    const Collection<DrawCall>* drawCalls() const { return &mDrawCalls; }

    //! Fills the color array with the given color
    void setColorArray(const fvec4& color)
    {
      u32 vert_count = (u32)(vertexArray() ? vertexArray()->size() : vertexAttribArray(VA_Position) ? vertexAttribArray(VA_Position)->data()->size() : 0);
      VL_CHECK( vert_count )
      ref<ArrayFloat4> color_array = new ArrayFloat4;
      color_array->resize(vert_count);
      for(u32 i=0; i<color_array->size(); ++i)
        color_array->at(i) = color;
      #if defined(VL_OPENGL_ES2)
        setVertexAttribArray(VA_Color, color_array.get());
      #else
        setColorArray(color_array.get());
      #endif
    }

    /** Removes all the previously installed arrays. */
    virtual void clearArrays(bool clear_draw_calls=true);

    // --- Renderable interface implementation ---

    /** Updates all the vertex buffer objects of both vertex arrays and draw calls that are marked as dirty. */
    virtual void updateDirtyBufferObject(EBufferObjectUpdateMode mode);

    /** Deletes all the vertex buffer objects of both vertex arrays and draw calls. */
    virtual void deleteBufferObject();

    // ------------------------------------------------------------------------
    // Geometry Tools
    // ------------------------------------------------------------------------

    /** Converts the fixed function pipeline arrays (vertex array, normal arrays) into the generic ones.
    * The generic attribute indices are allocated in the following way:
    * - vertex array -> vl::VA_Position
    * - normal array -> vl::VA_Normal
    * - color array  -> vl::VA_Color
    * - texture array 0..N -> vl::VA_TexCoord0 .. N
    * - secondary color array -> at the first free position from vl::VA_TexCoord0 (included)
    * - fog coord array  -> at the first free position from vl::VA_TexCoord0 (included)
    *
    * \remarks
    * - Already existing generic vertex attributes are overwritten if their binding location collides with one of the non-NULL fixed function pipeline arrays being converted. 
    * - After this function all the fixed function pipeline arrays are set to NULL.
    */
    void convertToVertexAttribs();

    /**
     * Computes the normals in a "smooth" way, i.e. averaging the normals of those 
     * polygons that share one or more vertices.
     *
     * This function computes smoothed normals for triangle primitives and leaves 
     * unchanged the normals of line and point primitives when possible, i.e. when 
     * they don't share vertices with the polygonal primitives.
     *
     * \note 
     * This function modifies the local buffers. After calling this you might want 
     * to update the buffers allocated on the GPU.
    */
    void computeNormals(bool verbose=false);

    /** Inverts the orientation of the normals.
     *  Returns \p true if the normals could be flipped. The function fails if the normals
     *  are defined in a format other than ArrayFloat3. */
    bool flipNormals();

    //! Converts all draw calls to triangles and fixes their winding according to the Geometry's normals.
    void fixTriangleWinding();

    /** 
    * Transforms vertices and normals belonging to this geometry.
    * If 'normalize' == true the normals are normalized after being transformed
    * \note This functions supports every possible vertex format, type and layout.
    * \sa
    *  - ArrayAbstract::transform()
    *  - ArrayAbstract::normalize()
    *  - ArrayAbstract::computeBoundingSphere()
    *  - ArrayAbstract::computeBoundingBox()
    */
    void transform(const mat4&matr, bool normalize = true);

    //! Converts all the DrawCall objects bound to a Geometry into DrawArrays.
    void convertDrawCallToDrawArrays();

    //! Merges all the PT_TRIANGLE_STRIP DrawElementsUInt objects into one single PT_TRIANGLE_STRIP DrawElementsUInt.
    //! @return The DrawCall containing the merged strips or NULL of none was merged.
    DrawCall* mergeTriangleStrips();

    //! Merges all the draw calls that use the given primitive type into one single draw call using primitive restart.
    void mergeDrawCallsWithPrimitiveRestart(EPrimitiveType primitive_type);

    //! Merges all the draw calls that use the given primitive type into one single MultiDrawElements draw call.
    void mergeDrawCallsWithMultiDrawElements(EPrimitiveType primitive_type);

    //! Merges all the draw calls that use the given primitive type or PT_TRIANGLES into one single PT_TRIANGLES draw call.
    //! Use primitive_type = PT_UNKNOWN to merge all primitive types (with the obvious exclusion of lines, points and adjacency ones).
    void mergeDrawCallsWithTriangles(EPrimitiveType primitive_type);

    //! Converts PT_QUADS, PT_QUADS_STRIP and PT_POLYGON into PT_TRIANGLE primitives.
    //! @note Eventual base vertex and primitive restart are baked into the resulting triangle soup.
    void triangulateDrawCalls();

    //! Shrinks DrawElements*/DrawRangeElements*/MultiDrawElements* to the best fitting of *UInt/UShort/UByte
    //! taking into account: primitive restart, instancing and base vertex.
    //! @note Primitive type can be any.
    void shrinkDrawCalls();

    //! Calls triangulateDrawCalls(), shrinkDrawCalls() and convertToVertexAttribs().
    //! @note At the moment this method does support MultiDrawElements nor DrawRangeElements but only DrawElements.
    void makeGLESFriendly();

    //! Sorts the vertices of the geometry (position, normals, textures, colors etc.) to maximize vertex-cache coherency.
    //! This function will work only if all the DrawCalls are DrawElements* and will generate a new set of DrawElementsUInt.
    //! This function will fail if any of the DrawCalls is using primitive restart functionality.
    //! \returns true if all the DrawCall are DrawElements* and none of the DrawCalls is using primitive restart.
    bool sortVertices();

    //! Regenerates the vertex position and attributes using the given new-to-old map.
    //! Where 'map_new_to_old[i] == j' means that the i-th new vertex attribute should take it's value from the old j-th vertex attribute.
    void regenerateVertices(const std::vector<u32>& map_new_to_old);

    //! Assigns a random color to each vertex of each DrawCall object. If a vertex is shared among more than one DrawCall object its color is undefined.
    void colorizePrimitives();

    //! Computes the tangent (and optionally bitangent) vectors used to form a TBN matrix to be used for bumpmapping.
    //! @param vert_count The number of elements stored in @a vertex, @a normal, @a texcoord, @a tangent and @a bitangent.
    //! @param vertex Array containing the vertex positions.
    //! @param normal Array containing the normals of the vertices.
    //! @param texcoord Array containing the 2d texture coordinates of the bumpmap.
    //! @param primitives The triangles, quads etc. defining the geometry of the object.
    //! @param tangent [out] Returns the tangent vector of the vertices. This parameter is mandatory.
    //! @param bitangent [out] Returns the bitangent vector of the vertics. This parameter can be NULL.
    // Based on:
    // Lengyel, Eric. “Computing Tangent Space Basis Vectors for an Arbitrary Mesh”. Terathon Software 3D Graphics Library, 2001. 
    // http://www.terathon.com/code/tangent.html
    static void computeTangentSpace(
      u32 vert_count, 
      const vl::fvec3 *vertex, 
      const vl::fvec3* normal,
      const vl::fvec2 *texcoord, 
      const vl::DrawCall* primitives,
      vl::fvec3 *tangent, 
      vl::fvec3 *bitangent );

    // ------------------------------------------------------------------------
    // IVertexAttribSet Interface Implementation
    // ------------------------------------------------------------------------

    void setVertexArray(ArrayAbstract* data);

    const ArrayAbstract* vertexArray() const { return mVertexArray.get(); }

    ArrayAbstract* vertexArray() { return mVertexArray.get(); }

    void setNormalArray(ArrayAbstract* data);

    const ArrayAbstract* normalArray() const { return mNormalArray.get(); }

    ArrayAbstract* normalArray() { return mNormalArray.get(); }

    void setColorArray(ArrayAbstract* data);

    const ArrayAbstract* colorArray() const { return mColorArray.get(); }

    ArrayAbstract* colorArray() { return mColorArray.get(); }

    void setSecondaryColorArray(ArrayAbstract* data);

    const ArrayAbstract* secondaryColorArray() const { return mSecondaryColorArray.get(); }

    ArrayAbstract* secondaryColorArray() { return mSecondaryColorArray.get(); }

    void setFogCoordArray(ArrayAbstract* data);

    const ArrayAbstract* fogCoordArray() const { return mFogCoordArray.get(); }

    ArrayAbstract* fogCoordArray() { return mFogCoordArray.get(); }

    void setTexCoordArray(int tex_unit, ArrayAbstract* data);

    const ArrayAbstract* texCoordArray(int tex_unit) const
    { 
      for(int i=0; i<mTexCoordArrays.size(); ++i)
        if (mTexCoordArrays.at(i)->mTextureSampler == tex_unit)
          return mTexCoordArrays.at(i)->mTexCoordArray.get();
      return NULL;
    }

    ArrayAbstract* texCoordArray(int tex_unit) 
    { 
      for(int i=0; i<mTexCoordArrays.size(); ++i)
        if (mTexCoordArrays.at(i)->mTextureSampler == tex_unit)
          return mTexCoordArrays.at(i)->mTexCoordArray.get();
      return NULL;
    }

    int texCoordArrayCount() const { return mTexCoordArrays.size(); }

    void getTexCoordArrayAt(int i, int& out_tex_unit, const ArrayAbstract* &tex_array) const
    {
      VL_CHECK(i<mTexCoordArrays.size());
      out_tex_unit = mTexCoordArrays[i]->mTextureSampler;
      tex_array = mTexCoordArrays[i]->mTexCoordArray.get();
    }

    void setVertexAttribArray(unsigned int attrib_location, ArrayAbstract* data, bool normalize=true, EVertexAttribInterpretation data_behav=VAI_NORMAL) { setVertexAttribArray(VertexAttribInfo(attrib_location, data, normalize, data_behav)); }

    void setVertexAttribArray(const VertexAttribInfo& info);

    const VertexAttribInfo* vertexAttribArray(unsigned int attrib_location) const;

    VertexAttribInfo* vertexAttribArray(unsigned int attrib_location);

    Collection<VertexAttribInfo>* vertexAttribArrays() { return &mVertexAttribArrays; }

    const Collection<VertexAttribInfo>* vertexAttribArrays() const { return &mVertexAttribArrays; }

  protected:
    virtual void computeBounds_Implementation();
    
    virtual void render_Implementation(const Actor* actor, const Shader* shader, const Camera* camera, OpenGLContext* gl_context) const;

    // render calls
    Collection<DrawCall> mDrawCalls;

    //  --- IVertexAttribSet interface concrete implementation ---

    // conventional vertex attributes
    ref<ArrayAbstract> mVertexArray;
    ref<ArrayAbstract> mNormalArray;
    ref<ArrayAbstract> mColorArray;
    ref<ArrayAbstract> mSecondaryColorArray;
    ref<ArrayAbstract> mFogCoordArray;
    Collection<TextureArray> mTexCoordArrays;
    // generic vertex attributes
    Collection<VertexAttribInfo> mVertexAttribArrays;
  };
  //------------------------------------------------------------------------------
}

#endif
