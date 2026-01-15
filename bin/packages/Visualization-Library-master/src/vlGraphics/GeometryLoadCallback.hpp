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

#ifndef GeometryLoadCallback_INCLUDE_ONCE
#define GeometryLoadCallback_INCLUDE_ONCE

#include <vlGraphics/Geometry.hpp>
#include <vlGraphics/TriangleStripGenerator.hpp>
#include <vlGraphics/DoubleVertexRemover.hpp>
#include <vlCore/LoadWriterManager.hpp>

namespace vl
{
  /**
   * Defines a set of actions to be executed to a Geometry as soon as it is loaded.
   */
  class GeometryLoadCallback: public LoadCallback
  {
    VL_INSTRUMENT_CLASS(vl::GeometryLoadCallback, LoadCallback)

  public:
    GeometryLoadCallback()
    {
      mMakeGLESFriendly    = false;
      mComputeNormals    = true;
      mUseBufferObjects           = true;
      mUseDisplayLists   = false;
      mTransformGeometry = false;
      mDiscardOriginalNormals = false;
      mRemoveDoubles          = false;
      mSortVertices           = false;
      mStripfy                = false;
      mConvertToDrawArrays    = false;
    }

    void operator()(ResourceDatabase* db)
    {
      if (stripfy())
        setRemoveDoubles(true);

      std::vector< vl::ref<vl::Geometry> > geom;
      db->get<Geometry>(geom);
      for(unsigned int i=0; i<geom.size(); ++i)
      {
        if (discardOriginalNormals())
          geom[i]->setNormalArray(NULL);

        if (computeNormals() && !geom[i]->normalArray())
          geom[i]->computeNormals();

        if (removeDoubles())
          DoubleVertexRemover().removeDoubles(geom[i].get());

        if (sortVertices())
          geom[i]->sortVertices();

        if (stripfy())
          TriangleStripGenerator().stripfy(geom[i].get(), 22, true, false, true);

        if (convertToDrawArrays())
          geom[i]->convertDrawCallToDrawArrays();

        geom[i]->setDisplayListEnabled(useDisplayLists());
        geom[i]->setBufferObjectEnabled(useBufferObjects());

        if (transformGeometry())
          geom[i]->transform(transformMatrix(),true);

        if (makeGLESFriendly())
          geom[i]->makeGLESFriendly();
      }
    }

    //! Discards the original normals
    bool discardOriginalNormals() const { return mDiscardOriginalNormals; }
    //! Discards the original normals
    void setDiscardOriginalNormals(bool on) { mDiscardOriginalNormals = on; }

    //! Compute normals if not present
    bool computeNormals() const { return mComputeNormals; }
    //! Compute normals if not present
    void setComputeNormals(bool cn) { mComputeNormals = cn; }

    //! Remove duplicated vertices
    bool removeDoubles() const { return mRemoveDoubles; }
    //! Remove duplicated vertices
    void setRemoveDoubles(bool rd) { mRemoveDoubles = rd; }

    //! Sorts the mesh's vertices for better performances
    void setSortVertices(bool on) { mSortVertices = on; }
    //! Sorts the mesh's vertices for better performances
    bool sortVertices() const { return mSortVertices; }

    //! Convert mesh into a set of triangle strips if possible
    void setStripfy(bool on) { mStripfy = on; }
    //! Convert mesh into a set of triangle strips if possible
    bool stripfy() const { return mStripfy; }

    //! Converts the Geometry DrawCall into DrawArrays. Useful in conjuction with \p setStripfy(true).
    bool convertToDrawArrays() const { return mConvertToDrawArrays; }
    //! Converts the Geometry DrawCall into DrawArrays. Useful in conjuction with \p setStripfy(true).
    void setConvertToDrawArrays(bool on) { mConvertToDrawArrays = on; }

    //! Enable display lists usage (overrides BufferObjects)
    void setUseDisplayLists(bool on) { mUseDisplayLists = on; }
    //! Enable display lists usage (overrides BufferObjects)
    bool useDisplayLists() const { return mUseDisplayLists; }

    //! Enable BufferObject usage if display lists are disabled
    void setUseBufferObjects(bool on) { mUseBufferObjects = on; }
    //! Enable BufferObject usage if display lists are disabled
    bool useBufferObjects() const { return mUseBufferObjects; }

    const mat4& transformMatrix() const { return mMatrix; }
    void setTransformMatrix(const mat4& m) { mMatrix = m; }

    //! Transforms the Geometries using transformMatrix().
    bool transformGeometry() const { return mTransformGeometry; }
    //! Transforms the Geometries using transformMatrix().
    void setTransformGeometry(bool on) { mTransformGeometry = on; }

    //! If true calls Geometry::makeGLESFriendly()
    bool makeGLESFriendly() const { return mMakeGLESFriendly; }
    //! If true calls Geometry::makeGLESFriendly()
    void setMakeGLESFriendly(bool on) { mMakeGLESFriendly = on; }

  protected:
    mat4 mMatrix;
    bool mTransformGeometry;
    bool mDiscardOriginalNormals;
    bool mComputeNormals;
    bool mRemoveDoubles;
    bool mSortVertices;
    bool mStripfy;
    bool mConvertToDrawArrays;
    bool mUseDisplayLists;
    bool mUseBufferObjects;
    bool mMakeGLESFriendly;
  };
}

#endif
