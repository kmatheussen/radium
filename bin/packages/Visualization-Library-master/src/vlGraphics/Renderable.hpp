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

#ifndef Renderable_INCLUDE_ONCE
#define Renderable_INCLUDE_ONCE

#include <vlCore/Object.hpp>
#include <vlCore/Transform.hpp>
#include <vlCore/AABB.hpp>
#include <vlCore/Sphere.hpp>
#include <vlCore/Log.hpp>
#include <vlGraphics/OpenGL.hpp>

namespace vl
{
  //------------------------------------------------------------------------------
  // Renderable
  //------------------------------------------------------------------------------
  class Actor;
  class Shader;
  class Transform;
  class Camera;
  class OpenGLContext;
  /** An abstract class that represents all the objects that can be rendered.
    * In order to render a Renderable you have to bind it to an Actor.
    * An Actor glues together a Renderable, an Effect and eventually a Transform.
    * Note that the same Renderable can be associated to more than one Actor.
    * 
    * \sa Actor, Effect, Shader, Transform, Geometry */
  class VLGRAPHICS_EXPORT Renderable: public Object
  {
    VL_INSTRUMENT_ABSTRACT_CLASS(vl::Renderable, Object)

    Renderable(const Renderable& other): Object(other)
    {
      VL_DEBUG_SET_OBJECT_NAME()
    }

  public:
    //! Constructor
    Renderable(): mBoundsUpdateTick(0), mDisplayList(0), mBoundsDirty(true), 
                  mDisplayListEnabled(false), mDisplayListDirty(true), mBufferObjectEnabled(true), mBufferObjectDirty(true){}
    
    //! Destructor
    virtual ~Renderable() { deleteDisplayList(); }

    //! Renders the Renderable and if necessary compiles the display list and updates the BufferObjects.
    void render(const Actor* actor, const Shader* shader, const Camera* camera, OpenGLContext* gl_context)
    {
      VL_CHECK_OGL();
      
      // display list have priority over BufferObjects
      if (isDisplayListEnabled())
      {
        if ( displayListDirty() )
        {
          if ( !displayList() )
          {
            setDisplayList( glGenLists(1) ); VL_CHECK_OGL();
          }
          VL_CHECK( displayList() );
          glNewList( displayList(), GL_COMPILE_AND_EXECUTE ); VL_CHECK_OGL();
            render_Implementation( actor, shader, camera, gl_context ); VL_CHECK_OGL();
          glEndList(); VL_CHECK_OGL();
          setDisplayListDirty( false );
        }
        else
        {
          VL_CHECK( displayList() );
          glCallList( displayList() );
        }
      }
      else
      {
        // update BufferObjects
        if (isBufferObjectEnabled() && isBufferObjectDirty())
        {
          updateDirtyBufferObject(BUM_KeepRamBuffer);
          setBufferObjectDirty(false);
        }

        // render
        render_Implementation( actor, shader, camera, gl_context ); VL_CHECK_OGL();
      }
      VL_CHECK_OGL();
    }

    //! Recomputes the bounding box and bounding sphere of a Renderable.
    void computeBounds() { computeBounds_Implementation(); setBoundsDirty(false); }

    //! Returns the bounds-update-tick which is a counter incremented every time the bounding box or bounding sphere is updated.
    long long boundsUpdateTick() const { return mBoundsUpdateTick; }
    
    //! Marks the bounding box and bounding sphere as dirty in order to be recomputed at the next rendering.
    void setBoundsDirty(bool dirty) { mBoundsDirty = dirty; }
    
    //! Returns whether the bounding sphere or bounding box are "dirty", that is, meant to be recomputed.
    bool boundsDirty() const { return mBoundsDirty; }
    
    //! Sets the bounding box of a Renderable.
    void setBoundingBox( const AABB& aabb )
    { 
      if (mAABB != aabb) 
      { 
        mAABB = aabb; 
        ++mBoundsUpdateTick; 
      } 
      setBoundsDirty(false); 
    }
    
    //! Sets the bounding sphere of a Renderable.
    void setBoundingSphere( const Sphere& sphere) 
    { 
      if (mSphere != sphere) 
      {
        mSphere = sphere; 
        ++mBoundsUpdateTick; 
      }
      setBoundsDirty(false); 
    }
    
    //! Returns the bounding box of a Renderable without recomputing the bounds if dirty.
    const AABB& boundingBox() const 
    { 
      if (boundsDirty())
        vl::Log::warning("Renderable::boundingBox() returning dirty bounding box, call computeBounds() first or call boundingBox() from a non-const Renderable!\n");
      return mAABB; 
    }
    
    //! Returns the bounding sphere of a Renderable without recomputing the bounds if dirty.
    const Sphere& boundingSphere() const 
    { 
      if (boundsDirty())
        vl::Log::warning("Renderable::boundingSphere() returning dirty bounding sphere, call computeBounds() first or call boundingSphere() from a non-const Renderable!\n");
      return mSphere; 
    }

    //! Returns the bounding box of a Renderable recomputing the bounds if dirty.
    const AABB& boundingBox() 
    { 
      if (boundsDirty())
        computeBounds();
      return mAABB; 
    }
    
    //! Returns the bounding sphere of a Renderable recomputing the bounds if dirty.
    const Sphere& boundingSphere() 
    { 
      if (boundsDirty())
        computeBounds();
      return mSphere; 
    }

    //! Returns the display list associated to a Renderable or 0 (zero) if no display list is associated.
    unsigned int displayList() const { return mDisplayList; }
    
    //! Manually assciates a display list to a Renderable (to be used with care).
    void setDisplayList(unsigned int disp_list) { mDisplayList = disp_list; }

    //! Returns \p true if display lists are enabled for a Renderable (disabled by default).
    bool isDisplayListEnabled() const { return mDisplayListEnabled; }

    //! Enable/disable display lists (disabled by default).
    void setDisplayListEnabled(bool enabled) { mDisplayListEnabled = enabled; }

    //! Whether the display list associated to a Renderable should be recompiled at the next rendering.
    bool displayListDirty() const { return mDisplayListDirty; }
    
    //! Whether the display list associated to a Renderable should be recompiled at the next rendering.
    void setDisplayListDirty(bool dirty) { mDisplayListDirty = dirty; }

    //! Returns \p true if BufferObject (vertex buffer object) are enabled for a Renderable (enabled by default).
    bool isBufferObjectEnabled() const { return mBufferObjectEnabled; }

    //! Enable/disable BufferObject (vertex buffer object) (enabled by default).
    void setBufferObjectEnabled(bool enabled) { mBufferObjectEnabled = enabled; }

    //! Whether BufferObjects associated to a Renderable should be recomputed on the next rendering.
    bool isBufferObjectDirty() const { return mBufferObjectDirty; }

    //! Whether BufferObjects associated to a Renderable should be recomputed on the next rendering.
    void setBufferObjectDirty(bool dirty) { mBufferObjectDirty = dirty; }

    //! Uploads the data stored in the local buffers on the GPU memory.
    //! If 'discard_local_data' is set to \p true the memory used by the local buffers is released.
    virtual void updateDirtyBufferObject(EBufferObjectUpdateMode) = 0;

    //! Destroys the BufferObject (vertex buffer objects) associated to this a Renderable.
    //! \note This function does not touch the local (non GPU) data stored in the buffers associated to the vertex attributes and DrawCall.
    virtual void deleteBufferObject() = 0;

    //! Deletes the display list currently associated to a Renderable.
    void deleteDisplayList() 
    {
      if (displayList())
        glDeleteLists(displayList(), 1);
      mDisplayList = 0;
    }

  protected:
    virtual void computeBounds_Implementation() = 0;
    virtual void render_Implementation(const Actor* actor, const Shader* shader, const Camera* camera, OpenGLContext* gl_context) const = 0;

  private:
    long long mBoundsUpdateTick;
    unsigned int mDisplayList;
    bool mBoundsDirty;
    bool mDisplayListEnabled;
    bool mDisplayListDirty;
    bool mBufferObjectEnabled;
    bool mBufferObjectDirty;
    AABB mAABB;
    Sphere mSphere;
  };
}

#endif
