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

#ifndef Transform_INCLUDE_ONCE
#define Transform_INCLUDE_ONCE

#include <vlCore/vlnamespace.hpp>
#include <vlCore/Object.hpp>
#include <vlCore/Matrix4.hpp>
#include <vector>
#include <set>
#include <algorithm>

namespace vl
{
  class Camera;

  //------------------------------------------------------------------------------
  // Transform
  //------------------------------------------------------------------------------
  /** Implements a 4x4 matrix transform used to define the position and orientation of an Actor.
    *
    * Transforms can be linked together to create a tree-like hierarchy.
    *
    * \par Optimizing Your Transforms
    *
    * - Don't use them unless strictly necessary: if an Actor uses an I (identity) transform you do not need one, just call \p vl::Actor::setTransform(NULL).
    *
    * - Don't create Transform hierarchies if not needed. Just call setLocalAndWorldMatrix() per-Transform if you have a set of parent-less Transforms.
    *
    * - If the root of a hierarchy is an I (identity) transform, let VL know it by calling \p setAssumeIdentityWorldMatrix(true). This will save
    *   unnecessary matrix multiplications when calling computeWorldMatrix() / computeWorldMatrixRecursive().
    *
    * - Call computeWorldMatrix() / computeWorldMatrixRecursive() not at each frame but only if the local matrix has actually changed.
    *
    * - Do not add a Transform hierarchy to vl::Rendering::transform() if such Transforms are not animated every frame. 
    *
    * - Remember: VL does not require your Actors to have a Transform or such Transforms to be part of any hierarchy, it just expect that the 
    *   worldMatrix() of an Actor's Transform (if it has any) is up to date at rendering time. How and when they are updated can be fine 
    *   tuned by the user according to the specific application needs.
    *
    * \sa setLocalAndWorldMatrix(), setAssumeIdentityWorldMatrix(), Rendering::transform()
    * \sa Actor, Rendering, Effect, Renderable, Geometry */
  class VLCORE_EXPORT Transform: public Object
  {
    VL_INSTRUMENT_CLASS(vl::Transform, Object)

  public:
    /** Constructor. */
    Transform(): mWorldMatrixUpdateTick(0), mAssumeIdentityWorldMatrix(false), mParent(NULL)
    {
      VL_DEBUG_SET_OBJECT_NAME()

      #if VL_TRANSFORM_USER_DATA 
        mTransformUserData = NULL;
      #endif
    }

    /** Constructor. The \p matrix parameter is used to set both the local and world matrix. */
    Transform(const mat4& matrix): mWorldMatrixUpdateTick(0), mAssumeIdentityWorldMatrix(false), mParent(NULL)
    { 
      VL_DEBUG_SET_OBJECT_NAME()

      #if VL_TRANSFORM_USER_DATA 
        mTransformUserData = NULL;
      #endif

      setLocalMatrix(matrix);
      setWorldMatrix(matrix);
    }

    /** Destructor. */
    ~Transform();

    /** Returns the parent Transform. */
    const Transform* parent() const { return mParent; }
    
    /** Returns the parent Transform. */
    Transform* parent() { return mParent; }

    /** Utility function equivalent to \p setLocalMatrix( mat4::getTranslation(x,y,z)*localMatrix() ).
      * After calling this you might want to call computeWorldMatrix() or computeWorldMatrixRecursive(). */
    void translate(real x, real y, real z);
    
    /** Utility function equivalent to \p setLocalMatrix( mat4::getTranslation(t)*localMatrix() ).
      * After calling this you might want to call computeWorldMatrix() or computeWorldMatrixRecursive(). */
    void translate(const vec3& t);
    
    /** Utility function equivalent to \p setLocalMatrix( mat4::getScaling(x,y,z)*localMatrix() ).
      * After calling this you might want to call computeWorldMatrix() or computeWorldMatrixRecursive(). */
    void scale(real x, real y, real z);
    
    /** Utility function equivalent to \p setLocalMatrix( mat4::getRotation(degrees,x,y,z)*localMatrix() ).
      * After calling this you might want to call computeWorldMatrix() or computeWorldMatrixRecursive(). */
    void rotate(real degrees, real x, real y, real z);
    
    /** Utility function equivalent to \p setLocalMatrix( mat4::getRotation(from,to)*localMatrix() ).
      * After calling this you might want to call computeWorldMatrix() or computeWorldMatrixRecursive(). */
    void rotate(const vec3& from, const vec3& to);

    /** Utility function equivalent to \p setLocalMatrix( m*localMatrix() ).
      * After calling this you might want to call computeWorldMatrix() or computeWorldMatrixRecursive(). */
    void preMultiply(const mat4& m);

    /** Utility function equivalent to \p setLocalMatrix( localMatrix()*m ).
      * After calling this you might want to call computeWorldMatrix() or computeWorldMatrixRecursive(). */
    void postMultiply(const mat4& m);

    /** The matrix representing the transform's local space.
      * After calling this you might want to call computeWorldMatrix() or computeWorldMatrixRecursive(). */
    void setLocalMatrix(const mat4& m)
    { 
      mLocalMatrix = m;
    }

    /** The matrix representing the transform's local space. */
    const mat4& localMatrix() const
    { 
      return mLocalMatrix;
    }

    /** Normally you should not use directly this function, call it only if you are sure you cannot do otherwise. 
      * Usually you want to call computeWorldMatrix() or computeWorldMatrixRecursive().
      * Calling this function will also increment the worldMatrixUpdateTick(). */
    void setWorldMatrix(const mat4& matrix) 
    { 
      mWorldMatrix = matrix; 
      ++mWorldMatrixUpdateTick;
    }

    /** Returns the world matrix used for rendering. */
    const mat4& worldMatrix() const 
    { 
      return mWorldMatrix;
    }

    /** Sets both the local and the world matrices.
      * This function is useful to quickly set those Transforms that do not have a parent, for which
      * is equivalent to: \p setLocalMatrix(matrix); \p computeWorldMatrix(NULL); */
    void setLocalAndWorldMatrix(const mat4& matrix)
    { 
      mLocalMatrix = matrix;
      setWorldMatrix(matrix);
    }

    /** Returns the internal update tick used to avoid unnecessary computations. The world matrix thick 
      * gets incremented every time the setWorldMatrix() or setLocalAndWorldMatrix() functions are called. */
    long long worldMatrixUpdateTick() const { return mWorldMatrixUpdateTick; }

    /** If set to true the world matrix of this transform will always be considered and identity.
      * Is usually used to save calculations for top Transforms with many sub-Transforms. */
    void setAssumeIdentityWorldMatrix(bool assume_I) { mAssumeIdentityWorldMatrix = assume_I; }

    /** If set to true the world matrix of this transform will always be considered and identity.
      * Is usually used to save calculations for top Transforms with many sub-Transforms. */
    bool assumeIdentityWorldMatrix() { return mAssumeIdentityWorldMatrix; }

    /** Computes the world matrix by concatenating the parent's world matrix with its own local matrix. */
    virtual void computeWorldMatrix(Camera* /*camera*/ = NULL)
    {
      if( assumeIdentityWorldMatrix() )
      {
        setWorldMatrix(mat4()); 
      }
      else
      /* top Transforms are usually assumeIdentityWorldMatrix() == true for performance reasons */
      if( parent() && !parent()->assumeIdentityWorldMatrix() )
      {
        setWorldMatrix( parent()->worldMatrix() * localMatrix() );
      }
      else
        setWorldMatrix( localMatrix() );
    }

    /** Computes the world matrix by concatenating the parent's world matrix with its local matrix, recursively descending to the children. */
    void computeWorldMatrixRecursive(Camera* camera = NULL)
    {
      computeWorldMatrix(camera);
      for(size_t i=0; i<mChildren.size(); ++i)
        mChildren[i]->computeWorldMatrixRecursive(camera);
    }

    /** Returns the matrix computed concatenating this Transform's local matrix with the local matrices of all its parents. */
    mat4 getComputedWorldMatrix()
    {
      mat4 world = localMatrix();
      Transform* par = parent();
      while(par)
      {
        world = par->localMatrix() * world;
        par = par->parent();
      }
      return world;
    }

    // --- --- children management --- ---

    /** Returns the array containing the child Transforms. */
    const ref<Transform>* children() const { if (mChildren.empty()) return NULL; else return &mChildren[0]; }

    /** Returns the array containing the child Transforms. */
    ref<Transform>* children() { if (mChildren.empty()) return NULL; else return &mChildren[0]; }

    /** Returns the number of child Transform. */
    size_t childrenCount() const { return mChildren.size(); }

    /** Adds a child transform. */
    void addChild(Transform* child)
    {
      VL_CHECK(child != NULL)
      VL_CHECK(child->mParent == NULL)

      mChildren.push_back(child);
      child->mParent = this;
    }
    
    /** Adds \p count children transforms. */
    void addChildren(Transform*const* children, size_t count)
    {
      VL_CHECK(children != NULL)

      if (count)
      {
        size_t insert_point = mChildren.size();
        mChildren.resize(mChildren.size() + count);
        vl::ref<Transform>* ptr = &mChildren[insert_point];
        for(size_t i=0; i<count; ++i, ++ptr)
        {
          VL_CHECK(children[i]->mParent == NULL);
          children[i]->mParent = this;
          (*ptr) = children[i];
        }
      }
    }

    void addChildren(const ref<Transform>* children, size_t count)
    {
      VL_CHECK(children != NULL)

      if (count)
      {
        size_t insert_point = mChildren.size();
        mChildren.resize(mChildren.size() + count);
        vl::ref<Transform>* ptr = &mChildren[insert_point];
        for(size_t i=0; i<count; ++i)
        {
          VL_CHECK(children[i]->mParent == NULL);
          ptr[i] = children[i];
          ptr[i]->mParent = this;
        }
      }
    }

    void addChildren(const std::vector< ref<Transform> >& children)
    {
      if (children.size())
        addChildren( &children[0], children.size() );
    }

    /** Adds the specified \p children transforms. */
    void addChildren(const std::vector< Transform* >& children)
    {
      if (children.size())
        addChildren( &children[0], children.size() );
    }

    /** Sets the \p index-th child. */
    void setChild(int index, Transform* child)
    {
      VL_CHECK(child)
      VL_CHECK( index < (int)mChildren.size() )
      mChildren[index]->mParent = NULL;
      mChildren[index] = child;
      mChildren[index]->mParent = this;
    }

    /** Returns the last child. */
    const Transform* lastChild() const { return mChildren.back().get(); }

    /** Returns the last child. */
    Transform* lastChild() { return mChildren.back().get(); }

    /** Removes the given \p child Transform. */
    void eraseChild(const Transform* child)
    {
      VL_CHECK(child != NULL)
      std::vector< ref<Transform> >::iterator it;
      it = std::find(mChildren.begin(), mChildren.end(), child);
      VL_CHECK(it != mChildren.end())
      if (it != mChildren.end())
      {
        (*it)->mParent = NULL;
        mChildren.erase(it);
      }
    }

    /** Removes \p count children Transforms starting at position \p index. */
    void eraseChildren(int index, int count)
    {
      VL_CHECK( index + count <= (int)mChildren.size() );

      for(int j=index; j<index+count; ++j)
        mChildren[j]->mParent = NULL;

      for(int i=index+count, j=index; i<(int)mChildren.size(); ++i, ++j)
        mChildren[j] = mChildren[i];

      mChildren.resize( mChildren.size() - count );
    }    

    /** Removes all the children of a Transform. */
    void eraseAllChildren()
    {
      for(int i=0; i<(int)mChildren.size(); ++i)
        mChildren[i]->mParent = NULL;
      mChildren.clear();
    }

    /** Removes all the children of a Transform recursively descending the hierarchy. */
    void eraseAllChildrenRecursive()
    {
      for(int i=0; i<(int)mChildren.size(); ++i)
      {
        mChildren[i]->eraseAllChildrenRecursive();
        mChildren[i]->mParent = NULL;
      }
      mChildren.clear();
    }

    /** Disassembles a hierarchy of Transforms like eraseAllChildrenRecursive() does plus assigns the local matrix to equal the world matrix. */
    void flattenHierarchy()
    {
      for(int i=0; i<(int)mChildren.size(); ++i)
      {
        mChildren[i]->setLocalAndWorldMatrix( mChildren[i]->worldMatrix() );
        mChildren[i]->eraseAllChildrenRecursive();
        mChildren[i]->mParent = NULL;
      }
      mChildren.clear();
    }

    /** Erases a Transform from it's parent and sets the local matrix to be equal to the world matrix. */
    void removeFromParent()
    {
      if (parent())
      {
        mParent->eraseChild(this);
        setLocalMatrix( worldMatrix() );
      }
    }

    /** Minimizes the amount of memory used to store the children Transforms. */
    void shrink()
    {
      std::vector< ref<Transform> > tmp (mChildren);
      mChildren.swap(tmp);
    }

    /** Minimizes recursively the amount of memory used to store the children Transforms. */
    void shrinkRecursive()
    {
      shrink();
      for(size_t i=0; i<mChildren.size(); ++i)
        mChildren[i]->shrinkRecursive();
    }

    /** Reserves space for \p count children. This function is very useful when you need to add one by one a large number of children transforms. 
      * \note This function does not affect the number of children, it only reallocates the buffer used to store them 
      * so that it's large enough to \p eventually contain \p count of them. This will make subsequent calls to addChild() quicker
      * as fewer or no reallocations of the buffer will be needed. */
    void reserveChildren(size_t count) { mChildren.reserve(count); }

    /** Checks whether there are duplicated entries in the Transform's children list. */
    bool hasDuplicatedChildren() const
    {
      std::set<const Transform*> tr_set;
      for(size_t i=0; i<mChildren.size(); ++i)
        tr_set.insert( mChildren[i].get() );
      return tr_set.size() != mChildren.size();
    }

#if VL_TRANSFORM_USER_DATA 
  public:
    void setTransformUserData(void* data) { mTransformUserData = data; }
    const void* transformUserData() const { return mTransformUserData; }
    void* transformUserData() { return mTransformUserData; }

  private:
    void* mTransformUserData;
#endif

  protected:
    mat4 mLocalMatrix;
    mat4 mWorldMatrix;
    long long mWorldMatrixUpdateTick;
    bool mAssumeIdentityWorldMatrix;
    std::vector< ref<Transform> > mChildren;
    Transform* mParent;
  };

}

#endif
