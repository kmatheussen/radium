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

#ifndef Object_INCLUDE_ONCE
#define Object_INCLUDE_ONCE

#include <vlCore/checks.hpp>
#include <vlCore/IMutex.hpp>
#include <vlCore/TypeInfo.hpp>
#include <string>

#if VL_DEBUG_LIVING_OBJECTS
  #include <set>
#endif

namespace vl
{
  template<class T> class ref;

  //------------------------------------------------------------------------------
  // Object
  //------------------------------------------------------------------------------
  /**
   * The base class for all the reference counted objects.
   * See also vl::ref.
  */
  class VLCORE_EXPORT Object
  {
    VL_INSTRUMENT_BASE_CLASS(vl::Object)

  public:
    //! Constructor.
    Object()
    {
      VL_DEBUG_SET_OBJECT_NAME()
      mRefCountMutex = NULL;
      mReferenceCount = 0;
      mAutomaticDelete = true;
      // user data
      #if VL_OBJECT_USER_DATA
        mUserData = NULL;
      #endif
      #if VL_DEBUG_LIVING_OBJECTS
        debug_living_objects()->insert(this);
        // mDebug_LivingObjects.insert(this);
      #endif
    }

    //! Copy constructor: copies the name, ref count mutex and user data.
    Object(const Object& other)
    {
      // copy the name, the ref count mutex and the user data.
      mObjectName = other.mObjectName;
      mRefCountMutex = other.mRefCountMutex;
      #if VL_OBJECT_USER_DATA
        mUserData = other.mUserData;
      #endif

      // mReferenceCount and mAutomaticDelete are not copiable.
      mReferenceCount  = 0;
      mAutomaticDelete = true;

      // debug living object
      #if VL_DEBUG_LIVING_OBJECTS
        debug_living_objects()->insert(this);
      #endif
    }

    //! Copy operator: copies the object's name, ref count mutex and user data.
    Object& operator=(const Object& other) 
    { 
      // copy the name, the ref count mutex and the user data.
      mObjectName = other.mObjectName;
      mRefCountMutex = other.mRefCountMutex;
      #if VL_OBJECT_USER_DATA
        mUserData = other.mUserData;
      #endif

      // mReferenceCount and mAutomaticDelete are not copiable.
      // ...

      return *this;
    }

    //! The name of the object, by default set to the object's class name.
    const std::string& objectName() const { return mObjectName; }

    //! The name of the object, by default set to the object's class name in debug builds.
    void setObjectName(const char* name) { mObjectName = name; }

    //! The mutex used to protect the reference counting of an Object across multiple threads.
    void setRefCountMutex(IMutex* mutex) { mRefCountMutex = mutex; }
    
    //! The mutex used to protect the reference counting of an Object across multiple threads.
    IMutex* refCountMutex() { return mRefCountMutex; }
    
    //! The mutex used to protect the reference counting of an Object across multiple threads.
    const IMutex* refCountMutex() const { return mRefCountMutex; }

    //! Returns the number of references of an object.
    int referenceCount() const 
    { 
      return mReferenceCount; 
    }

    //! Increments the reference count of an object.
    void incReference() const
    {
      // Lock mutex
      if (refCountMutex())
        const_cast<IMutex*>(refCountMutex())->lock();

      ++mReferenceCount;
      
      // Unlock mutex
      if(refCountMutex())
        const_cast<IMutex*>(refCountMutex())->unlock();
    }

    //! Decrements the reference count of an object and deletes it if both automaticDelete() is \p true the count reaches 0.
    void decReference()
    {
      // Save local copy in case of deletion.
      IMutex* mutex = mRefCountMutex;

      // Lock mutex.
      if (mutex)
        mutex->lock();

      VL_CHECK(mReferenceCount)
      --mReferenceCount;
      if (mReferenceCount == 0 && automaticDelete())
        delete this;

      // Unlock mutex.
      if (mutex)
        mutex->unlock();
    }

    //! If set to true the Object is deleted when its reference count reaches 0
    void setAutomaticDelete(bool autodel_on) { mAutomaticDelete = autodel_on; }

    //! If set to true the Object is deleted when its reference count reaches 0
    bool automaticDelete() const { return mAutomaticDelete; }

    //! Casts an Object to the specified class.
    template<class T>
    T* as() { return cast<T>(this); }

    //! Casts an Object to the specified class.
    template<class T>
    const T* as() const { return cast<const T>(this); }

#if VL_OBJECT_USER_DATA
  public:
    void* userData() { return mUserData; }
    const void* userData() const { return mUserData; }
    void setUserData(void* user_data) { mUserData = user_data; }

  private:
    void* mUserData;
#endif

  protected:
    virtual ~Object();
    std::string mObjectName;

    IMutex* mRefCountMutex;
    mutable int mReferenceCount;
    bool mAutomaticDelete;

  // debugging facilities

  public:
  #if VL_DEBUG_LIVING_OBJECTS
    static std::set< Object* >* mDebug_LivingObjects;
    static std::set< Object* >* debug_living_objects() 
    { 
      if (!mDebug_LivingObjects)
        mDebug_LivingObjects = new std::set< Object* >;
      return mDebug_LivingObjects;
    }
  #endif
  };
  //------------------------------------------------------------------------------
  // ref
  //------------------------------------------------------------------------------
  /**
   * The ref<> class is used to reference-count an Object.
   * When the last ref<> that points to an Object is deallocated also the pointed Object is deallocated.
   * @note IMPORTANT: assigning to a ref<> 'washes aways' the constness of an object.
   */
  template<class T>
  class ref
  {
  public:
    // 'const' is required as the copy constructor must have this signature.
    ref(const ref& other)
    {
      mObject = NULL;
      *this = other;
    }

    ref(const T* object=NULL)
    {
      mObject = const_cast<T*>(object);
      if (mObject)
        mObject->incReference();
    }

    template<class T2> ref(const ref<T2>& other)
    {
      mObject = NULL;
      *this = other;
    }

    ~ref() 
    {
      if (mObject)
        mObject->decReference();
      mObject = NULL;
    }

    // 'const' is required because operator= must have this signature.
    ref& operator=(const ref& other)
    {
      if (other)
        other->incReference();
      if (mObject)
        mObject->decReference();
      mObject = const_cast<T*>(other.get());
      return *this;
    }

    // 'const' is required because operator= must have this signature.
    ref& operator=(const T* other)
    {
      if (other)
        other->incReference();
      if (mObject)
        mObject->decReference();
      mObject = const_cast<T*>(other);
      return *this;
    }

    // 'const' is required because operator= must have this signature.
    template<class T2> ref& operator=(const ref<T2>& other)
    {
      if (other)
        other->incReference();
      if (mObject)
        mObject->decReference();
      mObject = const_cast<T2*>(other.get());
      return *this;
    }

    void swap(ref& other)
    {
      T* tmp = other.mObject;
      other = mObject; 
      mObject = tmp;
    }

    //! This is mainly useful when using ref<> with std::map, std::set, etc.
    T* get_writable() const { return mObject; }

    const T* get() const { return mObject; }
    const T* operator->() const { VL_CHECK(mObject); return mObject; }
    const T& operator*() const { VL_CHECK(mObject); return *mObject; }

    T* get() { return mObject; }
    T* operator->() { VL_CHECK(mObject); return mObject; }
    T& operator*() { VL_CHECK(mObject); return *mObject; }

    bool operator<(const ref& other) const { return mObject < other.mObject; }

    operator bool() const { return mObject != NULL; }

  protected:
    T* mObject;
  };
  // interaction with the other types
  template<class T1, class T2> inline bool operator==(const ref<T1> & o1, const ref<T2> & o2) { return o1.get() == o2.get(); }
  template<class T1, class T2> inline bool operator!=(const ref<T1> & o1, const ref<T2> & o2) { return o1.get() != o2.get(); }
  template<class T1, class T2> inline bool operator==(const ref<T1> & o1, T2 * o2) { return o1.get() == o2; }
  template<class T1, class T2> inline bool operator!=(const ref<T1> & o1, T2 * o2) { return o1.get() != o2; }
  template<class T1, class T2> inline bool operator==(T1 * o1, const ref<T2> & o2) { return o1 == o2.get(); }
  template<class T1, class T2> inline bool operator!=(T1 * o1, const ref<T2> & o2) { return o1 != o2.get(); }
}

#endif
