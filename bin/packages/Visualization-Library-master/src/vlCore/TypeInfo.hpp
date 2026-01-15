/**************************************************************************************/
/*                                                                                    */
/*  Visualization Library                                                             */
/*  http://www.visualizationlibrary.org                                               */
/*                                                                                    */
/*  Copyright (c) 2005-2011, Michele Bosi                                             */
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

#ifndef TypInfo_INCLUDE_ONCE
#define TypInfo_INCLUDE_ONCE

#include <vlCore/MurmurHash3.hpp>

namespace vl
{
  /**
   * \file TypeInfo.hpp
   * Set of macros and templates implementing a simple and portable RTTI system.
  */

  //---------------------------------------------------------------------------------------------------------------------
  //! Represents a class type.
  struct TypeInfo
  {
    //! Constructor.
    TypeInfo(const char* name): mName(name)
    {
      // compute string length
      const char* ptr = name;
      while( *ptr ) ++ptr;
      vl::MurmurHash3_x86_32(name, (int)(ptr - name), 0, &mHash);
      // printf("--- --- TypeInfo : %s = %x\n", name, mHash);
    }

    //! Equal operator
    bool operator==(const TypeInfo& other) const { return mHash == other.mHash; }

    //! Less operator
    bool operator<(const  TypeInfo& other) const { return mHash < other.mHash; }

    //! The name of the class including the namespace.
    const char* name() const { return mName; }

    //! The 32 bit hash of the name of the class including the namespace.
    u32 hash() const { return mHash; }

  private:
    // we could also use u32 mHash[4] and MurmurHash3_x86_128() for extra safety.
    u32 mHash;
    const char* mName;
  };
}
//---------------------------------------------------------------------------------------------------------------------
#define VL_GROUP(...) __VA_ARGS__
#define VL_TO_STR(...) #__VA_ARGS__
//---------------------------------------------------------------------------------------------------------------------
#define VL_INSTRUMENT_BASE_CLASS(ClassName)                                                                                \
public:                                                                                                                    \
  /* static functions */                                                                                                   \
  /** Returns the name of the class. */                                                                                    \
  static const char* Name() { return VL_TO_STR(ClassName); }                                                               \
  /** Returns the TypeInfo of the class. */                                                                                \
  static const ::vl::TypeInfo& Type() { static const ::vl::TypeInfo class_type(VL_TO_STR(ClassName)); return class_type; } \
                                                                                                                           \
  /* virtual functions */                                                                                                  \
  /** Returns the name of the object's class. */                                                                           \
  virtual const char* className() const { return VL_TO_STR(ClassName); }                                                   \
  /** Returns the TypeInfo of the object's class. */                                                                       \
  virtual const ::vl::TypeInfo& classType() const { return Type(); }                                                       \
  /** Returns \a true if \a type matches the object's class type. */                                                       \
  virtual bool isOfType(const ::vl::TypeInfo& type) const                                                                  \
  {                                                                                                                        \
    return type == Type();                                                                                                 \
  }                                                                                                                        \
  /* virtual Object* createThisType() const { return new ClassName; }                                              */      \
private:
//---------------------------------------------------------------------------------------------------------------------
#define VL_INSTRUMENT_ABSTRACT_BASE_CLASS(ClassName)                                                                       \
public:                                                                                                                    \
  /* static functions */                                                                                                   \
  /** Returns the name of the class. */                                                                                    \
  static const char* Name() { return VL_TO_STR(ClassName); }                                                               \
  /** Returns the TypeInfo of the class. */                                                                                \
  static const ::vl::TypeInfo& Type() { static const ::vl::TypeInfo class_type(VL_TO_STR(ClassName)); return class_type; } \
                                                                                                                           \
  /* virtual functions */                                                                                                  \
  /** Returns the name of the object's class. */                                                                           \
  virtual const char* className() const { return VL_TO_STR(ClassName); }                                                   \
  /** Returns the TypeInfo of the object's class. */                                                                       \
  virtual const ::vl::TypeInfo& classType() const { return Type(); }                                                       \
  /** Returns \a true if \a type matches the object's class type. */                                                       \
  virtual bool isOfType(const ::vl::TypeInfo& type) const                                                                  \
  {                                                                                                                        \
    return type == Type();                                                                                                 \
  }                                                                                                                        \
  /* virtual Object* createThisType() const = 0;                                                                   */      \
private:
//---------------------------------------------------------------------------------------------------------------------
#define VL_INSTRUMENT_CLASS(ClassName, BaseClass)                                                                          \
private:                                                                                                                   \
  typedef BaseClass super;                                                                                                 \
public:                                                                                                                    \
  /* static functions */                                                                                                   \
  /** Returns the name of the class. */                                                                                    \
  static const char* Name() { return VL_TO_STR(ClassName); }                                                               \
  /** Returns the TypeInfo of the class. */                                                                                \
  static const ::vl::TypeInfo& Type() { static const ::vl::TypeInfo class_type(VL_TO_STR(ClassName)); return class_type; } \
                                                                                                                           \
  /* virtual functions */                                                                                                  \
  /** Returns the name of the object's class. */                                                                           \
  virtual const char* className() const { return VL_TO_STR(ClassName); }                                                   \
  /** Returns the TypeInfo of the object's class. */                                                                       \
  virtual const ::vl::TypeInfo& classType() const { return Type(); }                                                       \
  /** Returns \a true if \a type matches the object's class type. */                                                       \
  virtual bool isOfType(const ::vl::TypeInfo& type) const                                                                  \
  {                                                                                                                        \
    return type == Type() || super::isOfType(type);                                                                        \
  }                                                                                                                        \
  /* virtual Object* createThisType() const { return new ClassName; }                                              */      \
private:
//---------------------------------------------------------------------------------------------------------------------
#define VL_INSTRUMENT_ABSTRACT_CLASS(ClassName, BaseClass)                                                                 \
private:                                                                                                                   \
  typedef BaseClass super;                                                                                                 \
public:                                                                                                                    \
  /* static functions */                                                                                                   \
  /** Returns the name of the class. */                                                                                    \
  static const char* Name() { return VL_TO_STR(ClassName); }                                                               \
  /** Returns the TypeInfo of the class. */                                                                                \
  static const ::vl::TypeInfo& Type() { static const ::vl::TypeInfo class_type(VL_TO_STR(ClassName)); return class_type; } \
                                                                                                                           \
  /* virtual functions */                                                                                                  \
  /** Returns the name of the object's class. */                                                                           \
  virtual const char* className() const { return VL_TO_STR(ClassName); }                                                   \
  /** Returns the TypeInfo of the object's class. */                                                                       \
  virtual const ::vl::TypeInfo& classType() const { return Type(); }                                                       \
  /** Returns \a true if \a type matches the object's class type. */                                                       \
  virtual bool isOfType(const ::vl::TypeInfo& type) const                                                                  \
  {                                                                                                                        \
    return type == Type() || super::isOfType(type);                                                                        \
  }                                                                                                                        \
  /* virtual Object* createThisType() const = 0;                                                                   */      \
private:
//---------------------------------------------------------------------------------------------------------------------
#define VL_INSTRUMENT_CLASS_2(ClassName, BaseClass1, BaseClass2)                                                           \
private:                                                                                                                   \
  typedef BaseClass1 super1;                                                                                               \
  typedef BaseClass2 super2;                                                                                               \
public:                                                                                                                    \
  /* static functions */                                                                                                   \
  /** Returns the name of the class. */                                                                                    \
  static const char* Name() { return VL_TO_STR(ClassName); }                                                               \
  /** Returns the TypeInfo of the class. */                                                                                \
  static const ::vl::TypeInfo& Type() { static const ::vl::TypeInfo class_type(VL_TO_STR(ClassName)); return class_type; } \
                                                                                                                           \
  /* virtual functions */                                                                                                  \
  /** Returns the name of the object's class. */                                                                           \
  virtual const char* className() const { return VL_TO_STR(ClassName); }                                                   \
  /** Returns the TypeInfo of the object's class. */                                                                       \
  virtual const ::vl::TypeInfo& classType() const { return Type(); }                                                       \
  /** Returns \a true if \a type matches the object's class type. */                                                       \
  virtual bool isOfType(const ::vl::TypeInfo& type) const                                                                  \
  {                                                                                                                        \
    return type == Type() || super1::isOfType(type) || super2::isOfType(type);                                             \
  }                                                                                                                        \
  /* virtual Object* createThisType() const { return new ClassName; }                                              */      \
private:
//---------------------------------------------------------------------------------------------------------------------
#define VL_INSTRUMENT_ABSTRACT_CLASS_2(ClassName, BaseClass1, BaseClass2)                                                  \
private:                                                                                                                   \
  typedef BaseClass1 super1;                                                                                               \
  typedef BaseClass2 super2;                                                                                               \
public:                                                                                                                    \
  /* static functions */                                                                                                   \
  /** Returns the name of the class. */                                                                                    \
  static const char* Name() { return VL_TO_STR(ClassName); }                                                               \
  /** Returns the TypeInfo of the class. */                                                                                \
  static const ::vl::TypeInfo& Type() { static const ::vl::TypeInfo class_type(VL_TO_STR(ClassName)); return class_type; } \
                                                                                                                           \
  /* virtual functions */                                                                                                  \
  /** Returns the name of the object's class. */                                                                           \
  virtual const char* className() const { return VL_TO_STR(ClassName); }                                                   \
  /** Returns the TypeInfo of the object's class. */                                                                       \
  virtual const ::vl::TypeInfo& classType() const { return Type(); }                                                       \
  /** Returns \a true if \a type matches the object's class type. */                                                       \
  virtual bool isOfType(const ::vl::TypeInfo& type) const                                                                  \
  {                                                                                                                        \
    return type == Type() || super1::isOfType(type) || super2::isOfType(type);                                             \
  }                                                                                                                        \
  /* virtual Object* createThisType() const = 0;                                                                   */      \
private:
//---------------------------------------------------------------------------------------------------------------------
namespace vl
{
  template<class B, class A>
  B* cast(A* obj)
  {
    if(obj && obj->isOfType(B::Type()))
      return static_cast<B*>(obj);
    else
      return NULL;
  }
  //---------------------------------------------------------------------------------------------------------------------
  template<class B, class A>
  const B* cast_const(const A* obj) // need rename to cast_const for GCC
  {
    if(obj && obj->isOfType(B::Type()))
      return static_cast<const B*>(obj);
    else
      return NULL;
  }
}
//---------------------------------------------------------------------------------------------------------------------

//---------------------------------------------------------------------------------------------------------------------
// USAGE EXAMPLES 
//---------------------------------------------------------------------------------------------------------------------

/***

namespace ns
{
  // NO INHERITANCE FROM INSTRUMENTED BASE CLASS
  class Base
  {
    VL_INSTRUMENT_BASE_CLASS(Base)
  };

  // SIMPLE INHERITANCE OF INSTRUMENTED CLASS
  class ClassA: public virtual Base
  {
    VL_INSTRUMENT_CLASS(ns::ClassA, Base)
  };

  // SIMPLE INHERITANCE OF INSTRUMENTED CLASS
  class ClassB: public virtual Base
  {
    VL_INSTRUMENT_CLASS(ns::ClassB, Base)
  };

  // MULTIPLE INHERITANCE
  class ClassAB: public ClassA, public ClassB
  {
    VL_INSTRUMENT_CLASS_2(ns::ClassAB, ClassA, ClassB)
  };

  // TEMPLATE CLASSES WITH MORE THAN 1 PARAMS
  template<class T1, class T2>
  class ClassT: public Base
  {
    VL_INSTRUMENT_CLASS(VL_GROUP(ns::ClassT<T1, T2>), Base)
  };

  // SUBCLASSES OF TEMPLATES WITH MORE THAN 1 PARAMS
  class ClassSubT: public ClassT<int, float>
  {
    VL_INSTRUMENT_CLASS(ns::ClassSubT, VL_GROUP(ClassT<int, float>))
  };
}

IMPORTANT NOTE:
  - The "ClassName" parameter of VL_INSTRUMENT_* should ALWAYS specify the full NAMESPACE.
  - The "BaseClass" parameter of VL_INSTRUMENT_* should not specify the namespace unless strictly necessary.

--- dynamic casting example ---

ns::ClassAB AB;
ns::ClassA* pA = &AB;
ns::ClassB* pB = &AB;
assert( vl::cast<ns::ClassAB>(pA)   != NULL )
assert( vl::cast<ns::ClassAB>(pB)   != NULL )
assert( vl::cast<ns::ClassSubT>(pA) == NULL )

or

assert( pA->as<ns::ClassAB>()   != NULL )
assert( pB->as<ns::ClassAB>()   != NULL )
assert( pA->as<ns::ClassSubT>() == NULL )

NOTE THAT UNLIKE dynamic_cast<> AND static_cast<> WE USE:

  vl::cast<ns::ClassAB>(pB)

INSTEAD OF:

  vl::cast<ns::ClassAB*>(pB)

REMARKS:
  
  There is no need to say that, similarly to dynamic_cast<>, one should avoid using VL TypeInfo facilities
  inside "fast" loops otherwise they won't be that "fast" anymore!

***/

#endif
