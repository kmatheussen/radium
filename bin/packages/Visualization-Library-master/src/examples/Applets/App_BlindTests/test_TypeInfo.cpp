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

//-----------------------------------------------------------------------------

#include <vlCore/Object.hpp>
#include <cstdio>
#include <set>

using namespace vl;

namespace ns
{
  // >>> NO INHERITANCE FROM INSTRUMENTED BASE CLASS
  class Base
  {
    VL_INSTRUMENT_BASE_CLASS(Base)
  };

  // >>> SIMPLE INHERITANCE OF INSTRUMENTED CLASS
  class ClassA: public virtual Base
  {
    VL_INSTRUMENT_CLASS(ns::ClassA, Base)
  };

  // >>> SIMPLE INHERITANCE OF INSTRUMENTED CLASS
  class ClassB: public virtual Base
  {
    VL_INSTRUMENT_CLASS(ns::ClassB, Base)
  };

  // >>> SIMPLE INHERITANCE OF INSTRUMENTED CLASS
  class ClassC: public Base
  {
    VL_INSTRUMENT_CLASS(ns::ClassC, Base)
  };

  // >>> MULTIPLE INHERITANCE
  class ClassAB: public ClassA, public ClassB
  {
    VL_INSTRUMENT_CLASS_2(ns::ClassAB, ClassA, ClassB)
  };

  // >>> TEMPLATE CLASSES WITH MORE THAN 1 PARAMS
  template<class T1, class T2>
  class ClassT: public Base
  {
    VL_INSTRUMENT_CLASS(VL_GROUP(ns::ClassT<class T1, class T2>), Base)
  };

  // >>> SUBCLASSES OF TEMPLATES WITH MORE THAN 1 PARAMS
  class ClassSubT: public ClassT<int, float>
  {
    VL_INSTRUMENT_CLASS(ns::ClassSubT, VL_GROUP(ClassT<int, float>))
  };
}

#define CHECK_CONDITION(CONDITION) if (!(CONDITION)) return false;

namespace blind_tests
{
  bool test_TypeInfo()
  {
    ns::ClassA A;
    ns::ClassB B;
    ns::ClassC C;
    ns::ClassAB AB, AB2;
    ns::ClassSubT SubT;

    ns::ClassA* pA = NULL;
    ns::ClassB* pB = NULL;
    const ns::Base* pBase = NULL;

    pBase = &AB;

    // exact type
    CHECK_CONDITION( SubT.classType() == ns::ClassSubT::Type() )
    CHECK_CONDITION( pBase->classType() == AB2.classType() )
    // template up cast
    CHECK_CONDITION( SubT.isOfType(ns::ClassT<int, float>::Type()) )
    // up cast
    CHECK_CONDITION( SubT.isOfType(ns::Base::Type()) )
    // non related
    CHECK_CONDITION( !SubT.isOfType(ns::ClassA::Type()) )
    // multiple inheritance
    CHECK_CONDITION( AB.isOfType(ns::ClassA::Type()) )
    CHECK_CONDITION( AB.isOfType(ns::ClassB::Type()) )
    CHECK_CONDITION( AB.isOfType(ns::Base::Type()) )
    CHECK_CONDITION( !AB.isOfType(ns::ClassSubT::Type()) )
    // downcast from pBase class
    CHECK_CONDITION( pBase->isOfType(ns::ClassAB::Type()) )
    CHECK_CONDITION( pBase->isOfType(ns::ClassA::Type()) )
    CHECK_CONDITION( pBase->isOfType(ns::ClassB::Type()) )
    CHECK_CONDITION( pBase->isOfType(ns::Base::Type()) )
    CHECK_CONDITION( !pBase->isOfType(ns::ClassSubT::Type()) )

    // CHECK_CONDITION( vl::cast<ns::ClassA>(&B) == NULL ) // compilation error, Ok.
    // CHECK_CONDITION( vl::cast<ns::ClassB>(&A) == NULL ) // compilation error, Ok.
    CHECK_CONDITION( vl::cast<ns::ClassA>(&AB) != NULL )
    CHECK_CONDITION( vl::cast<ns::ClassB>(&AB) != NULL )

    pBase = &C;
    CHECK_CONDITION( vl::cast_const<ns::ClassC>(pBase) != NULL )
    CHECK_CONDITION( vl::cast_const<ns::ClassSubT>(pBase) == NULL )

    pBase = &A;
    // CHECK_CONDITION( vl::cast<ns::ClassA>(pBase) != NULL ) // conversion from a virtual pBase class forbidden by C++, requires dynamic_cast, kind of Ok.
    // CHECK_CONDITION( vl::cast<ns::ClassB>(pBase) == NULL ) // conversion from a virtual pBase class forbidden by C++, requires dynamic_cast, kind of Ok.

    pA = &AB;
    pB = &AB;
    CHECK_CONDITION( vl::cast<ns::ClassAB>(pA) != NULL )
    CHECK_CONDITION( vl::cast<ns::ClassAB>(pB) != NULL )

    return true;
  }
}

//-----------------------------------------------------------------------------
