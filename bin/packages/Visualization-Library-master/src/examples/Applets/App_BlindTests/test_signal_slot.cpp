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

#include <vlCore/SignalSlot.hpp>
#include <vlCore/Object.hpp>
#include <vlCore/Log.hpp>
#include <vlCore/Say.hpp>
#include <vlCore/checks.hpp>

using namespace vl;

#define CONDITION(cond)                                \
  if (!(cond))                                         \
  {                                                    \
    vl::Log::print( Say("%s %n: condition \""#cond"\" failed.\n") << __FILE__ << __LINE__); \
    return false;                                      \
  }

namespace blind_tests
{
  class MyClassA: public vl::Object
  {
  public:
    MyClassA(): mValue1(0), mValue2(0) {}
    ~MyClassA() {}

    void reset() { mValue1 = 0; mValue2 = 0; }

    virtual int event_invocation1(int ev_type, void*)
    {
      mValue1 += ev_type + 1;
      return 0;
    }

    virtual int event_invocation2(int ev_type, void*)
    {
      mValue2 += ev_type + 2;
      return 0;
    }

    int mValue1;
    int mValue2;
  };

  class MyClassB: public MyClassA
  {
  public:
    virtual int event_invocation1(int ev_type, void*)
    {
      mValue1 += ev_type + 10;
      return 0;
    }

    virtual int event_invocation2(int ev_type, void*)
    {
      mValue2 += ev_type + 20;
      return 0;
    }
  };

  bool test_signal_slot()
  {
    // this calls B's event_invocation1/2 implementation via A's virtual function!
    ref<MyClassA> A = new MyClassB;
    Slot2<MyClassA, int, void*> slot_a1(A.get(), &MyClassA::event_invocation1);
    Slot2<MyClassA, int, void*>* slot_a2 = new Slot2<MyClassA, int, void*>(A.get(), &MyClassA::event_invocation2);

    // this calls directly B's event_invocation1/2 implementation.
    ref<MyClassB> B = new MyClassB;
    Slot2<MyClassA, int, void*> slot_b1(B.get(), &MyClassA::event_invocation1);
    Slot2<MyClassA, int, void*> slot_b2(B.get(), &MyClassA::event_invocation2);

    // our signals
    Signal2<int, void*> sigX;
    Signal2<int, void*>* sigY = new Signal2<int, void*>;

    sigX.connect(slot_a1);
    sigX.connect(*slot_a2);
    // test both connected
    sigX.emit_event(100, NULL); CONDITION(A->mValue1 == 110); CONDITION(A->mValue2 == 120);
    // test signal::disconnect_all()
    A->reset();
    sigX.disconnect_all();
    sigX.emit_event(100, NULL); CONDITION(A->mValue1 == 0); CONDITION(A->mValue2 == 0);
    // test signal::disconnect_slot() and idempotent connection
    A->reset();
    sigX.connect(slot_a1);
    sigX.connect(slot_a1);
    sigX.connect(*slot_a2);
    sigX.connect(*slot_a2);
    sigX.disconnect_slot(slot_a1);
    sigX.emit_event(100, NULL); CONDITION(A->mValue1 == 0); CONDITION(A->mValue2 == 120);
    // test signal::disconnect_object() and idempotent connection
    A->reset();
    B->reset();
    sigX.connect(slot_a1);
    sigX.connect(slot_b1);
    sigX.connect(*slot_a2);
    sigX.connect(slot_b2);
    sigX.disconnect_object(A.get());
    sigX.emit_event(100, NULL); CONDITION(A->mValue1 == 0); CONDITION(A->mValue2 == 0); CONDITION(B->mValue1 == 110); CONDITION(B->mValue2 == 120);
    // test slot::disconnect_signal() and multiple signals
    A->reset();
    B->reset();
    sigX.connect(slot_a1);
    sigX.connect(slot_b1);
    sigX.connect(*slot_a2);
    sigX.connect(slot_b2);
    sigY->connect(slot_a1);
    sigY->connect(slot_b1);
    sigY->connect(*slot_a2);
    sigY->connect(slot_b2);
    slot_a2->disconnect_signal(&sigX);
    sigX.emit_event(100, NULL); CONDITION(A->mValue1 == 110); CONDITION(A->mValue2 == 0); CONDITION(B->mValue1 == 110); CONDITION(B->mValue2 == 120);
    sigY->emit_event(200, NULL); CONDITION(A->mValue1 == 320); CONDITION(A->mValue2 == 220); CONDITION(B->mValue1 == 320); CONDITION(B->mValue2 == 340);
    // test slot::disconnect_all and multiple signals
    A->reset();
    B->reset();
    sigX.connect(slot_a1);
    sigX.connect(slot_b1);
    sigX.connect(*slot_a2);
    sigX.connect(slot_b2);
    sigY->connect(slot_a1);
    sigY->connect(slot_b1);
    sigY->connect(*slot_a2);
    sigY->connect(slot_b2);
    slot_a2->disconnect_all();
    sigX.emit_event(100, NULL); CONDITION(A->mValue1 == 110); CONDITION(A->mValue2 == 0); CONDITION(B->mValue1 == 110); CONDITION(B->mValue2 == 120);
    sigY->emit_event(200, NULL); CONDITION(A->mValue1 == 320); CONDITION(A->mValue2 == 0); CONDITION(B->mValue1 == 320); CONDITION(B->mValue2 == 340);
    // test slot destruction and multiple signals
    A->reset();
    B->reset();
    sigX.connect(slot_a1);
    sigX.connect(slot_b1);
    sigX.connect(*slot_a2);
    sigX.connect(slot_b2);
    sigY->connect(slot_a1);
    sigY->connect(slot_b1);
    sigY->connect(*slot_a2);
    sigY->connect(slot_b2);
    delete slot_a2; slot_a2 = NULL;
    sigX.emit_event(100, NULL); CONDITION(A->mValue1 == 110); CONDITION(A->mValue2 == 0); CONDITION(B->mValue1 == 110); CONDITION(B->mValue2 == 120);
    sigY->emit_event(200, NULL); CONDITION(A->mValue1 == 320); CONDITION(A->mValue2 == 0); CONDITION(B->mValue1 == 320); CONDITION(B->mValue2 == 340);
    // test signal destruction and multiple signals
    A->reset();
    B->reset();
    slot_a2 = new Slot2<MyClassA, int, void*>(A.get(), &MyClassA::event_invocation2);
    sigX.connect(slot_a1);
    sigX.connect(slot_b1);
    sigX.connect(*slot_a2);
    sigX.connect(slot_b2);
    sigY->connect(slot_a1);
    sigY->connect(slot_b1);
    sigY->connect(*slot_a2);
    sigY->connect(slot_b2);
    delete sigY; sigY = NULL;
    sigX.emit_event(100, NULL); CONDITION(A->mValue1 == 110); CONDITION(A->mValue2 == 120); CONDITION(B->mValue1 == 110); CONDITION(B->mValue2 == 120);

    // change target object and method
    Slot2<MyClassA, int, void*> slot_c(A.get(), &MyClassA::event_invocation2);
    slot_c.setTargetObject(B.get());
    slot_c.setMethod(&MyClassA::event_invocation1);

    return true;
  }

}

