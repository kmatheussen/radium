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
// BaseSlot*
//-----------------------------------------------------------------------------
template<VL_T_PAR_TYPENAME>
class VL_T_SIGNAL_NAME;

//! This is actually Slot1, Slot2, Slot3, etc.
template<VL_T_PAR_TYPENAME>
class VL_T_BASE_SLOT_NAME
{
  template<VL_T_PAR_TYPENAME2> friend class VL_T_SIGNAL_NAME;

  // incoming signals
  std::vector< VL_T_SIGNAL_NAME<VL_T_PAR_FORMAL_LIST>* > m_sigs;

  int m_rank;

  int m_trigger_count;

  void erase_signal(const VL_T_SIGNAL_NAME<VL_T_PAR_FORMAL_LIST>* sig)
  {
    for(size_t i=0; i<m_sigs.size(); ++i)
    {
      if( m_sigs[i] == sig )
      {
        m_sigs.erase(m_sigs.begin()+i);
        break;
      }
    }
    VL_CHECK( std::find(m_sigs.begin(), m_sigs.end(), sig) == m_sigs.end() );
  }

  VL_T_BASE_SLOT_NAME(const VL_T_BASE_SLOT_NAME&): m_rank(0), m_trigger_count(-1) { }

  VL_T_BASE_SLOT_NAME& operator=(const VL_T_BASE_SLOT_NAME&) { return *this; };

public:
  //! Constructor
  VL_T_BASE_SLOT_NAME(): m_rank(0), m_trigger_count(-1) {}

  //! Destructor
  virtual ~VL_T_BASE_SLOT_NAME() { disconnect_all(); }

  //! The object targeted by this slot.
  virtual const void* target_object() const  = 0;

  //! For internal use only.
  virtual int event_slot(VL_T_PAR_FORMAL_LIST) = 0;

  //! Disconnects all the signals from a slot.
  void disconnect_all();

  //! Disconnects the specified signal from a slot.
  void disconnect_signal(VL_T_SIGNAL_NAME<VL_T_PAR_FORMAL_LIST>* sig);

  //! Rank used to define the invocation order of the slot, the higher the rank the sooner the slot is called.
  void set_rank(int rank) { m_rank = rank; }

  //! Rank used to define the invocation order of the slot, the higher the rank the sooner the slot is called.
  int rank() const { return m_rank; }

  //! Number of times the slot can be called before it auto disconnects from all signals, -1 means forever, i.e. no auto disconnection will take place.
  void set_trigger_count(int count) { m_trigger_count = count; }

  //! Number of times the slot can be called before it auto disconnects from all signals, -1 means forever, i.e. no auto disconnection will take place.
  int trigger_count() const { return m_trigger_count; }
};

//-----------------------------------------------------------------------------
// Signal*
//-----------------------------------------------------------------------------
//! This is actually Signal1, Signal2, Signal3, etc.
template<VL_T_PAR_TYPENAME>
class VL_T_SIGNAL_NAME
{
  std::vector< VL_T_BASE_SLOT_NAME<VL_T_PAR_FORMAL_LIST>* > m_slots;

  static bool is_less(const VL_T_BASE_SLOT_NAME<VL_T_PAR_FORMAL_LIST>* a, const VL_T_BASE_SLOT_NAME<VL_T_PAR_FORMAL_LIST>* b)
  {
    return a->rank() < b->rank();
  }

public:
  virtual ~VL_T_SIGNAL_NAME() { disconnect_all(); }

  //! Calls all the connected signals in the order specified by their VL_T_BASE_SLOT_NAME::rank() value.
  //! If a slot returns a value other than 0 the subsequent slots are not called. This mechanism
  //! allows a simplified form of chain-of-responsibility. The signal returns the value returned by
  //! the last slot or -1 if no slot was connected to the signal.
  int emit_event(VL_T_PAR_LIST)
  {
    int res = -1;
    std::sort( m_slots.begin(), m_slots.end(), is_less);
    for( int i=(int)m_slots.size(); i--; )
    {
      // call only slots with trigger count != 0
      if ( m_slots[i]->m_trigger_count )
        res = m_slots[i]->event_slot(VL_T_PAR_CALL);
      // if counted trigger count then count down, -1 means infinite trigger count
      if( m_slots[i]->m_trigger_count > 0 )
        m_slots[i]->m_trigger_count--;
      // slots with trigger count == 0 are always removed.
      if ( m_slots[i]->m_trigger_count == 0 )
        disconnect_slot( *m_slots[i] );
      if (res)
        break;
    }
    return res;
  }

  //! Connect a slot to a signal
  void connect(VL_T_BASE_SLOT_NAME<VL_T_PAR_FORMAL_LIST>& slot)
  {
    if ( std::find(m_slots.begin(), m_slots.end(), &slot) == m_slots.end() )
    {
      slot.m_sigs.push_back(this);
      m_slots.push_back(&slot);
    }
  }

  //! Disconnects all the slots connected to a signal
  void disconnect_all()
  {
    for(int i=(int)m_slots.size(); i--; )
      m_slots[i]->erase_signal(this);
    m_slots.clear();
  }

  //! Disconnects a slot to a signal
  void disconnect_slot(const VL_T_BASE_SLOT_NAME<VL_T_PAR_FORMAL_LIST>& slot)
  {
    typename std::vector< VL_T_BASE_SLOT_NAME<VL_T_PAR_FORMAL_LIST>* >::iterator it = std::find(m_slots.begin(), m_slots.end(), &slot);
    if( it != m_slots.end() )
    {
      (*it)->erase_signal(this);
      m_slots.erase(it);
    }
    VL_CHECK(std::find(m_slots.begin(), m_slots.end(), &slot) == m_slots.end())
  }

  //! Disconnects all the slots connected to a signal which target the specified \a object.
  void disconnect_object(const void* object)
  {
    for(int i=(int)m_slots.size(); i--; )
    {
      if (m_slots[i]->target_object() == object)
      {
        m_slots[i]->erase_signal(this);
        m_slots.erase(m_slots.begin()+i);
      }
    }
  }
};

//-----------------------------------------------------------------------------
// BaseSlot*
//-----------------------------------------------------------------------------
template <VL_T_PAR_TYPENAME>
void VL_T_BASE_SLOT_NAME<VL_T_PAR_FORMAL_LIST>::disconnect_all()
{
  while(!m_sigs.empty())
    m_sigs.back()->disconnect_slot(*this);
}

template <VL_T_PAR_TYPENAME>
void VL_T_BASE_SLOT_NAME<VL_T_PAR_FORMAL_LIST>::disconnect_signal(VL_T_SIGNAL_NAME<VL_T_PAR_FORMAL_LIST>* sig)
{
  sig->disconnect_slot(*this);
  VL_CHECK( std::find(m_sigs.begin(), m_sigs.end(), sig) == m_sigs.end() );
}

//-----------------------------------------------------------------------------
// Slot*
//-----------------------------------------------------------------------------
//! This is actually Slot1, Slot2, Slot3, etc.
template<class T_class, VL_T_PAR_TYPENAME>
class VL_T_SLOT_NAME: public VL_T_BASE_SLOT_NAME<VL_T_PAR_FORMAL_LIST>
{
public:
  typedef int (T_class::*method_type)(VL_T_PAR_FORMAL_LIST);

public:
  VL_T_SLOT_NAME(): m_obj(NULL), m_method(NULL) {}

  VL_T_SLOT_NAME(T_class* obj, method_type method): m_obj(obj), m_method(method) {}

  void bind(T_class* obj, method_type method)
  {
    m_obj = obj;
    m_method = method;
  }

  const void* target_object() const { return m_obj; }

  int event_slot(VL_T_PAR_LIST)
  {
    VL_CHECK(m_obj);
    VL_CHECK(m_method);
    return (m_obj->*m_method)(VL_T_PAR_CALL);
  }

  void setTargetObject(T_class* target) { m_obj = target; }

  void setMethod(method_type method) { m_method = method; }

private:
  T_class* m_obj;
  method_type m_method;
};

//-----------------------------------------------------------------------------

#undef VL_T_PAR_TYPENAME
#undef VL_T_PAR_TYPENAME2
#undef VL_T_PAR_FORMAL_LIST
#undef VL_T_PAR_LIST       
#undef VL_T_PAR_CALL
#undef VL_T_SIGNAL_NAME    
#undef VL_T_BASE_SLOT_NAME 
#undef VL_T_SLOT_NAME      
