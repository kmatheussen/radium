
#ifndef RADIUM_COMMON_LOCKASSERTER_HPP
#define RADIUM_COMMON_LOCKASSERTER_HPP


namespace radium{

class LockAsserter{

  DEFINE_ATOMIC(int, number_of_readers);
  DEFINE_ATOMIC(int, number_of_writers);

  LockAsserter(const LockAsserter&) = delete;
  LockAsserter& operator=(const LockAsserter&) = delete;

public:
  
  LockAsserter(){
    ATOMIC_SET(number_of_readers, 0); // stupid c/c++
    ATOMIC_SET(number_of_writers, 0); // stupid c/c++
  }
  
  struct Exclusive {
    LockAsserter *lockAsserter;

    Exclusive(LockAsserter *lockAsserter)
      : lockAsserter(lockAsserter)
    {
      R_ASSERT(ATOMIC_GET(lockAsserter->number_of_readers)==0);
      R_ASSERT(ATOMIC_GET(lockAsserter->number_of_writers)==0);
      
      ATOMIC_ADD(lockAsserter->number_of_writers, 1);
    }
    
    ~Exclusive(){
      ATOMIC_ADD(lockAsserter->number_of_writers, -1);
    }
  };

  struct Shared {
    LockAsserter *lockAsserter;

    Shared(LockAsserter *lockAsserter)
      : lockAsserter(lockAsserter)
    {
      R_ASSERT(ATOMIC_GET(lockAsserter->number_of_writers)==0);
      
      ATOMIC_ADD(lockAsserter->number_of_readers, 1);
    }
    
    ~Shared(){
      ATOMIC_ADD(lockAsserter->number_of_readers, -1);
    }
  };
};
 


#define LOCKASSERTER_EXCLUSIVE(a) radium::LockAsserter::Exclusive _exclusive___(  const_cast<LockAsserter*>(a)  )
#define LOCKASSERTER_SHARED(a)    radium::LockAsserter::Shared    _shared___(     const_cast<LockAsserter*>(a)  )

}
#endif
