
#ifndef RADIUM_COMMON_LOCKASSERTER_HPP
#define RADIUM_COMMON_LOCKASSERTER_HPP

#include <QAtomicInt>


namespace radium{

class LockAsserter{

  QAtomicInt number_of_readers;
  QAtomicInt number_of_writers;

private:
  LockAsserter(const LockAsserter&);
  LockAsserter& operator=(const LockAsserter&);

public:
  
  LockAsserter(){}
  
  struct Exclusive {
    LockAsserter *lockAsserter;

    Exclusive(LockAsserter *lockAsserter)
      : lockAsserter(lockAsserter)
    {
      R_ASSERT(int(lockAsserter->number_of_readers)==0);
      R_ASSERT(int(lockAsserter->number_of_writers)==0);
      
      lockAsserter->number_of_writers.ref();
    }
    
    ~Exclusive(){
      lockAsserter->number_of_writers.deref();
    }
  };

  struct Shared {
    LockAsserter *lockAsserter;

    Shared(LockAsserter *lockAsserter)
      : lockAsserter(lockAsserter)
    {
      R_ASSERT(int(lockAsserter->number_of_writers)==0);
      
      lockAsserter->number_of_readers.ref();
    }
    
    ~Shared(){
      lockAsserter->number_of_readers.deref();
    }
  };
};
 


#define LOCKASSERTER_EXCLUSIVE(a) radium::LockAsserter::Exclusive _exclusive___(  const_cast<LockAsserter*>(a)  )
#define LOCKASSERTER_SHARED(a)    radium::LockAsserter::Shared    _shared___(     const_cast<LockAsserter*>(a)  )

}
#endif
