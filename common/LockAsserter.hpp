
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
      int num_writers = ATOMIC_GET(lockAsserter->number_of_writers);
      int num_readers = ATOMIC_GET(lockAsserter->number_of_readers);

      if(num_writers > 0)
        RError("Exclusive: NUM_WRITERS>0. writers: %d. readers: %d", num_writers, num_readers);
      if(num_readers > 0)
        RError("Exclusive: NUM_REASDERS>0: writers: %d. readers: %d", num_writers, num_readers);
      
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
      int num_writers = ATOMIC_GET(lockAsserter->number_of_writers);
      if(num_writers > 0)
        RError("Shared: NUM_WRITERS>0: %d", num_writers);
      
      ATOMIC_ADD(lockAsserter->number_of_readers, 1);
    }
    
    ~Shared(){
      ATOMIC_ADD(lockAsserter->number_of_readers, -1);
    }
  };
};
 


#define LOCKASSERTER_EXCLUSIVE(a) radium::LockAsserter::Exclusive _exclusive___(  const_cast<radium::LockAsserter*>(a)  )
#define LOCKASSERTER_SHARED(a)    radium::LockAsserter::Shared    _shared___(     const_cast<radium::LockAsserter*>(a)  )

}
#endif
