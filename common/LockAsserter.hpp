
#ifndef RADIUM_COMMON_LOCKASSERTER_HPP
#define RADIUM_COMMON_LOCKASSERTER_HPP


namespace radium{

class LockAsserter{

  DEFINE_ATOMIC(int, number_of_readers) = 0;
  DEFINE_ATOMIC(int, number_of_writers) = 0;

  LockAsserter(const LockAsserter&) = delete;
  LockAsserter& operator=(const LockAsserter&) = delete;

  const char *writer_track = NULL;
  DEFINE_ATOMIC(const char *, reader_track) = NULL;

public:
  
  LockAsserter(){
  }
  
  struct Exclusive {
    LockAsserter *lockAsserter;

    Exclusive(LockAsserter *lockAsserter_b, const char *new_writer_track)
      : lockAsserter(lockAsserter_b)
    {
      int num_writers = ATOMIC_GET(lockAsserter->number_of_writers);
      int num_readers = ATOMIC_GET(lockAsserter->number_of_readers);

      if(num_writers > 0)
        RError("Exclusive: NUM_WRITERS>0. writers: %d. readers: %d (Used by: %s)", num_writers, num_readers, lockAsserter->writer_track);
      if(num_readers > 0)
        RError("Exclusive: NUM_REASDERS>0: writers: %d. readers: %d (Used by: %s)", num_writers, num_readers, ATOMIC_GET(lockAsserter->reader_track));

      lockAsserter->writer_track = new_writer_track;
      ATOMIC_ADD(lockAsserter->number_of_writers, 1);
    }
    
    ~Exclusive(){
      ATOMIC_ADD(lockAsserter->number_of_writers, -1);
    }
  };

  struct Shared {
    LockAsserter *lockAsserter;

    Shared(LockAsserter *lockAsserter_b, const char *new_reader_track)
      : lockAsserter(lockAsserter_b)
    {
      int num_writers = ATOMIC_GET(lockAsserter->number_of_writers);
      if(num_writers > 0)
        RError("Shared: NUM_WRITERS>0: %d. (Used by: %s)", num_writers, lockAsserter->writer_track);

      ATOMIC_SET_RELAXED(lockAsserter->reader_track, new_reader_track);
      ATOMIC_ADD(lockAsserter->number_of_readers, 1);
    }
    
    ~Shared(){
      ATOMIC_ADD(lockAsserter->number_of_readers, -1);
    }
  };
};
 


#define LOCKASSERTER_EXCLUSIVE(a) radium::LockAsserter::Exclusive _exclusive___(  const_cast<radium::LockAsserter*>(a), CR_FORMATEVENT("LockWriter")  )
#define LOCKASSERTER_SHARED(a)    radium::LockAsserter::Shared    _shared___(     const_cast<radium::LockAsserter*>(a), CR_FORMATEVENT("LockReader")  )

#if defined(RELEASE)
#define LOCKASSERTER_EXCLUSIVE_NON_RELEASE(a)
#define LOCKASSERTER_SHARED_NON_RELEASE(a)
#else
#define LOCKASSERTER_EXCLUSIVE_NON_RELEASE(a) radium::LockAsserter::Exclusive _exclusive___(  const_cast<radium::LockAsserter*>(a), CR_FORMATEVENT("LockWriter")  )
#define LOCKASSERTER_SHARED_NON_RELEASE(a)    radium::LockAsserter::Shared    _shared___(     const_cast<radium::LockAsserter*>(a), CR_FORMATEVENT("LockReader")  )
#endif
  
}
#endif
