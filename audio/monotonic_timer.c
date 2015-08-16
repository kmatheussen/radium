// Copyright 2013 Alex Reece.
//
// A cross platform monotonic timer.

#include <unistd.h>

#define NANOS_PER_SECF 1000000000.0
#define USECS_PER_SEC 1000000

#include <stdint.h>



// CCALL/INITIALIZER macros picked up from http://stackoverflow.com/questions/1113409/attribute-constructor-equivalent-in-vc
#ifdef _MSC_VER

#define INITIALIZER(f) \
  static void __cdecl f(void);                                     \
  __declspec(allocate(".CRT$XCU")) void (__cdecl*f##_)(void) = f;  \
  static void __cdecl f(void)

#elif defined(__GNUC__)

#define CCALL
#define INITIALIZER(f)                               \
  static void f(void) __attribute__((constructor));  \
  static void f(void)

#endif



#if _POSIX_TIMERS > 0 && defined(_POSIX_MONOTONIC_CLOCK)
  // If we have it, use clock_gettime and CLOCK_MONOTONIC.

  #include <time.h>

  double monotonic_seconds() {
    struct timespec time;
    // Note: Make sure to link with -lrt to define clock_gettime.
    clock_gettime(CLOCK_MONOTONIC, &time);
    return ((double) time.tv_sec) + ((double) time.tv_nsec / (NANOS_PER_SECF));
  }

#elif defined(__APPLE__)
  // If we don't have CLOCK_MONOTONIC, we might be on a Mac. There we instead
  // use mach_absolute_time().

  #include <mach/mach_time.h>

  static mach_timebase_info_data_t info;
  static void __attribute__((constructor)) init_info() {
    mach_timebase_info(&info);
  }

  double monotonic_seconds() {
    uint64_t time = mach_absolute_time();
    double dtime = (double) time;
    dtime *= (double) info.numer;
    dtime /= (double) info.denom;
    return dtime / NANOS_PER_SECF;
  }

#elif defined(_WIN32)
  // On Windows, use QueryPerformanceCounter and QueryPerformanceFrequency.

  #include <windows.h>
  #include <stdio.h>
  #define NEED_RDTSC 1
  static inline double monotonic_rdtsc_seconds();

  static double PCFreq = 0.0;
  static int has_qpc = 0;

  // According to http://stackoverflow.com/q/1113409/447288, this will
  // make this function a constructor.
  // TODO(awreece) Actually attempt to compile on windows.


    INITIALIZER(init_pcfreq){
            
    // Accoring to http://stackoverflow.com/a/1739265/447288, this will
    // properly initialize the QueryPerformanceCounter.
    LARGE_INTEGER li;
    has_qpc = QueryPerformanceFrequency(&li);
    if (has_qpc) {
      PCFreq = ((double) li.QuadPart) / 1000.0;
    } else {
      fprintf(stderr, "Warning: OS doesnt support the QueryPerformanceCounter function. Using tsc instead for timing.\n");
    }    
  }


  double monotonic_seconds() {
    if (has_qpc) {
      LARGE_INTEGER li;
      QueryPerformanceCounter(&li);
      return ((double) li.QuadPart) / PCFreq;
    } else
      return monotonic_rdtsc_seconds();
  }

#else
  // Fall back to rdtsc. The reason we don't use clock() is this scary message
  // from the man page:
  //     "On several other implementations, the value returned by clock() also
  //      includes the times of any children whose status has been collected via
  //      wait(2) (or another wait-type call)."
  //
  // Also, clock() only has microsecond accuracy.
  //
  // This whitepaper offered excellent advice on how to use rdtscp for
  // profiling: http://download.intel.com/embedded/software/IA/324264.pdf
  //
  // Unfortunately, we can't follow its advice exactly with our semantics,
  // so we're just going to use rdtscp with cpuid.
  //
  // Note that rdtscp will only be available on new processors.

  #define NEED_RDTSC 1
  static inline double monotonic_rdtsc_seconds();
  double monotonic_seconds() {
    return monotonic_rdtsc_seconds();
  }

#endif


#if defined(NEED_RDTSC)
  
static inline uint64_t rdtsc() {
  uint32_t hi, lo;
  asm volatile("rdtscp\n"
               "movl %%edx, %0\n"
               "movl %%eax, %1\n"
               "cpuid"
               : "=r" (hi), "=r" (lo) : : "%rax", "%rbx", "%rcx", "%rdx");
  return (((uint64_t)hi) << 32) | (uint64_t)lo;
}

static uint64_t rdtsc_per_sec = 0;

INITIALIZER(init_rdtsc_per_sec) {
  uint64_t before, after;
  
  before = rdtsc();
  usleep(USECS_PER_SEC);
  after = rdtsc();
  
  rdtsc_per_sec = after - before;
}

static inline double monotonic_rdtsc_seconds(){
  return (double) rdtsc() / (double) rdtsc_per_sec;
}

#undef NEED_RDTSC
#endif
 
