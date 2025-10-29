//---------------------------------------------------------
// For conditions of distribution and use, see
// https://github.com/preshing/cpp11-on-multicore/blob/master/LICENSE
//---------------------------------------------------------

#ifndef __CPP11OM_SEMAPHORE_H__
#define __CPP11OM_SEMAPHORE_H__

#include <cassert>

#if !defined(USE_STD_COUNTING_SEMAPHORE)
#  error "error" // Should have been set in the Makefile.
#endif

// Add this test as well to be 100% sure we're not mixing semaphore types.
#if !USE_STD_COUNTING_SEMAPHORE
#  if defined(__clang__)
#     error Somethings wrong? If not, just comment out this line.
#  endif
#endif

/*
#if defined(__aarch64__) && defined(__MACH__)
#  define USE_STD_COUNTING_SEMAPHORE 1 // Mostly because of TSAN, since TSAN doesn't support mach semaphores (semaphore_t). But it might also be faster.
#elif
#  define USE_STD_COUNTING_SEMAPHORE 0
#endif
*/

#if USE_STD_COUNTING_SEMAPHORE

#if !defined(__clang__)
#  error "Not sure, but probably best not to use std::counting_semaphore with gcc for now. Might be more problems with it than the obvious deadlock (which is check for below.)"
#  if __GNUC__ < 11
#    error "This version of gcc deadlocks when using std::counting_semaphore"
#  endif
#  if (__GNUC__==11) && (__GNUC_MINOR__ < 5)
#    error "This version of gcc deadlocks when using std::counting_semaphore"
#  endif
#  if (__GNUC__==12) && (__GNUC_MINOR__ < 4)
#    error "This version of gcc deadlocks when using std::counting_semaphore"
#  endif
#  if (__GNUC__==13) && (__GNUC_MINOR__ < 3)
#    error "This version of gcc deadlocks when using std::counting_semaphore"
#  endif
#  if (__GNUC__==14) && (__GNUC_MINOR__ < 1)
#    error "This version of gcc deadlocks when using std::counting_semaphore"
#  endif
#endif // !defined(__clang__)

#include <semaphore>

namespace cpp11onmulticore{

class Semaphore
{
private:
	std::counting_semaphore<10000000> m_sema;

	Semaphore(const Semaphore& other) = delete;
	Semaphore& operator=(const Semaphore& other) = delete;

public:
	Semaphore(int initialCount = 0)
		: m_sema(initialCount)
	{
	}

	void wait()
	{
		m_sema.acquire();
	}

	void signal(int count = 1)
	{
		m_sema.release(count);
	}
};
}

#elif defined(_WIN32)
//---------------------------------------------------------
// Semaphore (Windows)
//---------------------------------------------------------

#include <windows.h>
#undef min
#undef max

namespace cpp11onmulticore{

//#error "TEST USE_WAIT_FOR_SINGLE_OBJECT before next release, and enable if it works"
#define USE_WAIT_FOR_SINGLE_OBJECT 0 // If set to 0, WaitOnAddress is used instead of instead WaitForSingleObject. (WaitOnAddress should be much faster.)

#if USE_WAIT_FOR_SINGLE_OBJECT
class Semaphore
{
private:
    HANDLE m_hSema;

    Semaphore(const Semaphore& other) = delete;
    Semaphore& operator=(const Semaphore& other) = delete;

public:
    Semaphore(int initialCount = 0)
    {
        assert(initialCount >= 0);
        m_hSema = CreateSemaphore(NULL, initialCount, MAXLONG, NULL);
    }

    ~Semaphore()
    {
        CloseHandle(m_hSema);
    }

    void wait()
    {
        WaitForSingleObject(m_hSema, INFINITE);
    }

    void signal(int count = 1)
    {
        ReleaseSemaphore(m_hSema, count, NULL);
    }
};

#else // USE_WAIT_FOR_SINGLE_OBJECT -> USE_WAIT_ON_ADDRESS

// Performance analysis seems to show that Radium uses significant time in WaitForSingleObject on Windows.
// This implementation uses WaitOnAddress instead, which should be much faster. However,
// it's not available in Windows 7 or older, so I'm not quite sure what to do about that yet.
// But either support for Windows 7 will be dropped, or I'll check at runtime which Semaphore implementation
// to use, probably the latter.
//
// Implemented by looking at this code: https://devblogs.microsoft.com/oldnewthing/20170612-00/?p=96375
//
class Semaphore
{
    using CounterType = int;

private:
    DEFINE_ATOMIC(CounterType, _counter) = 0;

#if !defined(RELEASE)
    DEFINE_ATOMIC(CounterType, _waiters) = 0;
#endif

    Semaphore(const Semaphore& other) = delete;
    Semaphore& operator=(const Semaphore& other) = delete;

public:
    Semaphore(CounterType initialCount = 0)
        : ATOMIC_NAME(_counter)(initialCount)
    {
        R_ASSERT_NON_RELEASE(initialCount >= 0);
    }

    ~Semaphore()
    {
        R_ASSERT_NON_RELEASE(ATOMIC_GET(_waiters) == 0);
    }

    int get_value(void) const
    {
        return ATOMIC_GET(_counter);
    }

    void wait()
    {
#if !defined(RELEASE)
        ATOMIC_ADD(_waiters, 1);
#endif
        for (;;)
        {
            int org_count = ATOMIC_GET(_counter);

            while (org_count == 0)
            {
                WaitOnAddress(&ATOMIC_NAME(_counter),
                          &org_count,
                          sizeof(CounterType),
                          INFINITE);

                org_count = ATOMIC_GET(_counter);
            }

            if (ATOMIC_COMPARE_AND_SET_INT(_counter, org_count, org_count-1))
                break;
        }
#if !defined(RELEASE)
        ATOMIC_ADD(_waiters, -1);
#endif
    }

    void signal(const int count = 1)
    {
        if (count <= 0)
        {
            R_ASSERT_NON_RELEASE(false);
            return;
        }

        ATOMIC_ADD(_counter, count);

        if (count == 1)
            WakeByAddressSingle(&ATOMIC_NAME(_counter));
        else
            WakeByAddressAll(&ATOMIC_NAME(_counter));
    }
};
#endif // USE_WAIT_ON_ADDRESS

}

#elif defined(__MACH__)
//---------------------------------------------------------
// Semaphore (Apple iOS and OSX)
// Can't use POSIX semaphores due to http://lists.apple.com/archives/darwin-kernel/2009/Apr/msg00010.html
//---------------------------------------------------------
#include <mach/mach.h>

namespace cpp11onmulticore{
class Semaphore
{
private:
    semaphore_t m_sema;

    Semaphore(const Semaphore& other) = delete;
    Semaphore& operator=(const Semaphore& other) = delete;

public:
    Semaphore(int initialCount = 0)
    {
        assert(initialCount >= 0);
        semaphore_create(mach_task_self(), &m_sema, SYNC_POLICY_FIFO, initialCount);
    }

    ~Semaphore()
    {
        semaphore_destroy(mach_task_self(), m_sema);
    }

    void wait()
    {
      
      for(;;){
        kern_return_t rc = semaphore_wait(m_sema);
        if (rc==KERN_SUCCESS)
          break;
        
	if (ATOMIC_GET(g_program_has_ended))
	{
		while(true)
		{
			msleep(1000); // Wait it out until the thread has been killed.
		}
	}
	else
	{
#if !defined(RELEASE)
		fprintf(stderr, "rc: %d\n", (int)rc);
		abort();
#endif
		
		if(rc!=KERN_ABORTED && rc!=KERN_TERMINATED)
			RError("RC: %d\n", (int)rc); // Codes here: https://opensource.apple.com/source/xnu/xnu-1228/osfmk/mach/kern_return.h.auto.html
		
	}
      }
    }

    void signal()
    {
        semaphore_signal(m_sema);
    }

    void signal(int count)
    {
        while (count-- > 0)
        {
            semaphore_signal(m_sema);
        }
    }
};
}

#elif defined(__unix__)

//---------------------------------------------------------
// Semaphore (POSIX, Linux)
//---------------------------------------------------------

#include <semaphore.h>
#include <errno.h>

//extern QAtomicInt g_num_waits;


namespace cpp11onmulticore{
class Semaphore
{
private:
    sem_t m_sema;

    Semaphore(const Semaphore& other) = delete;
    Semaphore& operator=(const Semaphore& other) = delete;

public:
    Semaphore(int initialCount = 0)
    {
        assert(initialCount >= 0);
        sem_init(&m_sema, 0, initialCount);
    }

    ~Semaphore()
    {
        sem_destroy(&m_sema);
    }

    void wait()
    {
      //g_num_waits.ref();
        // http://stackoverflow.com/questions/2013181/gdb-causes-sem-wait-to-fail-with-eintr-error
        int rc;
        do
        {
            rc = sem_wait(&m_sema);
        }
        while (rc == -1 && errno == EINTR);
    }

    void signal()
    {
        sem_post(&m_sema);
    }

    void signal(int count)
    {
        while (count-- > 0)
        {
            sem_post(&m_sema);
        }
    }
};
}

#else

#error Unsupported platform

#endif

// atomic not found when compiling with clang.
#if 0 //FOR_LINUX


#include <atomic>

//---------------------------------------------------------
// LightweightSemaphore
//---------------------------------------------------------
namespace cpp11onmulticore{
class LightweightSemaphore
{
private:
    std::atomic<int> m_count;
    Semaphore m_sema;

    void waitWithPartialSpinning()
    {
        int oldCount;
        // Is there a better way to set the initial spin count?
        // If we lower it to 1000, testBenaphore becomes 15x slower on my Core i7-5930K Windows PC,
        // as threads start hitting the kernel semaphore.
        int spin = 10000;
        while (spin--)
        {
            oldCount = m_count.load(std::memory_order_relaxed);
            if ((oldCount > 0) && m_count.compare_exchange_strong(oldCount, oldCount - 1, std::memory_order_acquire))
                return;
#if 1
            std::atomic_signal_fence(std::memory_order_acquire);     // Prevent the compiler from collapsing the loop.
#else
            pthread_yield(); // remarkably effective... (note that the "spin" integer must be lowered somewhat. Probably need a timer here instead.)
#endif
        }
        oldCount = m_count.fetch_sub(1, std::memory_order_acquire);
        if (oldCount <= 0)
        {
            m_sema.wait();
        }
    }

public:
    LightweightSemaphore(int initialCount = 0) : m_count(initialCount)
    {
        assert(initialCount >= 0);
    }

    bool tryWait()
    {
        int oldCount = m_count.load(std::memory_order_relaxed);
        return (oldCount > 0 && m_count.compare_exchange_strong(oldCount, oldCount - 1, std::memory_order_acquire));
    }

    void wait()
    {
        if (!tryWait())
            waitWithPartialSpinning();
    }

    void signal(int count = 1)
    {
        int oldCount = m_count.fetch_add(count, std::memory_order_release);
        int toRelease = -oldCount < count ? -oldCount : count;
        if (toRelease > 0)
        {
            m_sema.signal(toRelease);
        }
    }
};



typedef LightweightSemaphore DefaultSemaphoreType;
}
#endif



#endif // __CPP11OM_SEMAPHORE_H__
