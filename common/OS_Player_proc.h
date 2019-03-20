/* Copyright 2000 Kjetil S. Matheussen

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA. */

#ifndef RADIUM_COMMON_OS_PLAYER_PROC_H
#define RADIUM_COMMON_OS_PLAYER_PROC_H


//extern LANGSPEC void PLAYER_acquire_same_priority(void);
//extern LANGSPEC void PLAYER_drop_same_priority(void);

// PLAYER_lock is a RT-safe lock. Priority of the calling thread is set to the same value as the player before acquiring the lock.
// Can not be called from a player thread.
extern LANGSPEC void PLAYER_lock(void);
extern LANGSPEC void PLAYER_unlock(void);
extern LANGSPEC void PLAYER_maybe_pause_lock_a_little_bit(int iteration); // Check if we have held the lock for a certain amount of time. If we have, unlock, wait 10ms, lock.
  
static inline bool enter_radium_lock_if_playing_scope(void){
  R_ASSERT_NON_RELEASE(THREADING_is_main_thread());

  if (is_playing()){
    PLAYER_lock();
    return true;
  }else
    return false;  
}

static inline void leave_radium_lock_if_playing_scope(bool *was_locked){
  if (*was_locked)
    PLAYER_unlock();
  else
    R_ASSERT(!is_playing());
}

#if defined(__clang__)
#define SCOPED_PLAYER_LOCK_IF_PLAYING() bool radium_scoped_lock_if_playing __attribute__ ((__cleanup__(leave_radium_lock_if_playing_scope))) = enter_radium_lock_if_playing_scope();(void)radium_scoped_lock_if_playing 
#else
#define SCOPED_PLAYER_LOCK_IF_PLAYING() bool radium_scoped_lock_if_playing __attribute__ ((__cleanup__(leave_radium_lock_if_playing_scope))) = enter_radium_lock_if_playing_scope()
#endif
  
extern LANGSPEC bool MIXER_is_saving(void);

// Must be called in SoundPlugin::RT_process if doing things there which requires the PLAYER_lock. (separate lock because of multicore processing, PLAYER_lock is always held inside SoundPlugin::RT_process)
extern LANGSPEC void RT_PLAYER_runner_lock(void);
extern LANGSPEC void RT_PLAYER_runner_unlock(void);

extern LANGSPEC bool PLAYER_is_doing_RT_stuff(void);

extern LANGSPEC bool PLAYER_current_thread_has_lock(void);
extern LANGSPEC bool PLAYER_someone_has_player_lock(void);

extern LANGSPEC void StartPlayer(void);

extern LANGSPEC void StopPlayer(void);

extern LANGSPEC void PausePlayer(void);
extern LANGSPEC void StopPausePlayer(void);

extern LANGSPEC void OS_WaitForAShortTime(int milliseconds);
extern LANGSPEC void OS_WaitAtLeast(int milliseconds); // Use this function instead if it is important that we don't return too early.

extern LANGSPEC void PLAYER_volumeUp(float db);
extern LANGSPEC void PLAYER_volumeDown(float db);
extern LANGSPEC void PLAYER_mute(void);

extern LANGSPEC bool RT_jack_transport_play_request_is_finished(void);
extern LANGSPEC void RT_request_from_jack_transport_to_play(int64_t absabstime);

extern LANGSPEC void RT_request_to_start_playing_block(void);
extern LANGSPEC void RT_request_to_continue_playing_block(void);
extern LANGSPEC void RT_request_to_stop_playing(void);
extern LANGSPEC void RT_pause_plugins(void);


static inline bool is_doing_RT(void){
  return THREADING_is_player_or_runner_thread() || PLAYER_current_thread_has_lock();
}

#define ASSERT_IS_NONRT_MAIN_THREAD() R_ASSERT(THREADING_is_main_thread() && !PLAYER_current_thread_has_lock());

#define ASSERT_IS_NONRT_MAIN_THREAD_NON_RELEASE() R_ASSERT_NON_RELEASE(THREADING_is_main_thread() && !PLAYER_current_thread_has_lock());

#define ASSERT_NON_RT()                                                 \
  do{                                                                   \
    R_ASSERT(!THREADING_is_player_or_runner_thread());                  \
    R_ASSERT(!PLAYER_current_thread_has_lock());                        \
  }while(0)

#define ASSERT_NON_RT_NON_RELEASE()                                     \
  do{                                                                   \
    R_ASSERT_NON_RELEASE(!THREADING_is_player_or_runner_thread());      \
    R_ASSERT_NON_RELEASE(!PLAYER_current_thread_has_lock());            \
  }while(0)




#ifdef __cplusplus

namespace radium{

namespace{  

class PlayerLock{

  PlayerLock(const PlayerLock&) = delete;
  PlayerLock& operator=(const PlayerLock&) = delete;

public:

  const bool _enable;
  const bool _can_pause;
  
  PlayerLock(const bool enable = true, bool can_pause = true)
    : _enable(enable)
    , _can_pause(can_pause)
  {
    if (enable)
      PLAYER_lock();
  }

  ~PlayerLock(){
    if (_enable)
      PLAYER_unlock();
  }

  void maybe_pause_lock_a_little_bit(int i) const {
    if(_can_pause)
      PLAYER_maybe_pause_lock_a_little_bit(i);
  }
};
 
class PlayerUnlock{

  PlayerUnlock(const PlayerUnlock&) = delete;
  PlayerUnlock& operator=(const PlayerUnlock&) = delete;

public:

  const bool _enable;

  PlayerUnlock(const bool enable = true)
    : _enable(enable)
  {
    if (enable)
      PLAYER_unlock();
  }

  ~PlayerUnlock(){
    if (_enable)
      PLAYER_lock();
  }
};
 
struct PlayerRecursiveLock{
  bool gotit;

  PlayerRecursiveLock(const PlayerRecursiveLock&) = delete;
  PlayerRecursiveLock& operator=(const PlayerRecursiveLock&) = delete;

  PlayerRecursiveLock()
    : gotit(PLAYER_current_thread_has_lock())
  {
    if (!gotit)
      PLAYER_lock();
  }

  ~PlayerRecursiveLock(){
    if (!gotit)
      PLAYER_unlock();
  }
};
 
} // End anon. namespace
  
// TODO: Go through all use of PlayerRecursiveLock and see if it can be replaced with PlayerLockOnlyIfNeeded.
struct PlayerLockOnlyIfNeeded{
  bool has_lock;

  bool do_lock;

  void lock(){
    if (has_lock==false){
      if(do_lock)
        PLAYER_lock();
      has_lock = true;
    }
  }

  void maybe_pause(int i){
    if (has_lock && do_lock)
      PLAYER_maybe_pause_lock_a_little_bit(i);
  }

  void maybe_pause_or_lock(int i){
    maybe_pause(i);
    lock();
  }

  struct ScopedLockPause{
    bool has_lock;
    ScopedLockPause(PlayerLockOnlyIfNeeded *lock)
      : has_lock(lock==NULL ? false : lock->has_lock)
    {
      if(has_lock)
        PLAYER_unlock();        
    }
    ~ScopedLockPause(){
      if(has_lock)
        PLAYER_lock();
    }
  };
  
  PlayerLockOnlyIfNeeded(const PlayerLockOnlyIfNeeded&) = delete;
  PlayerLockOnlyIfNeeded& operator=(const PlayerLockOnlyIfNeeded&) = delete;

  
  PlayerLockOnlyIfNeeded(bool do_lock = true)
    : has_lock(PLAYER_current_thread_has_lock())
    , do_lock(has_lock==false && do_lock)
  {
    R_ASSERT_NON_RELEASE(has_lock==false);
  }
  
  ~PlayerLockOnlyIfNeeded(){
    if (has_lock){
      if(do_lock)
        PLAYER_unlock();
    }
  }  
};
 
} // End radium namespace


#endif

#endif

