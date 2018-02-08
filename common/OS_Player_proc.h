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

extern LANGSPEC bool PLAYER_current_thread_has_lock(void);
extern LANGSPEC bool PLAYER_someone_has_player_lock(void);
extern LANGSPEC bool PLAYER_player_has_player_lock(void);

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

#ifdef __cplusplus

namespace radium{

namespace{  

class PlayerLock{

  PlayerLock(const PlayerLock&) = delete;
  PlayerLock& operator=(const PlayerLock&) = delete;

public:

  const bool _enable;

  PlayerLock(const bool enable = true)
    : _enable(enable)
  {
    if (enable)
      PLAYER_lock();
  }

  ~PlayerLock(){
    if (_enable)
      PLAYER_unlock();
  }
};
 
struct PlayerRecursiveLock{
  bool gotit;

  PlayerRecursiveLock(const PlayerRecursiveLock&) = delete;
  PlayerRecursiveLock& operator=(const PlayerRecursiveLock&) = delete;

  PlayerRecursiveLock()
  : gotit (!PLAYER_current_thread_has_lock())
  {
    if (gotit)
      PLAYER_lock();
  }

  ~PlayerRecursiveLock(){
    if (gotit)
      PLAYER_unlock();
  }
};
 
} // End anon. namespace
  
// TODO: Go through all use of PlayerRecursiveLock and see if it can be replaced with PlayerLockOnlyIfNeeded.
struct PlayerLockOnlyIfNeeded{
  bool gotit = false;

  bool do_lock;

  void lock(){
    if (gotit==false){
      if(do_lock)
        PLAYER_lock();
      gotit = true;
    }
  }

  void maybe_pause(int i){
    if (gotit && do_lock)
      PLAYER_maybe_pause_lock_a_little_bit(i);
  }

  void maybe_pause_or_lock(int i){
    maybe_pause(i);
    lock();
  }
  
  PlayerLockOnlyIfNeeded(const PlayerLockOnlyIfNeeded&) = delete;
  PlayerLockOnlyIfNeeded& operator=(const PlayerLockOnlyIfNeeded&) = delete;

  
  PlayerLockOnlyIfNeeded(bool do_lock = true)
    : do_lock(do_lock)
  {
  }
  
  ~PlayerLockOnlyIfNeeded(){
    if (gotit){
      if(do_lock)
        PLAYER_unlock();
    }
  }  
};
 
} // End radium namespace


#endif

#endif

