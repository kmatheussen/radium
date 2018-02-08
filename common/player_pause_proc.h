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

#ifndef _RADIUM_COMMON_PLAYER_PAUSE_PROC_H
#define _RADIUM_COMMON_PLAYER_PAUSE_PROC_H


extern int g_pausing_level;

extern LANGSPEC void PC_Pause_set_pos(int blocknum, int realline);

extern LANGSPEC void PC_Pause(void);
extern LANGSPEC void PC_PauseNoMessage(void);

extern LANGSPEC void PC_StopPause(struct Tracker_Windows *window);
extern LANGSPEC void PC_StopPauseAtCurrCursorPos(struct Tracker_Windows *window);
extern LANGSPEC void PC_StopPause_ForcePlayBlock(struct Tracker_Windows *window);

#ifdef __cplusplus
namespace radium{
namespace{
  struct PlayerPause{
    const bool _enable;    
    PlayerPause(const bool enable = true)
      : _enable(enable)
    {
      if (_enable)
        PC_Pause();
    }
    ~PlayerPause(){
      if (_enable)
        PC_StopPause(NULL);
    }
  };
}

struct PlayerPauseOnlyIfNeeded{
  bool has_paused = false;

  bool do_it;

  void need_it(void){
    if (has_paused==false && do_it){
      PC_Pause();
      has_paused = true;
    }
  }

  PlayerPauseOnlyIfNeeded(const PlayerPauseOnlyIfNeeded&) = delete;
  PlayerPauseOnlyIfNeeded& operator=(const PlayerPauseOnlyIfNeeded&) = delete;

  
  PlayerPauseOnlyIfNeeded(bool do_it = true)
  : do_it(do_it)
  {
  }
  
  ~PlayerPauseOnlyIfNeeded(){
    if (has_paused)
      PC_StopPause(NULL);
  }  
};

}
#endif

#endif
