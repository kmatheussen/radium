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


extern LANGSPEC void PlayerTask(STime stime);
extern LANGSPEC void PlayBlockFromStart(struct Tracker_Windows *window,bool do_loop);
extern LANGSPEC void PlayBlockCurrPos2(struct Tracker_Windows *window, Place *place);
extern LANGSPEC void PlayBlockCurrPos(struct Tracker_Windows *window);
extern LANGSPEC void PlayRangeFromStart(struct Tracker_Windows *window);
extern LANGSPEC void PlayRangeCurrPos(struct Tracker_Windows *window);
extern LANGSPEC void PlayRangeCurrPos2(struct Tracker_Windows *window, Place *place);
extern LANGSPEC void Play_set_curr_playing_realline(int realline, int blocknum);
extern LANGSPEC void Play_get_curr_playing_realline(int *realline, int *blocknum);
extern LANGSPEC void PlayCallVeryOften(void);
extern LANGSPEC void PlaySongFromStart(struct Tracker_Windows *window);
extern LANGSPEC void PlaySongCurrPos(struct Tracker_Windows *window);
extern LANGSPEC void PlaySongCurrPos2(struct Tracker_Windows *window, Place *place);
extern LANGSPEC void PlayStop(void);

extern LANGSPEC STime PLAYER_get_block_delta_time(STime time);

