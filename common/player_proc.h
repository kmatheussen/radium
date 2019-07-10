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

#ifndef _RADIUM_COMMON_PLAYER_PROC_H
#define _RADIUM_COMMON_PLAYER_PROC_H

extern LANGSPEC void PlayerTask(double reltime, bool can_not_start_playing_right_now_because_we_have_told_jack_transport_that_we_are_not_ready_yet, float max_audio_cycle_fraction);
extern LANGSPEC void PlayBlockFromStart(struct Tracker_Windows *window,bool do_loop);
extern LANGSPEC void PlayBlockCurrPos2(struct Tracker_Windows *window, const Place *place);
extern LANGSPEC void PlayBlockCurrPos(struct Tracker_Windows *window);
extern LANGSPEC void PlayRangeFromStart(struct Tracker_Windows *window);
extern LANGSPEC void PlayRangeCurrPos(struct Tracker_Windows *window);
extern LANGSPEC void PlayRangeCurrPos2(struct Tracker_Windows *window, const Place *place);
extern LANGSPEC void Play_set_curr_playing_realline(int realline, int blocknum);
extern LANGSPEC void Play_get_curr_playing_realline(int *realline, int *blocknum);
extern LANGSPEC bool PlayerIsCurrentlyPlayingLoop(void);
extern LANGSPEC void PlayCallVeryOften(void);
extern LANGSPEC void PlaySongCurrPos(void);
extern LANGSPEC void PlaySong(double abstime);
extern LANGSPEC void PlaySong_from_jack_transport(int64_t absabstime);
extern LANGSPEC void PlaySongFromStart(void);
extern LANGSPEC void PlayStop(void);
extern LANGSPEC void PlayStop_from_jack_transport(void);
extern LANGSPEC void PLAYER_set_song_pos(int64_t pos, int64_t absabstime, bool called_from_jack_transport, bool update_curr_seqblock);

extern STime g_last_seq_time_converted_to_delta_time; // This one can be used since calls to patch->playnote, patch->stopnote, etc. are single threaded.
extern LANGSPEC int PLAYER_get_block_delta_time(struct SeqTrack *seqtrack, STime time);

#endif
