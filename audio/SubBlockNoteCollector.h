/* Copyright 2026 Kjetil S. Matheussen

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


#ifndef AUDIO_SUB_BLOCK_NOTE_COLLECTOR_H
#define AUDIO_SUB_BLOCK_NOTE_COLLECTOR_H

struct NoteEventCollector
{

	static constexpr int MAX_EVENTS = 128;

	enum EventType {
		NOTE_OFF,    // 0: sorts first at equal offsets (release before retrigger)
		NOTE_ON,     // 1: note starts before its same-offset pitch change
		NOTE_PITCH   // 2: pitch change of an already playing note
	};

	struct Event {
		int sample_offset;   // block_delta_time [0, RADIUM_BLOCKSIZE)
		EventType type;
		note_t note;
	};

	Event events[MAX_EVENTS];
	int num_events;

	NoteEventCollector()
		: num_events(0)
	{
	}

	void noteOn(int block_delta_time, const note_t &note)
	{
		if (num_events < MAX_EVENTS){
			events[num_events].sample_offset = block_delta_time;
			events[num_events].type = NOTE_ON;
			events[num_events].note = note;
			num_events++;
		}
	}

	void noteOff(int block_delta_time, const note_t &note)
	{
		if (num_events < MAX_EVENTS){
			events[num_events].sample_offset = block_delta_time;
			events[num_events].type = NOTE_OFF;
			events[num_events].note = note;
			num_events++;
		}
	}

	void notePitch(int block_delta_time, const note_t &note)
	{
		if (num_events < MAX_EVENTS){
			events[num_events].sample_offset = block_delta_time;
			events[num_events].type = NOTE_PITCH;
			events[num_events].note = note;
			num_events++;
		}
	}

	static bool comes_after(const Event &a, const Event &b)
	{
		if (a.sample_offset == b.sample_offset)
			return a.type > b.type; // at the same offset: NOTE_OFF < NOTE_ON < NOTE_PITCH

		return a.sample_offset > b.sample_offset;
    }

	// Insertion sort by sample_offset. At equal offsets the order is
	// NOTE_OFF, NOTE_ON, NOTE_PITCH.
	// RT-safe: no allocation, small N.
    void sort(void)
	{
		for (int i = 1; i < num_events; i++)
		{
			Event key = events[i];

			int j = i - 1;

             while (j >= 0 && comes_after(events[j], key))
             {
                 events[j + 1] = events[j];
                 j--;
             }

             events[j + 1] = key;
		}
	}

	void clear()
	{
		num_events = 0;
	}
};

#endif // AUDIO_SUB_BLOCK_NOTE_COLLECTOR_H
