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

/*
extern LANGSPEC const struct NodeLine *create_nodelines(
                                                        const struct Tracker_Windows *window,
                                                        const struct WBlocks *wblock,
                                                        const struct ListHeader3 *list,                                  
                                                        float (*get_x)(const struct WBlocks *wblock, const struct ListHeader3 *element), // should return a value between 0 and 1.
                                                        const struct ListHeader3 *last_element // may be null. may also contain more than one element.
                                                        );
*/

extern LANGSPEC const vector_t *get_nodeline_nodes(const struct NodeLine *nodelines, float y_offset);
extern LANGSPEC const vector_t *get_nodeline_nodes2(const struct NodeLine2 *nodelines, float y_offset);

extern LANGSPEC const struct NodeLine *GetTempoNodeLines(const struct Tracker_Windows *window, const struct WBlocks *wblock);
extern LANGSPEC const vector_t *GetTempoNodes(const struct Tracker_Windows *window, const struct WBlocks *wblock);


/*
extern LANGSPEC const struct NodeLine *GetPitchNodeLines(const struct Tracker_Windows *window, const struct WBlocks *wblock, const struct WTracks *wtrack, const struct Notes *note);
extern LANGSPEC const vector_t *GetPitchNodes(const struct Tracker_Windows *window, const struct WBlocks *wblock, const struct WTracks *wtrack, const struct Notes *note);
*/

#ifdef RADIUM_COMMON_TIMEDATA_HPP
extern const struct NodeLine2 *GetPitchNodeLines2(const struct Tracker_Windows *window, const struct WBlocks *wblock, const struct WTracks *wtrack, const struct Notes *note, const float track_pitch_min, const float track_pitch_max, const r::PitchTimeData::Reader &reader);
#endif

extern LANGSPEC const vector_t *GetPitchNodes2(const struct Tracker_Windows *window, const struct WBlocks *wblock, const struct WTracks *wtrack, const struct Notes *note, const float track_pitch_min, const float track_pitch_max);

//extern LANGSPEC const struct NodeLine *GetVelocityNodeLines(const struct Tracker_Windows *window, const struct WBlocks *wblock, const struct WTracks *wtrack, const struct Notes *note);
extern LANGSPEC const vector_t *GetVelocityNodes(const struct Tracker_Windows *window, const struct WBlocks *wblock, const struct WTracks *wtrack, const struct Notes *note);

extern LANGSPEC const struct NodeLine2 *GetVelocityNodeLines2(const struct Tracker_Windows *window, const struct WBlocks *wblock, const struct WTracks *wtrack, const struct Notes *note);
  
extern LANGSPEC const struct NodeLine2 *GetFxNodeLines(const struct Tracker_Windows *window, const struct WBlocks *wblock, const struct WTracks *wtrack, const struct FXs *fxs);
extern LANGSPEC const vector_t *GetFxNodes(const struct Tracker_Windows *window, const struct WBlocks *wblock, const struct WTracks *wtrack, const struct FXs *fxs);

/*
extern LANGSPEC const struct NodeLine *GetPianorollNodeLines(const struct Tracker_Windows *window, const struct WBlocks *wblock, const struct WTracks *wtrack, const struct Notes *note);
extern LANGSPEC const vector_t *GetPianorollNodes(const struct Tracker_Windows *window, const struct WBlocks *wblock, const struct WTracks *wtrack, const struct Notes *note);
*/

#ifdef RADIUM_COMMON_TIMEDATA_HPP
extern const struct NodeLine2 *GetPianorollNodeLines2(const struct Tracker_Windows *window, const struct WBlocks *wblock, const struct WTracks *wtrack, const struct Notes *note, const r::PitchTimeData::Reader &reader);
//extern LANGSPEC const vector_t *GetPianorollNodes2(const struct Tracker_Windows *window, const struct WBlocks *wblock, const struct WTracks *wtrack, const struct Notes *note);
#endif


  
#if !USE_OPENGL
extern LANGSPEC void MakeNodeLines(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
        struct WTracks *wtrack,
	Place *p1,
	Place *p2,
	float x1,float x2,
	float minx,float maxx,
	void *extrainfo,
	void (*ReturnNodeLineData)(
		struct Tracker_Windows *window,
		struct WBlocks *wblock,
                struct WTracks *wtrack,
		void *extrainfo,
		int firstlast,
		int realline,
		float u_y1,float u_y2,
		float u_x1,float u_x2
		)
	);

extern LANGSPEC void GetNodeLine(
		 struct TrackReallineElements *tre,
		 WArea *warea,
		 TBox *within,
		 TBox *ret
		 );
#endif
