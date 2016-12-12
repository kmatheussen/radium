
#ifndef FROM_SWIG_C
    %module radium
    %{
    /* Put headers and other declarations here */
    %}
#endif

extern int s_main(void);

#define CURR -1


/*      Song        */

/*
int RA_getNumWindows(void);
int RA_getNumBlocks(void);
void RA_setCurrOctave(void);

void RAP_delkeyBehaviour(void);
void RAP_returnkeyBehaviour(void);

void RA_AppendBlock(void);
*/


/*      Window      */
/*
int RA_getCurrBlock(int windownum);

int RA_isPlayalong(int windownum);
void RA_setPlayalong(int windownum);
void RA_unsetPlayalong(int windownum);

int RA_isScrollPlay(int windownum);
void RA_setScrollPlay(int windownum);
void RA_unsetScrollPlay(int windownum);

int RA_isEditOnOff(int windownum);
void RA_setEditOn(int windownum);
void RA_setEditOff(int windownum);

void RA_prevBlock(int windownum);
void RA_nextBlock(int windownum);
void RA_prevPlaylistBlock(int windownum);
void RA_nextPlaylistBlock(int windownum);

void RA_windowConfig(int windownum);
*/


/*      Blocks      */
/*
int RA_getNumTracks(int blocknum);
int RA_getCurrTrack(int blocknum);
int RA_getCurrSubTrack(int blocknum);
int RA_getNumLInes(int blocknum);
int RA_getCurrLine(int blocknum);
int RA_getNumReallines(int blocknum);
int RA_getCurrRealline(int blocknum);

void RA_scrollEditor(int blocknum,int num_reallines);
void RA_scrollEditorPercent(int blocknum,int percent);
void RA_setCursor(int blocknum,int tracknum,int subtracknum);
void RA_insertReallines_CurrPos(int blocknum,int num_reallines);
void RA_insertLines_CurrPos_REQ(void);
void RA_expandRealline_CurrPos(int blocknum,int num_reallines);
void RA_unexpandRealline_CurrPos(int blocknum);

void RA_insertBlock(int blocknum);
void RA_deleteBlock(int blocknum);
void RA_splitBlock(int blocknum,int linenum);

void RA_changeBlockNoteLengths_CurrPos(int blocknum);
void RA_minimizeBlock_CurrPos(int blocknum);
*/

/*     Tempos       */
/*
void RA_removeBPMs_CurrPos(int blocknum);
void RA_removeLPBs_CurrPos(int blocknum);
void RA_removeTemponodes_CurrPos(int blocknum);
*/




/*     Range        */
/*
int RA_isRanged(int blocknum);
int RA_getRangeStartTrack(int blocknum);
int RA_getRangeEndTrack(int blocknum);
int RA_getRangeStartLine(int blocknum);
int RA_getRangeEndLine(int blocknum);

void RA_setRange(int blocknum);
void RA_unsetRange(int blocknum);
void RA_setRangeStartTrack(int blocknum);
void RA_setRangeEndTrack(int blocknum);
void RA_setRangeStartLine(int blocknum);
void RA_setRangeEndLine(int blocknum);

void RA_markRange_CurrPos(int blocknum);
*/

/*     Lines       */



/*     Tracks      */

int RA_getNumNotes(int blocknum,int trackum);
/*
int RA_getNumStops(int blocknum,int tracknum);
int RA_getNumEffects(int blocknum,int tracknum);

void RA_selectPatch(int blocknum,int tracknum);

void RA_swapTracks(int blocknum,int track1,int track2);

void RA_changeTrackNoteLengths_CurrPos(int blocknum);
void RA_minimizeTrack_CurrPos(int blocknum);
*/
void RA_removeNotes(int blocknum,int tracknum);



/*     Notes      */
int RA_getNoteNote(int blocknum,int tracknum,int notenum);
float RA_getNoteVel(int blocknum,int tracknum,int notenum);
float RA_getNoteFloatStart(int blocknum,int tracknum,int notenum);
float RA_getNoteFloatEnd(int blocknum,int tracknum,int notenum);
//int RA_getNoteLineStart(int blocknum,int tracknum,int notenum);
//int RA_getNoteNumeratorStart(int blocknum,int tracknum,int notenum);
//int RA_getNoteDenominatorStart(int blocknum,int tracknum,int notenum);
//int RA_getNoteLineEnd(int blocknum,int tracknum,int notenum);
//int RA_getNoteNumeratorEnd(int blocknum,int tracknum,int notenum);
//int RA_getNoteDenominatorEnd(int blocknum,int tracknum,int notenum);

//void RA_removeNote_CurrPos(void);

void RA_addNote_FloatPlace(int blocknum,int tracknum,float start,int notenum,float volume,float end);


/*     Velocities */
//void RA_stopVelocity_CurrPos(void);


/*     Effects    */

//int RA_getNumEffectNodes(int blocknum,int tracknum,int effectnum);



/*     Playing    */
/*
void RA_PlayBlockFromStart(int blocknum);
void RA_PlaySongFromStart(void);
void RA_PlayBlock_CurrPos(int blocknum);
void RA_PlaySong_CurrPos(int windownum);
void RA_PlayStop(void);
*/

/*    ClipBoard  */
/*
void RA_CutRange(int windownum,int blocknum);
void RA_CutTrack_CurrPos(int windownum);

void RA_CopyRange_CurrPos(int windownum);
void RA_CopyTrack_CurrPos(int windownum);
void RA_CopyBlock_CurrPos(int windownum);

void RA_PasteRange_CurrPos(int windownum);
void RA_PasteTrack_CurrPos(int windownum);
void RA_PasteBlock_CurrPos(int windownum);
*/


/*    Transpose  */
/*
void RA_TransposeTrack(int windownum,int blocknum,int tracknum,int transpose);
void RA_TransposeBlock(int windownum,int blocknum,int transpose);
void RA_TransposeRange(int windownum,int blocknum,int transpose);
*/


/*    Amiga     */
//void AMIGA_configColors(void);




