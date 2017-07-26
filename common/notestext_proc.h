
#ifndef _RADIUM_COMMON_NOTESTEXT_PROC_H
#define _RADIUM_COMMON_NOTESTEXT_PROC_H

#define NUM_NOTESTEXTS 131

extern const char *NotesTexts2[NUM_NOTESTEXTS];

extern const char *NotesTexts3[NUM_NOTESTEXTS];

static inline const char *get_notename(const char *NotesTexts[], float notenum){
  if ((int)notenum >= NUM_NOTESTEXTS)
    return "-H";
  else if ((int)notenum < 0)
    return "-L";
  else
    return NotesTexts[(int)notenum];
}

#endif

