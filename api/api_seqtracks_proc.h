
#ifndef _RADIUM_API_API_SEQTRACKS_PROC_H
#define _RADIUM_API_API_SEQTRACKS_PROC_H

extern DEFINE_ATOMIC(bool, g_has_seqblock_marked_as_available);

extern LANGSPEC void API_curr_seqtrack_has_changed(void);
extern LANGSPEC void API_seqblock_has_been_deleted(int64_t seqblockid);
extern LANGSPEC void API_all_seqblocks_will_be_deleted(void);

#endif
