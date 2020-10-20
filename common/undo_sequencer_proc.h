#ifndef UNDO_SEQUENCER_PROC_H
#define UNDO_SEQUENCER_PROC_H

extern LANGSPEC void ADD_UNDO_FUNC(Sequencer(void));
extern LANGSPEC void ADD_UNDO_FUNC(Seqblock(int seqtracknum, int seqblocknum));
extern LANGSPEC void ADD_UNDO_FUNC(Seqblock_block_timing_state(struct Blocks *block));
extern LANGSPEC void ADD_UNDO_FUNC(SeqtrackAutomations(void));
extern LANGSPEC void ADD_UNDO_FUNC(SeqblockAutomation(int automationnum, int seqblocknum, int seqtracknum));
extern LANGSPEC void ADD_UNDO_FUNC(SeqblockFades(int seqtracknum, int seqblocknum));
extern LANGSPEC void ADD_UNDO_FUNC(EditorSeqtrackVolume(int seqtracknum));
extern LANGSPEC void ADD_UNDO_FUNC(SeqtrackConfig(void));

#endif
