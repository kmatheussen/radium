#ifndef RADIUM_COMMON_SEQBLOCK_ENVELOPE_PROC_H
#define RADIUM_COMMON_SEQBLOCK_ENVELOPE_PROC_H



extern LANGSPEC void RT_SEQBLOCK_ENVELOPE_called_before_scheduler(void);
extern LANGSPEC void RT_SEQBLOCK_ENVELOPE_called_before_editor(struct SeqTrack *seqtrack);
extern LANGSPEC void RT_SEQBLOCK_ENVELOPE_called_after_scheduler_and_before_audio(void);
extern LANGSPEC void RT_SEQBLOCK_ENVELOPE_called_when_player_stopped(void);

#ifdef __cplusplus

struct SeqblockEnvelope *SEQBLOCK_ENVELOPE_create(struct SeqTrack *seqtrack, struct SeqBlock *seqblock, const hash_t *automation_state);
void SEQBLOCK_ENVELOPE_free(struct SeqblockEnvelope *seqblockenvelope);
int SEQBLOCK_ENVELOPE_get_num_automations(struct SeqblockEnvelope *seqblockenvelope);
double SEQBLOCK_ENVELOPE_get_db(struct SeqblockEnvelope *seqblockenvelope, int nodenum);
double SEQBLOCK_ENVELOPE_get_seqtime(struct SeqblockEnvelope *seqblockenvelope, int nodenum);
int SEQBLOCK_ENVELOPE_get_logtype(struct SeqblockEnvelope *seqblockenvelope, int nodenum);
int SEQBLOCK_ENVELOPE_get_num_nodes(struct SeqblockEnvelope *seqblockenvelope);
int SEQBLOCK_ENVELOPE_add_node(struct SeqblockEnvelope *seqblockenvelope, double seqtime, double db, int logtype);
void SEQBLOCK_ENVELOPE_delete_node(struct SeqblockEnvelope *seqblockenvelope, int nodenum);
void SEQBLOCK_ENVELOPE_set_curr_node(struct SeqblockEnvelope *seqblockenvelope, int nodenum);
void SEQBLOCK_ENVELOPE_cancel_curr_node(struct SeqblockEnvelope *seqblockenvelope);
void SEQBLOCK_ENVELOPE_set_curr_automation(struct SeqTrack *seqtrack, struct SeqBlock *seqblock);

// May be called if it there is no current automation.
void SEQBLOCK_ENVELOPE_cancel_curr_automation(void);
void SEQBLOCK_ENVELOPE_set(struct SeqTrack *seqtrack, struct SeqBlock *seqblock, int nodenum, double seqtime, double db, int lotype);

////////////////////////

void RT_SEQBLOCK_ENVELOPE_called_after_editor_and_before_audio(const struct SeqTrack *seqtrack, int64_t seqblock_time);
void RT_SEQBLOCK_ENVELOPE_called_when_player_stopped(void);
hash_t *SEQBLOCK_ENVELOPE_get_state(const struct SeqblockEnvelope *seqblockenvelope);
void SEQBLOCK_ENVELOPE_apply_state(struct SeqblockEnvelope *seqblockenvelope, const hash_t *envelope_state);
float SEQBLOCK_ENVELOPE_get_node_x(struct SeqblockEnvelope *seqblockenvelope, int nodenum);
float SEQBLOCK_ENVELOPE_get_node_y(struct SeqblockEnvelope *seqblockenvelope, int seqtracknum, int nodenum);
void SEQBLOCK_ENVELOPE_paint(QPainter *p, const struct SeqBlock *seqblock, float x1, float y1, float x2, float y, bool paint_nodes);

#endif

#endif
