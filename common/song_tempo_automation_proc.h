#ifndef _RADIUM_COMMON_SONG_TEMPO_AUTOMATION_PROC_H
#define _RADIUM_COMMON_SONG_TEMPO_AUTOMATION_PROC_H

extern LANGSPEC double RT_TEMPOAUTOMATION_get_value(double abstime);
extern LANGSPEC double TEMPOAUTOMATION_get_value(int nodenum);
extern LANGSPEC double TEMPOAUTOMATION_get_abstime(int nodenum);
extern LANGSPEC int TEMPOAUTOMATION_get_logtype(int nodenum);
extern LANGSPEC int TEMPOAUTOMATION_get_num_nodes(void);
extern LANGSPEC void TEMPOAUTOMATION_add_node(double abstime, double value, int logtype);
extern LANGSPEC void TEMPOAUTOMATION_delete_node(int nodenum);
extern LANGSPEC void TEMPOAUTOMATION_set(int nodenum, double abstime, double value, int logtype);
extern LANGSPEC void TEMPOAUTOMATION_set_length(double end_time, bool do_shrink);
extern LANGSPEC double TEMPOAUTOMATION_get_length(void);
extern LANGSPEC double TEMPOAUTOMATION_get_absabstime(double abstime);

#if USE_QT4
class QPainter;
void TEMPOAUTOMATION_paint(QPainter *p, float x1, float y1, float x2, float y2, double start_time, double end_time);
#endif

#endif
