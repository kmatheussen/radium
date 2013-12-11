#ifndef QT_GFXTIMER_PROC_H
#define QT_GFXTIMER_PROC_H

#if 0
#ifdef __cplusplus
extern "C" {
#endif
#endif

typedef void (*VBlank_callback) (void *data);

#if 0
#ifdef __cplusplus
}
#endif
#endif

extern LANGSPEC void call_function_at_vertical_blank(VBlank_callback function, void *data);

#endif
