
#ifndef _RADIUM_API_API_REQUESTERS_PROC_H
#define _RADIUM_API_API_REQUESTERS_PROC_H

extern LANGSPEC void closeRequester(void);

#ifdef __cplusplus
void API_simplePopupMenu(const char *texts, std::function<void(int,bool)> callback3);
#endif

#endif
