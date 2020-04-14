
#ifndef _RADIUM_API_API_REQUESTERS_PROC_H
#define _RADIUM_API_API_REQUESTERS_PROC_H

extern LANGSPEC void closeRequester(void);

#ifdef __cplusplus
int64_t API_simplePopupMenu(const char *texts, std::function<void(int,bool)> callback3);
#endif

extern LANGSPEC void API_call_me_when_a_popup_menu_has_been_closed(void);

#endif
