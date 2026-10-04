#pragma once

#include <functional>

class QMenu;

extern int g_is_calling_from_menu;

QMenu *GFX_create_qmenu(const vector_t &v,
                        func_t *callback2);

void GFX_clear_menu_cache(void);

void GFX_ShowPopupSearchWaitScreen(void);

bool GFX_MenuActive();
QMenu *GFX_GetActiveMenu(void);
void GFX_MakeMakeMainMenuActive(void);

void GFX_call_when_menus_are_closed(std::function<void(void)> callback);
