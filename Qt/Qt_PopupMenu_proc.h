#pragma once

#include <functional>

class QMenu;
class QWidget;

extern int g_is_calling_from_menu;

QMenu *GFX_create_qmenu(const vector_t &v,
                        func_t *callback2);

void GFX_clear_menu_cache(void);

void GFX_ShowPopupSearchWaitScreen(void);

void GFX_set_pending_popup_position(int x, int y, bool is_hamburger);
void GFX_set_hamburger_button(QWidget *button);
void GFX_open_hamburger_popup_menu(QWidget *button);
bool GFX_HamburgerPopupIsOpen(void);
void GFX_CloseHamburgerPopup(void);

bool GFX_MenuActive();
QMenu *GFX_GetActiveMenu(void);
void GFX_MakeMakeMainMenuActive(void);
void GFX_stop_making_main_menu_active(void);

void GFX_call_when_menus_are_closed(std::function<void(void)> callback);
