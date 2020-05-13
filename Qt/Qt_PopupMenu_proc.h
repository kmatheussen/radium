#pragma once

class QMenu;

QMenu *GFX_create_qmenu(const vector_t &v,
                        func_t *callback2);

bool GFX_MenuActive();
QMenu *GFX_GetActiveMenu(void);
void GFX_MakeMakeMainMenuActive(void);
