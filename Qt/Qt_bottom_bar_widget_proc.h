
#pragma once

extern LANGSPEC bool EDITOR_switch_drunk_velocity(void);
  
#ifdef USE_QT4
QWidget *BottomBar_create(QWidget *parent, bool remove_editor_elements, bool include_navigator);
#endif
