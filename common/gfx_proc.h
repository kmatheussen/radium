#ifndef GFX_PROC_H
#define GFX_PROC_H

#include "blts_proc.h"
#include "../api/api_requesters_proc.h"
#include "windows_proc.h"


#define DO_GFX_BLT(OP) do{               \
    Blt_markVisible(window);                    \
    {                                           \
      OP;                                       \
    }                                           \
    Blt_clearNotUsedVisible(window);            \
    Blt_blt(window);                            \
  }while(0)

#define DO_GFX(OP) do{                          \
    DO_GFX_BLT(OP);                             \
    closeRequester();                           \
    checkIfWBlocksAreDirty();                   \
  }while(0)

#endif
