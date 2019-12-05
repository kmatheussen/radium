#ifndef GFX_PROC_H
#define GFX_PROC_H

#include "blts_proc.h"
#include "windows_proc.h"
#include "gfx_op_queue_proc.h"

#if !USE_OPENGL

#define DO_GFX(OP) do{                                                  \
    if(GFX_get_op_queue_size(window) >0 || window->must_redraw==true ){ \
      window->must_redraw = true;                                       \
      GFX_clear_op_queue(window);                                       \
    }else                                                               \
      Blt_markVisible(window);                                          \
    OP;                                                                 \
    if(window->must_redraw==false){                                     \
      checkIfWBlocksAreDirty();                                         \
      Blt_clearNotUsedVisible(window);                                  \
      Blt_blt(window);                                                   \
    }                                                                   \
  }while(0)

#else

#define DO_GFX(OP) do{                                                  \
    if(GFX_get_op_queue_size(window) >0 || window->must_redraw==true ){ \
      window->must_redraw = true;                                       \
      GFX_clear_op_queue(window);                                       \
    }                                                                   \
    OP;                                                                 \
    if(window->must_redraw==false)                                      \
      checkIfWBlocksAreDirty();                                         \
  }while(0)

#endif


#endif
