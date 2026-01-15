/**************************************************************************************/
/*                                                                                    */
/*  Visualization Library                                                             */
/*  http://www.visualizationlibrary.org                                               */
/*                                                                                    */
/*  Copyright (c) 2005-2011, Michele Bosi                                             */
/*  All rights reserved.                                                              */
/*                                                                                    */
/*  Redistribution and use in source and binary forms, with or without modification,  */
/*  are permitted provided that the following conditions are met:                     */
/*                                                                                    */
/*  - Redistributions of source code must retain the above copyright notice, this     */
/*  list of conditions and the following disclaimer.                                  */
/*                                                                                    */
/*  - Redistributions in binary form must reproduce the above copyright notice, this  */
/*  list of conditions and the following disclaimer in the documentation and/or       */
/*  other materials provided with the distribution.                                   */
/*                                                                                    */
/*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND   */
/*  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED     */
/*  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE            */
/*  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR  */
/*  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES    */
/*  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;      */
/*  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON    */
/*  ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT           */
/*  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS     */
/*  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                      */
/*                                                                                    */
/**************************************************************************************/

#ifndef OpenGLDefs_INCLUDE_ONCE
#define OpenGLDefs_INCLUDE_ONCE

#include <vlCore/checks.hpp>

#if defined(VL_OPENGL_ES1)

  #include <GLES/khronos_gl.h>
  #include <GLES/khronos_glext.h>
  #include <GLES/gles_extra_defines.h> // defines used by VL but not present in GLES 1.x

#elif defined(VL_OPENGL_ES2)

  #include <GLES2/khronos_gl2.h>
  #include <GLES2/khronos_gl2ext.h>
  #include <GLES2/gles_extra_defines.h> // defines used by VL but not present in GLES 2.x

#elif defined(VL_OPENGL)

  #if defined(VL_PLATFORM_WINDOWS)

    #include <GL/mesa_gl.h>
    #include <GL/glu.h>
    #include <GL/khronos_glext.h>
    #include <GL/khronos_wglext.h>

  #elif defined(VL_PLATFORM_LINUX)

    #include <GL/mesa_gl.h>
    #include <GL/glu.h>
    #include <GL/khronos_glext.h>
    extern "C" { extern void ( * glXGetProcAddress (const GLubyte *procName)) (void); }

  #elif defined(VL_PLATFORM_MACOSX)

    #include <GL/mesa_gl.h>
    #include <OpenGL/glu.h>
    #include <GL/khronos_glext.h>

  #else

    #error Unknown platform!

  #endif

#endif

/* Define NULL */
#ifndef NULL
  #define NULL 0
#endif

#endif
