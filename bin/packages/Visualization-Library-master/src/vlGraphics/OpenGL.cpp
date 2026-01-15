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

#include <vlGraphics/OpenGL.hpp>
#include <vlCore/String.hpp>
#include <vlCore/Log.hpp>
#include <vlCore/Say.hpp>
#include <algorithm>

#if defined(VL_OPENGL_ES1) || defined(VL_OPENGL_ES2)
  #include <EGL/egl.h> // for eglGetProcAddress()
#endif

//-----------------------------------------------------------------------------
namespace vl
{
  bool Is_OpenGL_Initialized = false;
  bool Is_OpenGL_Core_Profile = false;
  bool Is_OpenGL_Forward_Compatible = false;

  bool Has_GL_Version_1_1 = false;
  bool Has_GL_Version_1_2 = false;
  bool Has_GL_Version_1_3 = false;
  bool Has_GL_Version_1_4 = false;
  bool Has_GL_Version_1_5 = false;
  bool Has_GL_Version_2_0 = false;
  bool Has_GL_Version_2_1 = false;
  bool Has_GL_Version_3_0 = false;
  bool Has_GL_Version_3_1 = false;
  bool Has_GL_Version_3_2 = false;
  bool Has_GL_Version_3_3 = false;
  bool Has_GL_Version_4_0 = false;
  bool Has_GL_Version_4_1 = false;

  bool Has_Fixed_Function_Pipeline = false;

  // GLES defines

  bool Has_GLES_Version_1_1 = false;
  bool Has_GLES_Version_2_0 = false;
  bool Has_GLES = false;

  // Helper defines

  bool Has_GLSL = false;
  bool Has_GLSL_120_Or_More = false;
  bool Has_GLSL_130_Or_More = false;
  bool Has_GLSL_140_Or_More = false;
  bool Has_GLSL_150_Or_More = false;
  bool Has_GLSL_330_Or_More = false;
  bool Has_GLSL_400_Or_More = false;
  bool Has_GLSL_410_Or_More = false;
  bool Has_Geometry_Shader = false;
  bool Has_BufferObject = false;
  bool Has_FBO = false;
  bool Has_PBO = false;
  bool Has_FBO_Multisample = false;
  bool Has_Cubemap_Textures = false;
  bool Has_Texture_Rectangle = false;
  bool Has_Texture_Array = false;
  bool Has_Texture_Buffer = false;
  bool Has_Texture_Multisample = false;
  bool Has_Texture_3D = false;
  bool Has_Multitexture = false;
  bool Has_Primitive_Restart = false;
  bool Has_Occlusion_Query = false;
  bool Has_Transform_Feedback = false;
  bool Has_glGenerateMipmaps = false;
  bool Has_GL_GENERATE_MIPMAP = false;
  bool Has_Point_Sprite = false;
  bool Has_Base_Vertex = false;
  bool Has_Primitive_Instancing = false;

  #define VL_EXTENSION(extension) bool Has_##extension = false;
  #include <vlGraphics/GL/GLExtensionList.hpp>
  #undef VL_EXTENSION

  #define VL_GLES_EXTENSION(extension) bool Has_##extension = false;
  #include <vlGraphics/GL/GLESExtensionList.hpp>
  #undef VL_GLES_EXTENSION

  #if defined(VL_OPENGL)
    #define VL_GL_FUNCTION(TYPE, NAME) TYPE NAME = NULL;
    #include <vlGraphics/GL/GLFunctionList.hpp>
    #undef VL_GL_FUNCTION
  #endif

  #if defined(VL_OPENGL_ES1)
    #define VL_GL_FUNCTION(TYPE, NAME) TYPE NAME = NULL;
    #include <vlGraphics/GL/GLES1FunctionList.hpp>
    #undef VL_GL_FUNCTION
  #endif

  #if defined(VL_OPENGL_ES2)
    #define VL_GL_FUNCTION(TYPE, NAME) TYPE NAME = NULL;
    #include <vlGraphics/GL/GLES2FunctionList.hpp>
    #undef VL_GL_FUNCTION
  #endif

  const GLenum Translate_Enable[] =
  {
    // Common ones
    GL_BLEND,
    GL_CULL_FACE,
    GL_DEPTH_TEST,
    GL_STENCIL_TEST,
    GL_DITHER,
    GL_POLYGON_OFFSET_FILL, 
    GL_POLYGON_OFFSET_LINE, 
    GL_POLYGON_OFFSET_POINT,
    GL_COLOR_LOGIC_OP, 
    GL_MULTISAMPLE,

    // Smoothing
    GL_POINT_SMOOTH,
    GL_LINE_SMOOTH, 
    GL_POLYGON_SMOOTH,

    // Stippling
    GL_LINE_STIPPLE,
    GL_POLYGON_STIPPLE,

    // Point sprites
    GL_POINT_SPRITE, 
    GL_PROGRAM_POINT_SIZE, 

    // Fixed function pipeline
    GL_ALPHA_TEST, 
    GL_LIGHTING, 
    GL_COLOR_SUM,
    GL_FOG,
    GL_NORMALIZE,
    GL_RESCALE_NORMAL,

    // Available only under OpenGL 2.x
    GL_VERTEX_PROGRAM_TWO_SIDE,

    // OpenGL 3.2
    GL_TEXTURE_CUBE_MAP_SEAMLESS,

    // OpenGL 3.0
    GL_CLIP_DISTANCE0,
    GL_CLIP_DISTANCE1,
    GL_CLIP_DISTANCE2,
    GL_CLIP_DISTANCE3,
    GL_CLIP_DISTANCE4,
    GL_CLIP_DISTANCE5,
    GL_CLIP_DISTANCE6,
    GL_CLIP_DISTANCE7,

    // Multisampling
    GL_SAMPLE_ALPHA_TO_COVERAGE,
    GL_SAMPLE_ALPHA_TO_ONE,
    GL_SAMPLE_COVERAGE
  };

  const char* Translate_Enable_String[] =
  {
    // Common ones
    "GL_BLEND",
    "GL_CULL_FACE",
    "GL_DEPTH_TEST",
    "GL_STENCIL_TEST",
    "GL_DITHER",
    "GL_POLYGON_OFFSET_FILL", 
    "GL_POLYGON_OFFSET_LINE", 
    "GL_POLYGON_OFFSET_POINT",
    "GL_COLOR_LOGIC_OP", 
    "GL_MULTISAMPLE",
    
    // Smoothing
    "GL_POINT_SMOOTH",
    "GL_LINE_SMOOTH", 
    "GL_POLYGON_SMOOTH",

    // Stippling
    "GL_LINE_STIPPLE",
    "GL_POLYGON_STIPPLE",

    // Point sprites
    "GL_POINT_SPRITE", 
    "GL_PROGRAM_POINT_SIZE", 

    // Fixed function pipeline
    "GL_ALPHA_TEST", 
    "GL_LIGHTING", 
    "GL_COLOR_SUM",
    "GL_FOG",
    "GL_NORMALIZE",
    "GL_RESCALE_NORMAL",

    // Available only under OpenGL 2.x
    "GL_VERTEX_PROGRAM_TWO_SIDE",

    // OpenGL 3.2
    "GL_TEXTURE_CUBE_MAP_SEAMLESS",

    // OpenGL 3.0
    "GL_CLIP_DISTANCE0",
    "GL_CLIP_DISTANCE1",
    "GL_CLIP_DISTANCE2",
    "GL_CLIP_DISTANCE3",
    "GL_CLIP_DISTANCE4",
    "GL_CLIP_DISTANCE5",
    "GL_CLIP_DISTANCE6",
    "GL_CLIP_DISTANCE7",

    // Multisampling
    "GL_SAMPLE_ALPHA_TO_COVERAGE",
    "GL_SAMPLE_ALPHA_TO_ONE",
    "GL_SAMPLE_COVERAGE"
  };

  bool Is_Enable_Supported[EN_EnableCount] = 
  {
    // Common ones
    false /*GL_BLEND*/,
    false /*GL_CULL_FACE*/,
    false /*GL_DEPTH_TEST*/,
    false /*GL_STENCIL_TEST*/,
    false /*GL_DITHER*/,
    false /*GL_POLYGON_OFFSET_FILL*/, 
    false /*GL_POLYGON_OFFSET_LINE*/, 
    false /*GL_POLYGON_OFFSET_POINT*/,
    false /*GL_COLOR_LOGIC_OP*/, 
    false /*GL_MULTISAMPLE*/,

    // Smoothing
    false /*GL_POINT_SMOOTH*/,
    false /*GL_LINE_SMOOTH*/, 
    false /*GL_POLYGON_SMOOTH*/,

    // Stippling
    false /*GL_LINE_STIPPLE*/,
    false /*GL_POLYGON_STIPPLE*/,

    // Point sprites
    false /*GL_POINT_SPRITE*/, 
    false /*GL_PROGRAM_POINT_SIZE*/, 

    // Fixed function pipeline
    false /*GL_ALPHA_TEST*/, 
    false /*GL_LIGHTING*/, 
    false /*GL_COLOR_SUM*/,
    false /*GL_FOG*/,
    false /*GL_NORMALIZE*/,
    false /*GL_RESCALE_NORMAL*/,

    // Available only under OpenGL 2.x
    false /*GL_VERTEX_PROGRAM_TWO_SIDE*/,

    // OpenGL 3.2
    false /*GL_TEXTURE_CUBE_MAP_SEAMLESS*/,

    // OpenGL 3.0
    false /*GL_CLIP_DISTANCE0*/,
    false /*GL_CLIP_DISTANCE1*/,
    false /*GL_CLIP_DISTANCE2*/,
    false /*GL_CLIP_DISTANCE3*/,
    false /*GL_CLIP_DISTANCE4*/,
    false /*GL_CLIP_DISTANCE5*/,
    false /*GL_CLIP_DISTANCE6*/,
    false /*GL_CLIP_DISTANCE7*/,

    // Multisampling
    false /*GL_SAMPLE_ALPHA_TO_COVERAGE*/,
    false /*GL_SAMPLE_ALPHA_TO_ONE*/,
    false /*GL_SAMPLE_COVERAGE*/
  };

  VL_COMPILE_TIME_CHECK( EN_EnableCount == sizeof(Is_Enable_Supported) / sizeof(Is_Enable_Supported[0]) );
  VL_COMPILE_TIME_CHECK( EN_EnableCount == sizeof(Translate_Enable) / sizeof(Translate_Enable[0]) );
  VL_COMPILE_TIME_CHECK( EN_EnableCount == sizeof(Translate_Enable_String) / sizeof(Translate_Enable_String[0]) );
}
//-----------------------------------------------------------------------------
bool vl::initializeOpenGL()
{
  Is_OpenGL_Initialized = false;

  // clear errors
  glGetError();

  // check OpenGL context is present
  if (glGetError() != GL_NO_ERROR)
    return false;

  // - - - OpenGL function pointers - - -

  // opengl function pointer initialization
  #if defined(VL_OPENGL)
    #define VL_GL_FUNCTION(TYPE, NAME) NAME = (TYPE)getGLProcAddress(#NAME);
    #include <vlGraphics/GL/GLFunctionList.hpp>
  #endif

  // opengl function pointer initialization
  #if defined(VL_OPENGL_ES1)
    #define VL_GL_FUNCTION(TYPE, NAME) NAME = (TYPE)getGLProcAddress(#NAME);
    #include <vlGraphics/GL/GLES1FunctionList.hpp>
  #endif

  // opengl function pointer initialization
  #if defined(VL_OPENGL_ES2)
    #define VL_GL_FUNCTION(TYPE, NAME) NAME = (TYPE)getGLProcAddress(#NAME);
    #include <vlGraphics/GL/GLES2FunctionList.hpp>
  #endif

  // - - - OpenGL versions - - -

  // GLES detect
  #if defined(VL_OPENGL_ES1)
    Has_GLES = Has_GLES_Version_1_1 = true;
  #endif

  #if defined(VL_OPENGL_ES2)
    Has_GLES = Has_GLES_Version_2_0 = true;
  #endif

  // GL versions
  // OpenGL ES returns "OpenGL ES-XX N.M"
  const char* version_string = (const char*)glGetString(GL_VERSION);

  // These stay zero for GLES
  const int vmaj = Has_GLES ? 0 : version_string[0] - '0';
  const int vmin = Has_GLES ? 0 : version_string[2] - '0';

  // Check fixed function pipeline
#if defined(VL_OPENGL_ES2)
  Is_OpenGL_Core_Profile = false;
  Is_OpenGL_Forward_Compatible = false;
  Has_Fixed_Function_Pipeline = false;
#elif defined(VL_OPENGL_ES1)
  Is_OpenGL_Core_Profile = false;
  Is_OpenGL_Forward_Compatible = false;
  Has_Fixed_Function_Pipeline = true;
#else
  Is_OpenGL_Forward_Compatible = false;
  if( vmaj >= 3 )
  {
    int forward_compatible = 0;
    glGetIntegerv( GL_CONTEXT_FLAGS, &forward_compatible ); VL_CHECK_OGL();
    Is_OpenGL_Forward_Compatible = (forward_compatible & GL_CONTEXT_FLAG_FORWARD_COMPATIBLE_BIT) != 0;
  }

  Is_OpenGL_Core_Profile = false;
  const int version = vmaj*10 + vmin;
  if( version >= 32 )
  {
    // Valid for WGL and GLX
    #define CONTEXT_CORE_PROFILE_BIT          0x00000001
    #define CONTEXT_COMPATIBILITY_PROFILE_BIT 0x00000002
    #define CONTEXT_PROFILE_MASK              0x9126

    // Note: 
    // - This should be non-0 by when using wglCreateContextAttribs() and is == 0 when creating a GL context in the old way.
    // - Creating a context in the old way returns the highest compatible OpenGL version available thus the presence 
    //   of CONTEXT_COMPATIBILITY_PROFILE_BIT is not enough, we need to check the absence of CONTEXT_CORE_PROFILE_BIT
    int context_flags = 0;
    glGetIntegerv( CONTEXT_PROFILE_MASK, &context_flags ); VL_CHECK_OGL();
    Is_OpenGL_Core_Profile = (context_flags & CONTEXT_CORE_PROFILE_BIT) != 0;
  }

  Has_Fixed_Function_Pipeline = !Is_OpenGL_Forward_Compatible && !Is_OpenGL_Core_Profile;
#endif

  Has_GL_Version_1_1 = (vmaj == 1 && vmin >= 1) || (vmaj > 1 && Has_Fixed_Function_Pipeline);
  Has_GL_Version_1_2 = (vmaj == 1 && vmin >= 2) || (vmaj > 1 && Has_Fixed_Function_Pipeline);
  Has_GL_Version_1_3 = (vmaj == 1 && vmin >= 3) || (vmaj > 1 && Has_Fixed_Function_Pipeline);
  Has_GL_Version_1_4 = (vmaj == 1 && vmin >= 4) || (vmaj > 1 && Has_Fixed_Function_Pipeline);
  Has_GL_Version_1_5 = (vmaj == 1 && vmin >= 5) || (vmaj > 1 && Has_Fixed_Function_Pipeline);
  Has_GL_Version_2_0 = (vmaj == 2 && vmin >= 0) || (vmaj > 2 && Has_Fixed_Function_Pipeline);
  Has_GL_Version_2_1 = (vmaj == 2 && vmin >= 1) || (vmaj > 2 && Has_Fixed_Function_Pipeline);
  Has_GL_Version_3_0 = (vmaj == 3 && vmin >= 0) || (vmaj > 3 && Has_Fixed_Function_Pipeline);
  Has_GL_Version_3_1 = (vmaj == 3 && vmin >= 1) || (vmaj > 3 && Has_Fixed_Function_Pipeline);
  Has_GL_Version_3_2 = (vmaj == 3 && vmin >= 2) || (vmaj > 3 && Has_Fixed_Function_Pipeline);
  Has_GL_Version_3_3 = (vmaj == 3 && vmin >= 3) || (vmaj > 3 && Has_Fixed_Function_Pipeline);
  Has_GL_Version_4_0 = (vmaj == 4 && vmin >= 0) || (vmaj > 4 && Has_Fixed_Function_Pipeline);
  Has_GL_Version_4_1 = (vmaj == 4 && vmin >= 1) || (vmaj > 4 && Has_Fixed_Function_Pipeline);

  // - - - Extension strings init - - -

  std::string extensions = getOpenGLExtensions();

  #define VL_EXTENSION(extension) Has_##extension = strstr(extensions.c_str(), #extension" ") != NULL;
    #include <vlGraphics/GL/GLExtensionList.hpp>
  #undef VL_EXTENSION

  #define VL_GLES_EXTENSION(extension) Has_##extension = strstr(extensions.c_str(), #extension" ") != NULL;
    #include <vlGraphics/GL/GLESExtensionList.hpp>
  #undef VL_GLES_EXTENSION

#if defined(VL_OPENGL_ES1)
  // mic fixme
  // POWERVR emulator bugs: http://www.imgtec.com/forum/forum_posts.asp?TID=1379
  if (Has_GL_OES_texture_cube_map && glTexGenfOES == 0)
  {
    Has_GL_OES_texture_cube_map = false;
    Has_Cubemap_Textures = false;
    Log::error("GL_OES_texture_cube_map exposed but glTexGenfOES == NULL!\n"); /*VL_TRAP();*/
  }
  if(Has_GL_OES_blend_func_separate && glBlendFuncSeparateOES == 0)
  {
    Has_GL_OES_blend_func_separate = false;
    Log::error("GL_OES_blend_func_separate exposed but glBlendFuncSeparateOES == NULL!\n"); /*VL_TRAP();*/
  }
  if (Has_GL_OES_fixed_point && glAlphaFuncxOES == NULL)
  {
    Log::warning("GL_OES_fixed_point functions are not exposed with their OES suffix!\n");
  }
  if (Has_GL_OES_single_precision && glDepthRangefOES == NULL)
  {
    Log::warning("GL_OES_single_precision functions are not exposed with their OES suffix!\n");
  }
#endif

  // - - - Helper defines - - - 

  // Note that GL extensions pertaining to deprecated features seem to be exposed under Core profiles even if they are not supported (like Has_GL_SGIS_generate_mipmap)

  Has_GLSL = Has_GL_ARB_shading_language_100 || Has_GL_Version_2_0 || Has_GL_Version_3_0 || Has_GL_Version_4_0 || Has_GLES_Version_2_0;
  Has_GLSL_120_Or_More = Has_GL_Version_2_1 || Has_GL_Version_3_0 || Has_GL_Version_4_0;
  Has_GLSL_130_Or_More = Has_GL_Version_3_0 || Has_GL_Version_4_0;
  Has_GLSL_140_Or_More = Has_GL_Version_3_1 || Has_GL_Version_4_0;
  Has_GLSL_150_Or_More = Has_GL_Version_3_2 || Has_GL_Version_4_0;
  Has_GLSL_330_Or_More = Has_GL_Version_3_3 || Has_GL_Version_4_0;
  Has_GLSL_400_Or_More = Has_GL_Version_4_0;
  Has_GLSL_410_Or_More = Has_GL_Version_4_1;
  Has_Geometry_Shader  = Has_GL_NV_geometry_shader4 || Has_GL_EXT_geometry_shader4 || Has_GL_ARB_geometry_shader4 || Has_GL_Version_3_2 || Has_GL_Version_4_0;
  Has_BufferObject = Has_GL_ARB_vertex_buffer_object || Has_GL_Version_1_5 || Has_GL_Version_3_0 || Has_GL_Version_4_0 || Has_GLES;
  Has_FBO = Has_GL_EXT_framebuffer_object || Has_GL_ARB_framebuffer_object || Has_GL_Version_3_0 || Has_GL_Version_4_0 || Has_GL_OES_framebuffer_object || Has_GLES_Version_2_0;
  Has_PBO = Has_GL_ARB_pixel_buffer_object || Has_GL_EXT_pixel_buffer_object || Has_GL_Version_2_1 || Has_GL_Version_3_0 || Has_GL_Version_4_0;
  // We only support GL_ANGLE_framebuffer_blit for GLES, see also:
  // http://www.khronos.org/registry/gles/extensions/IMG/IMG_multisampled_render_to_texture.txt
  // http://www.khronos.org/registry/gles/extensions/APPLE/APPLE_framebuffer_multisample.txt
  Has_FBO_Multisample = Has_GL_Version_4_0 || Has_GL_Version_3_0 || Has_GL_ARB_framebuffer_object || Has_GL_EXT_framebuffer_multisample || Has_GL_ANGLE_framebuffer_multisample;
  Has_Cubemap_Textures = Has_GL_ARB_texture_cube_map || Has_GL_Version_1_3 || Has_GL_Version_3_0 || Has_GL_Version_4_0 || Has_GL_OES_texture_cube_map || Has_GLES_Version_2_0;
  Has_Texture_Rectangle = Has_GL_ARB_texture_rectangle || Has_GL_NV_texture_rectangle || Has_GL_Version_3_1 || Has_GL_Version_4_0;
  Has_Texture_Array = Has_GL_EXT_texture_array || Has_GL_Version_3_0 || Has_GL_Version_4_0;
  Has_Texture_Buffer = Has_GL_ARB_texture_buffer_object || Has_GL_EXT_texture_buffer_object || Has_GL_Version_3_1 || Has_GL_Version_4_0;
  Has_Texture_Multisample = Has_GL_ARB_texture_multisample || Has_GL_Version_3_2 || Has_GL_Version_4_0;
  Has_Texture_3D = Has_GL_EXT_texture3D || Has_GL_Version_1_2 || Has_GL_Version_3_0 || Has_GL_Version_4_0 || Has_GL_OES_texture_3D;
  Has_Multitexture = Has_GL_ARB_multitexture || Has_GL_Version_1_3 || Has_GL_Version_3_0 || Has_GL_Version_4_0 || Has_GLES;
  Has_Primitive_Restart = Has_GL_Version_3_1 || Has_GL_Version_4_0;
  Has_Occlusion_Query = Has_GL_ARB_occlusion_query || Has_GL_Version_1_5 || Has_GL_Version_3_0 || Has_GL_Version_4_0;
  Has_Transform_Feedback = Has_GL_NV_transform_feedback || Has_GL_EXT_transform_feedback || Has_GL_Version_3_0 || Has_GL_Version_4_0;
  Has_glGenerateMipmaps = Has_GL_ARB_framebuffer_object || Has_GL_Version_3_0 || Has_GL_Version_4_0 || Has_GLES_Version_2_0;
  Has_GL_GENERATE_MIPMAP = (Has_GL_SGIS_generate_mipmap && Has_Fixed_Function_Pipeline) || Has_GL_Version_1_4 || Has_GLES_Version_1_1;
  Has_Point_Sprite = Has_GL_NV_point_sprite || Has_GL_ARB_point_sprite || Has_GLSL || Has_GLES_Version_1_1;
  Has_Base_Vertex = Has_GL_Version_3_2 || Has_GL_Version_4_0 || Has_GL_ARB_draw_elements_base_vertex;
  Has_Primitive_Instancing = Has_GL_Version_3_1 || Has_GL_Version_4_0 || Has_GL_ARB_draw_instanced || Has_GL_EXT_draw_instanced;

  // - - - Resolve supported enables - - -

  // Common ones
  Is_Enable_Supported[EN_BLEND]        = true;
  Is_Enable_Supported[EN_CULL_FACE]    = true;
  Is_Enable_Supported[EN_DEPTH_TEST]   = true;
  Is_Enable_Supported[EN_STENCIL_TEST] = true;
  Is_Enable_Supported[EN_DITHER]       = true;
  Is_Enable_Supported[EN_POLYGON_OFFSET_FILL]  = true;
  Is_Enable_Supported[EN_POLYGON_OFFSET_LINE]  = !Has_GLES;
  Is_Enable_Supported[EN_POLYGON_OFFSET_POINT] = !Has_GLES;
  Is_Enable_Supported[EN_COLOR_LOGIC_OP]       = !Has_GLES_Version_2_0;
  Is_Enable_Supported[EN_MULTISAMPLE]          = !Has_GLES_Version_2_0;

  // Smooth
  Is_Enable_Supported[EN_POINT_SMOOTH]   = Has_GL_Version_1_1||Has_GLES_Version_1_1;
  Is_Enable_Supported[EN_LINE_SMOOTH]    = !Has_GLES_Version_2_0;
  Is_Enable_Supported[EN_POLYGON_SMOOTH] = !Has_GLES;

  // Stipple
  Is_Enable_Supported[EN_LINE_STIPPLE]    = Has_GL_Version_1_1;
  Is_Enable_Supported[EN_POLYGON_STIPPLE] = Has_GL_Version_1_1;

  // Point sprites
  // Point sprites when !Has_Fixed_Function_Pipeline is considered always enabled but GL_POINT_SPRITE should not be called even if GL_OES_point_sprite, GL_ARB_point_sprite etc. are exposed!
  // Note that calling glIsEnabled() with the two below under a Core profile returns true for the same reason.
  Is_Enable_Supported[EN_POINT_SPRITE]       = (Has_GL_NV_point_sprite||Has_GL_ARB_point_sprite||Has_GL_Version_2_0||Has_GL_OES_point_sprite||Has_GLES_Version_1_1) && Has_Fixed_Function_Pipeline;
  Is_Enable_Supported[EN_PROGRAM_POINT_SIZE] = Has_GLSL && !Has_GLES_Version_2_0; // Only OpenGL ES 2 does not support glPointSize()/GL_POINT_SIZE

  // Fixed function pipeline
  Is_Enable_Supported[EN_ALPHA_TEST]     = Has_GL_Version_1_1||Has_GLES_Version_1_1;
  Is_Enable_Supported[EN_LIGHTING]       = Has_GL_Version_1_1||Has_GLES_Version_1_1;
  Is_Enable_Supported[EN_COLOR_SUM]      = Has_GL_Version_1_1;
  Is_Enable_Supported[EN_FOG]            = Has_GL_Version_1_1||Has_GLES_Version_1_1;
  Is_Enable_Supported[EN_NORMALIZE]      = Has_GL_Version_1_1||Has_GLES_Version_1_1;
  Is_Enable_Supported[EN_RESCALE_NORMAL] = Has_GL_Version_1_2||Has_GLES_Version_1_1;

  // Available only under OpenGL 2.x
  Is_Enable_Supported[EN_VERTEX_PROGRAM_TWO_SIDE]   = ((Has_GL_ARB_vertex_program||Has_GL_NV_vertex_program) && Has_GL_Version_1_1) || Has_GL_Version_2_0;

  // OpenGL 3.2
  Is_Enable_Supported[EN_TEXTURE_CUBE_MAP_SEAMLESS] = Has_GL_AMD_seamless_cubemap_per_texture||Has_GL_ARB_seamless_cube_map||Has_GL_Version_3_2||Has_GL_Version_4_0;
  
  // Clipping planes
  int max_clip_planes = 0;
  // OpenGL ES 2 is the only one without clipping planes!
  if (!Has_GLES_Version_2_0)
  {
    glGetIntegerv(GL_MAX_CLIP_DISTANCES, &max_clip_planes); // GL_MAX_CLIP_DISTANCES == GL_MAX_CLIP_PLANES
  }
  Is_Enable_Supported[EN_CLIP_DISTANCE0] = max_clip_planes >= 1;
  Is_Enable_Supported[EN_CLIP_DISTANCE1] = max_clip_planes >= 2;
  Is_Enable_Supported[EN_CLIP_DISTANCE2] = max_clip_planes >= 3;
  Is_Enable_Supported[EN_CLIP_DISTANCE3] = max_clip_planes >= 4;
  Is_Enable_Supported[EN_CLIP_DISTANCE4] = max_clip_planes >= 5;
  Is_Enable_Supported[EN_CLIP_DISTANCE5] = max_clip_planes >= 6;
  Is_Enable_Supported[EN_CLIP_DISTANCE6] = max_clip_planes >= 7;
  Is_Enable_Supported[EN_CLIP_DISTANCE7] = max_clip_planes >= 8;

  // Multisampling
  Is_Enable_Supported[EN_SAMPLE_ALPHA_TO_COVERAGE] = Has_GL_Version_1_3||!Has_Fixed_Function_Pipeline||Has_GLES;
  Is_Enable_Supported[EN_SAMPLE_ALPHA_TO_ONE]      = Has_GL_Version_1_3||Has_GL_Version_3_0||Has_GL_Version_4_0||Has_GLES_Version_1_1;
  Is_Enable_Supported[EN_SAMPLE_COVERAGE]          = Has_GL_Version_1_3||!Has_Fixed_Function_Pipeline||Has_GLES;

#ifndef NDEBUG
  // Enables management debug code
  VL_CHECK_OGL();
  bool got_error = false;
  for(int i=0; i<EN_EnableCount; ++i)
  {
    glDisable(Translate_Enable[i]); // glIsEnabled() for some reason is not reliable!
    bool supported = glGetError() == GL_NO_ERROR;
    if (supported != Is_Enable_Supported[i])
    {
      Log::error( Say("%s: capability %s supported! This is a harmless glitch either in your GL driver or in VL.\n") << Translate_Enable_String[i] << (supported? "*IS*" : "*IS NOT*") );
      got_error = true;
    }
  }
  if(got_error)
  {
    printf("OpenGL Version = %s\n", glGetString(GL_VERSION));
    #define PRINT_INFO(STRING) printf(#STRING" = %d\n", STRING?1:0)
    PRINT_INFO(Is_OpenGL_Core_Profile);
    PRINT_INFO(Is_OpenGL_Forward_Compatible);
    PRINT_INFO(Has_GL_Version_4_1);
    PRINT_INFO(Has_GL_Version_4_0);
    PRINT_INFO(Has_GL_Version_3_3);
    PRINT_INFO(Has_GL_Version_3_2);
    PRINT_INFO(Has_GL_Version_3_1);
    PRINT_INFO(Has_GL_Version_3_0);
    PRINT_INFO(Has_GL_Version_2_1);
    PRINT_INFO(Has_GL_Version_2_0);
    PRINT_INFO(Has_GL_Version_1_5);
    PRINT_INFO(Has_GL_Version_1_4);
    PRINT_INFO(Has_GL_Version_1_3);
    PRINT_INFO(Has_GL_Version_1_2);
    PRINT_INFO(Has_GL_Version_1_1);
    // VL_TRAP();
  }
#endif

  VL_CHECK_OGL();
  return Is_OpenGL_Initialized = true;
}
//-----------------------------------------------------------------------------
const char* vl::getGLErrorString(int err)
{
  switch(err)
  {
  case GL_INVALID_ENUM:      return "Invalid enum";
  case GL_INVALID_VALUE:     return "Invalid value";
  case GL_INVALID_OPERATION: return "Invalid operation";
  case GL_STACK_OVERFLOW:    return "Stack overflow";
  case GL_STACK_UNDERFLOW:   return "Stack underflow";
  case GL_OUT_OF_MEMORY:     return "Out of memory";
  default:
    return "";
  }
}
//------------------------------------------------------------------------------
int vl::glcheck(const char* file, int line)
{
  unsigned int glerr = glGetError();
  // if an OpenGL context is available this must be clear!
  if ( glGetError() )
  {
    Log::bug( Say("%s:%n: NO OPENGL CONTEXT ACTIVE!\n") << file << line );
  }
  else
  if (glerr != GL_NO_ERROR)
  {
    String msg( (char*)getGLErrorString(glerr) );
    Log::bug( Say("glGetError() [%s:%n]: %s\n") << file << line << msg );
  }
  return glerr;
}
//------------------------------------------------------------------------------
// vl::getGLProcAddress() implementation based on GLEW's
//------------------------------------------------------------------------------
#if defined(VL_OPENGL_ES1) || defined(VL_OPENGL_ES2)
void* vl::getGLProcAddress(const char* name)
{
  void* func = (void*)eglGetProcAddress(name);
  /*if (func)
    Log::warning( String().printf("+ %s\n", name) );
  else
    Log::error( String().printf("- %s\n", name) );*/
  return func;
}
#elif defined(VL_PLATFORM_WINDOWS)
void* vl::getGLProcAddress(const char* name)
{
  return (void*)wglGetProcAddress((LPCSTR)name);
}
#elif defined(VL_PLATFORM_LINUX)
void* vl::getGLProcAddress(const char* name)
{
  return (void*)(*glXGetProcAddress)((const GLubyte*)name);
}
#elif defined(__APPLE__)
#include <stdlib.h>
#include <string.h>
#include <AvailabilityMacros.h>

#ifdef MAC_OS_X_VERSION_10_3

#include <dlfcn.h>

void* vl::getGLProcAddress(const char* name)
{
  static void* image = NULL;
  if (NULL == image)
  {
    image = dlopen("/System/Library/Frameworks/OpenGL.framework/Versions/Current/OpenGL", RTLD_LAZY);
  }
  return image ? dlsym(image, name) : NULL;
}

#else

#include <mach-o/dyld.h>

void* vl::getGLProcAddress(const char*name)
{
  static const struct mach_header* image = NULL;
  NSSymbol symbol;
  char* symbolName;
  if (NULL == image)
  {
    image = NSAddImage("/System/Library/Frameworks/OpenGL.framework/Versions/Current/OpenGL", NSADDIMAGE_OPTION_RETURN_ON_ERROR);
  }
  /* prepend a '_' for the Unix C symbol mangling convention */
  symbolName = malloc(strlen(name) + 2);
  strcpy(symbolName+1, name);
  symbolName[0] = '_';
  symbol = NULL;
  /* if (NSIsSymbolNameDefined(symbolName))
	 symbol = NSLookupAndBindSymbol(symbolName); */
  symbol = image ? NSLookupSymbolInImage(image, symbolName, NSLOOKUPSYMBOLINIMAGE_OPTION_BIND | NSLOOKUPSYMBOLINIMAGE_OPTION_RETURN_ON_ERROR) : NULL;
  free(symbolName);
  return symbol ? NSAddressOfSymbol(symbol) : NULL;
}

#endif /* MAC_OS_X_VERSION_10_3 */

/* __APPLE__ end */

#elif defined(__sgi) || defined (__sun)

#include <dlfcn.h>
#include <stdio.h>
#include <stdlib.h>

void* vl::getGLProcAddress(const char* name)
{
  static void* h = NULL;
  static void* gpa;

  if (h == NULL)
  {
    if ((h = dlopen(NULL, RTLD_LAZY | RTLD_LOCAL)) == NULL) return NULL;
    gpa = dlsym(h, "glXGetProcAddress");
  }

  if (gpa != NULL)
    return ((void*(*)(const GLubyte*))gpa)((const GLubyte*)name);
  else
    return dlsym(h, name);
}

/* __sgi || __sun end */

#else
void* vl::getGLProcAddress(const char* name)
{
  return NULL;
}
#endif
//------------------------------------------------------------------------------
