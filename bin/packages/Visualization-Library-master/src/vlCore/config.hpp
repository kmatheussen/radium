/**************************************************************************************/
/*                                                                                    */
/*  Visualization Library                                                             */
/*  http://www.visualizationlibrary.org                                               */
/*                                                                                    */
/*  Copyright (c) 2005-2010, Michele Bosi                                             */
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

///////////////////////////////////////////////////////////////////////////////
// Visualization Library Configuration File
///////////////////////////////////////////////////////////////////////////////

/**
 * \file config.hpp
 * Visualization Library configuration file.
*/

#ifndef VISUALIZATION_LIBRARY_CONFIG_INCLUDE_ONCE
#define VISUALIZATION_LIBRARY_CONFIG_INCLUDE_ONCE

#include <vlCore/platform.hpp>

/**
 * Enable/disable memory leaks debugging.
 *
 * - 0 = disable memory leaks debugging
 * - 1 = Objects::mDebug_LivingObjects will contain the set of currently living Object-s.
 *
 * This is useful when you want to track memory leaks and object construction/destruction.
 */
#define VL_DEBUG_LIVING_OBJECTS 0


/**
 * Forces checks to be done also in non debug modes.
 *
 * if set to 1 the VL_CHECK() and VL_CHECK_OGL() macros will be active also in release mode builds
 * if set to 0 the VL_CHECK() and VL_CHECK_OGL() macros will be active only in debug mode builds
 */
#define VL_FORCE_CHECKS 0


/**
 * Show a message box upon check failure. Only for Win32 platforms.
 * 1 = opens a MessageBox on failed check\n
 * 0 = opens a MessageBox on failed check\n
 */
#define VL_MESSAGEBOX_CHECK 0


/**
 * This define is used to set Visualization Library's pipeline precision.
 *
 * - 1 = use floating point single precision pipeline
 * - 2 = use floating point double precision pipeline
 *
 * In single precision mode the classes \p vec4, \p vec3, \p vec2, \p mat4, \p mat3, \p mat2 will be defined as typedefs to their \p fvec4, \p fmat4 etc. counter parts.
 *
 * In double precision mode the classes \p vec4, \p vec3, \p vec2, \p mat4, \p mat3, \p mat2 will be defined as typedefs to their \p dvec4, \p dmat4 etc. counter parts.
 *
 * In single precision mode vl::real is defined as \p float, in double precision mode vl::real is defined as \p double.
 *
 * Other classes are affected as well, in particular vl::Transform, vl::quat, vl::AABB and vl::Sphere.
 */
#define VL_PIPELINE_PRECISION 1


/**
 * Enables fast square root computation when using single precision mode.
 *
 * - 0 = disable fast square roots when in single precision floating point pipeline
 * - 1 = enable fast square roots when in single precision floating point pipeline
 *
 * Potential performance improvements:
 * - float sqrt will be up to 1.4x quicker
 * - float 1.0/sqrt will be up to 3x quicker
 * - vec3 normalization will be up to 2x quicker
 *
 * Please note that the precision of such operations is seriously affected.
 * Use with care! Under some platforms / compiler configurations this might produce wrong results,
 * like objects disappearing, transforms, matrices and vectors filled with garbage data etc.
 */
#define VL_FAST_SQUARE_ROOTS 0


/**
 * Set this to 1 to be able to attach user data to any vl::Object using the 
 * "setUserData(void*)" and "void* userData()" methods.
 * Useful to glue VL classes to the user's application logic.
 * \note This will add 4 or 8 bytes to each vl::Object instance.
 */
#define VL_OBJECT_USER_DATA 0


/**
 * Set this to 1 to be able to attach user data to any vl::Actor using the 
 * "setActorUserData(void*)" and "void* actorUserData()" methods. 
 * Useful to glue VL classes to the user's application logic.
 * \note This will add 4 or 8 bytes to each vl::Actor instance.
 */
#define VL_ACTOR_USER_DATA 0


/**
 * Set this to 1 to be able to attach user data to any vl::Transform using the 
 * "void setTransformUserData(void*)" and "void* transformUserData()" methods. 
 * Useful to glue Transform objects to the user's application animation engine.
 * \note This will add 4 or 8 bytes to each vl::Transform instance.
 */
#define VL_TRANSFORM_USER_DATA 0


/**
 * Set this to 1 to be able to attach user data to any vl::Shader using the 
 * "void setShaderUserData(void*)" and "void* shaderUserData()" methods. 
 * Useful to glue Shader objects and the user's application logic.
 * \note This will add 4 or 8 bytes to each vl::Shader instance.
 */
#define VL_SHADER_USER_DATA 0


/**
 * Defines the maximum number of LOD levels available to the Actor class.
 * Set this value to optimize VL to your application's needs.
 *
 * - minimum = 1
 * - maximum = application dependent
 */
#define VL_MAX_ACTOR_LOD 4


/**
 * Defines the maximum number of LOD levels available to the Effect class.
 * Set this value to optimize VL to your application's needs.
 *
 * - minimum = 1
 * - maximum = application dependent
 */
#define VL_MAX_EFFECT_LOD 4


/**
 * Maximum number of texture units used.
 * Allows VL to keep track of only the effectively used texture units.
 * Set this value to optimize VL to your application's needs.
 *
 * - minimum = 1
 * - maximum = OpenGL implementation dependent
 */
#define VL_MAX_TEXTURE_UNITS 8


/**
 * Maximum number of generic vertex attributes used by a single Geometry.
 * Allows VL to keep track of only the effectively used vertex attributes.
 * Set this value to optimize VL to your application's needs.
 *
 * - minimum = 1
 * - maximum = OpenGL implementation dependent
 */
#define VL_MAX_GENERIC_VERTEX_ATTRIB 8


/**
 * Maximum timer index that can be passed to vl::Time::start(int index), vl::Time::stop(int index) etc.
 */
#define VL_MAX_TIMERS 16


/**
 * Enable String copy-on-write mode.
 *
 * - 1 = vl::String copy on write feature enabled
 * - 0 = vl::String copy on write feature disabled
 */
#define VL_STRING_COPY_ON_WRITE 1


/**
 * Default byte alignment for the vl::Buffer class.
 */
#define VL_DEFAULT_BUFFER_BYTE_ALIGNMENT 16


// -------------------- Do Not Touch The Following Section --------------------

///////////////////////////////////////////////////

#ifndef NDEBUG
  #define VL_DEBUG_SET_OBJECT_NAME() this->mObjectName = className();
#else
  #define VL_DEBUG_SET_OBJECT_NAME()
#endif

///////////////////////////////////////////////////

// Pipeline precision settings
#if VL_PIPELINE_PRECISION == 2
  namespace vl { /** Defined as \p 'typedef \p double \p real' */ typedef double real; }
  //! Defined as \p glLoadMatrixd, used internally.
  #define VL_glLoadMatrix glLoadMatrixd
  //! Defined as \p glMultMatrixd, used internally.
  #define VL_glMultMatrix glMultMatrixd 
#else
  namespace vl { /** Defined as \p 'typedef \p float \p real' */ typedef float real; }
  namespace vl { typedef float real; }
  //! Defined as \p glLoadMatrixf, used internally.
  #define VL_glLoadMatrix glLoadMatrixf
  //! Defined as \p glMultMatrixf, used internally.
  #define VL_glMultMatrix glMultMatrixf
#endif

///////////////////////////////////////////////////

// Visual Studio special settings
#ifdef _MSC_VER
  #pragma warning( once : 4996 ) // function or variable may be unsafe
  #pragma warning( once : 4800 ) // forcing value to bool (performance warning)
  #pragma warning( once : 4127 ) // conditional expression is constant
  #pragma warning( once : 4100 ) // unreferenced formal parameter
  #pragma warning( disable : 4251 ) // non-dll type exposed by a dll type
#endif

///////////////////////////////////////////////////

// VLCORE_EXPORT macro
#if defined(VL_PLATFORM_WINDOWS) && !defined(VL_STATIC_LINKING)
  #ifdef VLCore_EXPORTS
    #define VLCORE_EXPORT __declspec(dllexport)
  #else
    #define VLCORE_EXPORT __declspec(dllimport)
  #endif
#else
  #define VLCORE_EXPORT
#endif

#endif // VISUALIZATION_LIBRARY_CONFIG_INCLUDE_ONCE
