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

#ifndef ProjViewTransfCallback_INCLUDE_ONCE
#define ProjViewTransfCallback_INCLUDE_ONCE

#include <vlCore/Object.hpp>
#include <vlGraphics/link_config.hpp>

namespace vl
{
  class Renderer;
  class GLSLProgram;
  class Transform;
  class Camera;

  //-----------------------------------------------------------------------------
  // ProjViewTransfCallback
  //-----------------------------------------------------------------------------
  /** Callback class to update the state of the \p projection, \p view, \p transform and \p normal matrices of a GLSLProgram or fixed function pipeline. 
  * By default it updates the GL_PROJECTION and GL_MODELVIEW matrices. If GLSL is used and any of \p vl_ModelViewMatrix, \p vl_ProjectionMatrix, 
  * \p vl_ModelViewProjectionMatrix or \p vl_NormalMatrix is used, then no legacy uniform matrix is updated (GL_MODELVIEW, gl_ModelViewMatrix etc.)
  * but only the vl_* ones.
  * Reimplement the updateMatrices() method to update any other camera/transform matrix you might need such as the ones defined in
  * http://www.opengl.org/registry/doc/GLSLangSpec.Full.1.10.59.pdf pag 45.
  */
  class VLGRAPHICS_EXPORT ProjViewTransfCallback: public Object
  {
    VL_INSTRUMENT_CLASS(vl::ProjViewTransfCallback, Object)

  public:
    ProjViewTransfCallback()
    {
      VL_DEBUG_SET_OBJECT_NAME()
    }

    //! Update matrices of the current GLSLProgram, if glsl_program == NULL then fixed function pipeline is active.
    virtual void updateMatrices(bool cam_changed, bool transf_changed, const GLSLProgram* glsl_program, const Camera* camera, const Transform* transform);
  };
}

#endif
