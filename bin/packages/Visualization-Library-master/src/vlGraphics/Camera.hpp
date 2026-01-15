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

#ifndef Camera_INCLUDE_ONCE
#define Camera_INCLUDE_ONCE

#include <vlCore/checks.hpp>
#include <vlCore/math_utils.hpp>
#include <vlCore/Vector4.hpp>
#include <vlGraphics/Framebuffer.hpp>
#include <vlGraphics/Viewport.hpp>
#include <vlGraphics/Frustum.hpp>
#include <vlCore/Ray.hpp>

namespace vl
{
  class AABB;
  //-----------------------------------------------------------------------------
  // Camera
  //-----------------------------------------------------------------------------
  /** Represents a virtual camera defining, among other things, the point of view from which scenes can be rendered. */
  class VLGRAPHICS_EXPORT Camera: public Object
  {
    VL_INSTRUMENT_CLASS(vl::Camera, Object)

  public:
    /** Constructs a perspective projecting camera with FOV = 60.0, Near Plane = 0.05, Far Plane = 10000.0 */
    Camera();

    /** The near and far clipping planes are adjusted to fit the provided \p scene_bounding_sphere.
    Optimizing the near and far clipping planes results in an optimized usage of the z-buffer with 
    the consequence of minimizing possible z-fighting artifacts, thus enhancing the rendering quality.
    \note Optimizing the near and far clipping planes might slightly impact the rendering performances 
    if the scene contains a huge number of objects.
    \note At the moment the near and far clipping planes optimization is available only when using a 
    projection matrix set up by setProjectionPerspective() or setProjectionOrtho*(). */
    void computeNearFarOptimizedProjMatrix(const Sphere& scene_bounding_sphere);

    /** Computes the Camera's frustum planes in world space.
    If nearFarClippingPlanesOptimized() == true the near and far culling planes distances 
    are respectively set to nearPlane() and farPlane(). */
    void computeFrustumPlanes();

    /** Loads the GL_MODELVIEW matrix with the Camera's view matrix multiplied by the specified model matrix. */
    void applyModelViewMatrix(const mat4& model_matrix) const;

    /** Loads the GL_MODELVIEW matrix with the Camera's view matrix. */
    void applyViewMatrix() const;

    /** Loads the GL_PROJECTION matrix with the Camera's projection matrix. */
    void applyProjMatrix() const;

    /** Returns the aspect ratio computed as viewport()->width()/viewport()->height(). 
    If viewport() == NULL the function returns 0. */
    real aspectRatio() const 
    {
      if (viewport())
        return (real)viewport()->width()/viewport()->height();
      else
        return 0;
    }

    /** The field of view of the camera. 
    \note This setting will not take effect until setProjectionPerspective() is called. */
    void setFOV(real fov) { mFOV = fov; }
    
    /** The field of view of the camera. */
    real fov() const { return mFOV; }

    /** The near clipping plane.
    \note This setting will not take effect until setProjectionPerspective() or setProjectionOrtho() is called. */
    void setNearPlane(real nearplane) { mNearPlane = nearplane; }

    /** The near clipping plane. */
    real nearPlane() const { return mNearPlane; }

    /** The far clipping plane. 
    \note This setting will not take effect until setProjectionPerspective() or setProjectionOrtho() is called. */
    void setFarPlane(real farplane) { mFarPlane = farplane; }

    /** The far clipping plane. */
    real farPlane() const { return mFarPlane; }

    /** 'left' parameter as passed to the last setProjectionFrustum() or setProjectionOrtho*() */
    real left() const { return mLeft; }
    void setLeft(real v) { mLeft = v; }

    /** 'right' parameter as passed to the last setProjectionFrustum() or setProjectionOrtho*() */
    real right() const { return mRight; }
    void setRight(real v) { mRight = v; }

    /** 'bottom' parameter as passed to the last setProjectionFrustum() or setProjectionOrtho*() */
    real bottom() const { return mBottom; }
    void setBottom(real v) { mBottom = v; }

    /** 'top' parameter as passed to the last setProjectionFrustum() or setProjectionOrtho*() */
    real top() const { return mTop; }
    void setTop(real v) { mTop = v; }

    /** The view frustum of the camera used to perform frustum culling. */
    void setFrustum(const Frustum& frustum) { mFrustum = frustum; }

    /** The view frustum of the camera used to perform frustum culling. */
    const Frustum& frustum() const { return mFrustum; }

    /** The view frustum of the camera used to perform frustum culling. */
    Frustum& frustum() { return mFrustum; }

    /** The viewport bound to a camera. */
    void setViewport(Viewport* viewport) { mViewport = viewport; }

    /** The viewport bound to a camera. */
    Viewport* viewport() { return mViewport.get(); }

    /** The viewport bound to a camera. */
    const Viewport* viewport() const { return mViewport.get(); }

    /** Bind the camera to a Transform. */
    void bindTransform(Transform* transform) { mBoundTransform = transform; }

    /** Returns the Transform bound to a camera. */
    const Transform* boundTransform() const { return mBoundTransform.get(); }

    /** Returns the Transform bound to a camera. */
    Transform* boundTransform() { return mBoundTransform.get(); }

    /** Sets the Camera's view matrix (inverse of the modeling matrix). The modelingMatrix() is also set as the inverse of the viewMatrix().
        @remarks The modelingMatrix() bring points from camera space to world space, where the viewMatrix() brings points from world space to camera space. */
    void setViewMatrix(const mat4& mat) { mViewMatrix = mat; mViewMatrix.getInverse(mModelingMatrix); }

    /** Returns the Camera's view matrix (inverse of the modeling matrix). This is what you would pass to OpenGL with "glMatrixMode(GL_MODELVIEW); glLoadMatrix(camera.viewMatrix().ptr());"
        @remarks The modelingMatrix() bring points from camera space to world space, where the viewMatrix() brings points from world space to camera space. */
    const mat4& viewMatrix() const { return mViewMatrix; }

    /** Sets the Camera's modelingMatrix() (inverse of the view matrix). The view matrix is also set as the inverse of the modelingMatrix().
        @remarks The modelingMatrix() bring points from camera space to world space, where the viewMatrix() brings points from world space to camera space. */
    void setModelingMatrix(const mat4& mat) { mModelingMatrix = mat; mModelingMatrix.getInverse(mViewMatrix); }

    /** Returns the Camera's modelingMatrix() (inverse of the view matrix).
        @remarks The modelingMatrix() bring points from camera space to world space, where the viewMatrix() brings points from world space to camera space. */
    const mat4& modelingMatrix() const { return mModelingMatrix; }

    /** The Camera's projection matrix. */
    void setProjectionMatrix(const mat4& mat, EProjectionMatrixType proj_type) { mProjectionMatrix = mat; mProjectionType = proj_type; }

    /** The Camera's projection matrix. */
    const mat4& projectionMatrix() const { return mProjectionMatrix; }

    /** The Camera's projection matrix type. */
    EProjectionMatrixType projectionMatrixType() const { return mProjectionType; }

    /** Builds a perspective projection matrix for the Camera based on the Camera's and Viewport's settings. 
    See also http://www.opengl.org/sdk/docs/man/xhtml/gluPerspective.xml for more information. */
    void setProjectionPerspective();

    /** Builds a perspective projection matrix for the Camera based on the Camera's and Viewport's settings. 
    See also http://www.opengl.org/sdk/docs/man/xhtml/gluPerspective.xml for more information. */
    void setProjectionPerspective(real fov, real near, real far);

    /** Produces a perspective projection matrix. 
    The <left, bottom, zfar> and <right, top, znear> parameters specify the points on the near clipping plane that are mapped to the lower-left and upper-right corners of the viewport, 
    respectively, assuming that the eye is located at (0,0,0). The zfar parameter specifies the location of the far clipping plane. Both znear and zfar must be positive.

    This function is more general that setProjectionPerspective() as it's capable of generating off-axis projections, while setProjectionPerspective() only produces 
    symmetrical (on-axis) projections. Since setProjectionFrustum() is more general than setProjectionPerspective(), you can use it in cases where setProjectionPerspective() 
    can't be used. Some examples include shadows projection, tiled renderings, and stereo views.
    \sa
    - http://www.opengl.org/sdk/docs/man/xhtml/glFrustum.xml 
    - http://www.opengl.org/resources/faq/technical/transformations.htm */
    void setProjectionFrustum(real left, real right, real bottom, real top, real znear, real zfar);

    /** Builds an orthographic projection matrix for the Camera based on the Camera's near/far planes and its Viewport's settings. 
    - See also http://www.opengl.org/sdk/docs/man/xhtml/glOrtho.xml and http://www.opengl.org/sdk/docs/man/xhtml/glOrtho2D.xml for more information.
    Equivalent to:
    \code
    setProjectionMatrix( mat4::getOrtho(left, right, bottom, top, znear, zfar), PMT_OrthographicProjection );
    \endcode */
    void setProjectionOrtho(real left, real right, real bottom, real top, real znear, real zfar);

    /** Builds an orthographic projection matrix for the Camera based on the Camera's near/far planes and its Viewport's settings. 
    - See also http://www.opengl.org/sdk/docs/man/xhtml/glOrtho.xml and http://www.opengl.org/sdk/docs/man/xhtml/glOrtho2D.xml for more information.
    Equivalent to:
    \code
    setProjectionMatrix( mat4::getOrtho(0, viewport()->width(), 0, viewport()->height(), nearPlane(), farPlane()), PMT_OrthographicProjection );
    \endcode */
    void setProjectionOrtho();

    /** Builds an orthographic projection matrix for the Camera based on the Camera's near/far planes and its Viewport's settings. 
    This function is similar to glOrtho2D() with the difference that it can optionally add a translation offset on both x and y. For example with
    an offset of -0.5 the center of the pixels will be exactly at integer coordinates 0, 1, 2, etc.
    - See also http://www.opengl.org/sdk/docs/man/xhtml/glOrtho.xml and http://www.opengl.org/sdk/docs/man/xhtml/glOrtho2D.xml for more information.
    Equivalent to:
    \code
    setProjectionMatrix( mat4::getOrtho(offset, viewport()->width() + offset, offset, viewport()->height() + offset, -1.0, +1.0), PMT_OrthographicProjection );
    \endcode */
    void setProjectionOrtho(real offset);

    /** Setup the modelview transform of the camera based on look-at parameters. 
    \param eye The position of the camera.
    \param center The point the camera is looking at.
    \param up The vector defining the up direction. */
    void setViewMatrixLookAt(const vec3& eye, const vec3& at, const vec3& up);

    /** Returns the look-at parameters of the modelview transform.
    \param eye The position of the camera.
    \param look The direction the camera is looking at.
    \param up The vector defining the Y positive direction of the camera.
    \param right The vector defining the X positive direction of the camera. */
    void getViewMatrixAsLookAt(vec3& eye, vec3& at, vec3& up, vec3& right) const;

    /** Projects a vector from world coordinates to viewport coordinates. */
    bool project(const vec4& in_world, vec4& out_viewp) const;

    /** Unprojects a vector from viewport coordinates to world coordinates.
    \param in_viewp The viewport coordinates point to be projected. Note that, in accordance to OpenGL conventions, 
    the viewport  coordinates have the Y axis pointing upwards and origin at the bottom left corner of the viewport.
    \param out_world The world coordinates of the projected point.
    \note The \p z coordinate of \p in_viewp determines the position of the projected point along the ray passing through \p in_viewp. 
    If \p z equals 0 then \p out_world will lay on the near clipping plane, if \p z equals 1 then \p out_world will lay on the far clipping plane. */
    bool unproject(const vec3& in_viewp, vec4& out_world) const;

    /** Unprojects a set of points. See unproject(const vec3& in_viewp, vec4& out_world) for more information. */
    bool unproject(std::vector<vec3>& points) const;

    /** Computes the ray passing through the point <viewp_x,viewp_y>.
    \param viewp_x The x position of the point defining the ray
    \param viewp_y The y position of the point defining the ray
    \note <viewp_x,viewp_y> is considered to be a point in viewport coordinate. In accordance to the OpenGL conventions the viewport coordinates
    have origin on the bottom left corner of the viewport, with the Y axis pointing upwards. Most GUI libraries define the 
    the orgin at the top left corner of the window and Y axis pointing downwards. You can convert such coordinates to OpenGL ones
    using the folloviewp_g simple formula: \p "opengl_y = window_height - coord_y". */
    Ray computeRay(int viewp_x, int viewp_y);

    /** Computes a 1 pixel wide frustum suitable to cull objects during ray intersection detection. */
    Frustum computeRayFrustum(int viewp_x, int viewp_y);

    /** Adjusts the camera position so that the given aabb can be properly viewed.
    \param aabb The AABB (in world coords) that should be visible from the newly computed camera position.
    \param dir The direction (in world coords) along which the camera should be displaced to view the given AABB.
    \param up The vector that defines the \p up direction (in world coords). Used to properly compute the new camera matrix.
    \param bias A bias factor used to adjust the computed camera distance from the given AABB. Values between 0 and 1 make the camera closer to the AABB center, values greater than 1 position the camera further away. */
    void adjustView(const AABB& aabb, const vec3& dir, const vec3& up, real bias=1.0f);

  protected:
    mat4 mViewMatrix;
    mat4 mModelingMatrix;
    mat4 mProjectionMatrix;
    ref<Viewport> mViewport;
    Frustum mFrustum;
    ref<Transform> mBoundTransform;
    real mFOV;
    real mLeft, mRight, mBottom, mTop;
    real mNearPlane;
    real mFarPlane;
    EProjectionMatrixType mProjectionType;
  };
}

#endif
