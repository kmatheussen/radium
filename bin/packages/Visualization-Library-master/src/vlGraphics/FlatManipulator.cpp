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

#include <vlGraphics/FlatManipulator.hpp>
#include <vlGraphics/OpenGLContext.hpp>

using namespace vl;

FlatManipulator::FlatManipulator(): mMode(NoMode),
      mTranslationButton(MiddleButton), mZoomButton(RightButton),
      mZoomSpeed(1.0f)
    {
      VL_DEBUG_SET_OBJECT_NAME()
    }

//-----------------------------------------------------------------------------
void FlatManipulator::setCamera(Camera* camera)
{
    mCamera = camera;
}
//-----------------------------------------------------------------------------
void FlatManipulator::mouseDownEvent(EMouseButton btn, int x, int y)
{
    if ( camera() == NULL )
        return;

    // if already busy ignore the event, wadda heck!
    if (mode() != NoMode)
        return;

    //if the rodent is out of cage, kisses and bye-bye
    int vx, vy;
    if(mouseInViewport(x, y, vx, vy) == false)
        return;
    //store the rodent's position in the cage
    mMouseStart = vec2(x,y);

    // enter new mode
    if (btn == translationButton())
    {//we need world pixel size only in translation mode
        mMode = TranslationMode;
        //now let's find the pixel size, assuming an orthographic projection
        mPixelSize.x() = 2.0/camera()->projectionMatrix().e(0,0)/
                            camera()->viewport()->width();
        mPixelSize.y() =  2.0/camera()->projectionMatrix().e(1,1)/
                            camera()->viewport()->height();
    }
    else
    if (btn == zoomButton())
    {
        mMode = ZoomMode;
    }

    VL_CHECK(openglContext()->framebuffer())

}
//-----------------------------------------------------------------------------
void FlatManipulator::mouseMoveEvent(int x, int y)
{
    if ( camera() == NULL )
        return;

    // ignore the event if the manipulator is not in any mode
    if (mode() == NoMode)
        return;

    VL_CHECK(openglContext()->framebuffer())

    if (mode() == ZoomMode)
    {
        float scale = 1.0-(y - mMouseStart.y())*zoomSpeed()/100.0;
        camera()->setViewMatrix(
                        mat4::getScaling(scale, scale,1.0)*
                        camera()->viewMatrix());
        mMouseStart = vec2(x,y);
    }
    else
    if (mode() == TranslationMode)
    {
        vec2 shift = (vec2(x,y) - mMouseStart)*mPixelSize;
        camera()->setViewMatrix(
                    mat4::getTranslation(shift.x(), -shift.y(),0)*
                    camera()->viewMatrix());
        mMouseStart = vec2(x,y);
    }

    // update the view
    openglContext()->update();
}
//-----------------------------------------------------------------------------
void FlatManipulator::mouseUpEvent(EMouseButton btn, int /*x*/, int /*y*/)
{
  if ( camera() == NULL )
    return;

  // if the manipulator is not doing anything ignore the event
  if (mode() == NoMode)
    return;

  // leave the mode
  if (btn == translationButton() && mMode == TranslationMode)
    mMode = NoMode;
  else
  if (btn == zoomButton() && mMode == ZoomMode)
    mMode = NoMode;
}
//-----------------------------------------------------------------------------
void FlatManipulator::enableEvent(bool enabled)
{
  if (enabled)
  {
    mMode = NoMode;
    if ( openglContext() )
    {
      openglContext()->setMouseVisible(true);
      openglContext()->setContinuousUpdate(false);
    }
  }
}
//-----------------------------------------------------------------------------
bool FlatManipulator::mouseInViewport(int mx, int my, int& vx, int& vy)
{
    vx = mx - camera()->viewport()->x();
    vy = openglContext()->height() - my - camera()->viewport()->y();

    //if outside camera's viewport, return false
    if (vx<0 || vy<0 ||
      vx >= camera()->viewport()->width() ||
      vy >= camera()->viewport()->height())
    {
        return false;
    }
    return true;
}
//-----------------------------------------------------------------------------
vl::ref<vl::Geometry> makeScales(bool X, bool Y, bool Z, int numArmTicks, float mmStep, float mmTickSize)
{
    vl::ref<vl::Geometry> scales = new vl::Geometry();
    int numRulers = (X?1:0)+(Y?1:0)+(Z?1:0);
    if(numRulers == 0) return scales;

    vl::ref<vl::ArrayFloat3>    points(new vl::ArrayFloat3());
    vl::ref<vl::ArrayFloat3>    scalesPoints(new vl::ArrayFloat3());

    //the length of one arm of scales
    const float mmArmLength = numArmTicks*mmStep;

    //calculate the number of points, having marks at every 10 mm
    int     numAxisPoints   = ( 2*numArmTicks + 1)*2;

    //resize the array to accomodate a ruler
    points->resize(numAxisPoints);
    //room for up to three orthogonal rulers
    scalesPoints->resize(numRulers*points->size());

    float xpos = mmStep;
    int t;
    //generate the X ruler
    for(t = 0; t < numArmTicks; t++)
    {
        float height = mmTickSize;

        if((t+1)%10 == 0)//takes priority over '5'
            height *= 2;
        else
        if((t+1)%5 == 0)
            height *= 1.5;

        points->at(4*t)     = vl::fvec3(-xpos, -height,0);
        points->at(4*t+1)   = vl::fvec3(-xpos, +height,0);
        points->at(4*t+2)   = vl::fvec3(xpos, -height,0);
        points->at(4*t+3)   = vl::fvec3(xpos, +height,0);

        xpos += mmStep;
    }
    //now add the axis line
    points->at(4*t)     = vl::fvec3(-mmArmLength, 0.0,0);
    points->at(4*t+1)   = vl::fvec3(mmArmLength, 0.0,0);

    int i=0;

    if(X)//transfer the X ruler into the main array
    {
        for(size_t p=0; p < points->size(); i++, p++)
        {
            scalesPoints->at(i) = points->at(p);
        }
    }
    if(Y)//transfer the Y ruler into the main array
    {
        //rotate the generic ruler around Z axis to create the Y ruler
        points->transform(vl::fmat4::getRotation(90, 0, 0, 1));

        //copy the Y ruler into the scales array
        for(size_t p=0; p < points->size(); i++, p++)
        {
            scalesPoints->at(i) = points->at(p);
        }
    }
    if(Z)//transfer the Z ruler into the main array
    {
        //rotate the ruler based on the last rotation
        vl::fvec3 rotaxis = Y ? vl::fvec3(1,0,0) : vl::fvec3(0,1,0);
        points->transform(vl::fmat4::getRotation(90,rotaxis));

        //copy the Y ruler into the scales array
        for(size_t p=0; p < points->size(); i++, p++)
        {
            scalesPoints->at(i) = points->at(p);
        }
    }
    scales->setVertexArray(scalesPoints.get());
    //every consecutive pair of points make a line
    scales->drawCalls()->push_back( new vl::DrawArrays( vl::PT_LINES, 0, numRulers*points->size() ) );
    return scales;
}
