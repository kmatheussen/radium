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

#include "BaseDemo.hpp"
#include <vlGraphics/Text.hpp>
#include <vlGraphics/FontManager.hpp>
#include <vlGraphics/GeometryPrimitives.hpp>

class App_Deformer: public BaseDemo
{
public:
  virtual void updateScene() 
  {
    if (mMode == AnimateMode)
    {
      float t = float( (cos(vl::dPi + 0.25*(vl::Time::currentTime() - mStartTime)*vl::dPi) + 1.0) * 0.5 );
      for(size_t i=0; i<mPoints->size(); ++i)
      {
        mPointsAnim->at(i) = (1.0f-t) * mPoints->at(i) + t * mPointsRest->at(i);
      }
    }
  }

  void initEvent()
  {
    vl::Log::notify(appletInfo());

    mMode = NoMode;

    openglContext()->setContinuousUpdate(false);

    // camera setup
    rendering()->as<vl::Rendering>()->camera()->setProjectionOrtho(-0.5f);
    rendering()->as<vl::Rendering>()->camera()->setViewMatrix( vl::mat4::getIdentity() );

    // disable trackball and ghost camera manipulator
    trackball()->setEnabled(false);
    ghostCameraManipulator()->setEnabled(false);

    // texture setup
    mTexture = new vl::Texture;
    mTexture->getTexParameter()->setWrapS(vl::TPW_CLAMP);
    mTexture->getTexParameter()->setWrapT(vl::TPW_CLAMP);
    mTexture->getTexParameter()->setMinFilter(vl::TPF_LINEAR);
    mTexture->getTexParameter()->setMagFilter(vl::TPF_LINEAR);
    mTextureMatrix = new vl::TextureMatrix;

    vl::ref<vl::Effect> image_fx = new vl::Effect;
    image_fx->shader()->gocTextureSampler(0)->setTexture(mTexture.get());
    image_fx->shader()->setRenderState( mTextureMatrix.get(), 0 );
    image_fx->shader()->enable(vl::EN_BLEND);
    image_fx->shader()->gocColor()->setValue(vl::white);

    mGrid = vl::makeGrid( vl::vec3(0,0,0), 1.0f, 1.0f, mSlices, mSlices, true, vl::fvec2(0,0), vl::fvec2(1,1) );
    mGrid->setBufferObjectEnabled(false);
    mGrid->transform(vl::mat4::getRotation(-90,1,0,0));
    mPoints = vl::cast<vl::ArrayFloat3>(mGrid->vertexArray());

    // save point coordinates for the animation keyframe
    mPointsRest = new vl::ArrayFloat3;
    mPointsRest->resize( mPoints->size() );
    memcpy(mPointsRest->ptr(), mPoints->ptr(), mPoints->bytesUsed());
    // animate points buffer
    mPointsAnim = new vl::ArrayFloat3;
    mPointsAnim->resize( mPoints->size() );

    mTransform = new vl::Transform;
    rendering()->as<vl::Rendering>()->transform()->addChild(mTransform.get());
    sceneManager()->tree()->addActor(mGrid.get(), image_fx.get(), mTransform.get());

    mCursorTransform = new vl::Transform;
    rendering()->as<vl::Rendering>()->transform()->addChild(mCursorTransform.get());
    mBrushSize = 100;
    vl::ref<vl::Effect> cursor_fx = new vl::Effect;
    cursor_fx->shader()->gocLogicOp()->set(vl::LO_INVERT);
    cursor_fx->shader()->enable(vl::EN_COLOR_LOGIC_OP);
    vl::ref<vl::Geometry> cursor = vl::makeCircle(vl::vec3(0,0,0), 1.0f);
    
    cursor->transform(vl::mat4::getRotation(-90,1,0,0));
    mCursorActor = sceneManager()->tree()->addActor(cursor.get(), cursor_fx.get(), mCursorTransform.get());
    // ensure the cursor is rendered over the image (whose render rank is 0 by default)
    mCursorActor->setRenderRank(1);

    mText = new vl::Text;
    mText->setFont( vl::defFontManager()->acquireFont("/font/bitstream-vera/Vera.ttf", 10) );
    mText->translate(0,-5,0);
    mText->setColor(vl::white);
    mText->setBackgroundColor(vl::fvec4(0,0,0,.75f));
    mText->setBackgroundEnabled(true);
    mHelpOn = true;
    updateText();

    vl::ref<vl::Effect> txt_fx = new vl::Effect;
    txt_fx->shader()->enable(vl::EN_BLEND);
    vl::Actor* txt_act = sceneManager()->tree()->addActor(mText.get(), txt_fx.get());
    // draw the text for last
    txt_act->setRenderRank(2);

    loadFile("/images/toy.jpg");
  }

  void reset()
  {
    memcpy(mPoints->ptr(), mPointsRest->ptr(), mPoints->bytesUsed());
    mPointsUndo.clear();
    openglContext()->update();
  }

  void updateText()
  {
    if (mHelpOn)
    {
      mText->setAlignment(vl::AlignHCenter | vl::AlignVCenter);
      mText->setViewportAlignment(vl::AlignHCenter | vl::AlignVCenter);
      mText->setText(
        "HELP:\n"
        "\n"
        "Drop an image in the window to start!\n"
        "\n"
        "<H> toggles this help\n"
        "<Esc> exits the program\n"
        "<F5> takes screenshot\n"
        "<Space> resets the picture\n"
        "<Ctrl+Z> undo\n"
        "<Enter> toggles animation\n"
        "<Left Button> drags the image\n"
        "<Right Button> scales the image\n"
        "<Mouse Wheel> sets the brush size\n"
        );
    }
    else
    {
      mText->setText("");
    }
  }

  void keyPressEvent(unsigned short ch, vl::EKey key)
  {
    // ignore left/right ctrl key events, we use Key_Ctrl only
    if (key == vl::Key_LeftCtrl || key == vl::Key_RightCtrl )
      return;

    if (key == vl::Key_H)
    {
      mHelpOn = !mHelpOn;
      updateText();
    }
    else
    // resets the grid
    if (key == vl::Key_Return)
    {
      if (mMode == NoMode)
      {
        mMode = AnimateMode;
        mStartTime = vl::Time::currentTime();
        mGrid->setVertexArray(mPointsAnim.get());
        openglContext()->setContinuousUpdate(true);
        mCursorActor->setEnableMask(0);
      }
      else
      if (mMode == AnimateMode)
      {
        mMode = NoMode;
        mGrid->setVertexArray(mPoints.get());
        openglContext()->setContinuousUpdate(false);
        openglContext()->update();
      }
    }

    if (mMode == AnimateMode)
      return;

    if (key == vl::Key_Space)
    {
      reset();
    }
    else
    if (openglContext()->isKeyPressed(vl::Key_Ctrl) && openglContext()->isKeyPressed(vl::Key_Z))
    {
      if (!mPointsUndo.empty())
      {
        memcpy(mPoints->ptr(), mPointsUndo.back()->ptr(), mPoints->bytesUsed());
        mPointsUndo.pop_back();
        openglContext()->update();
      }
    }
    else
    {
      if (key == vl::Key_F5)
        // disable the mouse cursor while taking the screenshot
        mCursorActor->setEnableMask(0);
      BaseDemo::keyPressEvent(ch,key);
      openglContext()->update();
    }
  }

  void mouseUpEvent(vl::EMouseButton btn, int, int)
  {
    if (mMode == TranslateMode && btn == vl::LeftButton)
      mMode = NoMode;
    else
    if (mMode == ScaleMode && btn == vl::RightButton)
      mMode = NoMode;
  }

  void mouseDownEvent(vl::EMouseButton btn, int x, int y)
  {
    if (mMode == AnimateMode)
      return;
    if (mMode != NoMode)
      return;
    else
    if(btn == vl::LeftButton)
      mMode = TranslateMode;
    else
    if (btn == vl::RightButton)
      mMode = ScaleMode;

    mMouseStart = vl::ivec2(x,y);
    updateSelection();

    // make a backup of the points for our undo
    vl::ref<vl::ArrayFloat3> undo_stage = new vl::ArrayFloat3;
    undo_stage->resize(mSlices*mSlices);
    memcpy(undo_stage->ptr(), mPoints->ptr(), mPoints->bytesUsed());
    mPointsUndo.push_back(undo_stage);
  }

  void updateSelection()
  {
    mSelection.clear();
    // select points
    vl::vec4 c = vl::vec4(mCursorTransform->worldMatrix().getT(),1);
    rendering()->as<vl::Rendering>()->camera()->project(c, c);
    for(int x=0; x<mSlices; ++x)
    for(int y=0; y<mSlices; ++y)
    {
      int i = x + mSlices*y;
      if ( x == 0 || y == 0 || x == mSlices-1 || y == mSlices-1 )
        continue;

      vl::vec4 p = vl::vec4((vl::vec3)mPoints->at(i),1);
      p = mTransform->worldMatrix() * p;
      rendering()->as<vl::Rendering>()->camera()->project(p, p);
      float distance = (float)( (p.xy()-c.xy()).length() );
      if (distance < mBrushSize)
      {
        Point point;
        point.mIndex     = i;
        point.mWeight    = 1.0f - distance / mBrushSize;
        point.mStartPos  = (vl::vec3)mPoints->at(i);
        point.mDirection = (p.xy()-c.xy()).normalize();
        mSelection.push_back(point);
      }
    }
  }

  void mouseWheelEvent(int w)
  {
    openglContext()->update();

    mBrushSize += 4*w;
    mBrushSize = vl::clamp(mBrushSize, 10, 1000);

    vl::mat4 m;
    m.scale((vl::real)mBrushSize/2.0f, (vl::real)mBrushSize/2.0f, (vl::real)mBrushSize/2.0f);
    m.translate(mCursorTransform->localMatrix().getT());
    mCursorTransform->setLocalMatrix(m);
  }

  void mouseMoveEvent(int x, int y)
  {
    if (mMode == AnimateMode)
      return;

    openglContext()->update();

    mCursorActor->setEnableMask(0xFFFFFFFF);

    vl::mat4 m;
    m.scale((vl::real)mBrushSize/2.0f, (vl::real)mBrushSize/2.0f, (vl::real)mBrushSize/2.0f);
    if (mMode == ScaleMode)
      m.translate(mCursorTransform->localMatrix().getT());
    else
      m.translate((vl::real)x, (vl::real)rendering()->as<vl::Rendering>()->camera()->viewport()->height()-y, 0);

    mCursorTransform->setLocalMatrix(m);

    if (mMode == TranslateMode)
    {
      float tx = +(float)(x-mMouseStart.x())/rendering()->as<vl::Rendering>()->camera()->viewport()->width();
      float ty = -(float)(y-mMouseStart.y())/rendering()->as<vl::Rendering>()->camera()->viewport()->height();
      for(unsigned i=0; i<mSelection.size(); ++i)
      {
        int ipt = mSelection[i].mIndex;
        float w = mSelection[i].mWeight;
        w = w*w;
        vl::vec3 new_pos = mSelection[i].mStartPos + vl::vec3(tx,ty,0)*w;
        new_pos = vl::clamp(new_pos,vl::vec3(-0.5,-0.5,-0.5),vl::vec3(0.5,0.5,0.5));
        mPoints->at(ipt) = (vl::fvec3)new_pos;
      }
    }
    else
    if (mMode == ScaleMode)
    {
      float scaling = 0.1f * (float)(y-mMouseStart.y())/rendering()->as<vl::Rendering>()->camera()->viewport()->height();
      for(unsigned i=0; i<mSelection.size(); ++i)
      {
        int ipt = mSelection[i].mIndex;
        float w = mSelection[i].mWeight;
        vl::vec3 new_pos = mSelection[i].mStartPos + vl::vec3(mSelection[i].mDirection*w*scaling,0);
        new_pos = vl::clamp(new_pos,vl::vec3(-0.5,-0.5,-0.5),vl::vec3(0.5,0.5,0.5));
        mPoints->at(ipt) = (vl::fvec3)new_pos;
      }
    }
  }

  void fileDroppedEvent(const std::vector<vl::String>& files)
  {
    loadFile(files[0]);
  }

  void loadFile(const vl::String& file)
  {
    if (mMode == AnimateMode)
      return;

    mImage = vl::loadImage(file);
    if (!mImage)
      return;
    if (file.endsWith(".dcm"))
      mImage->contrastHounsfieldAuto();

    mTexture->destroyTexture();
    mTexture->prepareTexture2D(mImage.get(), vl::TF_UNKNOWN);

    // perfectly center the texture texels (see GL_CLAMP documentation)
    vl::fmat4 m;
    float x_texel = 1.0f/mImage->width();
    float y_texel = 1.0f/mImage->height();
    float x_scale = 1.0f - x_texel;
    float y_scale = 1.0f - y_texel;
    m.scale(x_scale, y_scale, 1.0f);
    m.translate(x_texel/2.0f, y_texel/2.0f, 0.0f);
    mTextureMatrix->setMatrix(m);

    resizeEvent( rendering()->as<vl::Rendering>()->camera()->viewport()->width(), rendering()->as<vl::Rendering>()->camera()->viewport()->height() );
    reset();
  }

  void resizeEvent(int w, int h)
  {
    rendering()->as<vl::Rendering>()->camera()->viewport()->setWidth(w);
    rendering()->as<vl::Rendering>()->camera()->viewport()->setHeight(h);
    rendering()->as<vl::Rendering>()->camera()->setProjectionOrtho(-0.5f);

    if (mImage)
    {
      vl::mat4 m;
      m.translate(w/2.0f, h/2.0f, 0.0f);

      float x_scaling = (float)w / mImage->width();
      float y_scaling = (float)h / mImage->height();
      float scaling   = x_scaling < y_scaling ? x_scaling : y_scaling;

      m = m * vl::mat4::getScaling(scaling*mImage->width(), scaling*mImage->height(), scaling);
      mTransform->setLocalMatrix(m);
    }
  }

protected:
  class Point
  {
  public:
    int   mIndex;
    float mWeight;
    vl::vec3 mStartPos;
    vl::vec2 mDirection;
  };
  std::vector<Point> mSelection;
  vl::ref<vl::Actor> mCursorActor;
  vl::ref<vl::Image> mImage;
  vl::ref<vl::Texture> mTexture;
  vl::ref<vl::TextureMatrix> mTextureMatrix;
  vl::ref<vl::Transform> mTransform;
  vl::ref<vl::Transform> mCursorTransform;
  vl::ref<vl::ArrayFloat3> mPoints;
  vl::ref<vl::ArrayFloat3> mPointsRest;
  vl::ref<vl::ArrayFloat3> mPointsAnim;
  std::vector< vl::ref<vl::ArrayFloat3> > mPointsUndo;
  vl::ref<vl::Geometry> mGrid;
  vl::ref<vl::Text> mText;
  vl::ivec2 mMouseStart;
  vl::real mStartTime;
  int mBrushSize;
  enum { NoMode, TranslateMode, ScaleMode, AnimateMode } mMode;
  bool mHelpOn;

  static const int mSlices = 64;
};

// Have fun!

BaseDemo* Create_App_Deformer() { return new App_Deformer; }
