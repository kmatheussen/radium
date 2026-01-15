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

#include <vlVolume/VolumePlot.hpp>
#include <vlGraphics/Light.hpp>
#include <vlGraphics/SceneManager.hpp>
#include <vlGraphics/FontManager.hpp>
#include <vlCore/VisualizationLibrary.hpp>
#include <vlGraphics/GeometryPrimitives.hpp>

using namespace vl;

/** \class vl::VolumePlot
  <img src="pics/pagGuideMarchingCubes_1.jpg">

Example:
\code
  float range = 5.0f;
  fvec3 min_corner(-range,-range,-range);
  fvec3 max_corner(+range,+range,+range);

  VolumePlot plot;
  // set the function sampling space
  plot.setMinCorner(min_corner);
  plot.setMaxCorner(max_corner);
  // set various rendering options
  plot.isosurfaceEffect()->shader()->gocMaterial()->setSpecular(white);
  plot.isosurfaceEffect()->shader()->gocMaterial()->setShininess(50.0f);
  plot.isosurfaceEffect()->shader()->gocMaterial()->setDiffuse(fvec4(1.0f,0,0,0.5f));
  plot.isosurfaceEffect()->shader()->enable(EN_BLEND);
  plot.isosurfaceActor()->renderEventCallbacks()->push_back( new DepthSortCallback );
  plot.textTemplate()->setColor(yellow);
  // set the sampling resolution along the x, y and z directions
  plot.setSamplingResolution(ivec3(100,100,100));
  // function computation and plot generation
  plot.compute( my_func(), 0.900f );
  sceneManager()->tree()->addChild(plot.actorTreeMulti());
\endcode
 */

//-----------------------------------------------------------------------------
// VolumePlot
//-----------------------------------------------------------------------------
VolumePlot::VolumePlot()
{
  VL_DEBUG_SET_OBJECT_NAME()
  mActorTreeMulti = new ActorTree;
  mPlotTransform = new Transform;
  mIsosurfaceActor = new Actor;
  mIsosurfaceGeometry = new Geometry;
  mIsosurfaceEffect = new Effect;
  mBoxEffect = new Effect;
  mTextTemplate = new Text;
  mSamplingResolution = ivec3(64,64,64);
  mLabelFormat = "(%.2n %.2n %.2n)";
  mLabelFont = defFontManager()->acquireFont("/font/bitstream-vera/VeraMono.ttf", 8);
  mMinCorner = fvec3(-1,-1,-1);
  mMaxCorner = fvec3(+1,+1,+1);

  // defaults

  mIsosurfaceEffect->shader()->setRenderState( new Light, 0 );
  mIsosurfaceEffect->shader()->gocMaterial()->setFrontDiffuse(red);
  mIsosurfaceEffect->shader()->gocMaterial()->setBackDiffuse(green);
  mIsosurfaceEffect->shader()->enable(EN_LIGHTING);
  mIsosurfaceEffect->shader()->enable(EN_DEPTH_TEST);
  mIsosurfaceEffect->shader()->gocLightModel()->setTwoSide(true);

  mBoxEffect->shader()->gocPolygonMode()->set(PM_LINE, PM_LINE);
  mBoxEffect->shader()->enable(EN_DEPTH_TEST);

  textTemplate()->setColor(white);
}
//-----------------------------------------------------------------------------
/**
 * \param func The function to be evaluated at each grid point.
 * \param threshold The isovalue of the isosurface passed to the MarcingCubes algorithm.
 */
void VolumePlot::compute(const Function& func, float threshold)
{
  actorTreeMulti()->actors()->clear();
  mActors.clear();

  // volume_box outline
  ref<Geometry> box_outline = makeBox(AABB((vec3)minCorner(),(vec3)maxCorner()));

  // setup isosurface and actors

  MarchingCubes mc;

  mIsosurfaceGeometry->setVertexArray(mc.mVertsArray.get());
  mIsosurfaceGeometry->setNormalArray(mc.mNormsArray.get());
  mIsosurfaceGeometry->drawCalls()->clear();
  mIsosurfaceGeometry->drawCalls()->push_back(mc.mDrawElements.get());

  mIsosurfaceActor->setLod(0, mIsosurfaceGeometry.get());
  mIsosurfaceActor->setEffect(mIsosurfaceEffect.get());
  mIsosurfaceActor->setTransform(mPlotTransform.get());
  mActors.push_back(mIsosurfaceActor.get());
  mActors.push_back( new Actor(box_outline.get(),mBoxEffect.get(),mPlotTransform.get()) );

  ref<Volume> volume = new Volume;
  volume->setup( NULL, false, false, minCorner(), maxCorner(), mSamplingResolution );
  
  mc.volumeInfo()->push_back( new VolumeInfo( volume.get(), threshold ) );

  evaluateFunction(volume->values(), minCorner(), maxCorner(), func);

  // generate vertices and polygons
  mc.run(false);

  // setup labels
  setupLabels(labelFormat(), minCorner(), maxCorner(), labelFont(), mPlotTransform.get());

  for(unsigned i=0; i<mActors.size(); ++i)
    actorTreeMulti()->actors()->push_back(mActors[i].get());
}
//-----------------------------------------------------------------------------
void VolumePlot::setupLabels(const String& format, const fvec3& min_corner, const fvec3& max_corner, Font* font, Transform* root_tr)
{
  ref< Effect > text_fx = new Effect;
  text_fx->shader()->enable(EN_BLEND);
  text_fx->shader()->enable(EN_DEPTH_TEST);

  float coords[][3] =
  {
    {min_corner.x(), min_corner.y(), min_corner.z()},
    {max_corner.x(), min_corner.y(), min_corner.z()},
    {max_corner.x(), max_corner.y(), min_corner.z()},
    {min_corner.x(), max_corner.y(), min_corner.z()},
    {min_corner.x(), min_corner.y(), max_corner.z()},
    {max_corner.x(), min_corner.y(), max_corner.z()},
    {max_corner.x(), max_corner.y(), max_corner.z()},
    {min_corner.x(), max_corner.y(), max_corner.z()}
  };

  String coord_label[] = 
  {
    Say(format) << min_corner.x() << min_corner.y() <<  min_corner.z(),
    Say(format) << max_corner.x() << min_corner.y() <<  min_corner.z(),
    Say(format) << max_corner.x() << max_corner.y() <<  min_corner.z(),
    Say(format) << min_corner.x() << max_corner.y() <<  min_corner.z(),
    Say(format) << min_corner.x() << min_corner.y() <<  max_corner.z(),
    Say(format) << max_corner.x() << min_corner.y() <<  max_corner.z(),
    Say(format) << max_corner.x() << max_corner.y() <<  max_corner.z(),
    Say(format) << min_corner.x() << max_corner.y() <<  max_corner.z()
  };

  for(int i=0; i<8; ++i)
  {
    ref<Text> text = new Text;
    text->setDisplayListEnabled(false);
    text->setBufferObjectEnabled(false);
    text->setFont( font );
    text->setAlignment(AlignHCenter| AlignVCenter);
    text->setText(coord_label[i]);
    // template parameters
    text->setColor( textTemplate()->color() );
    text->setBorderColor( textTemplate()->borderColor() );
    text->setBorderEnabled( textTemplate()->borderEnabled() );
    text->setBackgroundColor( textTemplate()->backgroundColor() );
    text->setBackgroundEnabled( textTemplate()->backgroundEnabled() );
    text->setMargin( textTemplate()->margin() );
    text->setOutlineColor( textTemplate()->outlineColor() );
    text->setOutlineEnabled( textTemplate()->outlineEnabled() );
    text->setShadowColor( textTemplate()->shadowColor() );
    text->setShadowEnabled( textTemplate()->shadowEnabled() );
    text->setShadowVector( textTemplate()->shadowVector() );
    text->setMatrix( textTemplate()->matrix() );

    ref<Actor> text_a = new Actor( text.get(), text_fx.get(), new Transform );
    text_a->transform()->setLocalMatrix( mat4::getTranslation(coords[i][0],coords[i][1],coords[i][2]) );
    if (root_tr)
      root_tr->addChild( text_a->transform() );
    text_a->transform()->computeWorldMatrix();

    mActors.push_back( text_a.get() );
  }
}
//-----------------------------------------------------------------------------
void VolumePlot::evaluateFunction(float* scalar, const fvec3& min_corner, const fvec3& max_corner, const Function& func)
{
  fvec3 v;
  int w = mSamplingResolution.x();
  int h = mSamplingResolution.y();
  int d = mSamplingResolution.z();
  for(int z=0; z<d; ++z)
  {
    float tz = (float)z/(d-1);
    v.z() = min_corner.z()*(1.0f-tz) + max_corner.z()*tz;
    for(int y=0; y<h; ++y)
    {
      float ty = (float)y/(h-1);
      v.y() = min_corner.y()*(1.0f-ty) + max_corner.y()*ty;
      for(int x=0; x<w; ++x)
      {
        float tx = (float)x/(w-1);
        v.x() = min_corner.x()*(1.0f-tx) + max_corner.x()*tx;
        // evaluate function
        scalar[x+y*w+z*w*h] = func(v.x(),v.y(),v.z());
      }
    }
  }
}
//-----------------------------------------------------------------------------
