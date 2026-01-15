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

#ifndef VolumePlot_INCLUDE_ONCE
#define VolumePlot_INCLUDE_ONCE

#include <vlVolume/link_config.hpp>
#include <vlGraphics/Actor.hpp>
#include <vlGraphics/Geometry.hpp>
#include <vlGraphics/Text.hpp>
#include <vlGraphics/Effect.hpp>
#include <vlCore/Transform.hpp>
#include <vlGraphics/ActorTreeAbstract.hpp>
#include <vlGraphics/SceneManagerActorTree.hpp>
#include <vlVolume/MarchingCubes.hpp>

namespace vl
{
  class SceneManager;

  //! Generates a 3D plot with labels and isosurface. The isosurface is generated using the MarchingCubes algorithm.
  class VLVOLUME_EXPORT VolumePlot: public Object
  {
    VL_INSTRUMENT_CLASS(vl::VolumePlot, Object)

  public:
    //! A function to be used with VolumePlot
    class Function
    {
    public:
      virtual float operator()(float x, float y, float z) const = 0;
    };

  public:
    //! Constructor.
    VolumePlot();

    //! Computes the function and generates the plot. This method should be called after all the other methods.
    void compute(const Function& func, float threshold);

    //! The Actor representing the isosurface
    const Actor* isosurfaceActor() const { return mIsosurfaceActor.get(); }
    //! The Actor representing the isosurface
    Actor* isosurfaceActor() { return mIsosurfaceActor.get(); }

    //! The Geometry representing the isosurface
    const Geometry* isosurfaceGeometry() const { return mIsosurfaceGeometry.get(); }
    //! The Geometry representing the isosurface
    Geometry* isosurfaceGeometry() { return mIsosurfaceGeometry.get(); }

    //! Used to get/set the rendering options (like color, material, transparency) etc. of the isosurface 
    const Effect* isosurfaceEffect() const { return mIsosurfaceEffect.get(); }
    //! Used to get/set the rendering options (like color, material, transparency) etc. of the isosurface 
    Effect* isosurfaceEffect() { return mIsosurfaceEffect.get(); }

    //! Used to get/set the rendering options (like color, material, transparency) etc. of the box
    const Effect* boxEffect() const { return mBoxEffect.get(); }
    //! Used to get/set the rendering options (like color, material, transparency) etc. of the box
    Effect* boxEffect() { return mBoxEffect.get(); }

    //! Default value: fvec3(-1,-1,-1)
    const fvec3& minCorner() const { return mMinCorner; }
    //! Default value: fvec3(-1,-1,-1)
    void setMinCorner(const fvec3& min_corner) { mMinCorner = min_corner; }

    //! Default value: fvec3(+1,+1,+1)
    const fvec3& maxCorner() const { return mMaxCorner; }
    //! Default value: fvec3(+1,+1,+1)
    void setMaxCorner(const fvec3& max_corner) { mMaxCorner = max_corner; }

    //! The transform associated to the whole plot
    const Transform* plotTransform() const { return mPlotTransform.get(); }
    //! The transform associated to the whole plot
    Transform* plotTransform() { return mPlotTransform.get(); }
    //! The transform associated to the whole plot
    void setPlotTransform(Transform* tr) { mPlotTransform = tr; }

    //! Default value: ivec3(64,64,64)
    const ivec3& samplingResolution() const { return mSamplingResolution; }
    //! Default value: ivec3(64,64,64)
    void setSamplingResolution(const ivec3& size) { mSamplingResolution = size; }

    //! Sets the format of the labels
    const String& labelFormat() const { return mLabelFormat; }
    //! Sets the format of the label to be generated, es. "(%.2n %.2n %.2n)" or "<%.3n, %.3n, %.3n>"
    void setLabelFormat(const String& format) { mLabelFormat = format; }

    //! The Font to be used for the box labels
    const Font* labelFont() const { return mLabelFont.get(); }
    //! The Font to be used for the box labels
    Font* labelFont() { return mLabelFont.get(); }
    //! The Font to be used for the box labels
    void setLabelFont(Font* font) { mLabelFont = font; }

    //! A Text used to initialize the plot labels
    const Text* textTemplate() const { return mTextTemplate.get(); }
    //! A Text used to initialize the plot labels
    Text* textTemplate() { return mTextTemplate.get(); }

    ActorTree* actorTreeMulti() { return mActorTreeMulti.get(); }
    const ActorTree* actorTreeMulti() const { return mActorTreeMulti.get(); }

  protected:
    void setupLabels(const String& format, const fvec3& min_corner, const fvec3& max_corner, Font* font, Transform* root_tr);
    void evaluateFunction(float* scalar, const fvec3& min_corner, const fvec3& max_corner, const Function& func);

  protected:
    std::vector< ref<Actor> > mActors;
    ref<Transform> mPlotTransform;
    ivec3 mSamplingResolution;
    String mLabelFormat;
    ref< Font > mLabelFont;
    fvec3 mMinCorner;
    fvec3 mMaxCorner;
    ref<Geometry> mIsosurfaceGeometry;
    ref<Actor> mIsosurfaceActor;
    ref<Effect> mIsosurfaceEffect;
    ref<Effect> mBoxEffect;
    ref<Text> mTextTemplate;
    ref<ActorTree> mActorTreeMulti;
  };
}

#endif
