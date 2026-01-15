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

#include <vlMolecule/Molecule.hpp>
#include <vlGraphics/GeometryPrimitives.hpp>
#include <vlGraphics/Text.hpp>
#include <vlGraphics/Light.hpp>

using namespace vl;

//-----------------------------------------------------------------------------
class EffectCache
{
public:
  EffectCache(): mLight(new Light) {}

  void clear() { effects().clear(); }

  Effect* acquireEffect(const fvec4& color)
  {
    for(unsigned i=0; i<effects().size(); ++i)
    {
      if (effects()[i]->shader()->gocMaterial()->frontDiffuse() == color)
      {
        return effects()[i].get();
      }
    }

    ref<Effect> fx = new Effect;
    fx->shader()->enable(EN_DEPTH_TEST);
    fx->shader()->enable(EN_CULL_FACE);
    fx->shader()->enable(EN_LIGHTING);
    fx->shader()->setRenderState(mLight.get(), 0);
    fx->shader()->gocMaterial()->setDiffuse(color);
    effects().push_back(fx.get());
    return fx.get();
  }

  const std::vector< ref<Effect> >& effects() const { return mEffects; }
  std::vector< ref<Effect> >& effects() { return mEffects; }

  const Light* light() const { return mLight.get(); }
  Light* light() { return mLight.get(); }

protected:
  std::vector< ref<Effect> > mEffects;
  ref<Light> mLight;
};
//-----------------------------------------------------------------------------
class AtomGeometryCache
{
public:
  AtomGeometryCache(): mDetail(1) {}

  void clear() { mGeometryMap.clear(); }
  const std::map< float, ref<Geometry> >& geometryMap() const { return mGeometryMap; }
  std::map< float, ref<Geometry> >& geometryMap() { return mGeometryMap; }
  Geometry* acquireAtomGeometry(float radius)
  {
    std::map< float, ref<Geometry> >::iterator it = geometryMap().find(radius);
    if (it!=geometryMap().end())
      return it->second.get();
    else
    {
      ref<Geometry> sphere = makeIcosphere( vec3(0,0,0), radius*2.0f, detail() );
      geometryMap()[radius] = sphere;
      return sphere.get();
    }
  }

  int detail() const { return mDetail; }
  void setDetail(int detail) { mDetail = detail; }

protected:
  std::map< float, ref<Geometry> > mGeometryMap;
  int mDetail;
};
//-----------------------------------------------------------------------------
class BondGeometryCache
{
  class BondKey
  {
  public:
    float height;
    fvec4 col1;
    fvec4 col2;
    ECapsuleCap top_cap;
    ECapsuleCap bottom_cap;

    BondKey(float h, const fvec4& c1, const fvec4& c2, ECapsuleCap topcap, ECapsuleCap bottomcap): height(h), col1(c1), col2(c2), top_cap(topcap), bottom_cap(bottomcap) {}
    bool operator==(const BondKey& other) const
    {
      return height     == other.height  &&
             col1       == other.col1    &&
             col2       == other.col2    &&
             top_cap    == other.top_cap &&
             bottom_cap == other.bottom_cap;
    }
    bool operator<(const BondKey& other) const
    {
      if (top_cap!=other.top_cap)
        return top_cap<other.top_cap;
      else
      if (bottom_cap!=other.bottom_cap)
        return bottom_cap<other.bottom_cap;
      else
      if (height!=other.height)
        return height<other.height;
      else
      if (col1!=other.col1)
        return col1<other.col1;
      else
        return col2<other.col2;
    }
  };
public:
  BondGeometryCache(): mDetail(20), mDiameter(0.20f), mQuantization(100.0f) {}

  void clear() { mGeometryMap.clear(); }
  const std::map< BondKey, ref<Geometry> >& geometryMap() const { return mGeometryMap; }
  std::map< BondKey, ref<Geometry> >& geometryMap() { return mGeometryMap; }
  Geometry* acquireBondGeometry(float length, const fvec4& c1, const fvec4& c2, ECapsuleCap top_cap, ECapsuleCap bottom_cap)
  {
    float quant_lenght = int(length*quantization()) / quantization();
    BondKey key(quant_lenght,c1,c2,top_cap,bottom_cap);
    std::map< BondKey, ref<Geometry> >::iterator it = geometryMap().find( key );
    if (it!=geometryMap().end())
    {
      VL_CHECK(it->first == key)
      return it->second.get();
    }
    else
    {
      ref<Geometry> cylinder = makeCapsule( diameter()/2.0f, quant_lenght+2.0f/quantization(), detail(), top_cap, bottom_cap, c2, c1 );
      cylinder->computeNormals();
      geometryMap()[key] = cylinder;
      return cylinder.get();
    }
  }

  int detail() const { return mDetail; }
  void setDetail(int detail) { mDetail = detail; }

  float diameter() const { return mDiameter; }
  void setDiameter(float diameter) { mDiameter = diameter; }

  float quantization() const { return mQuantization; }
  void setQuantization(float quantization) { mQuantization = quantization; }

protected:
  std::map< BondKey, ref<Geometry> > mGeometryMap;
  int mDetail;
  float mDiameter;
  float mQuantization;
};
//-----------------------------------------------------------------------------
void Molecule::prepareForRendering()
{
  actorTree()->actors()->clear();
  transformTree()->eraseAllChildren();

  switch(moleculeStyle())
  {
    case MS_Wireframe:    wireframeStyle();    generateRings(); break;
    case MS_BallAndStick: ballAndStickStyle(); generateRings(); break;
    case MS_Sticks:       sticksStyle();       generateRings(); break;
    case MS_AtomsOnly:    atomsStyle();                         break;
  }
  generateAtomLabels();
  transformTree()->computeWorldMatrixRecursive();
}
//-----------------------------------------------------------------------------
void Molecule::generateAtomLabel(const Atom* atom, Transform* tr)
{
  if (atomLabelTemplate()->font() && 
      showAtomNames()             &&
      atom->visible()             &&
      atom->showAtomName()        )
  {
    ref<Text> text = new Text;
    // text label
    text->setText( atom->atomName().c_str() );
    // text template style
    text->setViewportAlignment( atomLabelTemplate()->viewportAlignment() );
    text->setTextAlignment( atomLabelTemplate()->textAlignment() );
    text->setShadowVector( atomLabelTemplate()->shadowVector() );
    text->setShadowEnabled( atomLabelTemplate()->shadowEnabled() );
    text->setShadowColor( atomLabelTemplate()->shadowColor() );
    text->setOutlineEnabled( atomLabelTemplate()->outlineEnabled() );
    text->setOutlineColor( atomLabelTemplate()->outlineColor() );
    text->setMode( atomLabelTemplate()->mode() );
    text->setMargin( atomLabelTemplate()->margin() );
    text->setKerningEnabled( atomLabelTemplate()->kerningEnabled() );
    text->setFont( atomLabelTemplate()->font() );
    text->setColor( atomLabelTemplate()->color() );
    text->setBorderEnabled( atomLabelTemplate()->borderEnabled() );
    text->setBorderColor( atomLabelTemplate()->borderColor() );
    text->setBackgroundEnabled( atomLabelTemplate()->backgroundEnabled() );
    text->setBackgroundColor( atomLabelTemplate()->backgroundColor() );
    text->setAlignment( atomLabelTemplate()->alignment() );
    // text actor
    ref<Actor> text_act = new Actor( text.get(), mAtomLabelEffect.get(), tr );
    actorTree()->actors()->push_back(text_act.get());
  }
}
//-----------------------------------------------------------------------------
void Molecule::generateAtomLabels()
{
  for(unsigned i=0; i<atoms().size(); ++i)
  {
    ref<Transform> tr = new Transform(mat4::getTranslation((vec3)atoms()[i]->coordinates()));
    transformTree()->addChild(tr.get());
    generateAtomLabel(atoms()[i].get(), tr.get());
  }
}
//-----------------------------------------------------------------------------
void Molecule::wireframeStyle()
{
  // no maps are generated for this style.
  mAtomToActorMap.clear();
  mActorToAtomMap.clear();
  mBondToActorMap.clear();
  mActorToBondMap.clear();

  ref<Geometry> geom = new Geometry;
  ref<ArrayFloat3> points = new ArrayFloat3;
  geom->setVertexArray(points.get());
  ref<ArrayFloat4> colors = new ArrayFloat4;
  geom->setColorArray(colors.get());
  std::vector<fvec3> pt;
  std::vector<fvec4> cols;
  for(unsigned ibond=0; ibond<bonds().size(); ++ibond)
  {
    Bond* b = bond(ibond);
    if (b->visible() && b->atom1()->visible() && b->atom2()->visible())
    {
      fvec4 c1 = b->color();
      fvec4 c2 = b->color();
      if (b->useAtomColors())
      {
        c1 = b->atom1()->color();
        c2 = b->atom2()->color();
      }
      if (c1 == c2)
      {
        pt.push_back( b->atom1()->coordinates() );
        pt.push_back( b->atom2()->coordinates() );
        cols.push_back(c1);
        cols.push_back(c1);
      }
      else
      {
        fvec3 center = (b->atom1()->coordinates() + b->atom2()->coordinates())/2.0f;
        pt.push_back( b->atom1()->coordinates() );
        pt.push_back( center );
        pt.push_back( center );
        pt.push_back( b->atom2()->coordinates() );
        cols.push_back(c1);
        cols.push_back(c1);
        cols.push_back(c2);
        cols.push_back(c2);
      }
    }
  }
  points->initFrom(pt);
  colors->initFrom(cols);
  geom->drawCalls()->push_back(new DrawArrays(PT_LINES, 0, (int)points->size()));

  ref<Effect> fx = new Effect;
  fx->shader()->enable(EN_DEPTH_TEST);
  if (smoothLines())
  {
    fx->shader()->enable(EN_BLEND);
    fx->shader()->enable(EN_LINE_SMOOTH);
  }
  if (lineWidth() != 1.0f)
    fx->shader()->gocLineWidth()->set(lineWidth());

  actorTree()->actors()->push_back( new Actor(geom.get(), fx.get(), NULL) );
}
//-----------------------------------------------------------------------------
void Molecule::atomsStyle()
{
  mAtomToActorMap.clear();
  mActorToAtomMap.clear();
  mBondToActorMap.clear();
  mActorToBondMap.clear();

  EffectCache fx_cache;
  AtomGeometryCache atom_geom_cache;
  atom_geom_cache.setDetail(atomDetail());
  for(unsigned iatom=0; iatom<atoms().size(); ++iatom)
  {
    if (atom(iatom)->visible())
    {
      Effect* fx = fx_cache.acquireEffect(atom(iatom)->color());
      float r = atom(iatom)->radius();
      ref<Geometry> ball = atom_geom_cache.acquireAtomGeometry(r);
      ref<Actor> atom_act = new Actor( ball.get(), fx, new Transform );
      atom_act->transform()->setLocalMatrix( mat4::getTranslation( (vec3)atom(iatom)->coordinates()) );
      transformTree()->addChild(atom_act->transform());
      actorTree()->actors()->push_back( atom_act.get() );

      // actor -> atom map
      if (isActorToMoleculeMapEnabled())
        mActorToAtomMap.insert( std::pair< ref<Actor>, ref<Atom> >(atom_act, atom(iatom)) );
      // atom -> actor map
      if (isMoleculeToActorMapEnabled())
        mAtomToActorMap.insert( std::pair< ref<Atom>, ref<Actor> >(atom(iatom), atom_act) );
    }
  }
}
//-----------------------------------------------------------------------------
void Molecule::ballAndStickStyle()
{
  mAtomToActorMap.clear();
  mActorToAtomMap.clear();
  mBondToActorMap.clear();
  mActorToBondMap.clear();

  EffectCache fx_cache;
  AtomGeometryCache atom_geom_cache;
  atom_geom_cache.setDetail(atomDetail());
  for(unsigned int iatom=0; iatom<atoms().size(); ++iatom)
  {
    if (atom(iatom)->visible())
    {
      Effect* fx = fx_cache.acquireEffect(atom(iatom)->color());
      float r = atom(iatom)->radius();
      ref<Geometry> ball = atom_geom_cache.acquireAtomGeometry(r);
      
      // mic fixme:
      // it would be nice to have a pool to accelerate Actor and Transform allocation

      ref<Actor> atom_act = new Actor( ball.get(), fx, new Transform );
      atom_act->transform()->setLocalMatrix( mat4::getTranslation( (vec3)atom(iatom)->coordinates()) );
      transformTree()->addChild(atom_act->transform());
      actorTree()->actors()->push_back( atom_act.get() );

      // actor -> atom map
      if (isActorToMoleculeMapEnabled())
        mActorToAtomMap.insert( std::pair< ref<Actor>, ref<Atom> >(atom_act, atom(iatom)) );
      // atom -> actor map
      if (isMoleculeToActorMapEnabled())
        mAtomToActorMap.insert( std::pair< ref<Atom>, ref<Actor> >(atom(iatom), atom_act) );
    }
  }

  ref<Effect> fx = new Effect;
  fx->shader()->enable(EN_DEPTH_TEST);
  fx->shader()->enable(EN_CULL_FACE);
  fx->shader()->gocMaterial()->setColorMaterialEnabled(true);
  fx->shader()->gocLightModel()->setTwoSide(false);
  fx->shader()->enable(EN_LIGHTING);
  fx->shader()->setRenderState( fx_cache.light(), 0 );
  // fx->shader()->gocPolygonMode()->set(PM_LINE, PM_LINE);

  BondGeometryCache bond_geom_cache;
  bond_geom_cache.setDetail(bondDetail());
  for(unsigned int ibond=0; ibond<bonds().size(); ++ibond)
  {
    if (bond(ibond)->visible() && bond(ibond)->atom1()->visible() && bond(ibond)->atom2()->visible())
    {
      Bond* b = bond(ibond);
      fvec4 c1 = b->color();
      fvec4 c2 = b->color();
      if (b->useAtomColors())
      {
        c1 = b->atom1()->color();
        c2 = b->atom2()->color();
      }
      float len = (b->atom1()->coordinates() - b->atom2()->coordinates()).length();
      float diam = b->radius()*2.0f;
      bond_geom_cache.setDiameter(diam);
      ref<Geometry> geom = bond_geom_cache.acquireBondGeometry(len,c1,c2,CC_NoCap,CC_NoCap);
      ref<Actor> bond_act = new Actor( geom.get(), fx.get(), new Transform );
      transformTree()->addChild(bond_act->transform());
      fvec3 center = (b->atom1()->coordinates() + b->atom2()->coordinates()) / 2.0f;
      fvec3 direction = (b->atom2()->coordinates() - b->atom1()->coordinates()).normalize();
      fmat4 mat = fmat4::getTranslation(center) * fmat4::getRotation(fvec3(0,1,0), direction);
      bond_act->transform()->setLocalMatrix( (mat4)mat );
      actorTree()->actors()->push_back( bond_act.get() );

      // actor -> bond map
      if (isActorToMoleculeMapEnabled())
        mActorToBondMap.insert( std::pair< ref<Actor>, ref<Bond> >(bond_act, bond(ibond)) );
      // bond -> actor map
      if (isMoleculeToActorMapEnabled())
        mBondToActorMap.insert( std::pair< ref<Bond>, ref<Actor> >(bond(ibond), bond_act) );
    }
  }
}
//-----------------------------------------------------------------------------
void Molecule::sticksStyle()
{
  mAtomToActorMap.clear();
  mActorToAtomMap.clear();
  mBondToActorMap.clear();
  mActorToBondMap.clear();

  ref<Effect> fx = new Effect;
  fx->shader()->enable(EN_DEPTH_TEST);
  fx->shader()->enable(EN_CULL_FACE);
  fx->shader()->gocMaterial()->setColorMaterialEnabled(true);
  fx->shader()->gocLightModel()->setTwoSide(false);
  fx->shader()->enable(EN_LIGHTING);
  fx->shader()->setRenderState( new Light, 0 );
  /*fx->shader()->gocPolygonMode()->set(PM_LINE, PM_LINE);*/

  BondGeometryCache bond_geom_cache;
  bond_geom_cache.setDetail(bondDetail());
  for(unsigned int ibond=0; ibond<bonds().size(); ++ibond)
  {
    if (bond(ibond)->visible() && bond(ibond)->atom1()->visible() && bond(ibond)->atom2()->visible())
    {
      Bond* b = bond(ibond);
      fvec4 c1 = b->color();
      fvec4 c2 = b->color();
      if (b->useAtomColors())
      {
        c1 = b->atom1()->color();
        c2 = b->atom2()->color();
      }
      float len = (b->atom1()->coordinates() - b->atom2()->coordinates()).length();
      float diam = b->radius()*2.0f;
      bond_geom_cache.setDiameter(diam);
      ref<Geometry> geom = bond_geom_cache.acquireBondGeometry(len,c1,c2,CC_RoundedCap,CC_RoundedCap);
      ref<Actor> bond_act = new Actor( geom.get(), fx.get(), new Transform );
      transformTree()->addChild(bond_act->transform());
      fvec3 center = (b->atom1()->coordinates() + b->atom2()->coordinates()) / 2.0f;
      fvec3 direction = (b->atom2()->coordinates() - b->atom1()->coordinates()).normalize();
      fmat4 mat = fmat4::getTranslation(center) * fmat4::getRotation(fvec3(0,1,0), direction);
      bond_act->transform()->setLocalMatrix( (mat4)mat );
      actorTree()->actors()->push_back( bond_act.get() );

      // actor -> bond map
      if (isActorToMoleculeMapEnabled())
        mActorToBondMap.insert( std::pair< ref<Actor>, ref<Bond> >(bond_act, bond(ibond)) );
      // bond -> actor map
      if (isMoleculeToActorMapEnabled())
        mBondToActorMap.insert( std::pair< ref<Bond>, ref<Actor> >(bond(ibond), bond_act) );
    }
  }
}
//-----------------------------------------------------------------------------
void Molecule::generateRings()
{
  if (!cycles().empty())
  {
    ref<Geometry> geom = new Geometry;
    ref<ArrayFloat3> points = new ArrayFloat3;
    geom->setVertexArray(points.get());
    ref<ArrayFloat4> colors = new ArrayFloat4;
    geom->setColorArray(colors.get());
    std::vector<fvec3> pt;
    std::vector<fvec4> cols;
    for(unsigned icycle=0; icycle<cycles().size(); ++icycle)
    {
      AABB aabb;
      for(unsigned iatom=0; iatom<cycle(icycle).size(); ++iatom)
        aabb += (vec3)cycle(icycle)[iatom]->coordinates();
      fvec3 center = (fvec3)aabb.center();

      for(unsigned iatom=0; iatom<cycle(icycle).size(); ++iatom)
      {
        int iatom2 = (iatom+1) % cycle(icycle).size();
        fvec3 v1 = cycle(icycle)[iatom ]->coordinates();
        fvec3 v2 = cycle(icycle)[iatom2]->coordinates();
        v1 += (center-v1).normalize() * ringOffset();
        v2 += (center-v2).normalize() * ringOffset();
        pt.push_back( v1 );
        pt.push_back( v2 );
        cols.push_back( aromaticRingColor() );
        cols.push_back( aromaticRingColor() );
      }
    }
    points->initFrom(pt);
    colors->initFrom(cols);
    geom->drawCalls()->push_back(new DrawArrays(PT_LINES, 0, (int)points->size()));

    ref<Effect> fx = new Effect;
    fx->shader()->enable(EN_DEPTH_TEST);

    actorTree()->actors()->push_back( new Actor(geom.get(), fx.get(), NULL) );
  }
}
//-----------------------------------------------------------------------------
