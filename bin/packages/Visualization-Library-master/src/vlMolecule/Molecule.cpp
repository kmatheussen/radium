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
#include <vlMolecule/RingExtractor.hpp>

using namespace vl;

//-----------------------------------------------------------------------------
Molecule::Molecule(): 
  mActorTree(new ActorTree), 
  mTransformTree(new Transform), 
  mTags(new KeyValues), 
  mAtomLabelTemplate(new Text),
  mAtomLabelEffect(new Effect)
{ 
  VL_DEBUG_SET_OBJECT_NAME()
  mAtomLabelEffect->shader()->enable(EN_BLEND);
  reset(); 
}
//-----------------------------------------------------------------------------
void Molecule::reset()
{
  mMoleculeStyle = MS_BallAndStick;
  mBondDetail = 20;
  mAtomDetail = 2;
  mRingOffset = 0.45f;
  mAromaticRingColor  = fvec4(0,1.0f,0,1.0f);
  mId = 0;
  mAtoms.clear();
  mBonds.clear();
  mCycles.clear(); 
  mMoleculeName.clear();
  tags()->clear();
  mActorTree->actors()->clear();
  mLineWidth= 1.0f;
  mSmoothLines = false;
  mShowAtomNames = false;
  // molecule / actor maps
  mMoleculeToActorMapEnabled = false;
  mActorToMoleculeMapEnabled = false;
  mAtomToActorMap.clear();
  mActorToAtomMap.clear();
  mBondToActorMap.clear();
  mActorToBondMap.clear();
}
//-----------------------------------------------------------------------------
Molecule& Molecule::operator=(const Molecule& other)
{
  reset();
  super::operator=(other);

  mMoleculeName  = other.mMoleculeName;
  *mTags         = *other.mTags;
  mMoleculeStyle = other.mMoleculeStyle;
  mAtomDetail    = other.mAtomDetail;
  mBondDetail    = other.mBondDetail;
  mRingOffset    = other.mRingOffset;
  mAromaticRingColor = other.mAromaticRingColor;
  mLineWidth    = other.mLineWidth;
  mSmoothLines  = other.mSmoothLines;

  std::map<const Atom*, Atom*> atom_map;
  for(unsigned i=0; i<other.atoms().size(); ++i)
  {
    atoms().push_back( new Atom( *other.atom(i) ) );
    atom_map[ other.atom(i) ] = atoms().back().get();
  }

  for(unsigned i=0; i<other.bonds().size(); ++i)
  {
    bonds().push_back( new Bond( *other.bond(i) ) );
    bonds().back()->setAtom1( atom_map[ other.bond(i)->atom1() ] );
    bonds().back()->setAtom2( atom_map[ other.bond(i)->atom2() ] );
  }

  mCycles.resize( other.mCycles.size() );
  for(unsigned i=0; i<other.mCycles.size(); ++i)
  {
    cycle(i).resize( other.cycle(i).size() );
    for(unsigned j=0; j<other.cycle(i).size(); ++j)
      cycle(i)[j] = atom_map[ other.cycle(i)[j].get() ];
  }
  return *this;
}
//-----------------------------------------------------------------------------
const Atom* Molecule::atom(int index) const { return mAtoms[index].get(); }
//-----------------------------------------------------------------------------
Atom* Molecule::atom(int index) { return mAtoms[index].get(); }
//-----------------------------------------------------------------------------
void Molecule::addAtom(Atom* atom) 
{ 
  prepareAtomInsert();
  atoms().push_back(atom); 
}
//-----------------------------------------------------------------------------
void Molecule::eraseAllAtoms()
{
  mAtoms.clear();
  mBonds.clear();
  mCycles.clear();
}
//-----------------------------------------------------------------------------
void Molecule::eraseAtom(int i)
{
  std::vector<Bond*> incident_bonds;
  incidentBonds(incident_bonds, atom(i));
  for(unsigned j=0; j<incident_bonds.size(); ++j)
    eraseBond( incident_bonds[j] );
  atoms().erase(atoms().begin() + i);
}
//-----------------------------------------------------------------------------
void Molecule::eraseAtom(Atom*a)
{
  for(unsigned i=0; i<atoms().size(); ++i)
  {
    if (atom(i) == a)
    {
      std::vector<Bond*> incident_bonds;
      incidentBonds(incident_bonds, a);
      for(unsigned j=0; j<incident_bonds.size(); ++j)
        eraseBond( incident_bonds[j] );
      atoms().erase(atoms().begin() + i);
      return;
    }
  }
}
//-----------------------------------------------------------------------------
Bond* Molecule::addBond(Atom* a1, Atom* a2)
{
  prepareBondInsert();
  ref<Bond> bond = new Bond;
  bond->setAtom1(a1);
  bond->setAtom2(a2);
  bonds().push_back(bond);
  return bond.get();
}
//-----------------------------------------------------------------------------
const Bond* Molecule::bond(int index) const { return mBonds[index].get(); }
//-----------------------------------------------------------------------------
Bond* Molecule::bond(int index) { return mBonds[index].get(); }
//-----------------------------------------------------------------------------
const Bond* Molecule::bond(Atom* a1, Atom* a2) const
{
  for(unsigned i=0; i<bonds().size(); ++i)
    if ( (bond(i)->atom1() == a1 && bond(i)->atom2() == a2) || (bond(i)->atom1() == a2 && bond(i)->atom2() == a1) )
      return bonds()[i].get();
  return NULL;
}
//-----------------------------------------------------------------------------
Bond* Molecule::bond(Atom* a1, Atom* a2)
{
  for(unsigned i=0; i<bonds().size(); ++i)
    if ( (bond(i)->atom1() == a1 && bond(i)->atom2() == a2) || (bond(i)->atom1() == a2 && bond(i)->atom2() == a1) )
      return bonds()[i].get();
  return NULL;
}
//-----------------------------------------------------------------------------
void Molecule::addBond(Bond* bond) 
{ 
  prepareBondInsert();
  bonds().push_back(bond); 
}
//-----------------------------------------------------------------------------
void Molecule::eraseBond(Bond*b)
{
  for(unsigned i=0; i<bonds().size(); ++i)
  {
    if (bond(i) == b)
    {
      bonds().erase(bonds().begin() + i);
      return;
    }
  }
}
//-----------------------------------------------------------------------------
void Molecule::eraseBond(int bond) { bonds().erase(bonds().begin() + bond); }
//-----------------------------------------------------------------------------
void Molecule::eraseAllBonds() { bonds().clear(); }
//-----------------------------------------------------------------------------
void Molecule::eraseBond(Atom* a1, Atom* a2)
{
  for(unsigned i=0; i<bonds().size(); ++i)
  {
    if ( (bond(i)->atom1() == a1 && bond(i)->atom2() == a2) ||
         (bond(i)->atom1() == a2 && bond(i)->atom2() == a1) )
    {
      bonds().erase(bonds().begin() + i);
      return;
    }
  }
}
//-----------------------------------------------------------------------------
void Molecule::eraseBond(int a1, int a2) { eraseBond(atom(a1), atom(a2)); }
//-----------------------------------------------------------------------------
void Molecule::computeAtomAdjacency()
{
  for(int i=0; i<atomCount(); ++i)
    atom(i)->adjacentAtoms().clear();
  for(int i=0; i<bondCount(); ++i)
  {
    bond(i)->atom1()->adjacentAtoms().push_back( bond(i)->atom2() );
    bond(i)->atom2()->adjacentAtoms().push_back( bond(i)->atom1() );
  }
}
//-----------------------------------------------------------------------------
void Molecule::incidentBonds(std::vector<Bond*>& incident_bonds, Atom* atom)
{
  incident_bonds.clear();
  for(int i=0; i<bondCount(); ++i)
    if(bond(i)->atom1() == atom || bond(i)->atom2() == atom)
      incident_bonds.push_back( bond(i) );
}
//-----------------------------------------------------------------------------
void Molecule::setCPKAtomColors()
{
  for(unsigned i=0; i<atoms().size(); ++i)
    atoms()[i]->setColor( atomInfo(atoms()[i]->atomType()).cpkColor() );
}
//-----------------------------------------------------------------------------
void Molecule::setAtomColors(const fvec4& color)
{
  for(unsigned i=0; i<atoms().size(); ++i)
    atoms()[i]->setColor( color );
}
//-----------------------------------------------------------------------------
void Molecule::setCalculatedAtomRadii(float percentage)
{
  for(unsigned i=0; i<atoms().size(); ++i)
    atoms()[i]->setRadius( (float)atomInfo(atoms()[i]->atomType()).calculatedRadius() * percentage );
}
//-----------------------------------------------------------------------------
void Molecule::setEmpiricalAtomRadii(float percentage)
{
  for(unsigned i=0; i<atoms().size(); ++i)
    atoms()[i]->setRadius( (float)atomInfo(atoms()[i]->atomType()).empiricalRadius() * percentage );
}
//-----------------------------------------------------------------------------
void Molecule::setCovalentAtomRadii(float percentage)
{
  for(unsigned i=0; i<atoms().size(); ++i)
    atoms()[i]->setRadius( (float)atomInfo(atoms()[i]->atomType()).covalentRadius() * percentage );
}
//-----------------------------------------------------------------------------
void Molecule::setVanDerWaalsAtomRadii(float percentage)
{
  for(unsigned i=0; i<atoms().size(); ++i)
    atoms()[i]->setRadius( (float)atomInfo(atoms()[i]->atomType()).vanDerWaalsRadius() * percentage );
}
//-----------------------------------------------------------------------------
void Molecule::setAtomRadii(float radius)
{
  for(unsigned i=0; i<atoms().size(); ++i)
    atoms()[i]->setRadius( radius );
}
//-----------------------------------------------------------------------------
void Molecule::setBondRadii(float radius)
{
  for(unsigned i=0; i<bonds().size(); ++i)
    bonds()[i]->setRadius( radius );
}
//-----------------------------------------------------------------------------
void Molecule::setAtomTypeVisible(EAtomType type, bool visible)
{
  for(unsigned i=0; i<atoms().size(); ++i)
    if (atom(i)->atomType() == type)
      atom(i)->setVisible(visible);
}
//-----------------------------------------------------------------------------
void Molecule::setAromaticBondsColor(const fvec4& color)
{
  for(unsigned i=0; i<bonds().size(); ++i)
  {
    if(bonds()[i]->bondType() == BT_Aromatic)
      bonds()[i]->setColor(color);
  }
}
//-----------------------------------------------------------------------------
