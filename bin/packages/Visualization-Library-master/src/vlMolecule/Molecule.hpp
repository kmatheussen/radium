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

#ifndef Molecule_INCLUDE_ONCE
#define Molecule_INCLUDE_ONCE

#include <vlMolecule/link_config.hpp>
#include <vlMolecule/Atom.hpp>
#include <vlMolecule/Bond.hpp>
#include <vlGraphics/Geometry.hpp>
#include <vlGraphics/Actor.hpp>
#include <vlGraphics/ActorTree.hpp>
#include <vlGraphics/Text.hpp>
#include <vlCore/String.hpp>
#include <vlCore/KeyValues.hpp>

namespace vl
{
  //! Defines the main molecule styles.
  typedef enum
  {
    MS_AtomsOnly,
    MS_BallAndStick,
    MS_Sticks,
    MS_Wireframe,
  } EMoleculeStyle;

  /** The Molecule class is used to manage and render 3D molecular structures.
   * \sa
   * - \ref pagGuideMolecule "Molecule Visualization Tutorial"
   * - Atom
   * - Bond
   *
   * <img src="pics/pagGuideMolecule.png">
   */
  class VLMOLECULE_EXPORT Molecule: public Object
  {
    VL_INSTRUMENT_CLASS(vl::Molecule, Object)

  public:
    Molecule();
    ~Molecule() { reset(); }
    Molecule(const Molecule& other): Object(other) { operator=(other); }
    Molecule& operator=(const Molecule& other);

    void reset();

    void setMoleculeName(const String& name) { mMoleculeName = name; }
    const String moleculeName() const { return mMoleculeName; }

    unsigned int id() const { return mId; }
    void setId(unsigned int id) { mId = id; }

    KeyValues* tags() { return mTags.get(); }
    const KeyValues* tags() const { return mTags.get(); }

    const std::vector< ref<Atom> >& atoms() const { return mAtoms; }
    std::vector< ref<Atom> >& atoms() { return mAtoms; }

    int atomCount() const { return (int)mAtoms.size(); }
    const Atom* atom(int index) const;
    Atom* atom(int index);
    void addAtom(Atom* atom);
    void eraseAtom(Atom*atom);
    void eraseAtom(int index);
    void eraseAllAtoms();

    const std::vector< ref<Bond> >& bonds() const { return mBonds; }
    std::vector< ref<Bond> >& bonds() { return mBonds; }

    int bondCount() const { return (int)mBonds.size(); }
    const Bond* bond(int index) const;
    Bond* bond(int index);
    const Bond* bond(Atom* a1, Atom* a2) const;
    Bond* bond(Atom* a1, Atom* a2);
    void addBond(Bond* bond);
    Bond* addBond(Atom* a1, Atom* a2);
    void eraseBond(Bond*bond);
    void eraseBond(int bond);
    void eraseBond(Atom* a1, Atom* a2);
    void eraseBond(int a1, int a2);
    void eraseAllBonds();

    void computeAtomAdjacency();
    void incidentBonds(std::vector<Bond*>& inc_bonds, Atom* atom);

    //! Returns the i-th cycle
    const std::vector< ref<Atom> >& cycle(int i) const { return mCycles[i]; }
    //! Returns the i-th cycle
    std::vector< ref<Atom> >& cycle(int i) { return mCycles[i]; }

    //! Returns the list of cycles
    const std::vector< std::vector< ref<Atom> > >& cycles() const { return mCycles; }
    //! Returns the list of cycles
    std::vector< std::vector< ref<Atom> > >& cycles() { return mCycles; }

    /** Generates the geometry representing the current molecule, atom and bond settings.
     *  The actors, geometry, and transforms generated by this function can be found in actorTree() and transformTree().
     */
    void prepareForRendering();

    //! The ActorTree node containing the Actor[s] representing the molecule.
    const ActorTree* actorTree() const { return mActorTree.get(); }
    //! The ActorTree node containing the Actor[s] representing the molecule.
    ActorTree* actorTree() { return mActorTree.get(); }

    //! Sets all the atoms' color to their CPK color.
    void setCPKAtomColors();
    //! Sets all the atoms' color to the specified color.
    void setAtomColors(const fvec4& color);

    //! Sets all the atoms' radii to their calculated atom radii
    void setCalculatedAtomRadii(float percentage=1.0f);
    //! Sets all the atoms' radii to their empirical atom radii
    void setEmpiricalAtomRadii(float percentage=1.0f);
    //! Sets all the atoms' radii to their covalent atom radii
    void setCovalentAtomRadii(float percentage=1.0f);
    //! Sets all the atoms' radii to their van der Waals atom radii
    void setVanDerWaalsAtomRadii(float percentage=1.0f);
    //! Sets all the atoms' radii to the specified one.
    void setAtomRadii(float radius);
    //! Sets all the bonds' radii to the specified one.
    void setBondRadii(float radius);

    void setAtomTypeVisible(EAtomType type, bool visible);

    //! The rendering style of the molecule
    void setMoleculeStyle(EMoleculeStyle style) { mMoleculeStyle = style; }
    //! The rendering style of the molecule
    EMoleculeStyle moleculeStyle() const { return mMoleculeStyle; }

    //! Geometrical detail used to render the atoms, usually between 0 and 3 (default is 2)
    void setAtomDetail(int detail) { mAtomDetail = detail; }
    //! Geometrical detail used to render the atoms, usually between 0 and 3 (default is 2)
    int atomDetail() const { return mAtomDetail; }

    //! Geometrical detail used to render the bonds, usually between 5 and 50 (default is 20)
    void setBondDetail(int detail) { mBondDetail = detail; }
    //! Geometrical detail used to render the bonds, usually between 5 and 50 (default is 20)
    int bondDetail() const { return mBondDetail; }

    float ringOffset() const { return mRingOffset; }
    void setRingOffset(float offset) { mRingOffset = offset; }

    void setAromaticBondsColor(const fvec4& color);
    void setAromaticRingColor(const fvec4& color) { mAromaticRingColor = color; }
    const fvec4& aromaticRingColor() const { return mAromaticRingColor; }

    float lineWidth() const { return mLineWidth; }
    bool smoothLines() const { return mSmoothLines; }

    void setLineWidth(float w) { mLineWidth = w; }
    void setSmoothLines(bool smooth) { mSmoothLines = smooth; }

    //! The transform tree used by the generated bonds, atoms and labels
    Transform* transformTree() { return mTransformTree.get(); }
    //! The transform tree used by the generated bonds, atoms and labels
    const Transform* transformTree() const { return mTransformTree.get(); }

    //! The text settings to be used to render the atom labels
    const Text* atomLabelTemplate() const { return mAtomLabelTemplate.get(); }
    //! The text settings to be used to render the atom labels
    Text* atomLabelTemplate() { return mAtomLabelTemplate.get(); }

    //! Globally defines whether the atom names should be rendered or not. See also Atom::setShowAtomName().
    void setShowAtomNames(bool show) { mShowAtomNames = show; }
    //! Globally defines whether the atom names should be rendered or not. See also Atom::setShowAtomName().
    bool showAtomNames() const{ return mShowAtomNames; }

    //! The Effect used to render the atom labels
    const Effect* atomLabelEffect() const { return mAtomLabelEffect.get(); }
    //! The Effect used to render the atom labels
    Effect* atomLabelEffect() { return mAtomLabelEffect.get(); }

    //! If enabled the atomToActorMap() and bondToActorMap() maps will be compiled next time the molecule's geometry is regenerated (except for wireframe style).
    void setMoleculeToActorMapEnabled(bool enabled) { mMoleculeToActorMapEnabled = enabled; }
    //! If enabled the atomToActorMap() and bondToActorMap() maps will be compiled next time the molecule's geometry is regenerated (except for wireframe style).
    bool isMoleculeToActorMapEnabled() const { return mMoleculeToActorMapEnabled; }

    //! If enabled the actorToAtomMap() and actorToBondMap() maps will be compiled next time the molecule's geometry is regenerated (except for wireframe style).
    void setActorToMoleculeMapEnabled(bool enabled) { mActorToMoleculeMapEnabled = enabled; }
    //! If enabled the actorToAtomMap() and actorToBondMap() maps will be compiled next time the molecule's geometry is regenerated (except for wireframe style).
    bool isActorToMoleculeMapEnabled() const { return mActorToMoleculeMapEnabled; }

    //! Maps an Atom to it's corresponding Actor
    const std::map< ref<Atom>, ref<Actor> >& atomToActorMap() const { return mAtomToActorMap; }
    //! Maps an Actor to it's corresponding Atom
    const std::map< ref<Actor>, ref<Atom> >& actorToAtomMap() const { return mActorToAtomMap; }
    //! Maps a Bond to it's corresponding Actor
    const std::map< ref<Bond>, ref<Actor> >& bondToActorMap() const { return mBondToActorMap; }
    //! Maps an Actor to it's corresponding Bond
    const std::map< ref<Actor>, ref<Bond> >& actorToBondMap() const { return mActorToBondMap; }

    //! Maps an Atom to it's corresponding Actor
    std::map< ref<Atom>, ref<Actor> >& atomToActorMap() { return mAtomToActorMap; }
    //! Maps an Actor to it's corresponding Atom
    std::map< ref<Actor>, ref<Atom> >& actorToAtomMap() { return mActorToAtomMap; }
    //! Maps a Bond to it's corresponding Actor
    std::map< ref<Bond>, ref<Actor> >& bondToActorMap() { return mBondToActorMap; }
    //! Maps an Actor to it's corresponding Bond
    std::map< ref<Actor>, ref<Bond> >& actorToBondMap() { return mActorToBondMap; }

  protected:
    void prepareAtomInsert(int bonus=100)
    {
      if (atoms().size() == atoms().capacity())
        atoms().reserve(atoms().size() + bonus);
    }
    void prepareBondInsert(int bonus=100)
    {
      if (bonds().size() == bonds().capacity())
        bonds().reserve(bonds().size() + bonus);
    }
    void wireframeStyle();
    void atomsStyle();
    void ballAndStickStyle();
    void sticksStyle();
    void generateRings();
    void generateAtomLabels();
    void generateAtomLabel(const Atom* atom, Transform* tr);

  protected:
    fvec4 mAromaticRingColor;
    ref<ActorTree> mActorTree;
    ref<Transform> mTransformTree;
    std::vector< ref<Atom> > mAtoms;
    std::vector< ref<Bond> > mBonds;
    std::vector< std::vector< ref<Atom> > > mCycles; 
    std::map< ref<Atom>, ref<Actor> > mAtomToActorMap;
    std::map< ref<Actor>, ref<Atom> > mActorToAtomMap;
    std::map< ref<Bond>, ref<Actor> > mBondToActorMap;
    std::map< ref<Actor>, ref<Bond> > mActorToBondMap;
    String mMoleculeName;
    ref<KeyValues> mTags;
    ref<Text> mAtomLabelTemplate;
    ref<Effect> mAtomLabelEffect;
    unsigned int mId;
    EMoleculeStyle mMoleculeStyle;
    int mAtomDetail;
    int mBondDetail;
    float mRingOffset;
    float mLineWidth;
    bool mSmoothLines;
    bool mShowAtomNames;
    bool mMoleculeToActorMapEnabled;
    bool mActorToMoleculeMapEnabled;
  };

  //! Loads a Tripos MOL2 file.
  //! The Molecule tags will contain the following key/value pairs:
  //! - \p "MultiMol2Index": the index (0-based) of the structure in a multi MOL2 file.
  //! - \p "FilePath": the full path of the file that contained the structure.
  VLMOLECULE_EXPORT bool loadMOL2(const String& path, std::vector< ref<Molecule> >& structures);

  //! Loads a Tripos MOL2 file.
  //! The Molecule tags will contain the following key/value pairs:
  //! - \p "MultiMol2Index": the index (0-based) of the structure in a multi MOL2 file.
  //! - \p "FilePath": the full path of the file that contained the structure.
  VLMOLECULE_EXPORT bool loadMOL2(VirtualFile* vfile, std::vector< ref<Molecule> >& structures);
}

#endif
