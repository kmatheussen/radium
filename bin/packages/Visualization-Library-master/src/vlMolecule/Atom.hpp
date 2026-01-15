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

#ifndef Atom_INCLUDE_ONCE
#define Atom_INCLUDE_ONCE

#include <vlCore/Object.hpp>
#include <vlCore/Vector4.hpp>
#include <vlMolecule/chem_database.hpp>
#include <vector>

namespace vl
{
  
  /** The Atom class represents an atom to be used with the Molecule class.
   * \sa
   * - \ref pagGuideMolecule "Molecule Visualization Tutorial"
   * - Molecule
   * - Bond
   *
   * <img src="pics/pagGuideMolecule.png">
   */
  class Atom: public Object
  {
    VL_INSTRUMENT_CLASS(vl::Atom, Object)

  public:
    Atom()
    {
      VL_DEBUG_SET_OBJECT_NAME()
      mId       = 0;
      mAtomType = AT_Unknown;
      mCoordinates = fvec3(0,0,0);
      mColor    = fvec4(1.0f,1.0f,1.0f,1.0f);
      mRadius   = 0.25f;
      mVisited  = false;
      mVisible  = true;
      mShowAtomName= false;
      /*mAtomName = nothing*/
    }

    Atom(const Atom& other): Object(other) { *this = other; }

    //! Assignment operator: does not assign the adjacent atoms
    Atom& operator=(const Atom& other)
    {
      mId       = other.mId;
      mAtomType = other.mAtomType;
      mCoordinates = other.mCoordinates;
      mRadius   = other.mRadius;
      mColor    = other.mColor;
      mVisible  = other.mVisible;
      mShowAtomName= other.mShowAtomName;
      // mAdjacentAtoms = other.mAdjacentAtoms; // do not copy
      // mVisited = other.mVisited;             // do not copy
      mAtomName = other.mAtomName;
      return *this;
    }

    // Returns true if \p atom is adjacent to an Atom
    bool isAtomAdjacent(Atom* atom) const
    {
      for(unsigned i=0; i<adjacentAtoms().size(); ++i)
        if (adjacentAtoms()[i] == atom)
          return true;
      return false;
    }

    const std::vector< Atom* >& adjacentAtoms() const { return mAdjacentAtoms; }
    std::vector< Atom* >& adjacentAtoms() { return mAdjacentAtoms; }

    EAtomType atomType() const { return mAtomType; }
    void setAtomType(EAtomType type) { mAtomType = type; }

    unsigned int id() const { return mId; }
    void setId(unsigned int id) { mId = id; }

    const fvec3& coordinates() const { return mCoordinates; }
    void setCoordinates(const fvec3& coordinates) { mCoordinates = coordinates; }

    float radius() const { return mRadius; }
    void setRadius(float radius) { mRadius = radius; }

    void setVisited(bool visited) { mVisited = visited; }
    bool visited() const { return mVisited; }

    void setAtomName(const std::string& name) { mAtomName = name; }
    const std::string& atomName() const { return mAtomName; }

    const fvec4& color() const { return mColor; }
    void setColor(const fvec4& color) { mColor = color; }

    bool visible() const { return mVisible; }
    void setVisible(bool visible) { mVisible = visible; }

    //! Defines whether the atom name should be rendered or not. See also Molecule::setShowAtomNames().
    void setShowAtomName(bool show) { mShowAtomName = show; }
    //! Defines whether the atom name should be rendered or not. See also Molecule::setShowAtomNames().
    bool showAtomName() const { return mShowAtomName; }

  protected:
    fvec4 mColor;
    fvec3 mCoordinates;
    EAtomType mAtomType;
    float mRadius;
    std::vector< Atom* > mAdjacentAtoms;
    std::string mAtomName;
    unsigned int mId;
    // Aid to visit a molecule.
    bool mVisited;
    // Whether is visible or not
    bool mVisible;
    // Display atom name
    bool mShowAtomName;
  };
}

#endif
