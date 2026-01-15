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

#ifndef Bond_INCLUDE_ONCE
#define Bond_INCLUDE_ONCE

#include <vlCore/Object.hpp>

namespace vl
{
  class Atom;

  //! Bond types.
  typedef enum
  {
    BT_None,
    BT_Single,
    BT_Double,
    BT_Triple,
    BT_Aromatic,
    BT_Amide,
    BT_Dummy,
    BT_Unknown
  } EBondType;

  /** The Bond class represents a bond to be used with the Molecule class.
   * \sa
   * - \ref pagGuideMolecule "Molecule Visualization Tutorial"
   * - Molecule
   * - Atom
   *
   * <img src="pics/pagGuideMolecule.png">
   */
  class Bond: public Object
  {
    VL_INSTRUMENT_CLASS(vl::Bond, Object)

  public:
    Bond(): mColor( 1.0f,1.0f,1.0f,1.0f ), mRadius(0.10f), mAtom1(NULL), mAtom2(NULL), mType(BT_Single), mId(0), mVisible(true), mUseAtomColors(true) 
    {
      VL_DEBUG_SET_OBJECT_NAME()
    }
    Bond(const Bond& other): Object(other) { *this = other; }

    unsigned int id() const { return mId; }
    void setId(unsigned int id) { mId = id; }

    void setBondType(EBondType type) { mType = type; }
    EBondType bondType() const { return mType; }

    void setAtom1( Atom* atom ) { mAtom1 = atom; }
    Atom* atom1() const { return mAtom1; }

    void setAtom2( Atom* atom ) { mAtom2 = atom; }
    Atom* atom2() const { return mAtom2; }

    void setVisible(bool visible) { mVisible = visible; }
    bool visible() const { return mVisible; }

    void setColor(const fvec4& color) { mColor = color; }
    const fvec4& color() const { return mColor; }

    void setUseAtomColors(bool use_atom_color) { mUseAtomColors = use_atom_color; }
    bool useAtomColors() const { return mUseAtomColors; }

    float radius() const { return mRadius; }
    void setRadius(float radius) { mRadius = radius; }

  protected:
    fvec4 mColor;
    float mRadius;
    Atom* mAtom1;
    Atom* mAtom2;
    EBondType mType;
    unsigned int mId;
    bool mVisible;
    bool mUseAtomColors;
  };
}

#endif
