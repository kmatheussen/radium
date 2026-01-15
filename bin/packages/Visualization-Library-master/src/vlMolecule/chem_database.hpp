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

#ifndef ChemDatabase_INCLUDE_ONCE
#define ChemDatabase_INCLUDE_ONCE

#include <vlMolecule/link_config.hpp>
#include <vlCore/Vector4.hpp>
#include <vlCore/String.hpp>

namespace vl
{
  //! Element types.
  typedef enum
  {
    AT_Hydrogen,
    AT_Helium,
    AT_Lithium,
    AT_Beryllium,
    AT_Boron,
    AT_Carbon,
    AT_Nitrogen,
    AT_Oxygen,
    AT_Fluorine,
    AT_Neon,
    AT_Sodium,
    AT_Magnesium,
    AT_Aluminium,
    AT_Silicon,
    AT_Phosphorus,
    AT_Sulfur,
    AT_Chlorine,
    AT_Argon,
    AT_Potassium,
    AT_Calcium,
    AT_Scandium,
    AT_Titanium,
    AT_Vanadium,
    AT_Chromium,
    AT_Manganese,
    AT_Iron,
    AT_Cobalt,
    AT_Nickel,
    AT_Copper,
    AT_Zinc,
    AT_Gallium,
    AT_Germanium,
    AT_Arsenic,
    AT_Selenium,
    AT_Bromine,
    AT_Krypton,
    AT_Rubidium,
    AT_Strontium,
    AT_Yttrium,
    AT_Zirconium,
    AT_Niobium,
    AT_Molybdenum,
    AT_Technetium,
    AT_Ruthenium,
    AT_Rhodium,
    AT_Palladium,
    AT_Silver,
    AT_Cadmium,
    AT_Indium,
    AT_Tin,
    AT_Antimony,
    AT_Tellurium,
    AT_Iodine,
    AT_Xenon,
    AT_Caesium,
    AT_Barium,
    AT_Lanthanum,
    AT_Cerium,
    AT_Praseodymium,
    AT_Neodymium,
    AT_Promethium,
    AT_Samarium,
    AT_Europium,
    AT_Gadolinium,
    AT_Terbium,
    AT_Dysprosium,
    AT_Holmium,
    AT_Erbium,
    AT_Thulium,
    AT_Ytterbium,
    AT_Lutetium,
    AT_Hafnium,
    AT_Tantalum,
    AT_Tungsten,
    AT_Rhenium,
    AT_Osmium,
    AT_Iridium,
    AT_Platinum,
    AT_Gold,
    AT_Mercury,
    AT_Thallium,
    AT_Lead,
    AT_Bismuth,
    AT_Polonium,
    AT_Astatine,
    AT_Radon,
    AT_Francium,
    AT_Radium,
    AT_Actinium,
    AT_Thorium,
    AT_Protactinium,
    AT_Uranium,
    AT_Neptunium,
    AT_Plutonium,
    AT_Americium,
    AT_Curium,
    AT_Berkelium,
    AT_Californium,
    AT_Einsteinium,
    AT_Fermium,
    AT_Mendelevium,
    AT_Nobelium,
    AT_Lawrencium,
    AT_Rutherfordium,
    AT_Dubnium,
    AT_Seaborgium,
    AT_Bohrium,
    AT_Hassium,
    AT_Meitnerium,
    AT_Darmstadtium,
    AT_Roentgenium,
    AT_Ununbium,
    AT_Ununtrium,
    AT_Ununquadium,
    AT_Ununpentium,
    AT_Ununhexium,
    AT_Ununseptium,
    AT_Ununoctium,

    AT_Unknown,
    AT_Count,
  } EAtomType;

  //! Encapsulates information regarding an atom type.
  class AtomInfo
  {
  public:
    //! Constructor.
    AtomInfo(EAtomType type, const char* name, const char* symbol, int atomic_num, double atomic_mass, double melting_pt, double boiling_pt, 
             double electroneg, double electron_aff, int valence, double calculated_r, double empirical_r, double covalent_r, double vdw_r,
             unsigned int cpk_color, unsigned int rasmol_color)
    {
      mType              = type;
      mName              = name;
      mSymbol            = symbol;
      mAtomicNumber      = atomic_num;
      mAtomicMass        = atomic_mass;
      mMeltingPoint      = melting_pt;
      mBoilingPoint      = boiling_pt;
      mElectronegativity = electroneg;
      mElectronAffinity  = electron_aff;
      mValence           = valence;
      mCalculatedRadius  = calculated_r;
      mEmpiricalRadius   = empirical_r;
      mCovalentRadius    = covalent_r;
      mVanDerWaalsRadius = vdw_r;
      mCPKColor = cpk_color;
      mRasMolColor = rasmol_color;
    }

    //! Returns the type of an atom
    EAtomType type() const { return mType; }
    //! Returns the name of the atom.
    const char* name() const { return mName; }
    //! Returns the symbol of the atom
    const char* symbol() const { return mSymbol; }
    //! Returns the atom's atomic number
    int atomicNumber() const { return mAtomicNumber; }
    //! Returns the atom mass
    double atomicMass() const { return mAtomicMass; }
    //! Returns the element's melting point in Kelvin
    double meltingPoint() const { return mMeltingPoint; }
    //! Returns the element's boiling point in Kelvin
    double boilingPoint() const { return mBoilingPoint; }
    //! Returns the atom electronegativity
    double electronegativity() const { return mElectronegativity; }
    //! Returns the electron affinity in Kj/mol
    double electronAffinity() const { return mElectronAffinity; }
    //! Returns the valence of the atom
    int valence() const { return mValence; }
    //! Returns the atom's calculated radius in Angstroms
    double calculatedRadius() const { return mCalculatedRadius / 100.0; }
    //! Returns the atom's empirical radius in Angstroms
    double empiricalRadius() const { return mEmpiricalRadius / 100.0; }
    //! Returns the atom's covalent radius in Angstroms
    double covalentRadius() const { return mCovalentRadius / 100.0; }
    //! Returns the atom's van der Waals radius in Angstroms
    double vanDerWaalsRadius() const { return mVanDerWaalsRadius / 100.0; }
    //! Returns the atom's CPK color
    fvec4 cpkColor() const 
    {
      fvec4 c;
      c.r() = ((mCPKColor >> 16) & 0xFF) / 255.0f;
      c.g() = ((mCPKColor >> 8)  & 0xFF) / 255.0f;
      c.b() =  (mCPKColor        & 0xFF) / 255.0f;
      c.a() = 1.0f;
      return c; 
    }
    //! Returns the atom's RasMol color
    fvec4 rasmolColor() const 
    {
      fvec4 c;
      c.r() = ((mRasMolColor >> 16) & 0xFF) / 255.0f;
      c.g() = ((mRasMolColor >> 8)  & 0xFF) / 255.0f;
      c.b() =  (mRasMolColor        & 0xFF) / 255.0f;
      c.a() = 1.0f;
      return c; 
    }

  protected:
    EAtomType mType;
    const char* mName;
    const char* mSymbol;
    int mAtomicNumber;
    int mValence;
    unsigned int mCPKColor;
    unsigned int mRasMolColor;
    double mAtomicMass;
    double mMeltingPoint;
    double mBoilingPoint;
    double mElectronegativity;
    double mElectronAffinity;
    double mCalculatedRadius;
    double mEmpiricalRadius;
    double mCovalentRadius;
    double mVanDerWaalsRadius;
  };

  //! Returns an AtomInfo representing the properties of the given atom type.
  VLMOLECULE_EXPORT const AtomInfo& atomInfo(EAtomType type);

  //! Translates a string containing atom type name, atom symbol or a Sybyl type into an EAtomType.
  VLMOLECULE_EXPORT EAtomType atomType(const char* type);
}

#endif
