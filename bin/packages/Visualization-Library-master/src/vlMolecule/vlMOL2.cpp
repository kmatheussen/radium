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

#include <vlCore/DiskFile.hpp>
#include <vlCore/MemoryFile.hpp>
#include <vlMolecule/Molecule.hpp>
#include <vlMolecule/RingExtractor.hpp>
#include <vlCore/TextStream.hpp>
#include <vlGraphics/Effect.hpp>
#include <vlGraphics/Scissor.hpp>
#include <vlGraphics/Texture.hpp>
#include <vlCore/Image.hpp>
#include <stdio.h>

using namespace vl;

//-----------------------------------------------------------------------------
namespace
{
  ref<Molecule> parseStructure(TextStream& text_stream)
  {
    #define NAME_CHAR_COUNT 128

    ref<Molecule> structure = new Molecule;
    std::string line;

    // find structure start
    bool mol2_start = false;
    while(text_stream.readLine(line))
    {
      if ( strstr(line.c_str(), "@<TRIPOS>MOLECULE") )
      {
        mol2_start = true;
		    break;
      }
    }
    if (!mol2_start)
      return NULL;

    // parse structure name
    text_stream.readLine(line);
    structure->setMoleculeName( String::trimStdString(line).c_str() );

    // skip lines until atom coordinates start
    while(text_stream.readLine(line) && !strstr(line.c_str(), "@<TRIPOS>ATOM")) { /*skip lines*/ }

    // read atoms
    char atom_name[NAME_CHAR_COUNT];
    char atom_type[NAME_CHAR_COUNT];
    while(text_stream.readLine(line) && !strstr(line.c_str(), "@<TRIPOS>BOND"))
    {
      fvec3 pos;
      int id = 0;
      int tokens = sscanf(line.c_str(), "%d %s %f %f %f %s", &id, atom_name, &pos.x(), &pos.y(), &pos.z(), atom_type);
      // make sure they are zero-terminated.
      atom_name[NAME_CHAR_COUNT-1] = 0;
      atom_type[NAME_CHAR_COUNT-1] = 0;
      if (tokens != 6)
        continue;

      ref<Atom> atom = new Atom;
      atom->setCoordinates( pos );
      atom->setAtomName( atom_name );

      // detect atom type
      char* ch = strstr(atom_type, ".");
      if ( ch != NULL )
        *ch = 0;
      EAtomType atype = atomType(atom_type);
      if (atype==AT_Unknown)
        atype=atomType(atom_name);
      atom->setAtomType( atype );
		  structure->addAtom( atom.get() );
	  }

	  // read bonds
	  char bond_type[NAME_CHAR_COUNT];
	  unsigned bond_atom1;
    unsigned bond_atom2;
    while(text_stream.readLine(line))
	  {
      if ( strstr(line.c_str(), "@<TRIPOS>") )
      {
        text_stream.ungetLine(line);
		    break;
      }
		  int id = 0;
      int tokens = sscanf(line.c_str(), "%d %d %d %s", &id, &bond_atom1, &bond_atom2, bond_type);
      // make sure it is zero-terminated.
      bond_type[NAME_CHAR_COUNT-1] = 0;
      if (tokens != 4)
        continue;
      --bond_atom1;
      --bond_atom2;
      if ( !(bond_atom1 < structure->atoms().size() && bond_atom2 < structure->atoms().size()) )
      {
        Log::error( Say("Bond indices out of range: a1 = %n, a2 = %n, atom count = %n.\n") << bond_atom1 << bond_atom2 << structure->atoms().size() );
        continue;
      }
      VL_CHECK( bond_atom1 < structure->atoms().size() )
      VL_CHECK( bond_atom2 < structure->atoms().size() )
		  ref<Bond> bond = new Bond;
		  bond->setAtom1( structure->atom(bond_atom1) );
		  bond->setAtom2( structure->atom(bond_atom2) );
		  bond->setBondType( BT_None );
		  if (bond_type[0] == '1')     bond->setBondType( BT_Single );
		  if (bond_type[0] == '2')     bond->setBondType( BT_Double );
		  if (bond_type[0] == '3')     bond->setBondType( BT_Triple );
		  if (strstr(bond_type, "ar")) bond->setBondType( BT_Aromatic );
		  if (strstr(bond_type, "am")) bond->setBondType( BT_Amide );
		  if (strstr(bond_type, "du")) bond->setBondType( BT_Dummy );
		  if (strstr(bond_type, "un")) bond->setBondType( BT_Unknown );
      if (bond->bondType() == BT_Aromatic)
      {
        bond->setUseAtomColors(false);
        bond->setColor(fvec4(0,1.0f,0,1.0f));
      }
      else
        bond->setUseAtomColors(true);

	    structure->addBond( bond.get() );
	  }

    // by default all the atom radii are set to be covalent
    structure->setCovalentAtomRadii();

    // by default set cpk colors
    structure->setCPKAtomColors();

    // compute adjacent atoms
    structure->computeAtomAdjacency();

    // compute aromatic rings
    RingExtractor ring_extractor(structure.get());
    ring_extractor.run();
    return structure;
  }
}
//-----------------------------------------------------------------------------
bool vl::loadMOL2(const String& path, std::vector< ref<Molecule> >& structures)
{
  ref<VirtualFile>   dfile = locateFile(path);
  ref<MemoryFile> mfile = new MemoryFile; // cache in the memory
  mfile->copy(dfile.get());
  if (!mfile->open(OM_ReadOnly))
	{
    Log::error( Say("Error opening file %s.\n") << path );
	  return false;
	}
  return loadMOL2(mfile.get(), structures);
}
//-----------------------------------------------------------------------------
bool vl::loadMOL2(VirtualFile* vfile, std::vector< ref<Molecule> >& structures)
{
  structures.clear();

  TextStream text_stream;
  text_stream.setInputFile(vfile);
  std::string line;

  // detect tripos mol2 file
  bool is_mol2 = false;
	while(text_stream.readLine(line))
  {
    const char* pos = strstr(line.c_str(), "@<TRIPOS>MOLECULE");
    if ( pos && line == pos )
    { 
      is_mol2 = true; 
      break; 
    }
  }
  if ( !is_mol2 )
  {
    Log::error( Say("Invalid structure file: %s\n") << vfile->path() );
    vfile->close();
    return false;
  }
  text_stream.seek(0);

  // extract multi-mol2 structures
  ref<Molecule> structure;
  while( (structure = parseStructure(text_stream) ) )
  {
    if (structure->atoms().size())
    {
      structures.push_back(structure);
      structure->tags()->set("MultiMol2Index") = Say("%n") << structures.size() - 1;
      structure->tags()->set("FilePath") = vfile->path();
    }
  }

	vfile->close();
	return true;
}
//-----------------------------------------------------------------------------
