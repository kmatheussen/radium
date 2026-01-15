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

#ifndef RingExtractor_INCLUDE_ONCE
#define RingExtractor_INCLUDE_ONCE

#include <vlCore/glsl_math.hpp>
#include <vector>
#include <algorithm>
#include <map>
#include <set>

namespace vl
{
  //! The RingExtractor class traverses a molecule's graph and detects various types of cycles, mainly used for aromatic ring detection.
  class RingExtractor
  {
  public:
    RingExtractor(Molecule* mol): mMolecule(mol) {}

    void setMolecule(Molecule* mol) { mMolecule = mol; }

    Molecule* molecule() const { return mMolecule; }

    void run()
    {
      if (!molecule()->atoms().empty())
      {
        bootstrap();
        removeDoubles();
        sortCycles();
        keepMinimalCycles();
        keepAromaticCycles();
        /*keepPlanarCycles(0.10f);*/
      }
    }

    void bootstrap()
    {
      if (!molecule()->atoms().empty())
      {
        molecule()->computeAtomAdjacency();
        for(int i=0; i<molecule()->atomCount(); ++i)
          molecule()->atom(i)->setVisited(false);
        std::vector< ref<Atom> > current_path;
        depthFirstVisit( molecule()->atoms()[0].get(), current_path );
      }
    }

    void depthFirstVisit(Atom* atom, std::vector< ref<Atom> >& current_path)
    {
      if ( !atom->visited() || current_path.empty())
      {
        atom->setVisited(true);
        current_path.push_back(atom);
        for(unsigned i=0; i<atom->adjacentAtoms().size(); ++i)
          depthFirstVisit( atom->adjacentAtoms()[i], current_path );
        current_path.pop_back();
        atom->setVisited(false);
      }
      else // cycle found
      {
        /* condition: atom->visited() && !current_path.empty() */

        for(size_t i = current_path.size()-1; i--; )
        {
          if ( current_path[i] == atom )
          {
            std::vector< ref<Atom> > cycle;
            for(; i<current_path.size(); ++i)
              cycle.push_back( current_path[i] );
            if (cycle.size() > 2)
              molecule()->cycles().push_back(cycle);
            break;
          }
        }
      }
    }

    void keepAromaticCycles()
    {
      std::vector< std::vector< ref<Atom> > > kept_cycles;
      for(unsigned icycle=0; icycle<molecule()->cycles().size(); ++icycle)
      {
        int ok = true;
        for(unsigned iatom=0; iatom<molecule()->cycle(icycle).size(); ++iatom)
        {
          int iatom2 = (iatom+1) % molecule()->cycle(icycle).size();
          Atom* atom1 = molecule()->cycle(icycle)[iatom].get();
          Atom* atom2 = molecule()->cycle(icycle)[iatom2].get();

          Bond* bond = molecule()->bond(atom1, atom2);
          if (bond && bond->bondType() != BT_Aromatic)
          {
            ok = false;
            break;
          }
        }
        if (ok && molecule()->cycle(icycle).size())
          kept_cycles.push_back(molecule()->cycle(icycle));
      }
      molecule()->cycles() = kept_cycles;
    }

    void sortCycles()
    {
      for(unsigned icycle=0; icycle<molecule()->cycles().size(); ++icycle)
      {
        std::vector< ref<Atom> >& cycle = molecule()->cycle(icycle);
        for(unsigned iatom=0; iatom<cycle.size()-1; ++iatom)
        {
          Atom* atom = cycle[iatom].get();
          for(unsigned j=iatom+1; j<cycle.size(); ++j)
          {
            if (atom->isAtomAdjacent(cycle[j].get()))
            {
              Atom* tmp = cycle[iatom+1].get();
              cycle[iatom+1] = cycle[j];
              cycle[j] = tmp;
              break;
            }
          }
        }
      }
    }

    void keepPlanarCycles(float epsilon)
    {
      std::vector< std::vector< ref<Atom> > > kept_cycles;
      for(unsigned icycle=0; icycle<molecule()->cycles().size(); ++icycle)
      {
        AABB aabb;
        for(unsigned iatom=0; iatom<molecule()->cycle(icycle).size(); ++iatom)
          aabb += (vec3)molecule()->cycle(icycle)[iatom]->coordinates();
        fvec3 center = (fvec3)aabb.center();

        fvec3 normal;
        for(unsigned iatom=0; iatom<molecule()->cycle(icycle).size(); ++iatom)
        {
          int iatom2 = (iatom+1) % molecule()->cycle(icycle).size();
          Atom* atom1 = molecule()->cycle(icycle)[iatom].get();
          Atom* atom2 = molecule()->cycle(icycle)[iatom2].get();
          fvec3 v1 = (atom1->coordinates()-center).normalize();
          fvec3 v2 = (atom2->coordinates()-center).normalize();
          normal += cross(v1, v2);
        }
        normal.normalize();

        int ok = true;
        for(unsigned iatom=0; iatom<molecule()->cycle(icycle).size(); ++iatom)
        {
          fvec3 v1   = molecule()->cycle(icycle)[iatom]->coordinates() - center;
          float dist = dot(normal, v1);
          if (fabs(dist)>epsilon)
          {
            ok = false;
            break;
          }
        }
        if (ok && molecule()->cycle(icycle).size())
          kept_cycles.push_back(molecule()->cycle(icycle));
      }
      molecule()->cycles() = kept_cycles;
    }

    void removeDoubles()
    {
      for(unsigned icycle=0; icycle<molecule()->cycles().size(); ++icycle)
        std::stable_sort(molecule()->cycle(icycle).begin(), molecule()->cycle(icycle).end());
      std::stable_sort(molecule()->cycles().begin(), molecule()->cycles().end());
      std::vector< std::vector< ref<Atom> > >::iterator new_end = std::unique(molecule()->cycles().begin(), molecule()->cycles().end());
      std::vector< std::vector< ref<Atom> > > unique_cycles;
      for(std::vector< std::vector< ref<Atom> > >::iterator it = molecule()->cycles().begin(); it != new_end; ++it)
        unique_cycles.push_back(*it);
      molecule()->cycles() = unique_cycles;
    }

    void keepMinimalCycles()
    {
      std::vector< std::vector< ref<Atom> > > sub_cycles;

      std::map<Atom*, bool> my_atom;
      for(unsigned j=0; j<molecule()->atoms().size(); ++j)
        my_atom[molecule()->atoms()[j].get()] = false;

      for(unsigned icycle=0; icycle<molecule()->cycles().size(); ++icycle)
      {
        // init
        for(unsigned j=0; j<molecule()->cycles()[icycle].size(); ++j)
          my_atom[ molecule()->cycles()[icycle][j].get() ] = true;

        bool is_sup_cycle = false;
        for(unsigned j=0; j<molecule()->cycles().size(); ++j)
        {
          if (j == icycle)
            continue;
          unsigned shared_atoms = 0;
          for(unsigned k=0; k<molecule()->cycles()[j].size(); k++)
            shared_atoms += my_atom[ molecule()->cycles()[j][k].get() ] ? 1 : 0;
          if ( shared_atoms == molecule()->cycles()[j].size() )
          {
            is_sup_cycle = true;
            break;
          }
        }
        if (!is_sup_cycle)
          sub_cycles.push_back( molecule()->cycles()[icycle] );

        // reset
        for(unsigned j=0; j<molecule()->cycles()[icycle].size(); ++j)
          my_atom[ molecule()->cycles()[icycle][j].get() ] = false;
      }
      molecule()->cycles() = sub_cycles;
    }

  protected:
    Molecule* mMolecule;
  };
}

#endif
