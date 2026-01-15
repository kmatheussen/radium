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

#ifndef VLXVisitorLinker_INCLUDE_ONCE
#define VLXVisitorLinker_INCLUDE_ONCE

#include <vlCore/VLXVisitor.hpp>
#include <vlCore/VLXValue.hpp>

namespace vl
{
  /** Substitutes IDs into VLXStructures using the provided link map.*/
  class VLXVisitorLinker: public VLXVisitor
  {
    VL_INSTRUMENT_CLASS(vl::VLXVisitorLinker, VLXVisitor)

  public:
    typedef enum 
    {
      NoError,
      UnresolvedID
    } EError;

  public:
    VLXVisitorLinker(const std::map< std::string, ref<VLXStructure> >* map=NULL)
    {
      mLinkMap = map;
      mError = NoError;
    }

    void setLinkMap(const std::map< std::string, ref<VLXStructure> >* map)
    {
      mLinkMap = map;
    }

    VLXStructure* link(const std::string& uid)
    {
      VL_CHECK(mLinkMap)
      VL_CHECK(!uid.empty())
      std::map< std::string, ref<VLXStructure> >::const_iterator it = mLinkMap->find(uid);
      if( it != mLinkMap->end() )
      {
        // this should never happen
        VL_CHECK(uid != "#NULL")

        /* Log::debug( Say("- ID '%s' linked to '%s'.\n") << uid << it->second->tag() ); */
        return it->second.get_writable();
      }
      else
      {
        if (uid != "#NULL")
        {
          mError = UnresolvedID;
          Log::error( Say("Could not link ID '%s' to anything!\n") << uid );
        }
        return NULL;
      }
    }

    virtual void visitStructure(VLXStructure* obj)
    {
      if (isVisited(obj))
        return;

      for(size_t i=0; i<obj->value().size(); ++i)
      {
        if (obj->value()[i].value().type() == VLXValue::Structure)
          obj->value()[i].value().getStructure()->acceptVisitor(this);
        else
        if (obj->value()[i].value().type() == VLXValue::List)
          obj->value()[i].value().getList()->acceptVisitor(this);
        else
        /*
        if (obj->value()[i].value().type() == VLXValue::ArrayID)
          obj->value()[i].value().getArrayID()->acceptVisitor(this);
        else
        */
        if (obj->value()[i].value().type() == VLXValue::ID)
        {
          // transform ID -> Structure
          VLXStructure* lnk_obj = link( obj->value()[i].value().getID() );
          obj->value()[i].value().setStructure( lnk_obj );
        }
      }
    }

    virtual void visitList(VLXList* list)
    {
      // this should happen only if the user manually creates loops
      if (isVisited(list))
      {
        Log::warning("VLXVisitorLinker: cycle detected on VLXList.\n");
        return;
      }

      for(size_t i=0; i<list->value().size(); ++i)
      {
        if (list->value()[i].type() == VLXValue::Structure)
          list->value()[i].getStructure()->acceptVisitor(this);
        if (list->value()[i].type() == VLXValue::List)
          list->value()[i].getList()->acceptVisitor(this);
        else
        /*
        if (list->value()[i].type() == VLXValue::ArrayID)
          list->value()[i].getArrayID()->acceptVisitor(this);
        else
        */
        if (list->value()[i].type() == VLXValue::ID)
        {
          // transform ID -> Structure
          VLXStructure* lnk_obj = link( list->value()[i].getID() );
          list->value()[i].setStructure( lnk_obj );
        }
      }
    }

    /*
    virtual void visitArray(VLXArrayString*)  {}

    virtual void visitArray(VLXArrayID* arr)
    {
      // retrieves the assigned Structure
      for(size_t i=0 ;i<arr->value().size(); ++i)
        arr->value()[i].setStructure ( link(arr->value()[i].uid()) );
    }

    virtual void visitArray(VLXArrayIdentifier*) {}
    */

    virtual void visitArray(VLXArrayInteger*)  {}

    virtual void visitArray(VLXArrayReal*)  {}

    EError error() const { return mError; }

    void setError(EError err) { mError = err; }

  private:
    const std::map< std::string, ref<VLXStructure> >* mLinkMap;
    EError mError;
  };
}

#endif
