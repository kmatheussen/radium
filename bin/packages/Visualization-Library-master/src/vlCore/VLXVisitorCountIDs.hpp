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

#ifndef VLXVisitorCountIDs_INCLUDE_ONCE
#define VLXVisitorCountIDs_INCLUDE_ONCE

#include <vlCore/VLXVisitor.hpp>
#include <vlCore/VLXValue.hpp>
#include <vlCore/Log.hpp>
#include <vlCore/Say.hpp>

namespace vl
{
  /** Counts the number of occurrencies of each ID. If an ID is occurring more than 1 it means that belongs to a VLXStructure which is referenced by somebody. */
  class VLXVisitorCountIDs: public VLXVisitor
  {
    VL_INSTRUMENT_CLASS(vl::VLXVisitorCountIDs, VLXVisitor)

  public:
    VLXVisitorCountIDs(): mIDSet(NULL) {}

    virtual void visitStructure(VLXStructure* obj)
    {
      if(!obj->uid().empty() && obj->uid() != "#NULL")
        (*mIDSet)[obj->uid()]++;

      if (isVisited(obj))
        return;

      for(size_t i=0; i<obj->value().size(); ++i)
      {
        VLXStructure::Value& keyval = obj->value()[i];
        if (keyval.value().type() == VLXValue::Structure)
          keyval.value().getStructure()->acceptVisitor(this);
        else
        if (keyval.value().type() == VLXValue::List)
          keyval.value().getList()->acceptVisitor(this);
        else
        /*
        if (keyval.value().type() == VLXValue::ArrayID)
          keyval.value().getArrayID()->acceptVisitor(this);
        else
        */
        if (keyval.value().type() == VLXValue::ID)
          (*mIDSet)[keyval.value().getID()]++;
      }
    }

    virtual void visitList(VLXList* list)
    {
      // this should happen only if the user manually creates loops
      if (isVisited(list))
      {
        Log::warning("VLXVisitorCountIDs: cycle detected on VLXList.\n");
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
          (*mIDSet)[list->value()[i].getID()]++;
      }
    }

    /*
    virtual void visitArray(VLXArrayString*)  {}

    virtual void visitArray(VLXArrayID* arr)
    {
      // retrieves the assigned Structure
      for(size_t i=0 ;i<arr->value().size(); ++i)
        (*mIDSet)[arr->value()[i].uid()]++;
    }

    virtual void visitArray(VLXArrayIdentifier*) {}
    */

    virtual void visitArray(VLXArrayInteger*)  {}

    virtual void visitArray(VLXArrayReal*)  {}

    void setIDSet(std::map< std::string, int >* uids) { mIDSet = uids; }

    std::map< std::string, int >* uidSet() { return mIDSet; }

    const std::map< std::string, int >* uidSet() const { return mIDSet; }

  private:
    std::map< std::string, int >* mIDSet;
  };
}

#endif
