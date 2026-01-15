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

#include <vlCore/VLXValue.hpp>

using namespace vl;

//-----------------------------------------------------------------------------
void VLXValue::release()
{
  switch(mType)
  {
  case Structure: 
    if (mUnion.mStructure)
      mUnion.mStructure->decReference(); 
    break;

  case List:
    if (mUnion.mList)
      mUnion.mList->decReference(); 
    break;

  case RawtextBlock:
    if (mUnion.mRawtextBlock)
      mUnion.mRawtextBlock->decReference(); 
    break;

  /*
  case ArrayString:
  case ArrayID:
  case ArrayIdentifier:
  */
  case ArrayInteger:
  case ArrayReal:
    if (mUnion.mArray)
      mUnion.mArray->decReference(); 
    break;

  case String:
  case ID:
  case Identifier:
    VL_CHECK(mUnion.mString)
    delete mUnion.mString; 
    mUnion.mString = NULL; 
    break;

  default:
    break;
  }

  mType = Integer;
  mUnion.mInteger = 0;
}
//-----------------------------------------------------------------------------
VLXValue& VLXValue::operator=(const VLXValue& other)
{
  mLineNumber = other.mLineNumber;

  // must be done first
  switch(other.mType)
  {
  case Structure:
    if (other.mUnion.mStructure)
      other.mUnion.mStructure->incReference(); 
    break;

  case List:
    if (other.mUnion.mList)
      other.mUnion.mList->incReference(); 
    break;

  case RawtextBlock:
    if (other.mUnion.mRawtextBlock)
      other.mUnion.mRawtextBlock->incReference(); 
    break;

  /*
  case ArrayString:
  case ArrayID:
  case ArrayIdentifier:
  */
  case ArrayInteger:
  case ArrayReal:
    if (other.mUnion.mArray)
      other.mUnion.mArray->incReference(); 
    break;

  default:
    break;
  }

  // must be done after
  release();

  mUnion = other.mUnion;
  mType = other.mType;

  // make local copy of the string
  if (mType == String || mType == Identifier || mType == ID)
    mUnion.mString = new std::string(*mUnion.mString);

  return *this;
}
//-----------------------------------------------------------------------------
VLXStructure* VLXValue::setStructure(VLXStructure* obj)
{
  release();
  mType = Structure;
  mUnion.mStructure = obj;
  if (mUnion.mStructure)
    mUnion.mStructure->incReference();
  return obj;
}
//-----------------------------------------------------------------------------
VLXList* VLXValue::setList(VLXList* list)
{
  VL_CHECK(list);

  release();
  mType = List;
  mUnion.mList = list;
  if (mUnion.mList)
    mUnion.mList->incReference();
  return list;
}
//-----------------------------------------------------------------------------
VLXRawtextBlock* VLXValue::setRawtextBlock(VLXRawtextBlock* fblock)
{
  VL_CHECK(fblock);

  release();
  mType = RawtextBlock;
  mUnion.mRawtextBlock = fblock;
  if (mUnion.mRawtextBlock)
    mUnion.mRawtextBlock->incReference();
  return fblock;
}
//-----------------------------------------------------------------------------
VLXArrayInteger* VLXValue::setArrayInteger(VLXArrayInteger* arr)
{
  VL_CHECK(arr);
  release();
  mType = ArrayInteger;
  mUnion.mArray = arr;
  if (mUnion.mArray)
    mUnion.mArray->incReference();
  return arr;
}
//-----------------------------------------------------------------------------
VLXArrayReal* VLXValue::setArrayReal(VLXArrayReal* arr)
{
  VL_CHECK(arr);
  release();
  mType = ArrayReal;
  mUnion.mArray = arr;
  if (mUnion.mArray)
    mUnion.mArray->incReference();
  return arr;
}
//-----------------------------------------------------------------------------
/*
VLXArrayString* VLXValue::setArrayString(VLXArrayString* arr)
{
  VL_CHECK(arr);
  release();
  mType = ArrayString;
  mUnion.mArray = arr;
  if (mUnion.mArray)
    mUnion.mArray->incReference();
  return arr;
}
//-----------------------------------------------------------------------------
VLXArrayIdentifier* VLXValue::setArrayIdentifier(VLXArrayIdentifier* arr)
{
  VL_CHECK(arr);
  release();
  mType = ArrayIdentifier;
  mUnion.mArray = arr;
  if (mUnion.mArray)
    mUnion.mArray->incReference();
  return arr;
}
//-----------------------------------------------------------------------------
VLXArrayID* VLXValue::setArrayID(VLXArrayID* arr)
{
  VL_CHECK(arr);
  release();
  mType = ArrayID;
  mUnion.mArray = arr;
  if (mUnion.mArray)
    mUnion.mArray->incReference();
  return arr;
}
*/
//-----------------------------------------------------------------------------
VLXArray* VLXValue::setArray(VLXArray* arr)
{
  if (arr->classType() == VLXArrayInteger::Type())
    return setArrayInteger(arr->as<VLXArrayInteger>());
  else
  if (arr->classType() == VLXArrayReal::Type())
    return setArrayReal(arr->as<VLXArrayReal>());
  /*
  else
  if (arr->classType() == VLXArrayString::Type())
    return setArrayString(arr->as<VLXArrayString>());
  else
  if (arr->classType() == VLXArrayIdentifier::Type())
    return setArrayIdentifier(arr->as<VLXArrayIdentifier>());
  else
  if (arr->classType() == VLXArrayID::Type())
    return setArrayID(arr->as<VLXArrayID>());
  */
  else
  {
    VL_TRAP();
    return NULL;
  }
}
//-----------------------------------------------------------------------------
