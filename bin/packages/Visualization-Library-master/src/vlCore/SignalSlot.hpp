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

#ifndef SignalSlot_INCLUDE_ONCE
#define SignalSlot_INCLUDE_ONCE

#include <vector>
#include <algorithm>
#include <vlCore/checks.hpp>

//! \cond EXCLUDE_SECTION

namespace vl
{

// Signal1, Slot1
#define VL_T_PAR_TYPENAME2   typename U_par1
#define VL_T_PAR_TYPENAME    typename T_par1
#define VL_T_PAR_FORMAL_LIST T_par1
#define VL_T_PAR_LIST        T_par1 par1
#define VL_T_PAR_CALL        par1
#define VL_T_SIGNAL_NAME     Signal1
#define VL_T_BASE_SLOT_NAME  BaseSlot1
#define VL_T_SLOT_NAME       Slot1
#include "SignalSlotX.hpp"

// Signal2, Slot2
#define VL_T_PAR_TYPENAME2   typename U_par1, typename U_par2
#define VL_T_PAR_TYPENAME    typename T_par1, typename T_par2
#define VL_T_PAR_FORMAL_LIST T_par1, T_par2
#define VL_T_PAR_LIST        T_par1 par1, T_par2 par2
#define VL_T_PAR_CALL        par1, par2
#define VL_T_SIGNAL_NAME     Signal2
#define VL_T_BASE_SLOT_NAME  BaseSlot2
#define VL_T_SLOT_NAME       Slot2
#include "SignalSlotX.hpp"

// Signal3, Slot3
#define VL_T_PAR_TYPENAME2   typename U_par1, typename U_par2, typename U_par3
#define VL_T_PAR_TYPENAME    typename T_par1, typename T_par2, typename T_par3
#define VL_T_PAR_FORMAL_LIST T_par1, T_par2, T_par3
#define VL_T_PAR_LIST        T_par1 par1, T_par2 par2, T_par2 par3
#define VL_T_PAR_CALL        par1, par2, par3
#define VL_T_SIGNAL_NAME     Signal3
#define VL_T_BASE_SLOT_NAME  BaseSlot3
#define VL_T_SLOT_NAME       Slot3
#include "SignalSlotX.hpp"

// Signal4, Slot4
#define VL_T_PAR_TYPENAME2   typename U_par1, typename U_par2, typename U_par3, typename U_par4
#define VL_T_PAR_TYPENAME    typename T_par1, typename T_par2, typename T_par3, typename T_par4
#define VL_T_PAR_FORMAL_LIST T_par1, T_par2, T_par3, T_par4
#define VL_T_PAR_LIST        T_par1 par1, T_par2 par2, T_par2 par3, T_par2 par4
#define VL_T_PAR_CALL        par1, par2, par3, par4
#define VL_T_SIGNAL_NAME     Signal4
#define VL_T_BASE_SLOT_NAME  BaseSlot4
#define VL_T_SLOT_NAME       Slot4
#include "SignalSlotX.hpp"

// Signal5, Slot5
#define VL_T_PAR_TYPENAME2   typename U_par1, typename U_par2, typename U_par3, typename U_par4, typename U_par5
#define VL_T_PAR_TYPENAME    typename T_par1, typename T_par2, typename T_par3, typename T_par4, typename T_par5
#define VL_T_PAR_FORMAL_LIST T_par1, T_par2, T_par3, T_par4, T_par5
#define VL_T_PAR_LIST        T_par1 par1, T_par2 par2, T_par2 par3, T_par2 par4, T_par2 par5
#define VL_T_PAR_CALL        par1, par2, par3, par4, par5
#define VL_T_SIGNAL_NAME     Signal5
#define VL_T_BASE_SLOT_NAME  BaseSlot5
#define VL_T_SLOT_NAME       Slot5
#include "SignalSlotX.hpp"

// Signal6, Slot6
#define VL_T_PAR_TYPENAME2   typename U_par1, typename U_par2, typename U_par3, typename U_par4, typename U_par5, typename U_par6
#define VL_T_PAR_TYPENAME    typename T_par1, typename T_par2, typename T_par3, typename T_par4, typename T_par5, typename T_par6
#define VL_T_PAR_FORMAL_LIST T_par1, T_par2, T_par3, T_par4, T_par5, T_par6
#define VL_T_PAR_LIST        T_par1 par1, T_par2 par2, T_par2 par3, T_par2 par4, T_par2 par5, T_par2 par6
#define VL_T_PAR_CALL        par1, par2, par3, par4, par5, par6
#define VL_T_SIGNAL_NAME     Signal6
#define VL_T_BASE_SLOT_NAME  BaseSlot6
#define VL_T_SLOT_NAME       Slot6
#include "SignalSlotX.hpp"

// Signal7, Slot7
#define VL_T_PAR_TYPENAME2   typename U_par1, typename U_par2, typename U_par3, typename U_par4, typename U_par5, typename U_par6, typename U_par7
#define VL_T_PAR_TYPENAME    typename T_par1, typename T_par2, typename T_par3, typename T_par4, typename T_par5, typename T_par6, typename T_par7
#define VL_T_PAR_FORMAL_LIST T_par1, T_par2, T_par3, T_par4, T_par5, T_par6, T_par7
#define VL_T_PAR_LIST        T_par1 par1, T_par2 par2, T_par2 par3, T_par2 par4, T_par2 par5, T_par2 par6, T_par2 par7
#define VL_T_PAR_CALL        par1, par2, par3, par4, par5, par6, par7
#define VL_T_SIGNAL_NAME     Signal7
#define VL_T_BASE_SLOT_NAME  BaseSlot7
#define VL_T_SLOT_NAME       Slot7
#include "SignalSlotX.hpp"

// Signal8, Slot8
#define VL_T_PAR_TYPENAME2   typename U_par1, typename U_par2, typename U_par3, typename U_par4, typename U_par5, typename U_par6, typename U_par7, typename U_par8
#define VL_T_PAR_TYPENAME    typename T_par1, typename T_par2, typename T_par3, typename T_par4, typename T_par5, typename T_par6, typename T_par7, typename T_par8
#define VL_T_PAR_FORMAL_LIST T_par1, T_par2, T_par3, T_par4, T_par5, T_par6, T_par7, T_par8
#define VL_T_PAR_LIST        T_par1 par1, T_par2 par2, T_par2 par3, T_par2 par4, T_par2 par5, T_par2 par6, T_par2 par7, T_par2 par8
#define VL_T_PAR_CALL        par1, par2, par3, par4, par5, par6, par7, par8
#define VL_T_SIGNAL_NAME     Signal8
#define VL_T_BASE_SLOT_NAME  BaseSlot8
#define VL_T_SLOT_NAME       Slot8
#include "SignalSlotX.hpp"

// Signal9, Slot9
#define VL_T_PAR_TYPENAME2   typename U_par1, typename U_par2, typename U_par3, typename U_par4, typename U_par5, typename U_par6, typename U_par7, typename U_par8, typename U_par9
#define VL_T_PAR_TYPENAME    typename T_par1, typename T_par2, typename T_par3, typename T_par4, typename T_par5, typename T_par6, typename T_par7, typename T_par8, typename T_par9
#define VL_T_PAR_FORMAL_LIST T_par1, T_par2, T_par3, T_par4, T_par5, T_par6, T_par7, T_par8, T_par9
#define VL_T_PAR_LIST        T_par1 par1, T_par2 par2, T_par2 par3, T_par2 par4, T_par2 par5, T_par2 par6, T_par2 par7, T_par2 par8, T_par2 par9
#define VL_T_PAR_CALL        par1, par2, par3, par4, par5, par6, par7, par8, par9
#define VL_T_SIGNAL_NAME     Signal9
#define VL_T_BASE_SLOT_NAME  BaseSlot9
#define VL_T_SLOT_NAME       Slot9
#include "SignalSlotX.hpp"

// Signal10, Slot10
#define VL_T_PAR_TYPENAME2   typename U_par1, typename U_par2, typename U_par3, typename U_par4, typename U_par5, typename U_par6, typename U_par7, typename U_par8, typename U_par9, typename U_par10
#define VL_T_PAR_TYPENAME    typename T_par1, typename T_par2, typename T_par3, typename T_par4, typename T_par5, typename T_par6, typename T_par7, typename T_par8, typename T_par9, typename T_par10
#define VL_T_PAR_FORMAL_LIST T_par1, T_par2, T_par3, T_par4, T_par5, T_par6, T_par7, T_par8, T_par9, T_par10
#define VL_T_PAR_LIST        T_par1 par1, T_par2 par2, T_par2 par3, T_par2 par4, T_par2 par5, T_par2 par6, T_par2 par7, T_par2 par8, T_par2 par9, T_par2 par10
#define VL_T_PAR_CALL        par1, par2, par3, par4, par5, par6, par7, par8, par9, par10
#define VL_T_SIGNAL_NAME     Signal10
#define VL_T_BASE_SLOT_NAME  BaseSlot10
#define VL_T_SLOT_NAME       Slot10
#include "SignalSlotX.hpp"

}

//! \endcond

#endif
