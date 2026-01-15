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

#ifndef Color_INCLUDE_ONCE
#define Color_INCLUDE_ONCE

// for more colors see: http://en.wikipedia.org/wiki/List_of_colors

#include <vlCore/Vector4.hpp>

namespace vl
{
  inline fvec4 makeColor(unsigned int color)
  {
    fvec4 c;
    c.r() = float(((color >> 24) & 0xFF) / 255.0f);
    c.g() = float(((color >> 16) & 0xFF) / 255.0f);
    c.b() = float(((color >>  8) & 0xFF) / 255.0f);
    c.a() = float(((color >>  0) & 0xFF) / 255.0f);
    return c;    
  }

  inline bool isValidColor( const fvec4& color ) { return color.a() >= 0; }

  static const fvec4 invalid_color = fvec4(0, 0, 0, -1);
  
  static const fvec4 black = makeColor(0x000000FF);
  
  static const fvec4 white = makeColor(0xFFFFFFFF);
  
  static const fvec4 red = makeColor(0xFF0000FF);
  
  static const fvec4 crimson = makeColor(0xDC143CFF);
  
  static const fvec4 violet = makeColor(0x9400D3FF);
  
  static const fvec4 orange = makeColor(0xFFA000FF);
  
  static const fvec4 yellow = makeColor(0xFFFF00FF);
  
  static const fvec4 gold = makeColor(0xFFD700FF);
  
  static const fvec4 green = makeColor(0x00FF00FF);
  
  static const fvec4 lightgreen = makeColor(0x90FF90FF);
  
  static const fvec4 darkgreen = makeColor(0x006400FF);
  
  static const fvec4 olivegreen = makeColor(0x556B2FFF);
  
  static const fvec4 blue = makeColor(0x0000FFFF);
  
  static const fvec4 darkblue = makeColor(0x00008BFF);
  
  static const fvec4 royalblue = makeColor(0x4169E1FF);
  
  static const fvec4 skyblue = makeColor(0x5555FFFF);  
  
  static const fvec4 midnightblue = makeColor(0x191970FF);
  
  static const fvec4 fuchsia = makeColor(0xFF00FFFF);
  
  static const fvec4 aqua = makeColor(0x00FFFFFF);
  
  static const fvec4 pink = makeColor(0xffb6c1FF);
  
  static const fvec4 salmonpink = makeColor(0xFF91A4FF);
  
  static const fvec4 turquoise = makeColor(0x30D5C8FF); 
  
  static const fvec4 darkturquoise = makeColor(0x008080FF);
  
  static const fvec4 gray = makeColor(0xA9A9A9FF);
  
  static const fvec4 lightgray = makeColor(0xD3D3D3FF);
  
  static const fvec4 darkgray = makeColor(0x808080FF);
}

#endif
