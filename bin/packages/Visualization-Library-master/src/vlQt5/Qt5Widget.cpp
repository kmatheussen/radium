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

#include <vlQt5/Qt5Widget.hpp>
#include <vlCore/Log.hpp>

using namespace vl;
using namespace vlQt5;

void Qt5Widget::translateKeyEvent(QKeyEvent* ev, unsigned short& unicode_out, EKey& key_out)
{
  // translate non unicode characters
  key_out     = Key_None;
  unicode_out = 0;

  switch(ev->key())
  {
    case Qt::Key_Clear:    key_out = Key_Clear; break;
    case Qt::Key_Control:  key_out = Key_Ctrl; break;
    // case Qt::Key_LCONTROL: key_out = Key_LeftCtrl; break;
    // case Qt::Key_RCONTROL: key_out = Key_RightCtrl; break;
    case Qt::Key_Alt:     key_out = Key_Alt; break;
    // case Qt::Key_LMENU:    key_out = Key_LeftAlt; break;
    // case Qt::Key_RMENU:    key_out = Key_RightAlt; break;
    case Qt::Key_Shift:    key_out = Key_Shift; break;
    // case Qt::Key_LSHIFT:   key_out = Key_LeftShift; break;
    // case Qt::Key_RSHIFT:   key_out = Key_RightShift; break;
    case Qt::Key_Insert:   key_out = Key_Insert; break;
    case Qt::Key_Delete:   key_out = Key_Delete; break;
    case Qt::Key_Home:     key_out = Key_Home; break;
    case Qt::Key_End:      key_out = Key_End; break;
    case Qt::Key_Print:    key_out = Key_Print; break;
    case Qt::Key_Pause:    key_out = Key_Pause; break;
    case Qt::Key_PageUp:   key_out = Key_PageUp; break;
    case Qt::Key_PageDown: key_out = Key_PageDown; break;
    case Qt::Key_Left:     key_out = Key_Left; break;
    case Qt::Key_Right:    key_out = Key_Right; break;
    case Qt::Key_Up:       key_out = Key_Up; break;
    case Qt::Key_Down:     key_out = Key_Down; break;
    case Qt::Key_F1:       key_out = Key_F1; break;
    case Qt::Key_F2:       key_out = Key_F2; break;
    case Qt::Key_F3:       key_out = Key_F3; break;
    case Qt::Key_F4:       key_out = Key_F4; break;
    case Qt::Key_F5:       key_out = Key_F5; break;
    case Qt::Key_F6:       key_out = Key_F6; break;
    case Qt::Key_F7:       key_out = Key_F7; break;
    case Qt::Key_F8:       key_out = Key_F8; break;
    case Qt::Key_F9:       key_out = Key_F9; break;
    case Qt::Key_F10:      key_out = Key_F10; break;
    case Qt::Key_F11:      key_out = Key_F11; break;
    case Qt::Key_F12:      key_out = Key_F12; break;

    case Qt::Key_0: key_out = Key_0; break;
    case Qt::Key_1: key_out = Key_1; break;
    case Qt::Key_2: key_out = Key_2; break;
    case Qt::Key_3: key_out = Key_3; break;
    case Qt::Key_4: key_out = Key_4; break;
    case Qt::Key_5: key_out = Key_5; break;
    case Qt::Key_6: key_out = Key_6; break;
    case Qt::Key_7: key_out = Key_7; break;
    case Qt::Key_8: key_out = Key_8; break;
    case Qt::Key_9: key_out = Key_9; break;

    case Qt::Key_A: key_out = Key_A; break;
    case Qt::Key_B: key_out = Key_B; break;
    case Qt::Key_C: key_out = Key_C; break;
    case Qt::Key_D: key_out = Key_D; break;
    case Qt::Key_E: key_out = Key_E; break;
    case Qt::Key_F: key_out = Key_F; break;
    case Qt::Key_G: key_out = Key_G; break;
    case Qt::Key_H: key_out = Key_H; break;
    case Qt::Key_I: key_out = Key_I; break;
    case Qt::Key_J: key_out = Key_J; break;
    case Qt::Key_K: key_out = Key_K; break;
    case Qt::Key_L: key_out = Key_L; break;
    case Qt::Key_M: key_out = Key_M; break;
    case Qt::Key_N: key_out = Key_N; break;
    case Qt::Key_O: key_out = Key_O; break;
    case Qt::Key_P: key_out = Key_P; break;
    case Qt::Key_Q: key_out = Key_Q; break;
    case Qt::Key_R: key_out = Key_R; break;
    case Qt::Key_S: key_out = Key_S; break;
    case Qt::Key_T: key_out = Key_T; break;
    case Qt::Key_U: key_out = Key_U; break;
    case Qt::Key_V: key_out = Key_V; break;
    case Qt::Key_W: key_out = Key_W; break;
    case Qt::Key_X: key_out = Key_X; break;
    case Qt::Key_Y: key_out = Key_Y; break;
    case Qt::Key_Z: key_out = Key_Z; break;
  }

  // fill unicode
  QString ustring = ev->text();
  if ( ustring.length() == 1 )
  {
    unicode_out = ustring[0].unicode();

    // fill key
    switch(unicode_out)
    {
      case L'0': key_out = Key_0; break;
      case L'1': key_out = Key_1; break;
      case L'2': key_out = Key_2; break;
      case L'3': key_out = Key_3; break;
      case L'4': key_out = Key_4; break;
      case L'5': key_out = Key_5; break;
      case L'6': key_out = Key_6; break;
      case L'7': key_out = Key_7; break;
      case L'8': key_out = Key_8; break;
      case L'9': key_out = Key_9; break;

      case L'A': key_out = Key_A; break;
      case L'B': key_out = Key_B; break;
      case L'C': key_out = Key_C; break;
      case L'D': key_out = Key_D; break;
      case L'E': key_out = Key_E; break;
      case L'F': key_out = Key_F; break;
      case L'G': key_out = Key_G; break;
      case L'H': key_out = Key_H; break;
      case L'I': key_out = Key_I; break;
      case L'J': key_out = Key_J; break;
      case L'K': key_out = Key_K; break;
      case L'L': key_out = Key_L; break;
      case L'M': key_out = Key_M; break;
      case L'N': key_out = Key_N; break;
      case L'O': key_out = Key_O; break;
      case L'P': key_out = Key_P; break;
      case L'Q': key_out = Key_Q; break;
      case L'R': key_out = Key_R; break;
      case L'S': key_out = Key_S; break;
      case L'T': key_out = Key_T; break;
      case L'U': key_out = Key_U; break;
      case L'V': key_out = Key_V; break;
      case L'W': key_out = Key_W; break;
      case L'X': key_out = Key_X; break;
      case L'Y': key_out = Key_Y; break;
      case L'Z': key_out = Key_Z; break;

      case L'a': key_out = Key_A; break;
      case L'b': key_out = Key_B; break;
      case L'c': key_out = Key_C; break;
      case L'd': key_out = Key_D; break;
      case L'e': key_out = Key_E; break;
      case L'f': key_out = Key_F; break;
      case L'g': key_out = Key_G; break;
      case L'h': key_out = Key_H; break;
      case L'i': key_out = Key_I; break;
      case L'j': key_out = Key_J; break;
      case L'k': key_out = Key_K; break;
      case L'l': key_out = Key_L; break;
      case L'm': key_out = Key_M; break;
      case L'n': key_out = Key_N; break;
      case L'o': key_out = Key_O; break;
      case L'p': key_out = Key_P; break;
      case L'q': key_out = Key_Q; break;
      case L'r': key_out = Key_R; break;
      case L's': key_out = Key_S; break;
      case L't': key_out = Key_T; break;
      case L'u': key_out = Key_U; break;
      case L'v': key_out = Key_V; break;
      case L'w': key_out = Key_W; break;
      case L'x': key_out = Key_X; break;
      case L'y': key_out = Key_Y; break;
      case L'z': key_out = Key_Z; break;

      case 13: key_out = Key_Return; break;
      case 8: key_out = Key_BackSpace; break;
      case 9: key_out = Key_Tab; break;
      case L' ': key_out = Key_Space; break;

      case 27: key_out = Key_Escape; break;
      case L'!': key_out = Key_Exclam; break;
      case L'"': key_out = Key_QuoteDbl; break;
      case L'#': key_out = Key_Hash; break;
      case L'$': key_out = Key_Dollar; break;
      case L'&': key_out = Key_Ampersand; break;
      case L'\'': key_out = Key_Quote; break;
      case L'(': key_out = Key_LeftParen; break;
      case L')': key_out = Key_RightParen; break;
      case L'*': key_out = Key_Asterisk; break;
      case L'+': key_out = Key_Plus; break;
      case L',': key_out = Key_Comma; break;
      case L'-': key_out = Key_Minus; break;
      case L'.': key_out = Key_Period; break;
      case L'\\': key_out = Key_Slash; break;
      case L':': key_out = Key_Colon; break;
      case L';': key_out = Key_Semicolon; break;
      case L'<': key_out = Key_Less; break;
      case L'=': key_out = Key_Equal; break;
      case L'>': key_out = Key_Greater; break;
      case L'?': key_out = Key_Question; break;
      case L'@': key_out = Key_At; break;
      case L'[': key_out = Key_LeftBracket; break;
      case L'/': key_out = Key_BackSlash; break;
      case L']': key_out = Key_RightBracket; break;
      case L'|': key_out = Key_Caret; break;
      case L'_': key_out = Key_Underscore; break;
      case L'`': key_out = Key_QuoteLeft; break;
    }
  }
}
//-----------------------------------------------------------------------------
