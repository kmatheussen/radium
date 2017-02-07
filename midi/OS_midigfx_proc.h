/* Copyright 2003 Kjetil S. Matheussen

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA. */


extern LANGSPEC void MIDIGFX_UpdateAll(void);
extern LANGSPEC void MIDIGFX_SetPanSlider(bool on,int value);
extern LANGSPEC void MIDIGFX_SetVolumeSlider(bool on,int value);
extern LANGSPEC void MIDIGFX_SetLSB(int lsb);
extern LANGSPEC void MIDIGFX_SetMSB(int msb);
extern LANGSPEC void MIDIGFX_SetChannel(int ch);
extern LANGSPEC void MIDIGFX_SetCCSlider(int slidernum,bool on,int value);

extern LANGSPEC void MIDIGFX_PP_Update(struct Instruments *instrument,struct Patch *patch,bool is_loading);

