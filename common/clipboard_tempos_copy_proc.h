/* Copyright 2000 Kjetil S. Matheussen

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


#ifndef _RADIUM_COMMON_CLIPBOARD_TEMPOS_COPY_PROC_H
#define _RADIUM_COMMON_CLIPBOARD_TEMPOS_COPY_PROC_H


extern LANGSPEC struct Swing *CB_CopySwings(
                                            const struct Swing *swing,
                                            const Place *cut // can be NULL
                                           );

extern LANGSPEC struct Signatures *CB_CopySignatures(
                                            const struct Signatures *signature
);

extern LANGSPEC struct LPBs *CB_CopyLPBs(
                                const struct LPBs *lpb
);

extern LANGSPEC struct Tempos *CB_CopyTempos(
                                    const struct Tempos *tempo
);

extern LANGSPEC struct TempoNodes *CB_CopyTempoNodes(
                                            const struct TempoNodes *temponode
);


#endif



