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


//extern void SetNum_Vel(struct WTracks *wtrack);

extern LANGSPEC int AddVelocity3(
                                 int logtype,
                                 int velocityvelocity,
                                 const Place *placement,
                                 struct Notes *note
                                 );

/*
extern LANGSPEC struct Velocities *AddVelocity2(
                                                int velocityvelocity,
                                                const Place *placement,
                                                struct Notes *note
                                                );
*/

extern LANGSPEC int AddVelocity(
                                int velocityvelocity,
                                const Place *placement,
                                struct Notes *note
                                );

#ifdef __cplusplus
extern int AddVelocity4(
                        int velocityvelocity,
                        const Ratio &ratio,
                        r::Note *note);
#endif
                 
extern LANGSPEC void AddVelocityCurrPos(struct Tracker_Windows *window);

extern LANGSPEC void IncreaseVelocityCurrPos(struct Tracker_Windows *window,int inc);

