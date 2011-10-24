
# (Before this line, some definitions from smakefile.smk will be inserted)

#----------------DEFININTIONS----------------------

OD=$(T)radiumobjects

DEFINES=define __USE_SYSBASE=1 define NOPAUSEPLAY
# define SYSBASEDEBUG
# define TRACKER_DEBUG
# define MEMORY_DEBUG
# define TRACKER_GB

#----------------CPU OPTIONS--------------------------

CPU=68060

#----------------FPU OPTIONS-------------------------

#MATH=s
#MATHLIB=LIB:scm.lib

MATH=8
MATHLIB=LIB:scm881nb.lib

#----------------COMPILER OPTIONS---------------------

IGNORE= ignore=51 ignore=105 ignore=220
# 51=c++ comment, but that one is allowed in latest c standard.
# 105=no external functions. Unnecesarry, used for t_gc.c
# 220= "=-","=+", etc. (very strange warning)

OPTIMIZE=optimize optimizetime optimizerinline optglobal \
	optimizercomplexity=100 optimizerdepth=100 \
	optimizerinlocal optimizerpeephole \
	optimizerrecurdepth=100 optloop

UPOPT=debug=line $(IGNORE) strict ansi CPU=$(CPU) \
	math=$(MATH) $(DEFINES) nostackcheck maxerr=6 \
	maxwarn=10 parm=reg error=100 error=87 error=a \
	IncludeDirectory=Amiga DATA=FARONLY $(OPTIMIZE) 
# nodebug

OPT= $(UPOPT)
# profile

PYPATH=IncludeDirectory=dev:python20_source/include

#----------------LINKER OPTIONS-----------------------

GBLIB=LIB:gc$(CPU)_FARDATA.lib
#GBLIB=LIB:gc_veryold.lib

PYTHONLIBS= $(PD)/Modules/Modules.lib \
$(PD)/Parser/Parser.lib \
$(PD)/Python/Python.lib \
$(PD)/Objects/Objects.lib \
$(PD)/Expat/lib/expat.lib \
$(PD)/Amiga/amigapython.lib \
LIB:mempoolsnb.lib $(MATHLIB) \
LIB:scnb.lib

LIBS=$(PYTHONLIBS) $(GBLIB) LIB:amiga.lib	

LOPT=LIB $(LIBS) verbose map $(T)map.map NOICON
# stripdebug
LPOPT=prelink

#------------------LINKING----------------------------

all: bin/radium bin/protoconfparser.py bin/protos.conf bin/common.py


OBJ1=  $(AD)Amiga_main.o $(AD)Amiga_config.o $(AD)Amiga_colors.o $(CD)windows.o \
       $(CD)window_config.o $(CD)list.o $(AD)GFX_Amiga_egc.o $(CD)song.o $(CD)blocks.o \
       $(CD)block_insert.o $(CD)block_split.o $(CD)block_delete.o $(CD)block_properties.o $(CD)tracks.o
OBJ14= $(CD)localzooms.o $(AD)Amiga_readstr.o $(CD)control.o $(AD)Amiga_endprogram.o \
       $(CD)lines.o $(CD)font.o $(CD)track_insert.o $(CD)track_onoff.o \
       $(AD)Amiga_error.o $(AD)Amiga_Semaphores.o
OBJ2=  $(CD)notes.o $(CD)notes_legalize.o $(CD)wblocks.o $(CD)wtracks.o $(CD)sliders.o \
       $(CD)gfx_wblocks.o $(CD)gfx_wblocks_reltempo.o $(CD)gfx_window_title.o
OBJ15= $(CD)gfx_tempotrackheader.o $(CD)gfx_upperleft.o $(CD)common.o \
       $(CD)gfx_wtracks.o $(CD)gfx_tempocolor.o
OBJ3=  $(CD)gfx_wtext.o $(CD)eventreciever.o $(CD)reallines.o $(CD)notestext.o \
       $(CD)trackreallines.o $(CD)clipboard_range.o $(CD)clipboard_range_calc.o \
       $(CD)clipboard_range_copy.o $(CD)clipboard_range_paste.o
OBJ16= $(CD)clipboard_range_cut.o $(CD)transpose.o $(CD)new/backwards.o \
       $(CD)new/invert.o $(CD)new/glissando.o $(CD)trackreallineelements.o $(CD)clipboard_track_copy.o \
       $(CD)clipboard_track_paste.o $(CD)clipboard_track_cut.o \
       $(CD)clipboard_tempos_copy.o
OBJ4=  $(CD)mouse.o $(CD)mouse_wtrack.o $(CD)mouse_wtrackheader.o \
       $(CD)mouse_tempoheader.o $(CD)mouse_wtrackborder.o $(CD)mouse_temponodeborder.o \
       $(CD)mouse_fxarea.o $(CD)mouse_vellinenode.o $(CD)mouse_vellineend.o \
       $(CD)mouse_vellinestart.o $(CD)mouse_fxnode.o $(CD)mouse_quantitize.o
OBJ17= $(CD)mouse_reltemposlider.o $(CD)tbox.o $(CD)area.o \
       $(CD)clipboard_localzooms.o $(CD)clipboard_block_copy.o \
       $(CD)clipboard_block_paste.o $(CD)quantitize.o
OBJ5=  $(AD)Amiga_debug.o $(CD)memory.o $(AD)Amiga_memory.o $(CD)placement.o \
       $(CD)t_gc.o $(CD)cursor.o $(CD)cursor_updown.o $(CD)subtrack.o \
       $(CD)velocities.o $(CD)blts.o $(CD)scroll.o $(CD)scroll_play.o $(CD)pixmap.o
OBJ6=  $(CD)realline_calc.o $(CD)gfx_subtrack.o $(CD)LPB.o $(CD)resizewindow.o \
       $(CD)gfx_wtrackheaders.o $(CD)gfx_wtrackheader_volpan.o $(CD)gfx_slider.o \
       $(CD)reallines_insert.o $(CD)gfx_shrink.o $(CD)gfx_shrink_t.o
OBJ7=  $(CD)nodelines.o $(CD)nodeboxes.o $(AD)plug-ins/midicluster.o \
       $(AD)plug-ins/camd_i_plugin.o $(AD)plug-ins/camd_fx.o $(CD)instruments.o \
       $(CD)patch.o $(CD)fxlines.o $(CD)fxlines_legalize.o $(AD)Amiga_bs.o
OBJ18= $(AD)Amiga_bs_edit.o $(CD)blocklist.o $(AD)plug-ins/camd_get_clustername.o \
       $(AD)plug-ins/camd_getMidiLink.o $(AD)plug-ins/camd_i_input.o \
       $(AD)plug-ins/camd_playfromstart.o

#Tempos and time
OBJ8=  $(CD)reltempo.o $(CD)temponodes.o $(CD)tempos.o $(CD)time.o $(CD)time2place.o \
       $(CD)mouse_temponodes.o $(CD)temponodes_legalize.o

#Playing
OBJ9=  $(CD)Ptask2Mtask.o $(AD)Amiga_Ptask2Mtask.o $(AD)Amiga_player.o $(CD)player.o \
       $(CD)PEQrealline.o $(CD)PEQmempool.o $(CD)PEQblock.o $(CD)PEQnotes.o \
       $(CD)PEQcommon.o $(CD)playerclass.o $(CD)player_startstop.o \
       $(CD)PEQvelocities.o $(CD)PEQ_calc.o $(CD)PEQfxs.o $(CD)player_pause.o \
       $(CD)PEQ_type.o $(CD)PEQ_calc_64bit.o $(AD)64bit.o $(CD)PEQ_clock.o

#Disk
OBJ10= $(CD)disk.o $(CD)disk_fxs.o $(CD)disk_wblock.o $(CD)disk_localzooms.o \
       $(CD)disk_track.o $(CD)disk_fx.o $(CD)disk_fxnodelines.o $(CD)disk_wtrack.o \
       $(CD)disk_temponodes.o $(CD)disk_tempos.o $(CD)disk_song.o \
       $(CD)disk_velocities.o $(CD)disk_block.o $(CD)disk_placement.o \
       $(CD)disk_load.o
OBJ11= $(CD)disk_instrument.o $(CD)disk_patches.o $(AD)plug-ins/disk_camd_i_plugin.o \
       $(CD)disk_stops.o $(CD)disk_playlist.o $(CD)disk_root.o $(CD)disk_notes.o \
       $(CD)disk_lpbs.o $(CD)disk_windows.o $(CD)disk_warea.o $(CD)disk_save.o \
       $(AD)plug-ins/disk_camd_fx.o $(CD)disk_slider.o \
       $(AD)plug-ins/disk_camd_mymidilinks.o $(AD)disk_amiga.o \
       mmd2loader/mmd2load.o


#Undo
OBJ12= $(CD)undo.o $(CD)undo_notes.o $(CD)undo_fxs.o $(CD)undo_temponodes.o \
       $(CD)undo_tempos.o $(CD)undo_lpbs.o $(CD)undo_notesandfxs.o \
       $(CD)undo_reallines.o $(CD)undo_tracks.o $(CD)undo_range.o \
       $(CD)undo_blocks.o $(CD)undo_trackheader.o $(CD)undo_reltempomax.o \
       $(CD)undo_maintempos.o $(CD)undo_block_insertdelete.o \
       $(CD)undo_reltemposlider.o $(CD)undo_block_mergesplit.o


#$(AD)instrprop
OBJ13= $(AD)instrprop/Amiga_instrprop.o $(AD)instrprop/Amiga_instrprop_init.o \
       $(AD)instrprop/Amiga_instrprop_temp_help.o \
       $(AD)instrprop/Amiga_instrprop_temp_camd.o \
       $(AD)instrprop/Amiga_instrprop_edit.o \
       $(AD)instrprop/Amiga_instrprop_camd_menues.o


#Python
OBJ19= $(API)api_keyplayedit.o $(API)api_noteedit.o $(API)api_navigate.o \
		 $(API)api_common.o $(API)radium_wrap.o $(API)wrapfunclist.o $(API)api_support.o

OBJ19=$(API)radium_wrap.o $(API)api_common.o $(API)api_keyplay.o $(API)api_keyplayedit.o $(API)api_navigate.o \
		$(API)api_noteedit.o $(API)api_support.o $(API)wrapfunclist.o $(API)api_trackonoff.o \
		$(API)api_zoom.o $(API)api_notemanipulate.o $(API)api_play.o $(API)api_clipboard.o $(API)api_undo.o \
		$(API)api_various.o

#Advanced
OBJ20= advanced/ad_noteadd.o

bin/radium: lib:gc$(CPU).lib \
						$(T)1.m $(T)2.m $(T)3.m $(T)4.m $(T)5.m $(T)6.m $(T)7.m \
						$(T)8.m $(T)9.m $(T)10.m $(T)11.m $(T)12.m $(T)13.m \
                  $(T)14.m $(T)15.m $(T)16.m $(T)17.m $(T)18.m $(T)19.m $(T)20.m
	slink with <<
TO bin/radium
FROM LIB:c_mempools.o
$(PD)/Modules/getbuildinfo.o
$(PD)/Modules/python.o $(PD)/Modules/main.o
$(OBJ1) $(OBJ2) $(OBJ3) $(OBJ4) $(OBJ5) $(OBJ6) $(OBJ7) $(OBJ8) $(OBJ9) $(OBJ10) $(OBJ11) $(OBJ12) $(OBJ13)
$(OBJ14) $(OBJ15) $(OBJ16) $(OBJ17) $(OBJ18) $(OBJ19) $(OBJ20)
$(LOPT)
<<




bin/protoconfparser.py: $(API)protoconfparser.py
	execute <<
	echo "\#This file is copied from ../api/ during make-process" >bin/protoconfparser.py
	type $(API)protoconfparser.py >>bin/protoconfparser.py
	<<

bin/protos.conf: $(API)protos.conf
	execute <<
	echo "\#Warning! Strange things may happen if this file is changed." >bin/protos.conf
	echo "\#The file is copied from ../api/ during make-process." >>bin/protos.conf
	type $(API)protos.conf >>bin/protos.conf
	<<

bin/common.py: $(API)common.py
	execute <<
	echo "\#This file is copied from ../api/ during make-process." >bin/common.py
	type $(API)common.py >>bin/common.py
	<<


$(T)1.m: $(OBJ1) $(T)radplace.m
	echo >$(T)1.m
	copy FROM $(OBJ1) TO $(OD) QUIET

$(T)2.m: $(OBJ2) $(T)radplace.m
	echo >$(T)2.m
	copy FROM $(OBJ2) TO $(OD) QUIET

$(T)3.m: $(OBJ3) $(T)radplace.m
	echo >$(T)3.m
	copy FROM $(OBJ3) TO $(OD) QUIET

$(T)4.m: $(OBJ4) $(T)radplace.m
	echo >$(T)4.m
	copy FROM $(OBJ4) TO $(OD) QUIET

$(T)5.m: $(OBJ5) $(T)radplace.m
	echo >$(T)5.m
	copy FROM $(OBJ5) TO $(OD) QUIET

$(T)6.m: $(OBJ6) $(T)radplace.m
	echo >$(T)6.m
	copy FROM $(OBJ6) TO $(OD) QUIET

$(T)7.m: $(OBJ7) $(T)radplace.m
	echo >$(T)7.m
	copy FROM $(OBJ7) TO $(OD) QUIET

$(T)8.m: $(OBJ8) $(T)radplace.m
	echo >$(T)8.m
	copy FROM $(OBJ8) TO $(OD) QUIET

$(T)9.m: $(OBJ9) $(T)radplace.m
	echo >$(T)9.m
	copy FROM $(OBJ9) TO $(OD) QUIET

$(T)10.m: $(OBJ10) $(T)radplace.m
	echo >$(T)10.m
	copy FROM $(OBJ10) TO $(OD) QUIET

$(T)11.m: $(OBJ11) $(T)radplace.m
	echo >$(T)11.m
	copy FROM $(OBJ11) TO $(OD) QUIET

$(T)12.m: $(OBJ12) $(T)radplace.m
	echo >$(T)12.m
	copy FROM $(OBJ12) TO $(OD) QUIET

$(T)13.m: $(OBJ13) $(T)radplace.m
	echo >$(T)13.m
	copy FROM $(OBJ13) TO $(OD) QUIET

$(T)14.m: $(OBJ14) $(T)radplace.m
	echo >$(T)14.m
	copy FROM $(OBJ14) TO $(OD) QUIET

$(T)15.m: $(OBJ15) $(T)radplace.m
	echo >$(T)15.m
	copy FROM $(OBJ15) TO $(OD) QUIET

$(T)16.m: $(OBJ16) $(T)radplace.m
	echo >$(T)16.m
	copy FROM $(OBJ16) TO $(OD) QUIET

$(T)17.m: $(OBJ17) $(T)radplace.m
	echo >$(T)17.m
	copy FROM $(OBJ17) TO $(OD) QUIET

$(T)18.m: $(OBJ18) $(T)radplace.m
	echo >$(T)18.m
	copy FROM $(OBJ18) TO $(OD) QUIET

$(T)19.m: $(OBJ19) $(T)radplace.m
	echo >$(T)19.m
	copy FROM $(OBJ19) TO $(OD) QUIET

$(T)20.m: $(OBJ20) $(T)radplace.m
	echo >$(T)20.m
	copy FROM $(OBJ20) TO $(OD) QUIET

$(T)radplace.m:
	echo "Failat 20" >$(T)radplace.m
	echo "delete >NIL: $(OD) ALL FORCE" >>$(T)radplace.m
	echo "makedir $(OD)" >>$(T)radplace.m
	execute $(T)radplace.m

#-----------------COMPILING-----------------------------

$(AD)Amiga_main.o: $(AD)Amiga_main.c config/config.h
	$(CC) $(AD)Amiga_main.c $(UPOPT) 

$(CD)windows.o: $(CD)windows.c 
	$(CC) $(CD)windows.c $(OPT)

$(AD)GFX_Amiga_egc.o: $(AD)GFX_Amiga_egc.c $(AD)OS_visual.h
	$(CC) $(AD)GFX_Amiga_egc.c $(UPOPT)

$(CD)list.o: $(CD)list.c 
	$(CC) $(CD)list.c $(OPT)

$(CD)song.o: $(CD)song.c 
	$(CC) $(CD)song.c $(OPT)

$(CD)blocks.o: $(CD)blocks.c 
	$(CC) $(CD)blocks.c $(OPT)

$(CD)tracks.o: $(CD)tracks.c 
	$(CC) $(CD)tracks.c $(OPT)

$(CD)localzooms.o: $(CD)localzooms.c 
	$(CC) $(CD)localzooms.c $(OPT)

$(CD)notes.o: $(CD)notes.c 
	$(CC) $(CD)notes.c $(OPT)

$(CD)wblocks.o: $(CD)wblocks.c 
	$(CC) $(CD)wblocks.c $(OPT)

$(CD)wtracks.o: $(CD)wtracks.c 
	$(CC) $(CD)wtracks.c $(OPT)

$(CD)sliders.o: $(CD)sliders.c 
	$(CC) $(CD)sliders.c $(OPT)

$(CD)gfx_wblocks.o: $(CD)gfx_wblocks.c 
	$(CC) $(CD)gfx_wblocks.c $(OPT)

$(CD)common.o: $(CD)common.c 
	$(CC) $(CD)common.c $(OPT)

$(CD)gfx_wtracks.o: $(CD)gfx_wtracks.c 
	$(CC) $(CD)gfx_wtracks.c $(OPT)

$(CD)gfx_wtext.o: $(CD)gfx_wtext.c 
	$(CC) $(CD)gfx_wtext.c $(OPT)

$(CD)eventreciever.o: $(CD)eventreciever.c $(API)radium_proc.h
	$(CC) $(CD)eventreciever.c $(UPOPT) $(PYPATH)

$(CD)reallines.o: $(CD)reallines.c
	$(CC) $(CD)reallines.c $(OPT)

$(CD)mouse.o: $(CD)mouse.c
	$(CC) $(CD)mouse.c $(UPOPT)

$(CD)notestext.o: $(CD)notestext.c
	$(CC) $(CD)notestext.c $(OPT)

$(CD)trackreallines.o: $(CD)trackreallines.c
	$(CC) $(CD)trackreallines.c $(OPT)

$(AD)Amiga_debug.o: $(AD)Amiga_debug.c
	$(CC) $(AD)Amiga_debug.c $(OPT) 

$(CD)memory.o: $(CD)memory.c
	$(CC) $(CD)memory.c $(OPT)

$(CD)placement.o: $(CD)placement.c
	$(CC) $(CD)placement.c $(OPT)

$(CD)t_gc.o: $(CD)t_gc.c
	$(CC) $(CD)t_gc.c $(OPT)

$(CD)cursor.o: $(CD)cursor.c
	$(CC) $(CD)cursor.c $(OPT)

$(CD)cursor_updown.o: $(CD)cursor_updown.c
	$(CC) $(CD)cursor_updown.c $(OPT)

$(CD)subtrack.o: $(CD)subtrack.c
	$(CC) $(CD)subtrack.c $(OPT)

$(CD)velocities.o: $(CD)velocities.c
	$(CC) $(CD)velocities.c $(OPT)

$(CD)realline_calc.o: $(CD)realline_calc.c
	$(CC) $(CD)realline_calc.c $(OPT)

$(CD)gfx_subtrack.o: $(CD)gfx_subtrack.c
	$(CC) $(CD)gfx_subtrack.c $(OPT)

$(CD)tempos.o: $(CD)tempos.c
	$(CC) $(CD)tempos.c $(OPT)

$(CD)LPB.o: $(CD)LPB.c
	$(CC) $(CD)LPB.c $(OPT)

$(CD)resizewindow.o: $(CD)resizewindow.c
	$(CC) $(CD)resizewindow.c $(OPT)

$(CD)gfx_wtrackheaders.o: $(CD)gfx_wtrackheaders.c
	$(CC) $(CD)gfx_wtrackheaders.c $(OPT)

$(CD)nodelines.o: $(CD)nodelines.c
	$(CC) $(CD)nodelines.c $(OPT)

$(CD)temponodes.o: $(CD)temponodes.c
	$(CC) $(CD)temponodes.c $(OPT)

$(AD)plug-ins/camd_i_plugin.o: $(AD)plug-ins/camd_i_plugin.c $(AD)plug-ins/camd_i_plugin.h
	$(CC) $(AD)plug-ins/camd_i_plugin.c $(OPT) disassemble=camd_i_plugin.dis

$(CD)instruments.o: $(CD)instruments.c
	$(CC) $(CD)instruments.c $(OPT)

$(CD)fxlines.o: $(CD)fxlines.c
	$(CC) $(CD)fxlines.c $(OPT)

$(AD)Amiga_readstr.o: $(AD)Amiga_readstr.c
	$(CC) $(AD)Amiga_readstr.c $(OPT)

$(CD)tbox.o: $(CD)tbox.c
	$(CC) $(CD)tbox.c $(UPOPT)


$(CD)area.o: $(CD)area.c
	$(CC) $(CD)area.c $(UPOPT)

$(CD)mouse_wtrack.o: $(CD)mouse_wtrack.c
	$(CC) $(CD)mouse_wtrack.c $(UPOPT)


$(CD)mouse_temponodes.o: $(CD)mouse_temponodes.c
	$(CC) $(CD)mouse_temponodes.c $(UPOPT)


$(CD)mouse_wtrackborder.o: $(CD)mouse_wtrackborder.c
	$(CC) $(CD)mouse_wtrackborder.c $(UPOPT)


$(CD)mouse_temponodeborder.o: $(CD)mouse_temponodeborder.c
	$(CC) $(CD)mouse_temponodeborder.c $(UPOPT)


$(CD)mouse_fxarea.o: $(CD)mouse_fxarea.c
	$(CC) $(CD)mouse_fxarea.c $(UPOPT)

$(CD)mouse_vellinenode.o: $(CD)mouse_vellinenode.c
	$(CC) $(CD)mouse_vellinenode.c $(UPOPT)

$(CD)mouse_vellinestart.o: $(CD)mouse_vellinestart.c
	$(CC) $(CD)mouse_vellinestart.c $(UPOPT)

$(CD)mouse_vellineend.o: $(CD)mouse_vellineend.c
	$(CC) $(CD)mouse_vellineend.c $(UPOPT)

$(CD)mouse_fxnode.o: $(CD)mouse_fxnode.c
	$(CC) $(CD)mouse_fxnode.c $(UPOPT)

$(AD)Amiga_bs.o: $(AD)Amiga_bs.c
	$(CC) $(AD)Amiga_bs.c $(OPT)

$(AD)Amiga_bs_edit.o: $(AD)Amiga_bs_edit.c
	$(CC) $(AD)Amiga_bs_edit.c $(OPT)

$(CD)blocklist.o: $(CD)blocklist.c
	$(CC) $(CD)blocklist.c $(OPT)

$(CD)time.o: $(CD)time.c
	$(CC) $(CD)time.c $(OPT) 

$(CD)reltempo.o: $(CD)reltempo.c
	$(CC) $(CD)reltempo.c $(OPT)


#player source-files.

$(CD)Ptask2Mtask.o: $(CD)Ptask2Mtask.c $(CD)playerclass.h
	$(CC) $(CD)Ptask2Mtask.c $(OPT) 

$(AD)Amiga_Ptask2Mtask.o: $(AD)Amiga_Ptask2Mtask.c
	$(CC) $(AD)Amiga_Ptask2Mtask.c $(OPT) 

$(AD)Amiga_player.o: $(AD)Amiga_player.c config/config.h
	$(CC) $(AD)Amiga_player.c $(UPOPT) disassemble=$(AD)Amiga_player.dis define PLAYERPRI=$(PLAYERPRI)

$(CD)player.o: $(CD)player.c $(CD)playerclass.h
	$(CC) $(CD)player.c $(UPOPT)  disassemble=player.dis

$(CD)PEQrealline.o: $(CD)PEQrealline.c $(CD)playerclass.h
	$(CC) $(CD)PEQrealline.c $(OPT) 

$(CD)PEQmempool.o: $(CD)PEQmempool.c $(CD)playerclass.h
	$(CC) $(CD)PEQmempool.c $(OPT) 

$(CD)PEQblock.o: $(CD)PEQblock.c $(CD)playerclass.h
	$(CC) $(CD)PEQblock.c $(OPT)

$(CD)PEQnotes.o: $(CD)PEQnotes.c $(CD)playerclass.h
	$(CC) $(CD)PEQnotes.c $(OPT)

$(CD)PEQcommon.o: $(CD)PEQcommon.c $(CD)playerclass.h
	$(CC) $(CD)PEQcommon.c $(UPOPT)

$(CD)playerclass.o: $(CD)playerclass.c $(CD)playerclass.h
	$(CC) $(CD)playerclass.c $(OPT)

$(CD)player_startstop.o: $(CD)player_startstop.c $(CD)playerclass.h
	$(CC) $(CD)player_startstop.c $(OPT)

$(CD)PEQvelocities.o: $(CD)PEQvelocities.c
	$(CC) $(CD)PEQvelocities.c $(OPT)

$(CD)PEQ_calc.o: $(CD)PEQ_calc.c
	$(CC) $(CD)PEQ_calc.c $(OPT)

$(CD)PEQfxs.o: $(CD)PEQfxs.c
	$(CC) $(CD)PEQfxs.c $(OPT)

$(CD)patch.o: $(CD)patch.c
	$(CC) $(CD)patch.c $(OPT)

$(CD)clipboard_range.o: $(CD)clipboard_range.c $(CD)clipboard_range.h
	$(CC) $(CD)clipboard_range.c $(OPT)

$(CD)clipboard_range_calc.o: $(CD)clipboard_range_calc.c $(CD)clipboard_range.h
	$(CC) $(CD)clipboard_range_calc.c $(OPT)

$(CD)clipboard_range_copy.o: $(CD)clipboard_range_copy.c $(CD)clipboard_range.h
	$(CC) $(CD)clipboard_range_copy.c $(OPT)

$(CD)clipboard_range_paste.o: $(CD)clipboard_range_paste.c $(CD)clipboard_range.h
	$(CC) $(CD)clipboard_range_paste.c $(OPT)

$(CD)clipboard_range_cut.o: $(CD)clipboard_range_cut.c $(CD)clipboard_range.h
	$(CC) $(CD)clipboard_range_cut.c $(OPT)

$(CD)player_pause.o: $(CD)player_pause.c $(CD)playerclass.h
	$(CC) $(CD)player_pause.c $(OPT)

$(CD)PEQ_type.o: $(CD)PEQ_type.c $(CD)playerclass.h
	$(CC) $(CD)PEQ_type.c $(OPT)


$(CD)transpose.o: $(CD)transpose.c
	$(CC) $(CD)transpose.c $(OPT)



$(CD)disk_wblock.o: $(CD)disk_wblock.c $(CD)disk.h
	$(CC) $(CD)disk_wblock.c $(OPT)


$(CD)disk_localzooms.o: $(CD)disk_localzooms.c $(CD)disk.h
	$(CC) $(CD)disk_localzooms.c $(OPT)


$(CD)disk_track.o: $(CD)disk_track.c $(CD)disk.h
	$(CC) $(CD)disk_track.c $(OPT)

$(CD)disk_fx.o: $(CD)disk_fx.c $(CD)disk.h
	$(CC) $(CD)disk_fx.c $(OPT)

$(CD)disk_fxnodelines.o: $(CD)disk_fxnodelines.c $(CD)disk.h
	$(CC) $(CD)disk_fxnodelines.c $(OPT)

$(CD)disk_wtrack.o: $(CD)disk_wtrack.c $(CD)disk.h
	$(CC) $(CD)disk_wtrack.c $(OPT)

$(CD)disk_temponodes.o: $(CD)disk_temponodes.c $(CD)disk.h
	$(CC) $(CD)disk_temponodes.c $(OPT)

$(CD)disk_tempos.o: $(CD)disk_tempos.c $(CD)disk.h
	$(CC) $(CD)disk_tempos.c $(OPT)

$(CD)disk_song.o: $(CD)disk_song.c $(CD)disk.h
	$(CC) $(CD)disk_song.c $(OPT)

$(CD)disk_velocities.o: $(CD)disk_velocities.c $(CD)disk.h
	$(CC) $(CD)disk_velocities.c $(OPT)

$(CD)disk_block.o: $(CD)disk_block.c $(CD)disk.h
	$(CC) $(CD)disk_block.c $(OPT)

$(CD)disk_placement.o: $(CD)disk_placement.c $(CD)disk.h
	$(CC) $(CD)disk_placement.c $(OPT)

$(CD)disk_stops.o: $(CD)disk_stops.c $(CD)disk.h
	$(CC) $(CD)disk_stops.c $(OPT)

$(CD)disk_playlist.o: $(CD)disk_playlist.c $(CD)disk.h
	$(CC) $(CD)disk_playlist.c $(OPT)

$(CD)disk_root.o: $(CD)disk_root.c $(CD)disk.h
	$(CC) $(CD)disk_root.c $(OPT)

$(CD)disk_notes.o: $(CD)disk_notes.c $(CD)disk.h
	$(CC) $(CD)disk_notes.c $(OPT)

$(CD)disk_lpbs.o: $(CD)disk_lpbs.c $(CD)disk.h
	$(CC) $(CD)disk_lpbs.c $(OPT)

$(CD)disk_windows.o: $(CD)disk_windows.c $(CD)disk.h
	$(CC) $(CD)disk_windows.c $(OPT)

$(CD)disk_fxs.o: $(CD)disk_fxs.c $(CD)disk.h
	$(CC) $(CD)disk_fxs.c $(OPT)

$(CD)disk.o: $(CD)disk.c $(CD)disk.h
	$(CC) $(CD)disk.c $(OPT)

$(CD)disk_warea.o: $(CD)disk_warea.c $(CD)disk.h
	$(CC) $(CD)disk_warea.c $(OPT)

$(CD)disk_save.o: $(CD)disk_save.c config/config.h
	$(CC) $(CD)disk_save.c $(OPT)

$(CD)disk_load.o: $(CD)disk_load.c config/config.h
	$(CC) $(CD)disk_load.c $(OPT)

$(CD)trackreallineelements.o: $(CD)trackreallineelements.c
	$(CC) $(CD)trackreallineelements.c $(OPT)

$(CD)control.o: $(CD)control.c
	$(CC) $(CD)control.c $(OPT)

$(CD)disk_instrument.o: $(CD)disk_instrument.c $(CD)disk.h
	$(CC) $(CD)disk_instrument.c $(OPT)

$(CD)disk_patches.o: $(CD)disk_patches.c $(CD)disk.h
	$(CC) $(CD)disk_patches.c $(OPT)

$(AD)plug-ins/camd_get_clustername.o: $(AD)plug-ins/camd_get_clustername.c $(AD)plug-ins/camd_i_plugin.h
	$(CC) $(AD)plug-ins/camd_get_clustername.c $(OPT)

$(AD)plug-ins/disk_camd_i_plugin.o: $(AD)plug-ins/disk_camd_i_plugin.c $(AD)plug-ins/camd_i_plugin.h
	$(CC) $(AD)plug-ins/disk_camd_i_plugin.c $(OPT)

$(AD)plug-ins/camd_getMidiLink.o: $(AD)plug-ins/camd_getMidiLink.c $(AD)plug-ins/camd_i_plugin.h
	$(CC) $(AD)plug-ins/camd_getMidiLink.c $(OPT)

$(CD)disk_slider.o: $(CD)disk_slider.c
	$(CC) $(CD)disk_slider.c $(OPT)

$(AD)Amiga_endprogram.o: $(AD)Amiga_endprogram.c
	$(CC) $(AD)Amiga_endprogram.c $(OPT)

$(CD)clipboard_track_copy.o: $(CD)clipboard_track_copy.c
	$(CC) $(CD)clipboard_track_copy.c $(OPT)

$(CD)clipboard_track_paste.o: $(CD)clipboard_track_paste.c
	$(CC) $(CD)clipboard_track_paste.c $(OPT)

$(CD)clipboard_track_cut.o: $(CD)clipboard_track_cut.c
	$(CC) $(CD)clipboard_track_cut.c $(OPT)

$(CD)lines.o: $(CD)lines.c
	$(CC) $(CD)lines.c $(OPT)

$(CD)notes_legalize.o: $(CD)notes_legalize.c
	$(CC) $(CD)notes_legalize.c $(OPT)

$(CD)temponodes_legalize.o: $(CD)temponodes_legalize.c
	$(CC) $(CD)temponodes_legalize.c $(OPT)

$(CD)fxlines_legalize.o: $(CD)fxlines_legalize.c
	$(CC) $(CD)fxlines_legalize.c $(OPT)

$(CD)reallines_insert.o: $(CD)reallines_insert.c
	$(CC) $(CD)reallines_insert.c $(OPT)

$(CD)block_properties.o: $(CD)block_properties.c
	$(CC) $(CD)block_properties.c $(OPT)

$(CD)clipboard_tempos_copy.o: $(CD)clipboard_tempos_copy.c
	$(CC) $(CD)clipboard_tempos_copy.c $(OPT)

$(CD)font.o: $(CD)font.c
	$(CC) $(CD)font.c $(OPT)

$(CD)clipboard_localzooms.o: $(CD)clipboard_localzooms.c
	$(CC) $(CD)clipboard_localzooms.c $(OPT)

$(CD)clipboard_block_copy.o: $(CD)clipboard_block_copy.c
	$(CC) $(CD)clipboard_block_copy.c $(OPT)

$(CD)clipboard_block_paste.o: $(CD)clipboard_block_paste.c
	$(CC) $(CD)clipboard_block_paste.c $(OPT)

$(CD)undo.o: $(CD)undo.c $(CD)undo.h
	$(CC) $(CD)undo.c $(OPT) define MAX_NUM_UNDOS=$(MAX_NUM_UNDOS)

$(CD)undo_notes.o: $(CD)undo_notes.c $(CD)undo.h
	$(CC) $(CD)undo_notes.c $(OPT)

$(CD)undo_fxs.o: $(CD)undo_fxs.c $(CD)undo.h
	$(CC) $(CD)undo_fxs.c $(OPT)

$(CD)undo_temponodes.o: $(CD)undo_temponodes.c $(CD)undo.h
	$(CC) $(CD)undo_temponodes.c $(OPT)

$(CD)undo_tempos.o: $(CD)undo_tempos.c $(CD)undo.h
	$(CC) $(CD)undo_tempos.c $(OPT)

$(CD)undo_lpbs.o: $(CD)undo_lpbs.c $(CD)undo.h
	$(CC) $(CD)undo_lpbs.c $(OPT)

$(CD)undo_notesandfxs.o: $(CD)undo_notesandfxs.c $(CD)undo.h
	$(CC) $(CD)undo_notesandfxs.c $(OPT)

$(CD)undo_reallines.o: $(CD)undo_reallines.c $(CD)undo.h
	$(CC) $(CD)undo_reallines.c $(OPT)

$(CD)undo_tracks.o: $(CD)undo_tracks.c $(CD)undo.h
	$(CC) $(CD)undo_tracks.c $(OPT)

$(CD)undo_range.o: $(CD)undo_range.c
	$(CC) $(CD)undo_range.c $(OPT)

$(CD)undo_blocks.o: $(CD)undo_blocks.c
	$(CC) $(CD)undo_blocks.c $(OPT)

$(CD)time2place.o: $(CD)time2place.c
	$(CC) $(CD)time2place.c $(OPT)

plug-ins/midicluster.o: $(AD)plug-ins/midicluster.a
	sc plug-ins/midicluster.a

$(CD)nodeboxes.o: $(CD)nodeboxes.c
	$(CC) $(CD)nodeboxes.c $(OPT)

$(CD)PEQ_calc_64bit.o: $(CD)PEQ_calc_64bit.c
	$(CC) $(CD)PEQ_calc_64bit.c $(OPT)

$(AD)64bit.o: $(AD)64bit.a
	$(CC) $(AD)64bit.a $(OPT)

$(CD)track_insert.o: $(CD)track_insert.c
	$(CC) $(CD)track_insert.c $(OPT)

$(AD)Amiga_error.o: $(AD)Amiga_error.c
	$(CC) $(AD)Amiga_error.c $(OPT)

$(CD)quantitize.o: $(CD)quantitize.c
	$(CC) $(CD)quantitize.c $(OPT)

$(CD)gfx_wtrackheader_volpan.o: $(CD)gfx_wtrackheader_volpan.c
	$(CC) $(CD)gfx_wtrackheader_volpan.c $(OPT)

#Note, Sas/C makes muforce spit out a lot of "WORD READ from $xxxxxxxx" messages 
#if don't have the "noopt" keyword here.
$(CD)mouse_wtrackheader.o: $(CD)mouse_wtrackheader.c
	$(CC) $(CD)mouse_wtrackheader.c $(OPT) noopt

$(CD)gfx_tempotrackheader.o: $(CD)gfx_tempotrackheader.c
	$(CC) $(CD)gfx_tempotrackheader.c $(OPT)

$(CD)undo_trackheader.o: $(CD)undo_trackheader.c
	$(CC) $(CD)undo_trackheader.c $(OPT)

$(CD)mouse_tempoheader.o: $(CD)mouse_tempoheader.c
	$(CC) $(CD)mouse_tempoheader.c $(OPT)

$(CD)undo_reltempomax.o: $(CD)undo_reltempomax.c
	$(CC) $(CD)undo_reltempomax.c $(OPT)

$(CD)undo_maintempos.o: $(CD)undo_maintempos.c
	$(CC) $(CD)undo_maintempos.c $(OPT)

$(CD)gfx_upperleft.o: $(CD)gfx_upperleft.c
	$(CC) $(CD)gfx_upperleft.c $(OPT)

$(CD)mouse_quantitize.o: $(CD)mouse_quantitize.c
	$(CC) $(CD)mouse_quantitize.c $(OPT)

$(CD)gfx_window_title.o: $(CD)gfx_window_title.c
	$(CC) $(CD)gfx_window_title.c $(OPT)

$(CD)window_config.o: $(CD)window_config.c
	$(CC) $(CD)window_config.c $(OPT)

$(CD)PEQ_clock.o: $(CD)PEQ_clock.c $(CD)playerclass.h
	$(CC) $(CD)PEQ_clock.c $(OPT)

$(AD)Amiga_Semaphores.o: $(AD)Amiga_Semaphores.c
	$(CC) $(AD)Amiga_Semaphores.c $(OPT)

$(AD)plug-ins/camd_i_input.o: $(AD)plug-ins/camd_i_input.c $(AD)plug-ins/camd_i_plugin.h
	$(CC) $(AD)plug-ins/camd_i_input.c $(OPT)

$(CD)block_insert.o: $(CD)block_insert.c
	$(CC) $(CD)block_insert.c $(OPT)

$(CD)block_delete.o: $(CD)block_delete.c
	$(CC) $(CD)block_delete.c $(OPT)

$(CD)undo_block_insertdelete.o: $(CD)undo_block_insertdelete.c
	$(CC) $(CD)undo_block_insertdelete.c $(OPT)

$(CD)track_onoff.o: $(CD)track_onoff.c
	$(CC) $(CD)track_onoff.c $(OPT)

$(AD)instrprop/Amiga_instrprop.o: $(AD)instrprop/Amiga_instrprop.c $(AD)plug-ins/camd_i_plugin.h
	$(CC) $(AD)instrprop/Amiga_instrprop.c $(OPT) ignore=147 ignore=178 ignore=154 ignore=120 ignore=93

$(AD)instrprop/Amiga_instrprop_temp_help.o: $(AD)instrprop/Amiga_instrprop_temp_help.c $(AD)plug-ins/camd_i_plugin.h
	$(CC) $(AD)instrprop/Amiga_instrprop_temp_help.c $(OPT) parm=both

$(AD)instrprop/Amiga_instrprop_temp_camd.o: $(AD)instrprop/Amiga_instrprop_temp_camd.c $(AD)plug-ins/camd_i_plugin.h
	$(CC) $(AD)instrprop/Amiga_instrprop_temp_camd.c $(OPT) parm=both

$(AD)instrprop/Amiga_instrprop_edit.o: $(AD)instrprop/Amiga_instrprop_edit.c $(AD)instrprop/Amiga_instrprop.h $(AD)plug-ins/camd_i_plugin.h
	$(CC) $(AD)instrprop/Amiga_instrprop_edit.c $(OPT)

$(AD)plug-ins/disk_camd_mymidilinks.o: $(AD)plug-ins/disk_camd_mymidilinks.c $(AD)plug-ins/camd_i_plugin.h
	$(CC) $(AD)plug-ins/disk_camd_mymidilinks.c $(OPT)

$(AD)disk_amiga.o: $(AD)disk_amiga.c $(AD)plug-ins/camd_i_plugin.h
	$(CC) $(AD)disk_amiga.c $(OPT)

$(AD)Amiga_config.o: $(AD)Amiga_config.c
	$(CC) $(AD)Amiga_config.c $(OPT)

$(AD)plug-ins/camd_playfromstart.o: $(AD)plug-ins/camd_playfromstart.c $(AD)plug-ins/camd_i_plugin.h
	$(CC) $(AD)plug-ins/camd_playfromstart.c $(OPT)

$(AD)Amiga_colors.o: $(AD)Amiga_colors.c
	$(CC) $(AD)Amiga_colors.c $(OPT)

$(AD)plug-ins/camd_fx.o: $(AD)plug-ins/camd_fx.c $(AD)plug-ins/camd_i_plugin.h
	$(CC) $(AD)plug-ins/camd_fx.c $(OPT)

$(AD)instrprop/Amiga_instrprop_camd_menues.o: $(AD)instrprop/Amiga_instrprop_camd_menues.c
	$(CC) $(AD)instrprop/Amiga_instrprop_camd_menues.c $(OPT)

$(AD)instrprop/Amiga_instrprop_init.o: $(AD)instrprop/Amiga_instrprop_init.c $(AD)plug-ins/camd_i_plugin.h
	$(CC) $(AD)instrprop/Amiga_instrprop_init.c $(OPT)  ignore=147 ignore=178 ignore=154 ignore=120 ignore=93

$(AD)plug-ins/disk_camd_fx.o: $(AD)plug-ins/disk_camd_fx.c $(CD)disk.h $(AD)plug-ins/camd_i_plugin.h
	$(CC) $(AD)plug-ins/disk_camd_fx.c $(OPT)

mmd2loader/mmd2load.o: mmd2loader/mmd2load.c
	$(CC) mmd2loader/mmd2load.c $(OPT) ignore=108 disassemble=mmd2loader/mmd2load.dis

$(CD)new/backwards.o: $(CD)new/backwards.c
	$(CC) $(CD)new/backwards.c $(OPT)

$(CD)new/invert.o: $(CD)new/invert.c
	$(CC) $(CD)new/invert.c $(OPT)

$(CD)gfx_slider.o: $(CD)gfx_slider.c
	$(CC) $(CD)gfx_slider.c $(OPT)

$(CD)gfx_wblocks_reltempo.o: $(CD)gfx_wblocks_reltempo.c
	$(CC) $(CD)gfx_wblocks_reltempo.c $(OPT)

$(CD)undo_reltemposlider.o: $(CD)undo_reltemposlider.c
	$(CC) $(CD)undo_reltemposlider.c $(OPT)

$(CD)mouse_reltemposlider.o: $(CD)mouse_reltemposlider.c
	$(CC) $(CD)mouse_reltemposlider.c $(OPT)

$(AD)Amiga_memory.o: $(AD)Amiga_memory.c
	$(CC) $(AD)Amiga_memory.c $(OPT)

$(CD)blts.o: $(CD)blts.c
	$(CC) $(CD)blts.c $(OPT)

$(CD)scroll.o: $(CD)scroll.c
	$(CC) $(CD)scroll.c $(OPT)

$(CD)pixmap.o: $(CD)pixmap.c
	$(CC) $(CD)pixmap.c $(OPT)

$(CD)gfx_shrink.o: $(CD)gfx_shrink.c
	$(CC) $(CD)gfx_shrink.c $(OPT)

$(CD)gfx_shrink_t.o: $(CD)gfx_shrink_t.c
	$(CC) $(CD)gfx_shrink_t.c $(OPT)

$(CD)scroll_play.o: $(CD)scroll_play.c
	$(CC) $(CD)scroll_play.c $(OPT)

$(CD)gfx_tempocolor.o: $(CD)gfx_tempocolor.c
	$(CC) $(CD)gfx_tempocolor.c $(OPT)

$(CD)block_split.o: $(CD)block_split.c
	$(CC) $(CD)block_split.c $(OPT)

$(CD)undo_block_mergesplit.o: $(CD)undo_block_mergesplit.c
	$(CC) $(CD)undo_block_mergesplit.c $(OPT)

$(CD)new/glissando.o: $(CD)new/glissando.c
	$(CC) $(CD)new/glissando.c $(OPT)




$(API)api_common.o:  $(API)api_common.c
	$(CC)  $(API)api_common.c $(OPT) $(PYPATH)

$(API)api_keyplay.o:  $(API)api_keyplay.c $(API)radium_proc.h
	$(CC)  $(API)api_keyplay.c $(OPT) $(PYPATH)

$(API)api_keyplayedit.o:  $(API)api_keyplayedit.c $(API)radium_proc.h
	$(CC)  $(API)api_keyplayedit.c $(OPT) $(PYPATH)

$(API)api_navigate.o:  $(API)api_navigate.c $(API)radium_proc.h
	$(CC)  $(API)api_navigate.c $(OPT) $(PYPATH)

$(API)api_noteedit.o:  $(API)api_noteedit.c $(API)radium_proc.h
	$(CC)  $(API)api_noteedit.c $(OPT) $(PYPATH)

$(API)api_trackonoff.o:  $(API)api_trackonoff.c $(API)radium_proc.h
	$(CC)  $(API)api_trackonoff.c $(OPT) $(PYPATH)

$(API)api_zoom.o:  $(API)api_zoom.c $(API)radium_proc.h
	$(CC)  $(API)api_zoom.c $(OPT) $(PYPATH)

$(API)api_notemanipulate.o:  $(API)api_notemanipulate.c $(API)radium_proc.h
	$(CC)  $(API)api_notemanipulate.c $(OPT) $(PYPATH)

$(API)api_play.o:  $(API)api_play.c $(API)radium_proc.h
	$(CC)  $(API)api_play.c $(OPT) $(PYPATH)

$(API)api_clipboard.o:  $(API)api_clipboard.c $(API)radium_proc.h
	$(CC)  $(API)api_clipboard.c $(OPT) $(PYPATH)

$(API)api_undo.o:  $(API)api_undo.c $(API)radium_proc.h
	$(CC)  $(API)api_undo.c $(OPT) $(PYPATH)

$(API)api_various.o:  $(API)api_various.c $(API)radium_proc.h
	$(CC)  $(API)api_various.c $(OPT) $(PYPATH)

$(API)radium_wrap.o:  $(API)radium_wrap.c $(API)radium_proc.h
	$(CC)  $(API)radium_wrap.c $(OPT) $(PYPATH) ignore=88


$(API)radium_wrap.c: $(API)protos.conf $(API)protoconfparser.py $(API)radium_wrap/radium_wrap_1.c $(API)radium_wrap/radium_wrap_3.c $(API)makeradiumwrap_c.sh $(API)makeradiumwrap_c.sh $(API)cat
	execute <<
	cd $(API)
	execute makeradiumwrap_c.sh
	cd ..
	<<

$(API)cat:
	copy c:type $(API)cat
	protect $(API)cat RWED

$(API)api_support.o:  $(API)api_support.c
	$(CC)  $(API)api_support.c $(OPT) $(PYPATH)

$(API)wrapfunclist.o: $(API)wrapfunclist.c
	$(CC) $(API)wrapfunclist.c $(OPT) $(PYPATH)

$(API)wrapfunclist.c: $(API)protos.conf $(API)protoconfparser.py
	echo "python protoconfparser.py wrapfunclist.c"
	execute <<
	cd $(API)
	python protoconfparser.py wrapfunclist.c
	cd ..
	<<

$(API)radium.i: $(API)protos.conf $(API)protoconfparser.py
	echo "python protoconfparser.py radium.i"
	execute <<
	cd $(API)
	python protoconfparser.py radium.i
	cd ..
	<<

$(API)radium_proc.h: $(API)protos.conf $(API)protoconfparser.py
	echo "python protoconfparser.py radium_proc.h"
	execute <<
	cd $(API)
	python protoconfparser.py radium_proc.h
	cd ..
	<<




advanced/ad_noteadd.o: advanced/ad_noteadd.c
	$(CC) advanced/ad_noteadd.c $(OPT) $(PYPATH)




#$(API).o: $(API).c
#	$(CC) $(API).c $(OPT)