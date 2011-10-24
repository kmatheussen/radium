#Have no idea how good this one works now. Problem is that I don't have
#enough memory to compile up this version anymore. Which allso means
#that tracker.c is not updated.


CPU=68000

OPT=strict ansi CPU=$(CPU) optimize optimizertime \
optimizerinline \
optglobal \
optloop \
optimizercomplexity=100 \
optimizerdepth=100 \
optimizerinlocal \
optimizerpeephole \
code=far \
optimizerrecurdepth=100 \
optsched \
ignore=51
# define TRACKER_DEBUG

# define TRACKER_PROFILE 1 \
# profile

tracker$(CPU): tracker.c smakefile_include.smk
	sc math=s link tracker.c $(OPT) LIB LIB:gc$(CPU).lib TO tracker$(CPU)


#tracker: tracker_in.c
#	sc link tracker_in.c $(OPT) LIB:sc.lib LIB:amiga.lib TO tracker$(CPU)

#tracker_in.c: GFX_Amiga_egc.c list.c mempools.c common.c nodelist.c nodelines.c gfx_wtext.c gfx_wtracks.c gfx_wblocks.c notestext.c trackreallines.c notes.c localzooms.c reallines.c tracks.c tempos.c blocks.c sliders.c wtracks.c wblocks.c playlist.c instruments.c windows.c song.c mouse.c eventreciever.c main.c debug_proc.h smakefile_include.smk
#	join GFX_Amiga_egc.c list.c mempools.c common.c nodelist.c nodelines.c gfx_wtext.c gfx_wtracks.c gfx_wblocks.c trackreallines.c notestext.c notes.c localzooms.c reallines.c tracks.c tempos.c blocks.c sliders.c wtracks.c wblocks.c playlist.c instruments.c windows.c song.c mouse.c eventreciever.c main.c TO tracker_in.c


