#! /local/gnu/bin/bash



#disk_camd_i_plugin_proc.h     
#midicluster.a
#disk_camd_fx.c             
#disk_camd_mymidilinks.c       
#minicamd_proc.h
#disk_camd_fx_proc.h        
#disk_camd_mymidilinks_proc.h  
#nsmtracker.h
#disk_camd_i_plugin.c       
#disk_os_proc.h

elem="
fx.c
getMidiLink.c
i_plugin.c
playfromstart_proc.h
fx_proc.h               
getMidiLink_proc.h  
i_plugin.h       
get_clustername.c       
i_input.c           
i_plugin_proc.h  
get_clustername_proc.h  
i_input_proc.h      
playfromstart.c  
"

for gakk in $elem; do
    mv camd_$gakk midi_$gakk
    #       sed -f ../midi_old/cs.sed <../amiga/plug-ins/$gakk >$gakk
done

#sed -f ../midi_old/cs.sed <../amiga/plug-ins/ >
#sed -f ../midi_old/cs.sed <../amiga/plug-ins/ >

