#ifndef USEMINICAMD

//#include <clib/camd_protos.h>
//#include <pragmas/camd_pragmas.h>
#include <proto/camd.h>

extern struct Library *CamdBase;

#else

#include "../minicamd/init_proc.h"
#include "../minicamd/PutMidi_proc.h"
#include "../minicamd/NextCluster_proc.h"
#include "../minicamd/clusters_proc.h"
#include "../minicamd/AddMidiLink_proc.h"
#include "../minicamd/AddMidiLink_proc.h"
#include "../minicamd/CreateMidi_proc.h"
#include "../minicamd/GetMidi_proc.h"

#endif

