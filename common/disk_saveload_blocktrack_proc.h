
#ifndef _RADIUM_COMMON_DISK_SAVELOAD_BLOCKTRACK_PROC_H
#define _RADIUM_COMMON_DISK_SAVELOAD_BLOCKTRACK_PROC_H

void SaveBlockToDisk(filepath_t filename, const struct WBlocks *block); // filename can be NULL or empty string
void LoadBlockFromDisk(filepath_t filename_c); // filename can be NULL or empty string

void SaveTrackToDisk(filepath_t filename_c, const struct WTracks *wtrack);
void LoadTrackFromDisk(filepath_t filename_c, struct Tracker_Windows *window, struct WBlocks *wblock, struct WTracks *old_wtrack);
  
#endif

