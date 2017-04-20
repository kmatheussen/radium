
#ifndef _RADIUM_COMMON_DISK_SAVELOAD_BLOCKTRACK_PROC_H
#define _RADIUM_COMMON_DISK_SAVELOAD_BLOCKTRACK_PROC_H

void SaveBlockToDisk(const char *filename, struct WBlocks *block); // filename can be NULL or empty string
void LoadBlockFromDisk(const char *filename_c); // filename can be NULL or empty string

  
#endif

