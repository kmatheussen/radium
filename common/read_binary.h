/* Copyright 2012-2013 Kjetil S. Matheussen

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


#ifndef RADIUM_COMMON_READ_BINARY_H
#define RADIUM_COMMON_READ_BINARY_H


#include <stdio.h>
#include <stdint.h>

#include "OS_disk_proc.h"


// based on code from soundtracker
static inline int32_t get_le_32 (unsigned char *src)
{
#if IS_LITTLE_ENDIAN
    return *(int32_t*)src;
#else
    return (src[0] << 0) + (src[1] << 8) + (src[2] << 16) + (src[3] << 24);
#endif
}

static inline void put_le_32 (unsigned char *src, int32_t value)
{
#if IS_LITTLE_ENDIAN
  memcpy(src, &value, 4);
#else
#error "not implemented"
#endif
}

static inline int64_t get_le_64 (unsigned char *src)
{
#if IS_LITTLE_ENDIAN
    return *(int64_t*)src;
#else
#error "not implemented"
#endif
}

static inline void put_le_64 (unsigned char *src, int64_t value)
{
#if IS_LITTLE_ENDIAN
  memcpy(src, &value, 8);
#else
#error "not implemented"
#endif
}

static inline uint32_t get_le_u32 (unsigned char *src)
{
#if IS_LITTLE_ENDIAN
    return *(int32_t*)src;
#else
    return (src[0] << 0) + (src[1] << 8) + (src[2] << 16) + (src[3] << 24);
#endif
}

static inline int32_t read_le32int(disk_t *file){
  unsigned char size_chars[4] = {0};
  if(DISK_read_binary(file, size_chars, 4) != 4)
    fprintf(stderr,"Reading file failed\n");
  return get_le_32(size_chars);
}

static inline uint32_t read_le32uint(disk_t *file){
  unsigned char size_chars[4] = {0};
  if(DISK_read_binary(file, size_chars, 4) != 4)
    fprintf(stderr,"Reading file failed\n");
  return get_le_u32(size_chars);
}

static inline uint32_t get_be_u32 (unsigned char *src)
{
#if IS_LITTLE_ENDIAN
    return (src[3] << 0) + (src[2] << 8) + (src[1] << 16) + (src[0] << 24);
#else
    return *(uint32_t*)src;
#endif
}

static inline uint32_t read_be32uint(disk_t *file){
  unsigned char size_chars[4] = {0};
  if(DISK_read_binary(file, size_chars, 4) != 4)
    fprintf(stderr,"Reading file failed\n");
  return get_be_u32(size_chars);
}


static inline uint16_t get_be_u16 (unsigned char *src)
{
#if IS_LITTLE_ENDIAN
    return (src[1] << 0) + (src[0] << 8);
#else
    return *(uint16_t*)src;
#endif
}

static inline uint16_t read_be16uint(disk_t *file){
  unsigned char size_chars[2] = {0};
  if(DISK_read_binary(file, size_chars, 2) != 2)
    fprintf(stderr,"Reading file failed\n");
  return get_be_u16(size_chars);
}

// based on code from soundtracker
static inline int16_t get_le_16 (unsigned char *src)
{
#if IS_LITTLE_ENDIAN
    return *(int16_t*)src;
#else
    return (src[0] << 0) + (src[1] << 8);
#endif
}

static inline void convert_16_bit_little_endian_to_native(int16_t *src, int num_frames){
#if IS_LITTLE_ENDIAN
  return;
#else
#error "Something is probably wrong. We dont target any big endian platforms."
  int i;
  for(i=0;i<num_frames;i++){
    src[i] = get_le_16(&src[i]);
  }
#endif
}

static inline int read_le16int(disk_t *file){
  unsigned char size_chars[2] = {};
  if(DISK_read_binary(file, size_chars, 2) != 2)
    fprintf(stderr,"Reading file failed\n");
  return get_le_16(size_chars);
}


static inline unsigned int read_8int(disk_t *file){
  unsigned char size_chars[1] = {};
  if(DISK_read_binary(file, size_chars, 1) != 1)
    fprintf(stderr,"Reading file failed\n");
  return size_chars[0];
}

static inline int read_8int_signed(disk_t *file){
  int8_t size_chars[1] = {};
  if(DISK_read_binary(file, size_chars, 1) !=1 )
    fprintf(stderr,"Reading file failed\n");
  return size_chars[0];
}

#endif // RADIUM_COMMON_READ_BINARY_H
