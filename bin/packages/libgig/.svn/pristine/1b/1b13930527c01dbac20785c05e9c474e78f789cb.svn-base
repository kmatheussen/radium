/***************************************************************************
 *                                                                         *
 *   libgig - C++ cross-platform Gigasampler format file access library    *
 *                                                                         *
 *   Copyright (C) 2003-2011 by Christian Schoenebeck                      *
 *                              <cuse@users.sourceforge.net>               *
 *                                                                         *
 *   This library is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This library is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this library; if not, write to the Free Software           *
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston,                 *
 *   MA  02111-1307  USA                                                   *
 ***************************************************************************/

#ifndef __RIFF_H__
#define __RIFF_H__

#ifdef WIN32
# define POSIX 0
#endif

#ifndef POSIX
# define POSIX 1
#endif

#ifndef DEBUG
# define DEBUG 0
#endif

#include <string>
#include <list>
#include <map>
#include <iostream>

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#if POSIX
# include <sys/types.h>
# include <sys/stat.h>
# include <fcntl.h>
# include <unistd.h>
#endif // POSIX

#ifdef _MSC_VER
// Visual C++ 2008 doesn't have stdint.h
typedef __int8 int8_t;
typedef __int16 int16_t;
typedef __int32 int32_t;
typedef __int64 int64_t;
typedef unsigned __int8 uint8_t;
typedef unsigned __int16 uint16_t;
typedef unsigned __int32 uint32_t;
typedef unsigned __int64 uint64_t;
#else
#include <stdint.h>
#endif

#ifdef WIN32
# include <windows.h>
  typedef unsigned int   uint;
#endif // WIN32

#include <stdio.h>

#if WORDS_BIGENDIAN
# define CHUNK_ID_RIFF	0x52494646
# define CHUNK_ID_RIFX	0x52494658
# define CHUNK_ID_LIST	0x4C495354

# define LIST_TYPE_INFO	0x494E464F
# define CHUNK_ID_ICMT	0x49434D54
# define CHUNK_ID_ICOP	0x49434F50
# define CHUNK_ID_ICRD	0x49435244
# define CHUNK_ID_IENG	0x49454E47
# define CHUNK_ID_INAM	0x494E414D
# define CHUNK_ID_IPRD	0x49505244
# define CHUNK_ID_ISFT	0x49534654

# define CHUNK_ID_SMPL	0x736D706C

#else  // little endian
# define CHUNK_ID_RIFF	0x46464952
# define CHUNK_ID_RIFX	0x58464952
# define CHUNK_ID_LIST	0x5453494C

# define LIST_TYPE_INFO	0x4F464E49
# define CHUNK_ID_ICMT	0x544D4349
# define CHUNK_ID_ICOP	0x504F4349
# define CHUNK_ID_ICRD	0x44524349
# define CHUNK_ID_IENG	0x474E4549
# define CHUNK_ID_INAM	0x4D414E49
# define CHUNK_ID_IPRD	0x44525049
# define CHUNK_ID_ISFT	0x54465349

# define CHUNK_ID_SMPL	0x6C706D73

#endif // WORDS_BIGENDIAN

#define CHUNK_HEADER_SIZE	8
#define LIST_HEADER_SIZE	12
#define RIFF_HEADER_SIZE	12


/**
 * @brief RIFF specific classes and definitions
 *
 * The Resource Interchange File Format (RIFF) is a generic tree-structured
 * meta-format which stores data in so called "chunks". It can be compared
 * to XML, but in contrast to XML, RIFF is entirely binary encoded, that is
 * not ASCII based. RIFF is used as basis for many file formats like AVI,
 * WAV, DLS and of course the Gigasampler file format. ;-)
 *
 * RIFF chunks can be seen as containers for data. There are two distinct
 * types of chunks:
 *
 * - @e ordinary @e chunks are the leafs of the data tree which encapsulate
 *   the actual data of the file (i.e. the sample data of a .wav file)
 *
 * - @e list @e chunks are the nodes of the data tree which hold an
 *   arbitrary amount of subchunks (can be both, list chunks and/or ordinary
 *   chunks)
 */
namespace RIFF {

    /* just symbol prototyping */
    class Chunk;
    class List;
    class File;

    typedef std::string String;

    /** Whether file stream is open in read or in read/write mode. */
    typedef enum {
        stream_mode_read       = 0,
        stream_mode_read_write = 1,
        stream_mode_closed     = 2
    } stream_mode_t;

    /** Current state of the file stream. */
    typedef enum {
        stream_ready       = 0,
        stream_end_reached = 1,
        stream_closed      = 2
    } stream_state_t;

    /** File stream position dependent to these relations. */
    typedef enum {
        stream_start    = 0,
        stream_curpos   = 1,
        stream_backward = 2,
        stream_end      = 3
    } stream_whence_t;

    /** Alignment of data bytes in memory (system dependant). */
    typedef enum {
        endian_little = 0,
        endian_big    = 1,
        endian_native = 2
    } endian_t;

    /** @brief Ordinary RIFF Chunk
     *
     * Provides convenient methods to access data of ordinary RIFF chunks
     * in general.
     */
    class Chunk {
        public:
            Chunk(File* pFile, unsigned long StartPos, List* Parent);
            String         GetChunkIDString();
            uint32_t       GetChunkID() { return ChunkID; }             ///< Chunk ID in unsigned integer representation.
            List*          GetParent()  { return pParent; }             ///< Returns pointer to the chunk's parent list chunk.
            unsigned long  GetSize()    { return CurrentChunkSize; }    ///< Chunk size in bytes (without header, thus the chunk data body)
            unsigned long  GetNewSize() { return NewChunkSize;     }    ///< New chunk size if it was modified with Resize().
            unsigned long  GetPos()     { return ulPos; }               ///< Position within the chunk data body
            unsigned long  GetFilePos() { return ulStartPos + ulPos; }  ///< Current, actual offset in file.
            unsigned long  SetPos(unsigned long Where, stream_whence_t Whence = stream_start);
            unsigned long  RemainingBytes();
            stream_state_t GetState();
            unsigned long  Read(void* pData, unsigned long WordCount, unsigned long WordSize);
            unsigned long  ReadInt8(int8_t* pData,     unsigned long WordCount = 1);
            unsigned long  ReadUint8(uint8_t* pData,   unsigned long WordCount = 1);
            unsigned long  ReadInt16(int16_t* pData,   unsigned long WordCount = 1);
            unsigned long  ReadUint16(uint16_t* pData, unsigned long WordCount = 1);
            unsigned long  ReadInt32(int32_t* pData,   unsigned long WordCount = 1);
            unsigned long  ReadUint32(uint32_t* pData, unsigned long WordCount = 1);
            int8_t         ReadInt8();
            uint8_t        ReadUint8();
            int16_t        ReadInt16();
            uint16_t       ReadUint16();
            int32_t        ReadInt32();
            uint32_t       ReadUint32();
            unsigned long  Write(void* pData, unsigned long WordCount, unsigned long WordSize);
            unsigned long  WriteInt8(int8_t* pData,     unsigned long WordCount = 1);
            unsigned long  WriteUint8(uint8_t* pData,   unsigned long WordCount = 1);
            unsigned long  WriteInt16(int16_t* pData,   unsigned long WordCount = 1);
            unsigned long  WriteUint16(uint16_t* pData, unsigned long WordCount = 1);
            unsigned long  WriteInt32(int32_t* pData,   unsigned long WordCount = 1);
            unsigned long  WriteUint32(uint32_t* pData, unsigned long WordCount = 1);
            void*          LoadChunkData();
            void           ReleaseChunkData();
            void           Resize(int iNewSize);
            virtual ~Chunk();
        protected:
            uint32_t      ChunkID;
            uint32_t      CurrentChunkSize;		/* in bytes */
            uint32_t      NewChunkSize;			/* in bytes (if chunk was scheduled to be resized) */
            List*         pParent;
            File*         pFile;
            unsigned long ulStartPos;		/* actual position in file where chunk (without header) starts */
            unsigned long ulPos; 		/* # of bytes from ulStartPos */
            uint8_t*      pChunkData;
            unsigned long ulChunkDataSize;

            Chunk(File* pFile);
            Chunk(File* pFile, List* pParent, uint32_t uiChunkID, uint uiBodySize);
            void          ReadHeader(unsigned long fPos);
            void          WriteHeader(unsigned long fPos);
            unsigned long ReadSceptical(void* pData, unsigned long WordCount, unsigned long WordSize);
            inline void   swapBytes_16(void* Word) {
                uint8_t byteCache = *((uint8_t*) Word);
                *((uint8_t*) Word)     = *((uint8_t*) Word + 1);
                *((uint8_t*) Word + 1) = byteCache;
            }
            inline void   swapBytes_32(void* Word) {
                uint8_t byteCache = *((uint8_t*) Word);
                *((uint8_t*) Word)     = *((uint8_t*) Word + 3);
                *((uint8_t*) Word + 3) = byteCache;
                byteCache = *((uint8_t*) Word + 1);
                *((uint8_t*) Word + 1) = *((uint8_t*) Word + 2);
                *((uint8_t*) Word + 2) = byteCache;
            }
            inline void   swapBytes(void* Word, unsigned long WordSize) {
                uint8_t byteCache;
                unsigned long lo = 0, hi = WordSize - 1;
                for (; lo < hi; hi--, lo++) {
                    byteCache = *((uint8_t*) Word + lo);
                    *((uint8_t*) Word + lo) = *((uint8_t*) Word + hi);
                    *((uint8_t*) Word + hi) = byteCache;
                }
            }
            inline String convertToString(uint32_t word) {
                String result;
                for (int i = 0; i < 4; i++) {
                    uint8_t byte = *((uint8_t*)(&word) + i);
                    char c = byte;
                    result += c;
                }
                return result;
            }
            virtual unsigned long WriteChunk(unsigned long ulWritePos, unsigned long ulCurrentDataOffset);
            virtual void __resetPos(); ///< Sets Chunk's read/write position to zero.

            friend class List;
    };

    /** @brief RIFF List Chunk
     *
     * Provides convenient methods to access data of RIFF list chunks and
     * their subchunks.
     */
    class List : public Chunk {
        public:
            List(File* pFile, unsigned long StartPos, List* Parent);
            String       GetListTypeString();
            uint32_t     GetListType() { return ListType; }   ///< Returns unsigned integer representation of the list's ID
            Chunk*       GetSubChunk(uint32_t ChunkID);
            List*        GetSubList(uint32_t ListType);
            Chunk*       GetFirstSubChunk();
            Chunk*       GetNextSubChunk();
            List*        GetFirstSubList();
            List*        GetNextSubList();
            unsigned int CountSubChunks();
            unsigned int CountSubChunks(uint32_t ChunkID);
            unsigned int CountSubLists();
            unsigned int CountSubLists(uint32_t ListType);
            Chunk*       AddSubChunk(uint32_t uiChunkID, uint uiBodySize);
            List*        AddSubList(uint32_t uiListType);
            void         DeleteSubChunk(Chunk* pSubChunk);
            void         MoveSubChunk(Chunk* pSrc, Chunk* pDst);
            virtual ~List();
        protected:
            typedef std::map<uint32_t, RIFF::Chunk*>  ChunkMap;
            typedef std::list<Chunk*>                 ChunkList;

            uint32_t   ListType;
            ChunkList* pSubChunks;
            ChunkMap*  pSubChunksMap;
            ChunkList::iterator ChunksIterator;
            ChunkList::iterator ListIterator;

            List(File* pFile);
            List(File* pFile, List* pParent, uint32_t uiListID);
            void ReadHeader(unsigned long fPos);
            void WriteHeader(unsigned long fPos);
            void LoadSubChunks();
            void LoadSubChunksRecursively();
            virtual unsigned long WriteChunk(unsigned long ulWritePos, unsigned long ulCurrentDataOffset);
            virtual void __resetPos(); ///< Sets List Chunk's read/write position to zero and causes all sub chunks to do the same.
            void DeleteChunkList();
    };

    /** @brief RIFF File
     *
     * Handles arbitrary RIFF files and provides together with its base
     * classes convenient methods to walk through, read and modify the
     * file's RIFF tree.
     */
    class File : public List {
        public:
            File(uint32_t FileType);
            File(const String& path);
            stream_mode_t GetMode();
            bool          SetMode(stream_mode_t NewMode);
            void SetByteOrder(endian_t Endian);
            String GetFileName();
            virtual void Save();
            virtual void Save(const String& path);
            virtual ~File();
        protected:
            #if POSIX
            int    hFileRead;  ///< handle / descriptor for reading from file
            int    hFileWrite; ///< handle / descriptor for writing to (some) file
            #elif defined(WIN32)
            HANDLE hFileRead;  ///< handle / descriptor for reading from file
            HANDLE hFileWrite; ///< handle / descriptor for writing to (some) file
            #else
            FILE*  hFileRead;  ///< handle / descriptor for reading from file
            FILE*  hFileWrite; ///< handle / descriptor for writing to (some) file
            #endif // POSIX
            String Filename;
            bool   bEndianNative;

            void LogAsResized(Chunk* pResizedChunk);
            void UnlogResized(Chunk* pResizedChunk);
            friend class Chunk;
            friend class List;
        private:
            stream_mode_t  Mode;
            ChunkList ResizedChunks; ///< All chunks which have been resized (enlarged / shortened).

            unsigned long GetFileSize();
            void ResizeFile(unsigned long ulNewSize);
            #if POSIX
            unsigned long __GetFileSize(int hFile);
            #elif defined(WIN32)
            unsigned long __GetFileSize(HANDLE hFile);
            #else
            unsigned long __GetFileSize(FILE* hFile);
            #endif
            void Cleanup();
    };

    /**
     * Will be thrown whenever an error occurs while handling a RIFF file.
     */
    class Exception {
        public:
            String Message;

            Exception(String Message) { Exception::Message = Message; }
            void PrintMessage();
            virtual ~Exception() {}
    };

    String libraryName();
    String libraryVersion();

} // namespace RIFF
#endif // __RIFF_H__
