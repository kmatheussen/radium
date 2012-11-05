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

#include <algorithm>
#include <set>
#include <string.h>

#include "RIFF.h"

#include "helper.h"

namespace RIFF {

// *************** Internal functions **************
// *

    /// Returns a human readable path of the given chunk.
    static String __resolveChunkPath(Chunk* pCk) {
        String sPath;
        for (Chunk* pChunk = pCk; pChunk; pChunk = pChunk->GetParent()) {
            if (pChunk->GetChunkID() == CHUNK_ID_LIST) {
                List* pList = (List*) pChunk;
                sPath = "->'" + pList->GetListTypeString() + "'" + sPath;
            } else {
                sPath = "->'" + pChunk->GetChunkIDString() + "'" + sPath;
            }
        }
        return sPath;
    }



// *************** Chunk **************
// *

    Chunk::Chunk(File* pFile) {
        #if DEBUG
        std::cout << "Chunk::Chunk(File* pFile)" << std::endl;
        #endif // DEBUG
        ulPos      = 0;
        pParent    = NULL;
        pChunkData = NULL;
        CurrentChunkSize = 0;
        NewChunkSize = 0;
        ulChunkDataSize = 0;
        ChunkID    = CHUNK_ID_RIFF;
        this->pFile = pFile;
    }

    Chunk::Chunk(File* pFile, unsigned long StartPos, List* Parent) {
        #if DEBUG
        std::cout << "Chunk::Chunk(File*,ulong,bool,List*),StartPos=" << StartPos << std::endl;
        #endif // DEBUG
        this->pFile   = pFile;
        ulStartPos    = StartPos + CHUNK_HEADER_SIZE;
        pParent       = Parent;
        ulPos         = 0;
        pChunkData    = NULL;
        CurrentChunkSize = 0;
        NewChunkSize = 0;
        ulChunkDataSize = 0;
        ReadHeader(StartPos);
    }

    Chunk::Chunk(File* pFile, List* pParent, uint32_t uiChunkID, uint uiBodySize) {
        this->pFile      = pFile;
        ulStartPos       = 0; // arbitrary usually, since it will be updated when we write the chunk
        this->pParent    = pParent;
        ulPos            = 0;
        pChunkData       = NULL;
        ChunkID          = uiChunkID;
        ulChunkDataSize  = 0;
        CurrentChunkSize = 0;
        NewChunkSize     = uiBodySize;
    }

    Chunk::~Chunk() {
        if (pFile) pFile->UnlogResized(this);
        if (pChunkData) delete[] pChunkData;
    }

    void Chunk::ReadHeader(unsigned long fPos) {
        #if DEBUG
        std::cout << "Chunk::Readheader(" << fPos << ") ";
        #endif // DEBUG
        ChunkID = 0;
        NewChunkSize = CurrentChunkSize = 0;
        #if POSIX
        if (lseek(pFile->hFileRead, fPos, SEEK_SET) != -1) {
            read(pFile->hFileRead, &ChunkID, 4);
            read(pFile->hFileRead, &CurrentChunkSize, 4);
        #elif defined(WIN32)
        if (SetFilePointer(pFile->hFileRead, fPos, NULL/*32 bit*/, FILE_BEGIN) != INVALID_SET_FILE_POINTER) {
            DWORD dwBytesRead;
            ReadFile(pFile->hFileRead, &ChunkID, 4, &dwBytesRead, NULL);
            ReadFile(pFile->hFileRead, &CurrentChunkSize, 4, &dwBytesRead, NULL);
        #else
        if (!fseek(pFile->hFileRead, fPos, SEEK_SET)) {
            fread(&ChunkID, 4, 1, pFile->hFileRead);
            fread(&CurrentChunkSize, 4, 1, pFile->hFileRead);
        #endif // POSIX
            #if WORDS_BIGENDIAN
            if (ChunkID == CHUNK_ID_RIFF) {
                pFile->bEndianNative = false;
            }
            #else // little endian
            if (ChunkID == CHUNK_ID_RIFX) {
                pFile->bEndianNative = false;
                ChunkID = CHUNK_ID_RIFF;
            }
            #endif // WORDS_BIGENDIAN
            if (!pFile->bEndianNative) {
                //swapBytes_32(&ChunkID);
                swapBytes_32(&CurrentChunkSize);
            }
            #if DEBUG
            std::cout << "ckID=" << convertToString(ChunkID) << " ";
            std::cout << "ckSize=" << CurrentChunkSize << " ";
            std::cout << "bEndianNative=" << pFile->bEndianNative << std::endl;
            #endif // DEBUG
            NewChunkSize = CurrentChunkSize;
        }
    }

    void Chunk::WriteHeader(unsigned long fPos) {
        uint32_t uiNewChunkID = ChunkID;
        if (ChunkID == CHUNK_ID_RIFF) {
            #if WORDS_BIGENDIAN
            if (pFile->bEndianNative) uiNewChunkID = CHUNK_ID_RIFX;
            #else // little endian
            if (!pFile->bEndianNative) uiNewChunkID = CHUNK_ID_RIFX;
            #endif // WORDS_BIGENDIAN
        }

        uint32_t uiNewChunkSize = NewChunkSize;
        if (!pFile->bEndianNative) {
            swapBytes_32(&uiNewChunkSize);
        }

        #if POSIX
        if (lseek(pFile->hFileWrite, fPos, SEEK_SET) != -1) {
            write(pFile->hFileWrite, &uiNewChunkID, 4);
            write(pFile->hFileWrite, &uiNewChunkSize, 4);
        }
        #elif defined(WIN32)
        if (SetFilePointer(pFile->hFileWrite, fPos, NULL/*32 bit*/, FILE_BEGIN) != INVALID_SET_FILE_POINTER) {
            DWORD dwBytesWritten;
            WriteFile(pFile->hFileWrite, &uiNewChunkID, 4, &dwBytesWritten, NULL);
            WriteFile(pFile->hFileWrite, &uiNewChunkSize, 4, &dwBytesWritten, NULL);
        }
        #else
        if (!fseek(pFile->hFileWrite, fPos, SEEK_SET)) {
            fwrite(&uiNewChunkID, 4, 1, pFile->hFileWrite);
            fwrite(&uiNewChunkSize, 4, 1, pFile->hFileWrite);
        }
        #endif // POSIX
    }

    /**
     *  Returns the String representation of the chunk's ID (e.g. "RIFF",
     *  "LIST").
     */
    String Chunk::GetChunkIDString() {
        return convertToString(ChunkID);
    }

    /**
     *  Sets the position within the chunk body, thus within the data portion
     *  of the chunk (in bytes).
     *
     *  <b>Caution:</b> the position will be reset to zero whenever
     *  File::Save() was called.
     *
     *  @param Where  - position offset (in bytes)
     *  @param Whence - optional: defines to what <i>\a Where</i> relates to,
     *                  if omitted \a Where relates to beginning of the chunk
     *                  data
     */
    unsigned long Chunk::SetPos(unsigned long Where, stream_whence_t Whence) {
     #if DEBUG
     std::cout << "Chunk::SetPos(ulong)" << std::endl;
     #endif // DEBUG
        switch (Whence) {
            case stream_curpos:
                ulPos += Where;
                break;
            case stream_end:
                ulPos = CurrentChunkSize - 1 - Where;
                break;
            case stream_backward:
                ulPos -= Where;
                break;
            case stream_start: default:
                ulPos = Where;
                break;
        }
        if (ulPos > CurrentChunkSize) ulPos = CurrentChunkSize;
        return ulPos;
    }

    /**
     *  Returns the number of bytes left to read in the chunk body.
     *  When reading data from the chunk using the Read*() Methods, the
     *  position within the chunk data (that is the chunk body) will be
     *  incremented by the number of read bytes and RemainingBytes() returns
     *  how much data is left to read from the current position to the end
     *  of the chunk data.
     *
     *  @returns  number of bytes left to read
     */
    unsigned long Chunk::RemainingBytes() {
       #if DEBUG
       std::cout << "Chunk::Remainingbytes()=" << CurrentChunkSize - ulPos << std::endl;
       #endif // DEBUG
        return (CurrentChunkSize > ulPos) ? CurrentChunkSize - ulPos : 0;
    }

    /**
     *  Returns the current state of the chunk object.
     *  Following values are possible:
     *  - RIFF::stream_ready :
     *    chunk data can be read (this is the usual case)
     *  - RIFF::stream_closed :
     *    the data stream was closed somehow, no more reading possible
     *  - RIFF::stream_end_reached :
     *    already reached the end of the chunk data, no more reading
     *    possible without SetPos()
     */
    stream_state_t Chunk::GetState() {
      #if DEBUG
      std::cout << "Chunk::GetState()" << std::endl;
      #endif // DEBUG
        #if POSIX
        if (pFile->hFileRead == 0) return stream_closed;
        #elif defined (WIN32)
        if (pFile->hFileRead == INVALID_HANDLE_VALUE)
            return stream_closed;
        #else
        if (pFile->hFileRead == NULL) return stream_closed;
        #endif // POSIX
        if (ulPos < CurrentChunkSize) return stream_ready;
        else                          return stream_end_reached;
    }

    /**
     *  Reads \a WordCount number of data words with given \a WordSize and
     *  copies it into a buffer pointed by \a pData. The buffer has to be
     *  allocated and be sure to provide the correct \a WordSize, as this
     *  will be important and taken into account for eventual endian
     *  correction (swapping of bytes due to different native byte order of
     *  a system). The position within the chunk will automatically be
     *  incremented.
     *
     *  @param pData      destination buffer
     *  @param WordCount  number of data words to read
     *  @param WordSize   size of each data word to read
     *  @returns          number of successfully read data words or 0 if end
     *                    of file reached or error occured
     */
    unsigned long Chunk::Read(void* pData, unsigned long WordCount, unsigned long WordSize) {
       #if DEBUG
       std::cout << "Chunk::Read(void*,ulong,ulong)" << std::endl;
       #endif // DEBUG
        if (ulStartPos == 0) return 0; // is only 0 if this is a new chunk, so nothing to read (yet)
        if (ulPos >= CurrentChunkSize) return 0;
        if (ulPos + WordCount * WordSize >= CurrentChunkSize) WordCount = (CurrentChunkSize - ulPos) / WordSize;
        #if POSIX
        if (lseek(pFile->hFileRead, ulStartPos + ulPos, SEEK_SET) < 0) return 0;
        unsigned long readWords = read(pFile->hFileRead, pData, WordCount * WordSize);
        if (readWords < 1) return 0;
        readWords /= WordSize;
        #elif defined(WIN32)
        if (SetFilePointer(pFile->hFileRead, ulStartPos + ulPos, NULL/*32 bit*/, FILE_BEGIN) == INVALID_SET_FILE_POINTER) return 0;
        DWORD readWords;
        ReadFile(pFile->hFileRead, pData, WordCount * WordSize, &readWords, NULL);
        if (readWords < 1) return 0;
        readWords /= WordSize;
        #else // standard C functions
        if (fseek(pFile->hFileRead, ulStartPos + ulPos, SEEK_SET)) return 0;
        unsigned long readWords = fread(pData, WordSize, WordCount, pFile->hFileRead);
        #endif // POSIX
        if (!pFile->bEndianNative && WordSize != 1) {
            switch (WordSize) {
                case 2:
                    for (unsigned long iWord = 0; iWord < readWords; iWord++)
                        swapBytes_16((uint16_t*) pData + iWord);
                    break;
                case 4:
                    for (unsigned long iWord = 0; iWord < readWords; iWord++)
                        swapBytes_32((uint32_t*) pData + iWord);
                    break;
                default:
                    for (unsigned long iWord = 0; iWord < readWords; iWord++)
                        swapBytes((uint8_t*) pData + iWord * WordSize, WordSize);
                    break;
            }
        }
        SetPos(readWords * WordSize, stream_curpos);
        return readWords;
    }

    /**
     *  Writes \a WordCount number of data words with given \a WordSize from
     *  the buffer pointed by \a pData. Be sure to provide the correct
     *  \a WordSize, as this will be important and taken into account for
     *  eventual endian correction (swapping of bytes due to different
     *  native byte order of a system). The position within the chunk will
     *  automatically be incremented.
     *
     *  @param pData      source buffer (containing the data)
     *  @param WordCount  number of data words to write
     *  @param WordSize   size of each data word to write
     *  @returns          number of successfully written data words
     *  @throws RIFF::Exception  if write operation would exceed current
     *                           chunk size or any IO error occured
     *  @see Resize()
     */
    unsigned long Chunk::Write(void* pData, unsigned long WordCount, unsigned long WordSize) {
        if (pFile->Mode != stream_mode_read_write)
            throw Exception("Cannot write data to chunk, file has to be opened in read+write mode first");
        if (ulPos >= CurrentChunkSize || ulPos + WordCount * WordSize > CurrentChunkSize)
            throw Exception("End of chunk reached while trying to write data");
        if (!pFile->bEndianNative && WordSize != 1) {
            switch (WordSize) {
                case 2:
                    for (unsigned long iWord = 0; iWord < WordCount; iWord++)
                        swapBytes_16((uint16_t*) pData + iWord);
                    break;
                case 4:
                    for (unsigned long iWord = 0; iWord < WordCount; iWord++)
                        swapBytes_32((uint32_t*) pData + iWord);
                    break;
                default:
                    for (unsigned long iWord = 0; iWord < WordCount; iWord++)
                        swapBytes((uint8_t*) pData + iWord * WordSize, WordSize);
                    break;
            }
        }
        #if POSIX
        if (lseek(pFile->hFileWrite, ulStartPos + ulPos, SEEK_SET) < 0) {
            throw Exception("Could not seek to position " + ToString(ulPos) +
                            " in chunk (" + ToString(ulStartPos + ulPos) + " in file)");
        }
        unsigned long writtenWords = write(pFile->hFileWrite, pData, WordCount * WordSize);
        if (writtenWords < 1) throw Exception("POSIX IO Error while trying to write chunk data");
        writtenWords /= WordSize;
        #elif defined(WIN32)
        if (SetFilePointer(pFile->hFileWrite, ulStartPos + ulPos, NULL/*32 bit*/, FILE_BEGIN) == INVALID_SET_FILE_POINTER) {
            throw Exception("Could not seek to position " + ToString(ulPos) +
                            " in chunk (" + ToString(ulStartPos + ulPos) + " in file)");
        }
        DWORD writtenWords;
        WriteFile(pFile->hFileWrite, pData, WordCount * WordSize, &writtenWords, NULL);
        if (writtenWords < 1) throw Exception("Windows IO Error while trying to write chunk data");
        writtenWords /= WordSize;
        #else // standard C functions
        if (fseek(pFile->hFileWrite, ulStartPos + ulPos, SEEK_SET)) {
            throw Exception("Could not seek to position " + ToString(ulPos) +
                            " in chunk (" + ToString(ulStartPos + ulPos) + " in file)");
        }
        unsigned long writtenWords = fwrite(pData, WordSize, WordCount, pFile->hFileWrite);
        #endif // POSIX
        SetPos(writtenWords * WordSize, stream_curpos);
        return writtenWords;
    }

    /** Just an internal wrapper for the main <i>Read()</i> method with additional Exception throwing on errors. */
    unsigned long Chunk::ReadSceptical(void* pData, unsigned long WordCount, unsigned long WordSize) {
        unsigned long readWords = Read(pData, WordCount, WordSize);
        if (readWords != WordCount) throw RIFF::Exception("End of chunk data reached.");
        return readWords;
    }

    /**
     * Reads \a WordCount number of 8 Bit signed integer words and copies it
     * into the buffer pointed by \a pData. The buffer has to be allocated.
     * The position within the chunk will automatically be incremented.
     *
     * @param pData             destination buffer
     * @param WordCount         number of 8 Bit signed integers to read
     * @returns                 number of read integers
     * @throws RIFF::Exception  if an error occured or less than
     *                          \a WordCount integers could be read!
     */
    unsigned long Chunk::ReadInt8(int8_t* pData, unsigned long WordCount) {
       #if DEBUG
       std::cout << "Chunk::ReadInt8(int8_t*,ulong)" << std::endl;
       #endif // DEBUG
        return ReadSceptical(pData, WordCount, 1);
    }

    /**
     * Writes \a WordCount number of 8 Bit signed integer words from the
     * buffer pointed by \a pData to the chunk's body, directly to the
     * actual "physical" file. The position within the chunk will
     * automatically be incremented. Note: you cannot write beyond the
     * boundaries of the chunk, to append data to the chunk call Resize()
     * before.
     *
     * @param pData             source buffer (containing the data)
     * @param WordCount         number of 8 Bit signed integers to write
     * @returns                 number of written integers
     * @throws RIFF::Exception  if an IO error occured
     * @see Resize()
     */
    unsigned long Chunk::WriteInt8(int8_t* pData, unsigned long WordCount) {
        return Write(pData, WordCount, 1);
    }

    /**
     * Reads \a WordCount number of 8 Bit unsigned integer words and copies
     * it into the buffer pointed by \a pData. The buffer has to be
     * allocated. The position within the chunk will automatically be
     * incremented.
     *
     * @param pData             destination buffer
     * @param WordCount         number of 8 Bit unsigned integers to read
     * @returns                 number of read integers
     * @throws RIFF::Exception  if an error occured or less than
     *                          \a WordCount integers could be read!
     */
    unsigned long Chunk::ReadUint8(uint8_t* pData, unsigned long WordCount) {
       #if DEBUG
       std::cout << "Chunk::ReadUint8(uint8_t*,ulong)" << std::endl;
       #endif // DEBUG
        return ReadSceptical(pData, WordCount, 1);
    }

    /**
     * Writes \a WordCount number of 8 Bit unsigned integer words from the
     * buffer pointed by \a pData to the chunk's body, directly to the
     * actual "physical" file. The position within the chunk will
     * automatically be incremented. Note: you cannot write beyond the
     * boundaries of the chunk, to append data to the chunk call Resize()
     * before.
     *
     * @param pData             source buffer (containing the data)
     * @param WordCount         number of 8 Bit unsigned integers to write
     * @returns                 number of written integers
     * @throws RIFF::Exception  if an IO error occured
     * @see Resize()
     */
    unsigned long Chunk::WriteUint8(uint8_t* pData, unsigned long WordCount) {
        return Write(pData, WordCount, 1);
    }

    /**
     * Reads \a WordCount number of 16 Bit signed integer words and copies
     * it into the buffer pointed by \a pData. The buffer has to be
     * allocated. Endian correction will automatically be done if needed.
     * The position within the chunk will automatically be incremented.
     *
     * @param pData             destination buffer
     * @param WordCount         number of 16 Bit signed integers to read
     * @returns                 number of read integers
     * @throws RIFF::Exception  if an error occured or less than
     *                          \a WordCount integers could be read!
     */
    unsigned long Chunk::ReadInt16(int16_t* pData, unsigned long WordCount) {
      #if DEBUG
      std::cout << "Chunk::ReadInt16(int16_t*,ulong)" << std::endl;
      #endif // DEBUG
        return ReadSceptical(pData, WordCount, 2);
    }

    /**
     * Writes \a WordCount number of 16 Bit signed integer words from the
     * buffer pointed by \a pData to the chunk's body, directly to the
     * actual "physical" file. The position within the chunk will
     * automatically be incremented. Note: you cannot write beyond the
     * boundaries of the chunk, to append data to the chunk call Resize()
     * before.
     *
     * @param pData             source buffer (containing the data)
     * @param WordCount         number of 16 Bit signed integers to write
     * @returns                 number of written integers
     * @throws RIFF::Exception  if an IO error occured
     * @see Resize()
     */
    unsigned long Chunk::WriteInt16(int16_t* pData, unsigned long WordCount) {
        return Write(pData, WordCount, 2);
    }

    /**
     * Reads \a WordCount number of 16 Bit unsigned integer words and copies
     * it into the buffer pointed by \a pData. The buffer has to be
     * allocated. Endian correction will automatically be done if needed.
     * The position within the chunk will automatically be incremented.
     *
     * @param pData             destination buffer
     * @param WordCount         number of 8 Bit unsigned integers to read
     * @returns                 number of read integers
     * @throws RIFF::Exception  if an error occured or less than
     *                          \a WordCount integers could be read!
     */
    unsigned long Chunk::ReadUint16(uint16_t* pData, unsigned long WordCount) {
      #if DEBUG
      std::cout << "Chunk::ReadUint16(uint16_t*,ulong)" << std::endl;
      #endif // DEBUG
        return ReadSceptical(pData, WordCount, 2);
    }

    /**
     * Writes \a WordCount number of 16 Bit unsigned integer words from the
     * buffer pointed by \a pData to the chunk's body, directly to the
     * actual "physical" file. The position within the chunk will
     * automatically be incremented. Note: you cannot write beyond the
     * boundaries of the chunk, to append data to the chunk call Resize()
     * before.
     *
     * @param pData             source buffer (containing the data)
     * @param WordCount         number of 16 Bit unsigned integers to write
     * @returns                 number of written integers
     * @throws RIFF::Exception  if an IO error occured
     * @see Resize()
     */
    unsigned long Chunk::WriteUint16(uint16_t* pData, unsigned long WordCount) {
        return Write(pData, WordCount, 2);
    }

    /**
     * Reads \a WordCount number of 32 Bit signed integer words and copies
     * it into the buffer pointed by \a pData. The buffer has to be
     * allocated. Endian correction will automatically be done if needed.
     * The position within the chunk will automatically be incremented.
     *
     * @param pData             destination buffer
     * @param WordCount         number of 32 Bit signed integers to read
     * @returns                 number of read integers
     * @throws RIFF::Exception  if an error occured or less than
     *                          \a WordCount integers could be read!
     */
    unsigned long Chunk::ReadInt32(int32_t* pData, unsigned long WordCount) {
       #if DEBUG
       std::cout << "Chunk::ReadInt32(int32_t*,ulong)" << std::endl;
       #endif // DEBUG
        return ReadSceptical(pData, WordCount, 4);
    }

    /**
     * Writes \a WordCount number of 32 Bit signed integer words from the
     * buffer pointed by \a pData to the chunk's body, directly to the
     * actual "physical" file. The position within the chunk will
     * automatically be incremented. Note: you cannot write beyond the
     * boundaries of the chunk, to append data to the chunk call Resize()
     * before.
     *
     * @param pData             source buffer (containing the data)
     * @param WordCount         number of 32 Bit signed integers to write
     * @returns                 number of written integers
     * @throws RIFF::Exception  if an IO error occured
     * @see Resize()
     */
    unsigned long Chunk::WriteInt32(int32_t* pData, unsigned long WordCount) {
        return Write(pData, WordCount, 4);
    }

    /**
     * Reads \a WordCount number of 32 Bit unsigned integer words and copies
     * it into the buffer pointed by \a pData. The buffer has to be
     * allocated. Endian correction will automatically be done if needed.
     * The position within the chunk will automatically be incremented.
     *
     * @param pData             destination buffer
     * @param WordCount         number of 32 Bit unsigned integers to read
     * @returns                 number of read integers
     * @throws RIFF::Exception  if an error occured or less than
     *                          \a WordCount integers could be read!
     */
    unsigned long Chunk::ReadUint32(uint32_t* pData, unsigned long WordCount) {
       #if DEBUG
       std::cout << "Chunk::ReadUint32(uint32_t*,ulong)" << std::endl;
       #endif // DEBUG
        return ReadSceptical(pData, WordCount, 4);
    }

    /**
     * Writes \a WordCount number of 32 Bit unsigned integer words from the
     * buffer pointed by \a pData to the chunk's body, directly to the
     * actual "physical" file. The position within the chunk will
     * automatically be incremented. Note: you cannot write beyond the
     * boundaries of the chunk, to append data to the chunk call Resize()
     * before.
     *
     * @param pData             source buffer (containing the data)
     * @param WordCount         number of 32 Bit unsigned integers to write
     * @returns                 number of written integers
     * @throws RIFF::Exception  if an IO error occured
     * @see Resize()
     */
    unsigned long Chunk::WriteUint32(uint32_t* pData, unsigned long WordCount) {
        return Write(pData, WordCount, 4);
    }

    /**
     * Reads one 8 Bit signed integer word and increments the position within
     * the chunk.
     *
     * @returns                 read integer word
     * @throws RIFF::Exception  if an error occured
     */
    int8_t Chunk::ReadInt8() {
      #if DEBUG
      std::cout << "Chunk::ReadInt8()" << std::endl;
      #endif // DEBUG
        int8_t word;
        ReadSceptical(&word,1,1);
        return word;
    }

    /**
     * Reads one 8 Bit unsigned integer word and increments the position
     * within the chunk.
     *
     * @returns                 read integer word
     * @throws RIFF::Exception  if an error occured
     */
    uint8_t Chunk::ReadUint8() {
      #if DEBUG
      std::cout << "Chunk::ReadUint8()" << std::endl;
      #endif // DEBUG
        uint8_t word;
        ReadSceptical(&word,1,1);
        return word;
    }

    /**
     * Reads one 16 Bit signed integer word and increments the position
     * within the chunk. Endian correction will automatically be done if
     * needed.
     *
     * @returns                 read integer word
     * @throws RIFF::Exception  if an error occured
     */
    int16_t Chunk::ReadInt16() {
      #if DEBUG
      std::cout << "Chunk::ReadInt16()" << std::endl;
      #endif // DEBUG
        int16_t word;
        ReadSceptical(&word,1,2);
        return word;
    }

    /**
     * Reads one 16 Bit unsigned integer word and increments the position
     * within the chunk. Endian correction will automatically be done if
     * needed.
     *
     * @returns                 read integer word
     * @throws RIFF::Exception  if an error occured
     */
    uint16_t Chunk::ReadUint16() {
      #if DEBUG
      std::cout << "Chunk::ReadUint16()" << std::endl;
      #endif // DEBUG
        uint16_t word;
        ReadSceptical(&word,1,2);
        return word;
    }

    /**
     * Reads one 32 Bit signed integer word and increments the position
     * within the chunk. Endian correction will automatically be done if
     * needed.
     *
     * @returns                 read integer word
     * @throws RIFF::Exception  if an error occured
     */
    int32_t Chunk::ReadInt32() {
      #if DEBUG
      std::cout << "Chunk::ReadInt32()" << std::endl;
      #endif // DEBUG
        int32_t word;
        ReadSceptical(&word,1,4);
        return word;
    }

    /**
     * Reads one 32 Bit unsigned integer word and increments the position
     * within the chunk. Endian correction will automatically be done if
     * needed.
     *
     * @returns                 read integer word
     * @throws RIFF::Exception  if an error occured
     */
    uint32_t Chunk::ReadUint32() {
      #if DEBUG
      std::cout << "Chunk::ReadUint32()" << std::endl;
      #endif // DEBUG
        uint32_t word;
        ReadSceptical(&word,1,4);
        return word;
    }

    /** @brief Load chunk body into RAM.
     *
     * Loads the whole chunk body into memory. You can modify the data in
     * RAM and save the data by calling File::Save() afterwards.
     *
     * <b>Caution:</b> the buffer pointer will be invalidated once
     * File::Save() was called. You have to call LoadChunkData() again to
     * get a new, valid pointer whenever File::Save() was called.
     *
     * You can call LoadChunkData() again if you previously scheduled to
     * enlarge this chunk with a Resize() call. In that case the buffer will
     * be enlarged to the new, scheduled chunk size and you can already
     * place the new chunk data to the buffer and finally call File::Save()
     * to enlarge the chunk physically and write the new data in one rush.
     * This approach is definitely recommended if you have to enlarge and
     * write new data to a lot of chunks.
     *
     * @returns a pointer to the data in RAM on success, NULL otherwise
     * @throws Exception if data buffer could not be enlarged
     * @see ReleaseChunkData()
     */
    void* Chunk::LoadChunkData() {
        if (!pChunkData && pFile->Filename != "" && ulStartPos != 0) {
            #if POSIX
            if (lseek(pFile->hFileRead, ulStartPos, SEEK_SET) == -1) return NULL;
            #elif defined(WIN32)
            if (SetFilePointer(pFile->hFileRead, ulStartPos, NULL/*32 bit*/, FILE_BEGIN) == INVALID_SET_FILE_POINTER) return NULL;
            #else
            if (fseek(pFile->hFileRead, ulStartPos, SEEK_SET)) return NULL;
            #endif // POSIX
            unsigned long ulBufferSize = (CurrentChunkSize > NewChunkSize) ? CurrentChunkSize : NewChunkSize;
            pChunkData = new uint8_t[ulBufferSize];
            if (!pChunkData) return NULL;
            memset(pChunkData, 0, ulBufferSize);
            #if POSIX
            unsigned long readWords = read(pFile->hFileRead, pChunkData, GetSize());
            #elif defined(WIN32)
            DWORD readWords;
            ReadFile(pFile->hFileRead, pChunkData, GetSize(), &readWords, NULL);
            #else
            unsigned long readWords = fread(pChunkData, 1, GetSize(), pFile->hFileRead);
            #endif // POSIX
            if (readWords != GetSize()) {
                delete[] pChunkData;
                return (pChunkData = NULL);
            }
            ulChunkDataSize = ulBufferSize;
        } else if (NewChunkSize > ulChunkDataSize) {
            uint8_t* pNewBuffer = new uint8_t[NewChunkSize];
            if (!pNewBuffer) throw Exception("Could not enlarge chunk data buffer to " + ToString(NewChunkSize) + " bytes");
            memset(pNewBuffer, 0 , NewChunkSize);
            memcpy(pNewBuffer, pChunkData, ulChunkDataSize);
            delete[] pChunkData;
            pChunkData      = pNewBuffer;
            ulChunkDataSize = NewChunkSize;
        }
        return pChunkData;
    }

    /** @brief Free loaded chunk body from RAM.
     *
     * Frees loaded chunk body data from memory (RAM). You should call
     * File::Save() before calling this method if you modified the data to
     * make the changes persistent.
     */
    void Chunk::ReleaseChunkData() {
        if (pChunkData) {
            delete[] pChunkData;
            pChunkData = NULL;
        }
    }

    /** @brief Resize chunk.
     *
     * Resizes this chunk's body, that is the actual size of data possible
     * to be written to this chunk. This call will return immediately and
     * just schedule the resize operation. You should call File::Save() to
     * actually perform the resize operation(s) "physically" to the file.
     * As this can take a while on large files, it is recommended to call
     * Resize() first on all chunks which have to be resized and finally to
     * call File::Save() to perform all those resize operations in one rush.
     *
     * <b>Caution:</b> You cannot directly write to enlarged chunks before
     * calling File::Save() as this might exceed the current chunk's body
     * boundary!
     *
     * @param iNewSize - new chunk body size in bytes (must be greater than zero)
     * @throws RIFF::Exception  if \a iNewSize is less than 1
     * @see File::Save()
     */
    void Chunk::Resize(int iNewSize) {
        if (iNewSize <= 0)
            throw Exception("There is at least one empty chunk (zero size): " + __resolveChunkPath(this));
        if (NewChunkSize == iNewSize) return;
        NewChunkSize = iNewSize;
        pFile->LogAsResized(this);
    }

    /** @brief Write chunk persistently e.g. to disk.
     *
     * Stores the chunk persistently to its actual "physical" file.
     *
     * @param ulWritePos - position within the "physical" file where this
     *                     chunk should be written to
     * @param ulCurrentDataOffset - offset of current (old) data within
     *                              the file
     * @returns new write position in the "physical" file, that is
     *          \a ulWritePos incremented by this chunk's new size
     *          (including its header size of course)
     */
    unsigned long Chunk::WriteChunk(unsigned long ulWritePos, unsigned long ulCurrentDataOffset) {
        const unsigned long ulOriginalPos = ulWritePos;
        ulWritePos += CHUNK_HEADER_SIZE;

        if (pFile->Mode != stream_mode_read_write)
            throw Exception("Cannot write list chunk, file has to be opened in read+write mode");

        // if the whole chunk body was loaded into RAM
        if (pChunkData) {
            // make sure chunk data buffer in RAM is at least as large as the new chunk size
            LoadChunkData();
            // write chunk data from RAM persistently to the file
            #if POSIX
            lseek(pFile->hFileWrite, ulWritePos, SEEK_SET);
            if (write(pFile->hFileWrite, pChunkData, NewChunkSize) != NewChunkSize) {
                throw Exception("Writing Chunk data (from RAM) failed");
            }
            #elif defined(WIN32)
            SetFilePointer(pFile->hFileWrite, ulWritePos, NULL/*32 bit*/, FILE_BEGIN);
            DWORD dwBytesWritten;
            WriteFile(pFile->hFileWrite, pChunkData, NewChunkSize, &dwBytesWritten, NULL);
            if (dwBytesWritten != NewChunkSize) {
                throw Exception("Writing Chunk data (from RAM) failed");
            }
            #else
            fseek(pFile->hFileWrite, ulWritePos, SEEK_SET);
            if (fwrite(pChunkData, 1, NewChunkSize, pFile->hFileWrite) != NewChunkSize) {
                throw Exception("Writing Chunk data (from RAM) failed");
            }
            #endif // POSIX
        } else {
            // move chunk data from the end of the file to the appropriate position
            int8_t* pCopyBuffer = new int8_t[4096];
            unsigned long ulToMove = (NewChunkSize < CurrentChunkSize) ? NewChunkSize : CurrentChunkSize;
            #if defined(WIN32)
            DWORD iBytesMoved = 1; // we have to pass it via pointer to the Windows API, thus the correct size must be ensured
            #else
            int iBytesMoved = 1;
            #endif
            for (unsigned long ulOffset = 0; ulToMove > 0 && iBytesMoved > 0; ulOffset += iBytesMoved, ulToMove -= iBytesMoved) {
                iBytesMoved = (ulToMove < 4096) ? ulToMove : 4096;
                #if POSIX
                lseek(pFile->hFileRead, ulStartPos + ulCurrentDataOffset + ulOffset, SEEK_SET);
                iBytesMoved = read(pFile->hFileRead, pCopyBuffer, iBytesMoved);
                lseek(pFile->hFileWrite, ulWritePos + ulOffset, SEEK_SET);
                iBytesMoved = write(pFile->hFileWrite, pCopyBuffer, iBytesMoved);
                #elif defined(WIN32)
                SetFilePointer(pFile->hFileRead, ulStartPos + ulCurrentDataOffset + ulOffset, NULL/*32 bit*/, FILE_BEGIN);
                ReadFile(pFile->hFileRead, pCopyBuffer, iBytesMoved, &iBytesMoved, NULL);
                SetFilePointer(pFile->hFileWrite, ulWritePos + ulOffset, NULL/*32 bit*/, FILE_BEGIN);
                WriteFile(pFile->hFileWrite, pCopyBuffer, iBytesMoved, &iBytesMoved, NULL);
                #else
                fseek(pFile->hFileRead, ulStartPos + ulCurrentDataOffset + ulOffset, SEEK_SET);
                iBytesMoved = fread(pCopyBuffer, 1, iBytesMoved, pFile->hFileRead);
                fseek(pFile->hFileWrite, ulWritePos + ulOffset, SEEK_SET);
                iBytesMoved = fwrite(pCopyBuffer, 1, iBytesMoved, pFile->hFileWrite);
                #endif
            }
            delete[] pCopyBuffer;
            if (iBytesMoved < 0) throw Exception("Writing Chunk data (from file) failed");
        }

        // update this chunk's header
        CurrentChunkSize = NewChunkSize;
        WriteHeader(ulOriginalPos);

        // update chunk's position pointers
        ulStartPos = ulOriginalPos + CHUNK_HEADER_SIZE;
        ulPos      = 0;

        // add pad byte if needed
        if ((ulStartPos + NewChunkSize) % 2 != 0) {
            const char cPadByte = 0;
            #if POSIX
            lseek(pFile->hFileWrite, ulStartPos + NewChunkSize, SEEK_SET);
            write(pFile->hFileWrite, &cPadByte, 1);
            #elif defined(WIN32)
            SetFilePointer(pFile->hFileWrite, ulStartPos + NewChunkSize, NULL/*32 bit*/, FILE_BEGIN);
            DWORD dwBytesWritten;
            WriteFile(pFile->hFileWrite, &cPadByte, 1, &dwBytesWritten, NULL);
            #else
            fseek(pFile->hFileWrite, ulStartPos + NewChunkSize, SEEK_SET);
            fwrite(&cPadByte, 1, 1, pFile->hFileWrite);
            #endif
            return ulStartPos + NewChunkSize + 1;
        }

        return ulStartPos + NewChunkSize;
    }

    void Chunk::__resetPos() {
        ulPos = 0;
    }



// *************** List ***************
// *

    List::List(File* pFile) : Chunk(pFile) {
      #if DEBUG
      std::cout << "List::List(File* pFile)" << std::endl;
      #endif // DEBUG
        pSubChunks    = NULL;
        pSubChunksMap = NULL;
    }

    List::List(File* pFile, unsigned long StartPos, List* Parent)
      : Chunk(pFile, StartPos, Parent) {
        #if DEBUG
        std::cout << "List::List(File*,ulong,bool,List*)" << std::endl;
        #endif // DEBUG
        pSubChunks    = NULL;
        pSubChunksMap = NULL;
        ReadHeader(StartPos);
        ulStartPos    = StartPos + LIST_HEADER_SIZE;
    }

    List::List(File* pFile, List* pParent, uint32_t uiListID)
      : Chunk(pFile, pParent, CHUNK_ID_LIST, 0) {
        pSubChunks    = NULL;
        pSubChunksMap = NULL;
        ListType      = uiListID;
    }

    List::~List() {
      #if DEBUG
      std::cout << "List::~List()" << std::endl;
      #endif // DEBUG
        DeleteChunkList();
    }

    void List::DeleteChunkList() {
        if (pSubChunks) {
            ChunkList::iterator iter = pSubChunks->begin();
            ChunkList::iterator end  = pSubChunks->end();
            while (iter != end) {
                delete *iter;
                iter++;
            }
            delete pSubChunks;
            pSubChunks = NULL;
        }
        if (pSubChunksMap) {
            delete pSubChunksMap;
            pSubChunksMap = NULL;
        }
    }

    /**
     *  Returns subchunk with chunk ID <i>\a ChunkID</i> within this chunk
     *  list. Use this method if you expect only one subchunk of that type in
     *  the list. It there are more than one, it's undetermined which one of
     *  them will be returned! If there are no subchunks with that desired
     *  chunk ID, NULL will be returned.
     *
     *  @param ChunkID - chunk ID of the sought subchunk
     *  @returns         pointer to the subchunk or NULL if there is none of
     *                   that ID
     */
    Chunk* List::GetSubChunk(uint32_t ChunkID) {
      #if DEBUG
      std::cout << "List::GetSubChunk(uint32_t)" << std::endl;
      #endif // DEBUG
        if (!pSubChunksMap) LoadSubChunks();
        return (*pSubChunksMap)[ChunkID];
    }

    /**
     *  Returns sublist chunk with list type <i>\a ListType</i> within this
     *  chunk list. Use this method if you expect only one sublist chunk of
     *  that type in the list. It there are more than one, it's undetermined
     *  which one of them will be returned! If there are no sublists with
     *  that desired list type, NULL will be returned.
     *
     *  @param ListType - list type of the sought sublist
     *  @returns          pointer to the sublist or NULL if there is none of
     *                    that type
     */
    List* List::GetSubList(uint32_t ListType) {
        #if DEBUG
        std::cout << "List::GetSubList(uint32_t)" << std::endl;
        #endif // DEBUG
        if (!pSubChunks) LoadSubChunks();
        ChunkList::iterator iter = pSubChunks->begin();
        ChunkList::iterator end  = pSubChunks->end();
        while (iter != end) {
            if ((*iter)->GetChunkID() == CHUNK_ID_LIST) {
                List* l = (List*) *iter;
                if (l->GetListType() == ListType) return l;
            }
            iter++;
        }
        return NULL;
    }

    /**
     *  Returns the first subchunk within the list. You have to call this
     *  method before you can call GetNextSubChunk(). Recall it when you want
     *  to start from the beginning of the list again.
     *
     *  @returns  pointer to the first subchunk within the list, NULL
     *            otherwise
     */
    Chunk* List::GetFirstSubChunk() {
        #if DEBUG
        std::cout << "List::GetFirstSubChunk()" << std::endl;
        #endif // DEBUG
        if (!pSubChunks) LoadSubChunks();
        ChunksIterator = pSubChunks->begin();
        return (ChunksIterator != pSubChunks->end()) ? *ChunksIterator : NULL;
    }

    /**
     *  Returns the next subchunk within the list. You have to call
     *  GetFirstSubChunk() before you can use this method!
     *
     *  @returns  pointer to the next subchunk within the list or NULL if
     *            end of list is reached
     */
    Chunk* List::GetNextSubChunk() {
        #if DEBUG
        std::cout << "List::GetNextSubChunk()" << std::endl;
        #endif // DEBUG
        if (!pSubChunks) return NULL;
        ChunksIterator++;
        return (ChunksIterator != pSubChunks->end()) ? *ChunksIterator : NULL;
    }

    /**
     *  Returns the first sublist within the list (that is a subchunk with
     *  chunk ID "LIST"). You have to call this method before you can call
     *  GetNextSubList(). Recall it when you want to start from the beginning
     *  of the list again.
     *
     *  @returns  pointer to the first sublist within the list, NULL
     *            otherwise
     */
    List* List::GetFirstSubList() {
        #if DEBUG
        std::cout << "List::GetFirstSubList()" << std::endl;
        #endif // DEBUG
        if (!pSubChunks) LoadSubChunks();
        ListIterator            = pSubChunks->begin();
        ChunkList::iterator end = pSubChunks->end();
        while (ListIterator != end) {
            if ((*ListIterator)->GetChunkID() == CHUNK_ID_LIST) return (List*) *ListIterator;
            ListIterator++;
        }
        return NULL;
    }

    /**
     *  Returns the next sublist (that is a subchunk with chunk ID "LIST")
     *  within the list. You have to call GetFirstSubList() before you can
     *  use this method!
     *
     *  @returns  pointer to the next sublist within the list, NULL if
     *            end of list is reached
     */
    List* List::GetNextSubList() {
        #if DEBUG
        std::cout << "List::GetNextSubList()" << std::endl;
        #endif // DEBUG
        if (!pSubChunks) return NULL;
        if (ListIterator == pSubChunks->end()) return NULL;
        ListIterator++;
        ChunkList::iterator end = pSubChunks->end();
        while (ListIterator != end) {
            if ((*ListIterator)->GetChunkID() == CHUNK_ID_LIST) return (List*) *ListIterator;
            ListIterator++;
        }
        return NULL;
    }

    /**
     *  Returns number of subchunks within the list.
     */
    unsigned int List::CountSubChunks() {
        if (!pSubChunks) LoadSubChunks();
        return pSubChunks->size();
    }

    /**
     *  Returns number of subchunks within the list with chunk ID
     *  <i>\a ChunkId</i>.
     */
    unsigned int List::CountSubChunks(uint32_t ChunkID) {
        unsigned int result = 0;
        if (!pSubChunks) LoadSubChunks();
        ChunkList::iterator iter = pSubChunks->begin();
        ChunkList::iterator end  = pSubChunks->end();
        while (iter != end) {
            if ((*iter)->GetChunkID() == ChunkID) {
                result++;
            }
            iter++;
        }
        return result;
    }

    /**
     *  Returns number of sublists within the list.
     */
    unsigned int List::CountSubLists() {
        return CountSubChunks(CHUNK_ID_LIST);
    }

    /**
     *  Returns number of sublists within the list with list type
     *  <i>\a ListType</i>
     */
    unsigned int List::CountSubLists(uint32_t ListType) {
        unsigned int result = 0;
        if (!pSubChunks) LoadSubChunks();
        ChunkList::iterator iter = pSubChunks->begin();
        ChunkList::iterator end  = pSubChunks->end();
        while (iter != end) {
            if ((*iter)->GetChunkID() == CHUNK_ID_LIST) {
                List* l = (List*) *iter;
                if (l->GetListType() == ListType) result++;
            }
            iter++;
        }
        return result;
    }

    /** @brief Creates a new sub chunk.
     *
     * Creates and adds a new sub chunk to this list chunk. Note that the
     * chunk's body size given by \a uiBodySize must be greater than zero.
     * You have to call File::Save() to make this change persistent to the
     * actual file and <b>before</b> performing any data write operations
     * on the new chunk!
     *
     * @param uiChunkID  - chunk ID of the new chunk
     * @param uiBodySize - size of the new chunk's body, that is its actual
     *                     data size (without header)
     * @throws RIFF::Exception if \a uiBodySize equals zero
     */
    Chunk* List::AddSubChunk(uint32_t uiChunkID, uint uiBodySize) {
        if (uiBodySize == 0) throw Exception("Chunk body size must be at least 1 byte");
        if (!pSubChunks) LoadSubChunks();
        Chunk* pNewChunk = new Chunk(pFile, this, uiChunkID, 0);
        pSubChunks->push_back(pNewChunk);
        (*pSubChunksMap)[uiChunkID] = pNewChunk;
        pNewChunk->Resize(uiBodySize);
        NewChunkSize += CHUNK_HEADER_SIZE;
        pFile->LogAsResized(this);
        return pNewChunk;
    }

    /** @brief Moves a sub chunk.
     *
     * Moves a sub chunk from one position in a list to another
     * position in the same list. The pSrc chunk is placed before the
     * pDst chunk.
     *
     * @param pSrc - sub chunk to be moved
     * @param pDst - the position to move to. pSrc will be placed
     *               before pDst. If pDst is 0, pSrc will be placed
     *               last in list.
     */
    void List::MoveSubChunk(Chunk* pSrc, Chunk* pDst) {
        if (!pSubChunks) LoadSubChunks();
        pSubChunks->remove(pSrc);
        ChunkList::iterator iter = find(pSubChunks->begin(), pSubChunks->end(), pDst);
        pSubChunks->insert(iter, pSrc);
    }

    /** @brief Creates a new list sub chunk.
     *
     * Creates and adds a new list sub chunk to this list chunk. Note that
     * you have to add sub chunks / sub list chunks to the new created chunk
     * <b>before</b> trying to make this change persisten to the actual
     * file with File::Save()!
     *
     * @param uiListType - list ID of the new list chunk
     */
    List* List::AddSubList(uint32_t uiListType) {
        if (!pSubChunks) LoadSubChunks();
        List* pNewListChunk = new List(pFile, this, uiListType);
        pSubChunks->push_back(pNewListChunk);
        (*pSubChunksMap)[CHUNK_ID_LIST] = pNewListChunk;
        NewChunkSize += LIST_HEADER_SIZE;
        pFile->LogAsResized(this);
        return pNewListChunk;
    }

    /** @brief Removes a sub chunk.
     *
     * Removes the sub chunk given by \a pSubChunk from this list and frees
     * it completely from RAM. The given chunk can either be a normal sub
     * chunk or a list sub chunk. In case the given chunk is a list chunk,
     * all its subchunks (if any) will be removed recursively as well. You
     * should call File::Save() to make this change persistent at any time.
     *
     * @param pSubChunk - sub chunk or sub list chunk to be removed
     */
    void List::DeleteSubChunk(Chunk* pSubChunk) {
        if (!pSubChunks) LoadSubChunks();
        pSubChunks->remove(pSubChunk);
        if ((*pSubChunksMap)[pSubChunk->GetChunkID()] == pSubChunk) {
            pSubChunksMap->erase(pSubChunk->GetChunkID());
            // try to find another chunk of the same chunk ID
            ChunkList::iterator iter = pSubChunks->begin();
            ChunkList::iterator end  = pSubChunks->end();
            for (; iter != end; ++iter) {
                if ((*iter)->GetChunkID() == pSubChunk->GetChunkID()) {
                    (*pSubChunksMap)[pSubChunk->GetChunkID()] = *iter;
                    break; // we're done, stop search
                }
            }
        }
        delete pSubChunk;
    }

    void List::ReadHeader(unsigned long fPos) {
      #if DEBUG
      std::cout << "List::Readheader(ulong) ";
      #endif // DEBUG
        Chunk::ReadHeader(fPos);
        if (CurrentChunkSize < 4) return;
        NewChunkSize = CurrentChunkSize -= 4;
        #if POSIX
        lseek(pFile->hFileRead, fPos + CHUNK_HEADER_SIZE, SEEK_SET);
        read(pFile->hFileRead, &ListType, 4);
        #elif defined(WIN32)
        SetFilePointer(pFile->hFileRead, fPos + CHUNK_HEADER_SIZE, NULL/*32 bit*/, FILE_BEGIN);
        DWORD dwBytesRead;
        ReadFile(pFile->hFileRead, &ListType, 4, &dwBytesRead, NULL);
        #else
        fseek(pFile->hFileRead, fPos + CHUNK_HEADER_SIZE, SEEK_SET);
        fread(&ListType, 4, 1, pFile->hFileRead);
        #endif // POSIX
      #if DEBUG
      std::cout << "listType=" << convertToString(ListType) << std::endl;
      #endif // DEBUG
        if (!pFile->bEndianNative) {
            //swapBytes_32(&ListType);
        }
    }

    void List::WriteHeader(unsigned long fPos) {
        // the four list type bytes officially belong the chunk's body in the RIFF format
        NewChunkSize += 4;
        Chunk::WriteHeader(fPos);
        NewChunkSize -= 4; // just revert the +4 incrementation
        #if POSIX
        lseek(pFile->hFileWrite, fPos + CHUNK_HEADER_SIZE, SEEK_SET);
        write(pFile->hFileWrite, &ListType, 4);
        #elif defined(WIN32)
        SetFilePointer(pFile->hFileWrite, fPos + CHUNK_HEADER_SIZE, NULL/*32 bit*/, FILE_BEGIN);
        DWORD dwBytesWritten;
        WriteFile(pFile->hFileWrite, &ListType, 4, &dwBytesWritten, NULL);
        #else
        fseek(pFile->hFileWrite, fPos + CHUNK_HEADER_SIZE, SEEK_SET);
        fwrite(&ListType, 4, 1, pFile->hFileWrite);
        #endif // POSIX
    }

    void List::LoadSubChunks() {
       #if DEBUG
       std::cout << "List::LoadSubChunks()";
       #endif // DEBUG
        if (!pSubChunks) {
            pSubChunks    = new ChunkList();
            pSubChunksMap = new ChunkMap();
            #if defined(WIN32)
            if (pFile->hFileRead == INVALID_HANDLE_VALUE) return;
            #else
            if (!pFile->hFileRead) return;
            #endif
            unsigned long uiOriginalPos = GetPos();
            SetPos(0); // jump to beginning of list chunk body
            while (RemainingBytes() >= CHUNK_HEADER_SIZE) {
                Chunk* ck;
                uint32_t ckid;
                Read(&ckid, 4, 1);
       #if DEBUG
       std::cout << " ckid=" << convertToString(ckid) << std::endl;
       #endif // DEBUG
                if (ckid == CHUNK_ID_LIST) {
                    ck = new RIFF::List(pFile, ulStartPos + ulPos - 4, this);
                    SetPos(ck->GetSize() + LIST_HEADER_SIZE - 4, RIFF::stream_curpos);
                }
                else { // simple chunk
                    ck = new RIFF::Chunk(pFile, ulStartPos + ulPos - 4, this);
                    SetPos(ck->GetSize() + CHUNK_HEADER_SIZE - 4, RIFF::stream_curpos);
                }
                pSubChunks->push_back(ck);
                (*pSubChunksMap)[ckid] = ck;
                if (GetPos() % 2 != 0) SetPos(1, RIFF::stream_curpos); // jump over pad byte
            }
            SetPos(uiOriginalPos); // restore position before this call
        }
    }

    void List::LoadSubChunksRecursively() {
        for (List* pList = GetFirstSubList(); pList; pList = GetNextSubList())
            pList->LoadSubChunksRecursively();
    }

    /** @brief Write list chunk persistently e.g. to disk.
     *
     * Stores the list chunk persistently to its actual "physical" file. All
     * subchunks (including sub list chunks) will be stored recursively as
     * well.
     *
     * @param ulWritePos - position within the "physical" file where this
     *                     list chunk should be written to
     * @param ulCurrentDataOffset - offset of current (old) data within
     *                              the file
     * @returns new write position in the "physical" file, that is
     *          \a ulWritePos incremented by this list chunk's new size
     *          (including its header size of course)
     */
    unsigned long List::WriteChunk(unsigned long ulWritePos, unsigned long ulCurrentDataOffset) {
        const unsigned long ulOriginalPos = ulWritePos;
        ulWritePos += LIST_HEADER_SIZE;

        if (pFile->Mode != stream_mode_read_write)
            throw Exception("Cannot write list chunk, file has to be opened in read+write mode");

        // write all subchunks (including sub list chunks) recursively
        if (pSubChunks) {
            for (ChunkList::iterator iter = pSubChunks->begin(), end = pSubChunks->end(); iter != end; ++iter) {
                ulWritePos = (*iter)->WriteChunk(ulWritePos, ulCurrentDataOffset);
            }
        }

        // update this list chunk's header
        CurrentChunkSize = NewChunkSize = ulWritePos - ulOriginalPos - LIST_HEADER_SIZE;
        WriteHeader(ulOriginalPos);

        // offset of this list chunk in new written file may have changed
        ulStartPos = ulOriginalPos + LIST_HEADER_SIZE;

        return ulWritePos;
    }

    void List::__resetPos() {
        Chunk::__resetPos();
        if (pSubChunks) {
            for (ChunkList::iterator iter = pSubChunks->begin(), end = pSubChunks->end(); iter != end; ++iter) {
                (*iter)->__resetPos();
            }
        }
    }

    /**
     *  Returns string representation of the lists's id
     */
    String List::GetListTypeString() {
        return convertToString(ListType);
    }



// *************** File ***************
// *

//HACK: to avoid breaking DLL compatibility to older versions of libgig we roll the new std::set<Chunk*> into the old std::list<Chunk*> container, should be replaced on member variable level soon though
#define _GET_RESIZED_CHUNKS() \
	(reinterpret_cast<std::set<Chunk*>*>(ResizedChunks.front()))

    /** @brief Create new RIFF file.
     *
     * Use this constructor if you want to create a new RIFF file completely
     * "from scratch". Note: there must be no empty chunks or empty list
     * chunks when trying to make the new RIFF file persistent with Save()!
     *
     * Note: by default, the RIFF file will be saved in native endian
     * format; that is, as a RIFF file on little-endian machines and
     * as a RIFX file on big-endian. To change this behaviour, call
     * SetByteOrder() before calling Save().
     *
     * @param FileType - four-byte identifier of the RIFF file type
     * @see AddSubChunk(), AddSubList(), SetByteOrder()
     */
    File::File(uint32_t FileType) : List(this) {
        //HACK: see _GET_RESIZED_CHUNKS() comment
        ResizedChunks.push_back(reinterpret_cast<Chunk*>(new std::set<Chunk*>));
        #if defined(WIN32)
        hFileRead = hFileWrite = INVALID_HANDLE_VALUE;
        #else
        hFileRead = hFileWrite = 0;
        #endif
        Mode = stream_mode_closed;
        bEndianNative = true;
        ulStartPos = RIFF_HEADER_SIZE;
        ListType = FileType;
    }

    /** @brief Load existing RIFF file.
     *
     * Loads an existing RIFF file with all its chunks.
     *
     * @param path - path and file name of the RIFF file to open
     * @throws RIFF::Exception if error occured while trying to load the
     *                         given RIFF file
     */
    File::File(const String& path) : List(this), Filename(path) {
       #if DEBUG
       std::cout << "File::File("<<path<<")" << std::endl;
       #endif // DEBUG
        try {
            bEndianNative = true;
            //HACK: see _GET_RESIZED_CHUNKS() comment
            ResizedChunks.push_back(reinterpret_cast<Chunk*>(new std::set<Chunk*>));
            #if POSIX
            hFileRead = hFileWrite = open(path.c_str(), O_RDONLY | O_NONBLOCK);
            if (hFileRead <= 0) {
                hFileRead = hFileWrite = 0;
                throw RIFF::Exception("Can't open \"" + path + "\"");
            }
            #elif defined(WIN32)
            hFileRead = hFileWrite = CreateFile(
                                         path.c_str(), GENERIC_READ,
                                         FILE_SHARE_READ | FILE_SHARE_WRITE,
                                         NULL, OPEN_EXISTING,
                                         FILE_ATTRIBUTE_NORMAL |
                                         FILE_FLAG_RANDOM_ACCESS, NULL
                                     );
            if (hFileRead == INVALID_HANDLE_VALUE) {
                hFileRead = hFileWrite = INVALID_HANDLE_VALUE;
                throw RIFF::Exception("Can't open \"" + path + "\"");
            }
            #else
            hFileRead = hFileWrite = fopen(path.c_str(), "rb");
            if (!hFileRead) throw RIFF::Exception("Can't open \"" + path + "\"");
            #endif // POSIX
            Mode = stream_mode_read;
            ulStartPos = RIFF_HEADER_SIZE;
            ReadHeader(0);
            if (ChunkID != CHUNK_ID_RIFF && ChunkID != CHUNK_ID_RIFX) {
                throw RIFF::Exception("Not a RIFF file");
            }
        }
        catch (...) {
            Cleanup();
            throw;
        }
    }

    String File::GetFileName() {
        return Filename;
    }

    stream_mode_t File::GetMode() {
        return Mode;
    }

    /** @brief Change file access mode.
     *
     * Changes files access mode either to read-only mode or to read/write
     * mode.
     *
     * @param NewMode - new file access mode
     * @returns true if mode was changed, false if current mode already
     *          equals new mode
     * @throws RIFF::Exception if new file access mode is unknown
     */
    bool File::SetMode(stream_mode_t NewMode) {
        if (NewMode != Mode) {
            switch (NewMode) {
                case stream_mode_read:
                    #if POSIX
                    if (hFileRead) close(hFileRead);
                    hFileRead = hFileWrite = open(Filename.c_str(), O_RDONLY | O_NONBLOCK);
                    if (hFileRead < 0) {
                        hFileRead = hFileWrite = 0;
                        throw Exception("Could not (re)open file \"" + Filename + "\" in read mode");
                    }
                    #elif defined(WIN32)
                    if (hFileRead != INVALID_HANDLE_VALUE) CloseHandle(hFileRead);
                    hFileRead = hFileWrite = CreateFile(
                                                 Filename.c_str(), GENERIC_READ,
                                                 FILE_SHARE_READ | FILE_SHARE_WRITE,
                                                 NULL, OPEN_EXISTING,
                                                 FILE_ATTRIBUTE_NORMAL |
                                                 FILE_FLAG_RANDOM_ACCESS,
                                                 NULL
                                             );
                    if (hFileRead == INVALID_HANDLE_VALUE) {
                        hFileRead = hFileWrite = INVALID_HANDLE_VALUE;
                        throw Exception("Could not (re)open file \"" + Filename + "\" in read mode");
                    }
                    #else
                    if (hFileRead) fclose(hFileRead);
                    hFileRead = hFileWrite = fopen(Filename.c_str(), "rb");
                    if (!hFileRead) throw Exception("Could not (re)open file \"" + Filename + "\" in read mode");
                    #endif
                    __resetPos(); // reset read/write position of ALL 'Chunk' objects
                    break;
                case stream_mode_read_write:
                    #if POSIX
                    if (hFileRead) close(hFileRead);
                    hFileRead = hFileWrite = open(Filename.c_str(), O_RDWR | O_NONBLOCK);
                    if (hFileRead < 0) {
                        hFileRead = hFileWrite = open(Filename.c_str(), O_RDONLY | O_NONBLOCK);
                        throw Exception("Could not open file \"" + Filename + "\" in read+write mode");
                    }
                    #elif defined(WIN32)
                    if (hFileRead != INVALID_HANDLE_VALUE) CloseHandle(hFileRead);
                    hFileRead = hFileWrite = CreateFile(
                                                 Filename.c_str(),
                                                 GENERIC_READ | GENERIC_WRITE,
                                                 FILE_SHARE_READ,
                                                 NULL, OPEN_ALWAYS,
                                                 FILE_ATTRIBUTE_NORMAL |
                                                 FILE_FLAG_RANDOM_ACCESS,
                                                 NULL
                                             );
                    if (hFileRead == INVALID_HANDLE_VALUE) {
                        hFileRead = hFileWrite = CreateFile(
                                                     Filename.c_str(), GENERIC_READ,
                                                     FILE_SHARE_READ | FILE_SHARE_WRITE,
                                                     NULL, OPEN_EXISTING,
                                                     FILE_ATTRIBUTE_NORMAL |
                                                     FILE_FLAG_RANDOM_ACCESS,
                                                     NULL
                                                 );
                        throw Exception("Could not (re)open file \"" + Filename + "\" in read+write mode");
                    }
                    #else
                    if (hFileRead) fclose(hFileRead);
                    hFileRead = hFileWrite = fopen(Filename.c_str(), "r+b");
                    if (!hFileRead) {
                        hFileRead = hFileWrite = fopen(Filename.c_str(), "rb");
                        throw Exception("Could not open file \"" + Filename + "\" in read+write mode");
                    }
                    #endif
                    __resetPos(); // reset read/write position of ALL 'Chunk' objects
                    break;
                case stream_mode_closed:
                    #if POSIX
                    if (hFileRead)  close(hFileRead);
                    if (hFileWrite) close(hFileWrite);
                    #elif defined(WIN32)
                    if (hFileRead  != INVALID_HANDLE_VALUE) CloseHandle(hFileRead);
                    if (hFileWrite != INVALID_HANDLE_VALUE) CloseHandle(hFileWrite);
                    #else
                    if (hFileRead)  fclose(hFileRead);
                    if (hFileWrite) fclose(hFileWrite);
                    #endif
                    hFileRead = hFileWrite = 0;
                    break;
                default:
                    throw Exception("Unknown file access mode");
            }
            Mode = NewMode;
            return true;
        }
        return false;
    }

    /** @brief Set the byte order to be used when saving.
     *
     * Set the byte order to be used in the file. A value of
     * endian_little will create a RIFF file, endian_big a RIFX file
     * and endian_native will create a RIFF file on little-endian
     * machines and RIFX on big-endian machines.
     *
     * @param Endian - endianess to use when file is saved.
     */
    void File::SetByteOrder(endian_t Endian) {
        #if WORDS_BIGENDIAN
        bEndianNative = Endian != endian_little;
        #else
        bEndianNative = Endian != endian_big;
        #endif
    }

    /** @brief Save changes to same file.
     *
     * Make all changes of all chunks persistent by writing them to the
     * actual (same) file. The file might temporarily grow to a higher size
     * than it will have at the end of the saving process, in case chunks
     * were grown.
     *
     * @throws RIFF::Exception if there is an empty chunk or empty list
     *                         chunk or any kind of IO error occured
     */
    void File::Save() {
        // make sure the RIFF tree is built (from the original file)
        LoadSubChunksRecursively();

        // reopen file in write mode
        SetMode(stream_mode_read_write);

        // to be able to save the whole file without loading everything into
        // RAM and without having to store the data in a temporary file, we
        // enlarge the file with the sum of all _positive_ chunk size
        // changes, move current data towards the end of the file with the
        // calculated sum and finally update / rewrite the file by copying
        // the old data back to the right position at the beginning of the file

        // first we sum up all positive chunk size changes (and skip all negative ones)
        unsigned long ulPositiveSizeDiff = 0;
        std::set<Chunk*>* resizedChunks = _GET_RESIZED_CHUNKS();
        for (std::set<Chunk*>::const_iterator iter = resizedChunks->begin(), end = resizedChunks->end(); iter != end; ++iter) {
            if ((*iter)->GetNewSize() == 0) {
                throw Exception("There is at least one empty chunk (zero size): " + __resolveChunkPath(*iter));
            }
            unsigned long newSizePadded = (*iter)->GetNewSize() + (*iter)->GetNewSize() % 2;
            unsigned long oldSizePadded = (*iter)->GetSize() + (*iter)->GetSize() % 2;
            if (newSizePadded > oldSizePadded) ulPositiveSizeDiff += newSizePadded - oldSizePadded;
        }

        unsigned long ulWorkingFileSize = GetFileSize();

        // if there are positive size changes...
        if (ulPositiveSizeDiff > 0) {
            // ... we enlarge this file first ...
            ulWorkingFileSize += ulPositiveSizeDiff;
            ResizeFile(ulWorkingFileSize);
            // ... and move current data by the same amount towards end of file.
            int8_t* pCopyBuffer = new int8_t[4096];
            const unsigned long ulFileSize = GetSize() + RIFF_HEADER_SIZE;
            #if defined(WIN32)
            DWORD iBytesMoved = 1; // we have to pass it via pointer to the Windows API, thus the correct size must be ensured
            #else
            int iBytesMoved = 1;
            #endif
            for (unsigned long ulPos = ulFileSize; iBytesMoved > 0; ) {
                iBytesMoved = (ulPos < 4096) ? ulPos : 4096;
                ulPos -= iBytesMoved;
                #if POSIX
                lseek(hFileRead, ulPos, SEEK_SET);
                iBytesMoved = read(hFileRead, pCopyBuffer, iBytesMoved);
                lseek(hFileWrite, ulPos + ulPositiveSizeDiff, SEEK_SET);
                iBytesMoved = write(hFileWrite, pCopyBuffer, iBytesMoved);
                #elif defined(WIN32)
                SetFilePointer(hFileRead, ulPos, NULL/*32 bit*/, FILE_BEGIN);
                ReadFile(hFileRead, pCopyBuffer, iBytesMoved, &iBytesMoved, NULL);
                SetFilePointer(hFileWrite, ulPos + ulPositiveSizeDiff, NULL/*32 bit*/, FILE_BEGIN);
                WriteFile(hFileWrite, pCopyBuffer, iBytesMoved, &iBytesMoved, NULL);
                #else
                fseek(hFileRead, ulPos, SEEK_SET);
                iBytesMoved = fread(pCopyBuffer, 1, iBytesMoved, hFileRead);
                fseek(hFileWrite, ulPos + ulPositiveSizeDiff, SEEK_SET);
                iBytesMoved = fwrite(pCopyBuffer, 1, iBytesMoved, hFileWrite);
                #endif
            }
            delete[] pCopyBuffer;
            if (iBytesMoved < 0) throw Exception("Could not modify file while trying to enlarge it");
        }

        // rebuild / rewrite complete RIFF tree
        unsigned long ulTotalSize  = WriteChunk(0, ulPositiveSizeDiff);
        unsigned long ulActualSize = __GetFileSize(hFileWrite);

        // resize file to the final size
        if (ulTotalSize < ulActualSize) ResizeFile(ulTotalSize);

        // forget all resized chunks
        resizedChunks->clear();
    }

    /** @brief Save changes to another file.
     *
     * Make all changes of all chunks persistent by writing them to another
     * file. <b>Caution:</b> this method is optimized for writing to
     * <b>another</b> file, do not use it to save the changes to the same
     * file! Use File::Save() in that case instead! Ignoring this might
     * result in a corrupted file, especially in case chunks were resized!
     *
     * After calling this method, this File object will be associated with
     * the new file (given by \a path) afterwards.
     *
     * @param path - path and file name where everything should be written to
     */
    void File::Save(const String& path) {
        //TODO: we should make a check here if somebody tries to write to the same file and automatically call the other Save() method in that case

        // make sure the RIFF tree is built (from the original file)
        LoadSubChunksRecursively();

        if (Filename.length() > 0) SetMode(stream_mode_read);
        // open the other (new) file for writing and truncate it to zero size
        #if POSIX
        hFileWrite = open(path.c_str(), O_RDWR | O_CREAT, S_IRUSR | S_IWUSR | S_IRGRP);
        if (hFileWrite < 0) {
            hFileWrite = hFileRead;
            throw Exception("Could not open file \"" + path + "\" for writing");
        }
        #elif defined(WIN32)
        hFileWrite = CreateFile(
                         path.c_str(), GENERIC_WRITE, FILE_SHARE_READ,
                         NULL, OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL |
                         FILE_FLAG_RANDOM_ACCESS, NULL
                     );
        if (hFileWrite == INVALID_HANDLE_VALUE) {
            hFileWrite = hFileRead;
            throw Exception("Could not open file \"" + path + "\" for writing");
        }
        #else
        hFileWrite = fopen(path.c_str(), "w+b");
        if (!hFileWrite) {
            hFileWrite = hFileRead;
            throw Exception("Could not open file \"" + path + "\" for writing");
        }
        #endif // POSIX
        Mode = stream_mode_read_write;

        // write complete RIFF tree to the other (new) file
        unsigned long ulTotalSize  = WriteChunk(0, 0);
        unsigned long ulActualSize = __GetFileSize(hFileWrite);

        // resize file to the final size (if the file was originally larger)
        if (ulTotalSize < ulActualSize) ResizeFile(ulTotalSize);

        // forget all resized chunks
        _GET_RESIZED_CHUNKS()->clear();

        #if POSIX
        if (hFileWrite) close(hFileWrite);
        #elif defined(WIN32)
        if (hFileWrite != INVALID_HANDLE_VALUE) CloseHandle(hFileWrite);
        #else
        if (hFileWrite) fclose(hFileWrite);
        #endif
        hFileWrite = hFileRead;

        // associate new file with this File object from now on
        Filename = path;
        Mode = (stream_mode_t) -1;       // Just set it to an undefined mode ...
        SetMode(stream_mode_read_write); // ... so SetMode() has to reopen the file handles.
    }

    void File::ResizeFile(unsigned long ulNewSize) {
        #if POSIX
        if (ftruncate(hFileWrite, ulNewSize) < 0)
            throw Exception("Could not resize file \"" + Filename + "\"");
        #elif defined(WIN32)
        if (
            SetFilePointer(hFileWrite, ulNewSize, NULL/*32 bit*/, FILE_BEGIN) == INVALID_SET_FILE_POINTER ||
            !SetEndOfFile(hFileWrite)
        ) throw Exception("Could not resize file \"" + Filename + "\"");
        #else
        # error Sorry, this version of libgig only supports POSIX and Windows systems yet.
        # error Reason: portable implementation of RIFF::File::ResizeFile() is missing (yet)!
        #endif
    }

    File::~File() {
       #if DEBUG
       std::cout << "File::~File()" << std::endl;
       #endif // DEBUG
        Cleanup();
    }

    void File::Cleanup() {
        #if POSIX
        if (hFileRead) close(hFileRead);
        #elif defined(WIN32)
        if (hFileRead != INVALID_HANDLE_VALUE) CloseHandle(hFileRead);
        #else
        if (hFileRead) fclose(hFileRead);
        #endif // POSIX
        DeleteChunkList();
        pFile = NULL;
        //HACK: see _GET_RESIZED_CHUNKS() comment
        delete _GET_RESIZED_CHUNKS();
    }

    void File::LogAsResized(Chunk* pResizedChunk) {
        _GET_RESIZED_CHUNKS()->insert(pResizedChunk);
    }

    void File::UnlogResized(Chunk* pResizedChunk) {
        _GET_RESIZED_CHUNKS()->erase(pResizedChunk);
    }

    unsigned long File::GetFileSize() {
        return __GetFileSize(hFileRead);
    }

    #if POSIX
    unsigned long File::__GetFileSize(int hFile) {
        struct stat filestat;
        fstat(hFile, &filestat);
        long size = filestat.st_size;
        return size;
    }
    #elif defined(WIN32)
    unsigned long File::__GetFileSize(HANDLE hFile) {
        DWORD dwSize = ::GetFileSize(hFile, NULL /*32bit*/);
        if (dwSize == INVALID_FILE_SIZE)
            throw Exception("Windows FS error: could not determine file size");
        return dwSize;
    }
    #else // standard C functions
    unsigned long File::__GetFileSize(FILE* hFile) {
        long curpos = ftell(hFile);
        fseek(hFile, 0, SEEK_END);
        long size = ftell(hFile);
        fseek(hFile, curpos, SEEK_SET);
        return size;
    }
    #endif


// *************** Exception ***************
// *

    void Exception::PrintMessage() {
        std::cout << "RIFF::Exception: " << Message << std::endl;
    }


// *************** functions ***************
// *

    /**
     * Returns the name of this C++ library. This is usually "libgig" of
     * course. This call is equivalent to DLS::libraryName() and
     * gig::libraryName().
     */
    String libraryName() {
        return PACKAGE;
    }

    /**
     * Returns version of this C++ library. This call is equivalent to
     * DLS::libraryVersion() and gig::libraryVersion().
     */
    String libraryVersion() {
        return VERSION;
    }

} // namespace RIFF
