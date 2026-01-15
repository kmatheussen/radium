/**************************************************************************************/
/*                                                                                    */
/*  Visualization Library                                                             */
/*  http://www.visualizationlibrary.org                                               */
/*                                                                                    */
/*  Copyright (c) 2005-2010, Michele Bosi                                             */
/*  All rights reserved.                                                              */
/*                                                                                    */
/*  Redistribution and use in source and binary forms, with or without modification,  */
/*  are permitted provided that the following conditions are met:                     */
/*                                                                                    */
/*  - Redistributions of source code must retain the above copyright notice, this     */
/*  list of conditions and the following disclaimer.                                  */
/*                                                                                    */
/*  - Redistributions in binary form must reproduce the above copyright notice, this  */
/*  list of conditions and the following disclaimer in the documentation and/or       */
/*  other materials provided with the distribution.                                   */
/*                                                                                    */
/*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND   */
/*  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED     */
/*  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE            */
/*  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR  */
/*  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES    */
/*  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;      */
/*  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON    */
/*  ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT           */
/*  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS     */
/*  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                      */
/*                                                                                    */
/**************************************************************************************/

#include "ioDAT.hpp"
#include <vlCore/LoadWriterManager.hpp>
#include <vlCore/VisualizationLibrary.hpp>
#include <vlCore/VisualizationLibrary.hpp>
#include <vlCore/FileSystem.hpp>
#include <vlCore/VirtualFile.hpp>
#include <vlCore/Image.hpp>
#include <vlCore/TextStream.hpp>
#include <stdio.h>

using namespace vl;

#include <vlCore/ImageTools.hpp>

//-----------------------------------------------------------------------------
ref<Image> vl::loadDAT( const String& path )
{
  ref<VirtualFile> file = defFileSystem()->locateFile(path);
  if ( !file )
  {
    Log::error( Say("File '%s' not found.\n") << path );
    return NULL;
  }

  return loadDAT(file.get());
}
//-----------------------------------------------------------------------------
ref<Image> vl::loadDAT(VirtualFile* file)
{
  if (!file->open(OM_ReadOnly))
  {
    Log::error( Say("loadDAT: could not find DAT file '%s'.\n") << file->path() );
    return NULL;
  }

  #define BUFFER_SIZE 1024

  ref<TextStream> stream = new TextStream(file);
  std::string line;
  char buffer[BUFFER_SIZE ];
  char filename[BUFFER_SIZE ];
  char typ[BUFFER_SIZE ];
  char fmt[BUFFER_SIZE ];
  float a=0,b=0,c=0;
  int width=0, height=0, depth=0, bytealign=1;
  EImageFormat format;
  EImageType type;

  // safe way, get a line first then sscanf the string
  stream->readLine(line);
  if ( sscanf(line.c_str(), "%s %s", buffer, filename) != 2 )
    return NULL;
  // make sure it is zero terminated
  filename[BUFFER_SIZE-1] = 0;
  stream->readLine(line);
  if ( sscanf(line.c_str(), "%s %d %d %d", buffer, &width, &height, &depth) != 4 )
    return NULL;
  stream->readLine(line);
  if ( sscanf(line.c_str(), "%s %f %f %f", buffer, &a,&b,&c) != 4 )
    return NULL;
  stream->readLine(line);
  if ( sscanf(line.c_str(), "%s %s", buffer, typ) != 2 )
    return NULL;
  // make sure it is zero terminated
  typ[BUFFER_SIZE-1] = 0;
  stream->readLine(line);
  if ( sscanf(line.c_str(), "%s %s", buffer, fmt) != 2 )
    return NULL;
  // make sure it is zero terminated
  fmt[BUFFER_SIZE-1] = 0;
  file->close();

  // strip quotes
  int len = strlen(filename);
  if (len > 2)
  {
    if (filename[0] == '\'' || filename[0] == '"')
      memmove(filename, filename + 1, len);
    len = (int)strlen(filename);
    if (filename[len-1] == '\'' || filename[len-1] == '"')
      filename[len-1] = 0;
  }

  // extract path
  String raw_file = file->path().extractPath() + filename;

  if (String(typ) == "UCHAR")
    type = IT_UNSIGNED_BYTE;
  else
  {
    Log::error( Say("loadDAT('%s'): type '%s' not supported.\n") << file->path() << typ );
    return NULL;
  }

  if (String(fmt) == "LUMINANCE")
    format = IF_LUMINANCE;
  else
  if (String(fmt) == "LUMINANCE_ALPHA")
    format = IF_LUMINANCE_ALPHA;
  else
  if (String(fmt) == "RGB")
    format = IF_RGB;
  else
  if (String(fmt) == "RGBA")
    format = IF_RGBA;
  else
  {
    Log::error( Say("loadDAT('%s'): format '%s' not supported.\n") << file->path() << fmt );
    return NULL;
  }

  ref<VirtualFile> rawf = defFileSystem()->locateFile(raw_file);
  if (rawf)
  {
    return loadRAW( rawf.get(), -1, width, height, depth, bytealign, format, type );
  }
  else
  {
    Log::error( Say("loadDAT('%s'): could not find RAW file '%s'.\n") << file->path() << raw_file );
    return NULL;
  }
}
//-----------------------------------------------------------------------------
bool vl::isDAT(VirtualFile* file)
{
  #define BUFFER_SIZE 1024

  if (!file->open(OM_ReadOnly))
    return false;

  ref<TextStream> stream = new TextStream(file);
  std::string line;
  char buffer[BUFFER_SIZE];
  char filename[BUFFER_SIZE];
  char typ[BUFFER_SIZE];
  char fmt[BUFFER_SIZE];
  float a=0,b=0,c=0;
  int width=0, height=0, depth=0;
  EImageFormat format;
  EImageType type;

  // safe way, get a line first then sscanf the string
  stream->readLine(line);
  if ( sscanf(line.c_str(), "%s %s", buffer, filename) != 2 )
    return false;
  // make sure it is zero terminated
  filename[BUFFER_SIZE-1] = 0;
  stream->readLine(line);
  if ( sscanf(line.c_str(), "%s %d %d %d", buffer, &width, &height, &depth) != 4 )
    return false;
  stream->readLine(line);
  if ( sscanf(line.c_str(), "%s %f %f %f", buffer, &a,&b,&c) != 4 )
    return false;
  stream->readLine(line);
  if ( sscanf(line.c_str(), "%s %s", buffer, typ) != 2 )
    return false;
  // make sure it is zero terminated
  typ[BUFFER_SIZE-1] = 0;
  stream->readLine(line);
  if ( sscanf(line.c_str(), "%s %s", buffer, fmt) != 2 )
    return false;
  // make sure it is zero terminated
  fmt[BUFFER_SIZE-1] = 0;
  file->close();

  // strip quotes
  int len = strlen(filename);
  if (len > 2)
  {
    if (filename[0] == '\'' || filename[0] == '"')
      memmove(filename,filename + 1, len);
    len = (int)strlen(filename);
    if (filename[len-1] == '\'' || filename[len-1] == '"')
      filename[len-1] = 0;
  }

  // extract path
  String raw_file = file->path().extractPath() + filename;

  // check type
  if (String(typ) == "UCHAR")
    type = IT_UNSIGNED_BYTE;
  else
    return false;

  if (String(fmt) == "LUMINANCE")
    format = IF_LUMINANCE;
  else
  if (String(fmt) == "LUMINANCE_ALPHA")
    format = IF_LUMINANCE_ALPHA;
  else
  if (String(fmt) == "RGB")
    format = IF_RGB;
  else
  if (String(fmt) == "RGBA")
    format = IF_RGBA;
  else
    return false;

  if (defFileSystem()->locateFile(raw_file))
    return true;
  else
    return false;
}
//-----------------------------------------------------------------------------
