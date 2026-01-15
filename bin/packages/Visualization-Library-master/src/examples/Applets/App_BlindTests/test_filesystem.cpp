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

#include <vlCore/VisualizationLibrary.hpp>
#include <vlCore/LoadWriterManager.hpp>
#include <vlGraphics/Effect.hpp>
#include <vlCore/ZippedDirectory.hpp>
#include <vlGraphics/Array.hpp>
#include <vlCore/DiskFile.hpp>
#include <vlCore/GZipCodec.hpp>
#include <vlCore/FileSystem.hpp>
#include <vlGraphics/Text.hpp>
#include <vlGraphics/FontManager.hpp>
#include <vlCore/TextStream.hpp>
#include <vlGraphics/Geometry.hpp>
#include <time.h>

using namespace vl;

#define CONDITION(cond)                                \
  if (!(cond))                                         \
  {                                                    \
    vl::Log::print( Say("%s %n: condition \""#cond"\" failed.\n") << __FILE__ << __LINE__); \
    return false;                                      \
  }

namespace blind_tests
{
  bool testDataIO();
  bool testGZipStream();

  bool test_filesystem()
  {
    srand((unsigned int)time(NULL));

    // actual test

    CONDITION(testDataIO());

    CONDITION(testGZipStream());

    // install the zipped directory
    vl::ref<vl::ZippedDirectory> zdir = new vl::ZippedDirectory("/ztest.zip");
    vl::ref<vl::VirtualFile> vfile;

    // seek test
    vfile = zdir->file("/seek.dat");
    vfile->open(vl::OM_ReadOnly);
    long long seek_size = vfile->size();
    for(int i=0; i<100; ++i)
    {
      int pos = int(rand() % ( (seek_size-64*4)/4 ));
      vfile->seekSet( pos*4 );
      int buffer[64];
      memset(buffer,0,sizeof(int)*64);
      vfile->read(buffer, sizeof(int)*64);
      CONDITION(buffer[0] == pos)
    }
    vfile->close();

    vfile = zdir->file("/triangle_fur.gs");
    std::vector<vl::String> files;
    zdir->listFilesRecursive(files);
    vl::Log::print("Zip file content:\n");
    for(unsigned i=0; i<files.size(); ++i)
    {
      // check decompression
      vl::ref<vl::ZippedFile> zfile = zdir->zippedFile(files[i]);
      unsigned int crc32 = zfile->crc32();
      vl::Log::print( vl::Say("%s: comp.meth. = %n; crc = %s; size = %n/%n\n")
        << files[i].toStdString().c_str()
        << zfile->zippedFileInfo()->compressionMethod()
        << (zfile->zippedFileInfo()->crc32() == crc32 ? "OK":"BAD")
        << zfile->zippedFileInfo()->compressedSize()
        << zfile->size()
        );
      CONDITION(zfile->zippedFileInfo()->crc32() == crc32)

      // check concurrent reading
      if ( zfile->path().endsWith(".gs") || zfile->path().endsWith(".txt") )
      {
        vl::ref<vl::TextStream> txtstream1 = new vl::TextStream(zdir->file(files[i]).get());
        vl::ref<vl::TextStream> txtstream2 = new vl::TextStream(zdir->file(files[i]).get());
        vl::String line1, line2, text1, text2;

        for(int i=0; i<5; ++i)
        {
          if (txtstream1->readLine(line1))
          {
            text1 += line1;
            vl::Log::print( vl::Say("1==> %s\n") << line1.toStdString().c_str() );
          }
          if (txtstream2->readLine(line2))
          {
            text2 += line2;
            vl::Log::print( vl::Say("2==> %s\n") << line2.toStdString().c_str() );
          }
        }

        CONDITION(text1 == text2)
      }
    }

    // OBJ and material loading from zip file

    vl::defFileSystem()->directories().push_back(zdir.get());
    vl::ref<vl::ResourceDatabase> res_db = vl::loadResource("/obj_test_mesh.obj");

    vl::Log::print( vl::Say("obj geometries = %n\n") << res_db->count<vl::Geometry>() );
    vl::Log::print( vl::Say("obj materials  = %n\n") << res_db->count<vl::Material>() );
    vl::Log::print( vl::Say("obj effects    = %n\n") << res_db->count<vl::Effect>() );

    CONDITION(res_db->count<vl::Geometry>() != 0)    
    CONDITION(res_db->count<vl::Material>() != 0)
    CONDITION(res_db->count<vl::Effect>()   != 0)

    return true;
  }

  bool testGZipStream()
  {
    std::vector<vl::String> file_list;
    vl::defFileSystem()->listFilesRecursive(file_list,"/images/*");

    for( unsigned i=0; i<file_list.size(); ++i )
    {
      printf("\nfile = '%s'\n", file_list[i].toStdString().c_str());

      vl::ref<vl::VirtualFile> file         = vl::locateFile(file_list[i]);
      vl::ref<vl::VirtualFile> gzip_outfile = new vl::DiskFile("temp_file.gz");
      vl::ref<vl::GZipCodec>   gzip_codec   = new vl::GZipCodec;

      gzip_codec->setStream(gzip_outfile.get());
      gzip_codec->setCompressionLevel(rand()%10); // 0-9

      // read original file
      std::vector<unsigned char> buffer;
      buffer.resize( (int)file->size() );
      file->open(vl::OM_ReadOnly);
      file->read(&buffer[0], buffer.size());
      file->close();

      // write compressed file
      gzip_codec->setStream(gzip_outfile.get());
      gzip_codec->open(vl::OM_WriteOnly);
      for(unsigned i=0; i<buffer.size();)
      {
        int remains = (int)buffer.size() - i;
        int bytes = rand()%(remains+1);
        gzip_codec->write(&buffer[i], bytes);
        i+=bytes;
      }
      // gzip_codec->write(&buffer[0], buffer.size());
      gzip_codec->close();

      printf("compression ratio = %.2f\n", gzip_codec->compressionRatio());

      // compute md5 of the original file
      vl::MD5CheckSum md5_a = file->md5();
      // compute md5 from the compressed files
      vl::MD5CheckSum md5_b = gzip_codec->md5();

      printf("md5 == %s\n", md5_a.toStdString().c_str());
      printf("md5 == %s\n", md5_b.toStdString().c_str());
      CONDITION( md5_a == md5_b )
    }
    return true;
  }

  bool testDataIO()
  {
    bool ok = true;
    vl::DiskFile fdata;

    // --- double ---
    std::vector<double> f64_data_1;
    std::vector<double> f64_data_2;
    for(double i=-10; i<=10; ++i)
      f64_data_1.push_back(i);
    f64_data_2.resize(f64_data_1.size());

    fdata.open("test_data_io.dat", vl::OM_WriteOnly);
      fdata.writeDouble(&f64_data_1[0], f64_data_1.size(), true);
    fdata.close();
    fdata.open(vl::OM_ReadOnly);
      fdata.readDouble(&f64_data_2[0], f64_data_2.size(), true);
    fdata.close();
    ok &= memcmp(&f64_data_1[0], &f64_data_2[0], sizeof(f64_data_1[0])*f64_data_1.size()) == 0;
    VL_CHECK(ok);

    fdata.open(vl::OM_WriteOnly);
      fdata.writeDouble(&f64_data_1[0], f64_data_1.size(), false);
    fdata.close();
    fdata.open(vl::OM_ReadOnly);
      fdata.readDouble(&f64_data_2[0], f64_data_2.size(), false);
    fdata.close();
    ok &= memcmp(&f64_data_1[0], &f64_data_2[0], sizeof(f64_data_1[0])*f64_data_1.size()) == 0;
    VL_CHECK(ok);

    // --- float ---
    std::vector<float> f32_data_1;
    std::vector<float> f32_data_2;
    for(float i=-10; i<=10; ++i)
      f32_data_1.push_back(i);
    f32_data_2.resize(f32_data_1.size());

    fdata.open(vl::OM_WriteOnly);
      fdata.writeFloat(&f32_data_1[0], f32_data_1.size(), true);
    fdata.close();
    fdata.open(vl::OM_ReadOnly);
      fdata.readFloat(&f32_data_2[0], f32_data_2.size(), true);
    fdata.close();
    ok &= memcmp(&f32_data_1[0], &f32_data_2[0], sizeof(f32_data_1[0])*f32_data_1.size()) == 0;
    VL_CHECK(ok);

    fdata.open(vl::OM_WriteOnly);
      fdata.writeFloat(&f32_data_1[0], f32_data_1.size(), false);
    fdata.close();
    fdata.open(vl::OM_ReadOnly);
      fdata.readFloat(&f32_data_2[0], f32_data_2.size(), false);
    fdata.close();
    ok &= memcmp(&f32_data_1[0], &f32_data_2[0], sizeof(f32_data_1[0])*f32_data_1.size()) == 0;
    VL_CHECK(ok);

    // --- unsigned long long ---
    std::vector<unsigned long long> u64_data_1;
    std::vector<unsigned long long> u64_data_2;
    for(unsigned long long i=0; i<=10; ++i)
      u64_data_1.push_back(i);
    u64_data_2.resize(u64_data_1.size());

    fdata.open(vl::OM_WriteOnly);
      fdata.writeUInt64(&u64_data_1[0], u64_data_1.size(), true);
    fdata.close();
    fdata.open(vl::OM_ReadOnly);
      fdata.readUInt64(&u64_data_2[0], u64_data_2.size(), true);
    fdata.close();
    ok &= memcmp(&u64_data_1[0], &u64_data_2[0], sizeof(u64_data_1[0])*u64_data_1.size()) == 0;
    VL_CHECK(ok);

    fdata.open(vl::OM_WriteOnly);
      fdata.writeUInt64(&u64_data_1[0], u64_data_1.size(), false);
    fdata.close();
    fdata.open(vl::OM_ReadOnly);
      fdata.readUInt64(&u64_data_2[0], u64_data_2.size(), false);
    fdata.close();
    ok &= memcmp(&u64_data_1[0], &u64_data_2[0], sizeof(u64_data_1[0])*u64_data_1.size()) == 0;
    VL_CHECK(ok);

    // --- signed long long ---
    std::vector<signed long long> s64_data_1;
    std::vector<signed long long> s64_data_2;
    for(signed long long i=-10; i<=10; ++i)
      s64_data_1.push_back(i);
    s64_data_2.resize(s64_data_1.size());

    fdata.open(vl::OM_WriteOnly);
      fdata.writeSInt64(&s64_data_1[0], s64_data_1.size(), true);
    fdata.close();
    fdata.open(vl::OM_ReadOnly);
      fdata.readSInt64(&s64_data_2[0], s64_data_2.size(), true);
    fdata.close();
    ok &= memcmp(&s64_data_1[0], &s64_data_2[0], sizeof(s64_data_1[0])*s64_data_1.size()) == 0;
    VL_CHECK(ok);

    fdata.open(vl::OM_WriteOnly);
      fdata.writeSInt64(&s64_data_1[0], s64_data_1.size(), false);
    fdata.close();
    fdata.open(vl::OM_ReadOnly);
      fdata.readSInt64(&s64_data_2[0], s64_data_2.size(), false);
    fdata.close();
    ok &= memcmp(&s64_data_1[0], &s64_data_2[0], sizeof(s64_data_1[0])*s64_data_1.size()) == 0;
    VL_CHECK(ok);

    // --- unsigned long ---
    std::vector<unsigned int> u32_data_1;
    std::vector<unsigned int> u32_data_2;
    for(unsigned long i=0; i<=10; ++i)
      u32_data_1.push_back(i);
    u32_data_2.resize(u32_data_1.size());

    fdata.open(vl::OM_WriteOnly);
      fdata.writeUInt32(&u32_data_1[0], u32_data_1.size(), true);
    fdata.close();
    fdata.open(vl::OM_ReadOnly);
      fdata.readUInt32(&u32_data_2[0], u32_data_2.size(), true);
    fdata.close();
    ok &= memcmp(&u32_data_1[0], &u32_data_2[0], sizeof(u32_data_1[0])*u32_data_1.size()) == 0;
    VL_CHECK(ok);

    fdata.open(vl::OM_WriteOnly);
      fdata.writeUInt32(&u32_data_1[0], u32_data_1.size(), false);
    fdata.close();
    fdata.open(vl::OM_ReadOnly);
      fdata.readUInt32(&u32_data_2[0], u32_data_2.size(), false);
    fdata.close();
    ok &= memcmp(&u32_data_1[0], &u32_data_2[0], sizeof(u32_data_1[0])*u32_data_1.size()) == 0;
    VL_CHECK(ok);

    // --- signed long ---
    std::vector<signed int> s32_data_1;
    std::vector<signed int> s32_data_2;
    for(signed long i=-10; i<=10; ++i)
      s32_data_1.push_back(i);
    s32_data_2.resize(s32_data_1.size());

    fdata.open(vl::OM_WriteOnly);
      fdata.writeSInt32(&s32_data_1[0], s32_data_1.size(), true);
    fdata.close();
    fdata.open(vl::OM_ReadOnly);
      fdata.readSInt32(&s32_data_2[0], s32_data_2.size(), true);
    fdata.close();
    ok &= memcmp(&s32_data_1[0], &s32_data_2[0], sizeof(s32_data_1[0])*s32_data_1.size()) == 0;
    VL_CHECK(ok);

    fdata.open(vl::OM_WriteOnly);
      fdata.writeSInt32(&s32_data_1[0], s32_data_1.size(), false);
    fdata.close();
    fdata.open(vl::OM_ReadOnly);
      fdata.readSInt32(&s32_data_2[0], s32_data_2.size(), false);
    fdata.close();
    ok &= memcmp(&s32_data_1[0], &s32_data_2[0], sizeof(s32_data_1[0])*s32_data_1.size()) == 0;
    VL_CHECK(ok);

    // --- unsigned short ---
    std::vector<unsigned short> u16_data_1;
    std::vector<unsigned short> u16_data_2;
    for(unsigned short i=0; i<=10; ++i)
      u16_data_1.push_back(i);
    u16_data_2.resize(u16_data_1.size());

    fdata.open(vl::OM_WriteOnly);
      fdata.writeUInt16(&u16_data_1[0], u16_data_1.size(), true);
    fdata.close();
    fdata.open(vl::OM_ReadOnly);
      fdata.readUInt16(&u16_data_2[0], u16_data_2.size(), true);
    fdata.close();
    ok &= memcmp(&u16_data_1[0], &u16_data_2[0], sizeof(u16_data_1[0])*u16_data_1.size()) == 0;
    VL_CHECK(ok);

    fdata.open(vl::OM_WriteOnly);
      fdata.writeUInt16(&u16_data_1[0], u16_data_1.size(), false);
    fdata.close();
    fdata.open(vl::OM_ReadOnly);
      fdata.readUInt16(&u16_data_2[0], u16_data_2.size(), false);
    fdata.close();
    ok &= memcmp(&u16_data_1[0], &u16_data_2[0], sizeof(u16_data_1[0])*u16_data_1.size()) == 0;
    VL_CHECK(ok);

    // --- signed short ---
    std::vector<signed short> s16_data_1;
    std::vector<signed short> s16_data_2;
    for(signed short i=-10; i<=10; ++i)
      s16_data_1.push_back(i);
    s16_data_2.resize(s16_data_1.size());

    fdata.open(vl::OM_WriteOnly);
      fdata.writeSInt16(&s16_data_1[0], s16_data_1.size(), true);
    fdata.close();
    fdata.open(vl::OM_ReadOnly);
      fdata.readSInt16(&s16_data_2[0], s16_data_2.size(), true);
    fdata.close();
    ok &= memcmp(&s16_data_1[0], &s16_data_2[0], sizeof(s16_data_1[0])*s16_data_1.size()) == 0;
    VL_CHECK(ok);

    fdata.open(vl::OM_WriteOnly);
      fdata.writeSInt16(&s16_data_1[0], s16_data_1.size(), false);
    fdata.close();
    fdata.open(vl::OM_ReadOnly);
      fdata.readSInt16(&s16_data_2[0], s16_data_2.size(), false);
    fdata.close();
    ok &= memcmp(&s16_data_1[0], &s16_data_2[0], sizeof(s16_data_1[0])*s16_data_1.size()) == 0;
    VL_CHECK(ok);

     // --- unsigned char ---
    std::vector<unsigned char> u8_data_1;
    std::vector<unsigned char> u8_data_2;
    for(unsigned char i=0; i<=10; ++i)
      u8_data_1.push_back(i);
    u8_data_2.resize(u8_data_1.size());

    fdata.open(vl::OM_WriteOnly);
      fdata.writeUInt8(&u8_data_1[0], u8_data_1.size());
    fdata.close();
    fdata.open(vl::OM_ReadOnly);
      fdata.readUInt8(&u8_data_2[0], u8_data_2.size());
    fdata.close();
    ok &= memcmp(&u8_data_1[0], &u8_data_2[0], sizeof(u8_data_1[0])*u8_data_1.size()) == 0;
    VL_CHECK(ok);

    // --- signed char ---
    std::vector<char> s8_data_1;
    std::vector<char> s8_data_2;
    for(char i=-10; i<=10; ++i)
      s8_data_1.push_back(i);
    s8_data_2.resize(s8_data_1.size());

    fdata.open(vl::OM_WriteOnly);
      fdata.writeSInt8(&s8_data_1[0], s8_data_1.size());
    fdata.close();
    fdata.open(vl::OM_ReadOnly);
      fdata.readSInt8(&s8_data_2[0], s8_data_2.size());
    fdata.close();
    ok &= memcmp(&s8_data_1[0], &s8_data_2[0], sizeof(s8_data_1[0])*s8_data_1.size()) == 0;
    VL_CHECK(ok);

    // --- single value functions ---

    // f64
    fdata.open(vl::OM_WriteOnly);
    for(size_t i=0; i<f64_data_1.size(); ++i)
      fdata.writeDouble(f64_data_1[i], true);
    fdata.close();
    memset(&f64_data_2[0], 0, sizeof(f64_data_2[0])*f64_data_2.size());
    fdata.open(vl::OM_ReadOnly);
    for(size_t i=0; i<f64_data_1.size(); ++i)
      f64_data_2[i] = fdata.readDouble(true);
    fdata.close();
    ok &= memcmp(&f64_data_1[0], &f64_data_2[0], sizeof(f64_data_1[0])*f64_data_1.size()) == 0;
    VL_CHECK(ok);

    fdata.open(vl::OM_WriteOnly);
    for(size_t i=0; i<f64_data_1.size(); ++i)
      fdata.writeDouble(f64_data_1[i], false);
    fdata.close();
    memset(&f64_data_2[0], 0, sizeof(f64_data_2[0])*f64_data_2.size());
    fdata.open(vl::OM_ReadOnly);
    for(size_t i=0; i<f64_data_1.size(); ++i)
      f64_data_2[i] = fdata.readDouble(false);
    fdata.close();
    ok &= memcmp(&f64_data_1[0], &f64_data_2[0], sizeof(f64_data_1[0])*f64_data_1.size()) == 0;
    VL_CHECK(ok);

    // f32
    fdata.open(vl::OM_WriteOnly);
    for(size_t i=0; i<f32_data_1.size(); ++i)
      fdata.writeFloat(f32_data_1[i], true);
    fdata.close();
    memset(&f32_data_2[0], 0, sizeof(f32_data_2[0])*f32_data_2.size());
    fdata.open(vl::OM_ReadOnly);
    for(size_t i=0; i<f32_data_1.size(); ++i)
      f32_data_2[i] = fdata.readFloat(true);
    fdata.close();
    ok &= memcmp(&f32_data_1[0], &f32_data_2[0], sizeof(f32_data_1[0])*f32_data_1.size()) == 0;
    VL_CHECK(ok);

    fdata.open(vl::OM_WriteOnly);
    for(size_t i=0; i<f32_data_1.size(); ++i)
      fdata.writeFloat(f32_data_1[i], false);
    fdata.close();
    memset(&f32_data_2[0], 0, sizeof(f32_data_2[0])*f32_data_2.size());
    fdata.open(vl::OM_ReadOnly);
    for(size_t i=0; i<f32_data_1.size(); ++i)
      f32_data_2[i] = fdata.readFloat(false);
    fdata.close();
    ok &= memcmp(&f32_data_1[0], &f32_data_2[0], sizeof(f32_data_1[0])*f32_data_1.size()) == 0;
    VL_CHECK(ok);

    // u64
    fdata.open(vl::OM_WriteOnly);
    for(size_t i=0; i<u64_data_1.size(); ++i)
      fdata.writeUInt64(u64_data_1[i], true);
    fdata.close();
    memset(&u64_data_2[0], 0, sizeof(u64_data_2[0])*u64_data_2.size());
    fdata.open(vl::OM_ReadOnly);
    for(size_t i=0; i<u64_data_1.size(); ++i)
      u64_data_2[i] = fdata.readUInt64(true);
    fdata.close();
    ok &= memcmp(&u64_data_1[0], &u64_data_2[0], sizeof(u64_data_1[0])*u64_data_1.size()) == 0;
    VL_CHECK(ok);

    fdata.open(vl::OM_WriteOnly);
    for(size_t i=0; i<u64_data_1.size(); ++i)
      fdata.writeUInt64(u64_data_1[i], false);
    fdata.close();
    memset(&u64_data_2[0], 0, sizeof(u64_data_2[0])*u64_data_2.size());
    fdata.open(vl::OM_ReadOnly);
    for(size_t i=0; i<u64_data_1.size(); ++i)
      u64_data_2[i] = fdata.readUInt64(false);
    fdata.close();
    ok &= memcmp(&u64_data_1[0], &u64_data_2[0], sizeof(u64_data_1[0])*u64_data_1.size()) == 0;
    VL_CHECK(ok);

    // s64
    fdata.open(vl::OM_WriteOnly);
    for(size_t i=0; i<s64_data_1.size(); ++i)
      fdata.writeSInt64(s64_data_1[i], true);
    fdata.close();
    memset(&s64_data_2[0], 0, sizeof(s64_data_2[0])*s64_data_2.size());
    fdata.open(vl::OM_ReadOnly);
    for(size_t i=0; i<s64_data_1.size(); ++i)
      s64_data_2[i] = fdata.readSInt64(true);
    fdata.close();
    ok &= memcmp(&s64_data_1[0], &s64_data_2[0], sizeof(s64_data_1[0])*s64_data_1.size()) == 0;
    VL_CHECK(ok);

    fdata.open(vl::OM_WriteOnly);
    for(size_t i=0; i<s64_data_1.size(); ++i)
      fdata.writeSInt64(s64_data_1[i], false);
    fdata.close();
    memset(&s64_data_2[0], 0, sizeof(s64_data_2[0])*s64_data_2.size());
    fdata.open(vl::OM_ReadOnly);
    for(size_t i=0; i<s64_data_1.size(); ++i)
      s64_data_2[i] = fdata.readSInt64(false);
    fdata.close();
    ok &= memcmp(&s64_data_1[0], &s64_data_2[0], sizeof(s64_data_1[0])*s64_data_1.size()) == 0;
    VL_CHECK(ok);

    // u32
    fdata.open(vl::OM_WriteOnly);
    for(size_t i=0; i<u32_data_1.size(); ++i)
      fdata.writeUInt32(u32_data_1[i], true);
    fdata.close();
    memset(&u32_data_2[0], 0, sizeof(u32_data_2[0])*u32_data_2.size());
    fdata.open(vl::OM_ReadOnly);
    for(size_t i=0; i<u32_data_1.size(); ++i)
      u32_data_2[i] = fdata.readUInt32(true);
    fdata.close();
    ok &= memcmp(&u32_data_1[0], &u32_data_2[0], sizeof(u32_data_1[0])*u32_data_1.size()) == 0;
    VL_CHECK(ok);

    fdata.open(vl::OM_WriteOnly);
    for(size_t i=0; i<u32_data_1.size(); ++i)
      fdata.writeUInt32(u32_data_1[i], false);
    fdata.close();
    memset(&u32_data_2[0], 0, sizeof(u32_data_2[0])*u32_data_2.size());
    fdata.open(vl::OM_ReadOnly);
    for(size_t i=0; i<u32_data_1.size(); ++i)
      u32_data_2[i] = fdata.readUInt32(false);
    fdata.close();
    ok &= memcmp(&u32_data_1[0], &u32_data_2[0], sizeof(u32_data_1[0])*u32_data_1.size()) == 0;
    VL_CHECK(ok);

    // s32
    fdata.open(vl::OM_WriteOnly);
    for(size_t i=0; i<s32_data_1.size(); ++i)
      fdata.writeSInt32(s32_data_1[i], true);
    fdata.close();
    memset(&s32_data_2[0], 0, sizeof(s32_data_2[0])*s32_data_2.size());
    fdata.open(vl::OM_ReadOnly);
    for(size_t i=0; i<s32_data_1.size(); ++i)
      s32_data_2[i] = fdata.readSInt32(true);
    fdata.close();
    ok &= memcmp(&s32_data_1[0], &s32_data_2[0], sizeof(s32_data_1[0])*s32_data_1.size()) == 0;
    VL_CHECK(ok);

    fdata.open(vl::OM_WriteOnly);
    for(size_t i=0; i<s32_data_1.size(); ++i)
      fdata.writeSInt32(s32_data_1[i], false);
    fdata.close();
    memset(&s32_data_2[0], 0, sizeof(s32_data_2[0])*s32_data_2.size());
    fdata.open(vl::OM_ReadOnly);
    for(size_t i=0; i<s32_data_1.size(); ++i)
      s32_data_2[i] = fdata.readSInt32(false);
    fdata.close();
    ok &= memcmp(&s32_data_1[0], &s32_data_2[0], sizeof(s32_data_1[0])*s32_data_1.size()) == 0;
    VL_CHECK(ok);

    // u16
    fdata.open(vl::OM_WriteOnly);
    for(size_t i=0; i<u16_data_1.size(); ++i)
      fdata.writeUInt16(u16_data_1[i], true);
    fdata.close();
    memset(&u16_data_2[0], 0, sizeof(u16_data_2[0])*u16_data_2.size());
    fdata.open(vl::OM_ReadOnly);
    for(size_t i=0; i<u16_data_1.size(); ++i)
      u16_data_2[i] = fdata.readUInt16(true);
    fdata.close();
    ok &= memcmp(&u16_data_1[0], &u16_data_2[0], sizeof(u16_data_1[0])*u16_data_1.size()) == 0;
    VL_CHECK(ok);

    fdata.open(vl::OM_WriteOnly);
    for(size_t i=0; i<u16_data_1.size(); ++i)
      fdata.writeUInt16(u16_data_1[i], false);
    fdata.close();
    memset(&u16_data_2[0], 0, sizeof(u16_data_2[0])*u16_data_2.size());
    fdata.open(vl::OM_ReadOnly);
    for(size_t i=0; i<u16_data_1.size(); ++i)
      u16_data_2[i] = fdata.readUInt16(false);
    fdata.close();
    ok &= memcmp(&u16_data_1[0], &u16_data_2[0], sizeof(u16_data_1[0])*u16_data_1.size()) == 0;
    VL_CHECK(ok);

    // s16
    fdata.open(vl::OM_WriteOnly);
    for(size_t i=0; i<s16_data_1.size(); ++i)
      fdata.writeSInt16(s16_data_1[i], true);
    fdata.close();
    memset(&s16_data_2[0], 0, sizeof(s16_data_2[0])*s16_data_2.size());
    fdata.open(vl::OM_ReadOnly);
    for(size_t i=0; i<s16_data_1.size(); ++i)
      s16_data_2[i] = fdata.readSInt16(true);
    fdata.close();
    ok &= memcmp(&s16_data_1[0], &s16_data_2[0], sizeof(s16_data_1[0])*s16_data_1.size()) == 0;
    VL_CHECK(ok);

    fdata.open(vl::OM_WriteOnly);
    for(size_t i=0; i<s16_data_1.size(); ++i)
      fdata.writeSInt16(s16_data_1[i], false);
    fdata.close();
    memset(&s16_data_2[0], 0, sizeof(s16_data_2[0])*s16_data_2.size());
    fdata.open(vl::OM_ReadOnly);
    for(size_t i=0; i<s16_data_1.size(); ++i)
      s16_data_2[i] = fdata.readSInt16(false);
    fdata.close();
    ok &= memcmp(&s16_data_1[0], &s16_data_2[0], sizeof(s16_data_1[0])*s16_data_1.size()) == 0;
    VL_CHECK(ok);

    // u8
    fdata.open(vl::OM_WriteOnly);
    for(size_t i=0; i<u8_data_1.size(); ++i)
      fdata.writeUInt8(u8_data_1[i]);
    fdata.close();
    memset(&u8_data_2[0], 0, sizeof(u8_data_2[0])*u8_data_2.size());
    fdata.open(vl::OM_ReadOnly);
    for(size_t i=0; i<u8_data_1.size(); ++i)
      u8_data_2[i] = fdata.readUInt8();
    fdata.close();
    ok &= memcmp(&u8_data_1[0], &u8_data_2[0], sizeof(u8_data_1[0])*u8_data_1.size()) == 0;
    VL_CHECK(ok);

    // s8
    fdata.open(vl::OM_WriteOnly);
    for(size_t i=0; i<s8_data_1.size(); ++i)
      fdata.writeSInt8(s8_data_1[i]);
    fdata.close();
    memset(&s8_data_2[0], 0, sizeof(s8_data_2[0])*s8_data_2.size());
    fdata.open(vl::OM_ReadOnly);
    for(size_t i=0; i<s8_data_1.size(); ++i)
      s8_data_2[i] = fdata.readSInt8();
    fdata.close();
    ok &= memcmp(&s8_data_1[0], &s8_data_2[0], sizeof(s8_data_1[0])*s8_data_1.size()) == 0;
    VL_CHECK(ok);

    return ok;
  }
}
