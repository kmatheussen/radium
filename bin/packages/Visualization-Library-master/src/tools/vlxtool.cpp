

#include <cstdio>
#include <cstdlib>
#include <string.h>
#include <string>
#include <vector>
#include <vlCore/VisualizationLibrary.hpp>
#include <vlCore/Time.hpp>
#include <vlCore/ResourceDatabase.hpp>
#include <vlGraphics/plugins/ioVLX.hpp>
#include <vlGraphics/expandResourceDatabase.hpp>

using namespace vl;

void printHelp()
{
  printf("\nusage:\n");
  printf("  vlxtool -in file1 file file3 ... -out file_out\n");
  printf("\nexamples:\n");
  printf("  >  vlxtool -in file1.obj file2.3ds file3.vlb -out file_out.vlt\n");
  printf("     Merges the contents of file1.obj, file2.3ds and file3.vlb into file_out.vlt:\n\n");
  printf("  >  vlxtool -in file.vlt -out file.vlb\n");
  printf("     Converts a VLT file to its VLB representation:\n\n");
  printf("  >  vlxtool -in file.vlb -out file.vlt\n");
  printf("     Converts a VLB file to its VLT representation:\n\n");
}

int main(int argc, const char* argv[])
{
  VisualizationLibrary::init(true);

  printf("vlxtool 1.0 - Visualization Library VLX Utility\n");
  printf("Converts any type of resource to .vlt/.vlb\n\n");

  std::vector<std::string> in_files;
  String out_file;

  bool input = false;
  bool output = false;

  for(int i=1; i<argc; ++i)
  {
    if ( strcmp(argv[i], "-in") == 0)
    {
      input = true;
      output = false;
    }
    else
    if ( strcmp(argv[i], "-out") == 0)
    {
      input = false;
      output = true;
    }
    else
    if (input)
    {
      in_files.push_back(argv[i]);
    }
    else
    if (output)
    {
      if (out_file.empty())
        out_file = argv[i];
      else
      {
        printf("too many output files.\n");
        return 1;
      }
    }
    else
    {
      printf("Unknown option:'%s'\n", argv[i]);
      printHelp();
      return 1;
    }
  }

  if (in_files.empty() || out_file.empty())
  {
    if (in_files.empty())
      printf("Missing input file list.\n");
    
    if (out_file.empty())
      printf("Missing output file.\n");

    printHelp();
    return 1;
  }

  printf("Loading...\n");
  ref<ResourceDatabase> db = new ResourceDatabase;
  for(size_t i=0; i<in_files.size(); ++i)
  {
    Time timer; timer.start();
    printf("\t%s ", in_files[i].c_str());
    ref<ResourceDatabase> res = vl::loadResource(in_files[i].c_str(), true);
    if (res)
    {
      printf("\t... %.2fs\n", timer.elapsed());
      db->resources().insert(db->resources().end(), res->resources().begin(), res->resources().end());
    }
    else
    {
      printf("\t... FAILED\n");
      return 1;
    }
  }

  expandResourceDatabase(db.get());

  Time timer; timer.start();
  if (out_file.endsWith(".vlt"))
  {
    printf("Saving VLT...\n");
    printf("\t%s ", out_file.toStdString().c_str());
    vl::saveVLT(out_file, db.get());
    printf("\t... %.2fs\n", timer.elapsed());
  }
  else
  if (out_file.endsWith(".vlb"))
  {
    printf("Saving VLB...\n");
    printf("\t%s ", out_file.toStdString().c_str());
    vl::saveVLB(out_file, db.get());
    printf("\t... %.2fs\n", timer.elapsed());
  }
  else
  {
    printf("FAILED: output file must be either a .vlt or .vlb\n");
    return 1;
  }

  return 0;
}
