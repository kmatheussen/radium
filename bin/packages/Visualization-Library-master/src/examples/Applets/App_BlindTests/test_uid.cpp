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

//-----------------------------------------------------------------------------

#include <vlCore/UUID.hpp>
#include <vlCore/Random.hpp>
#include <vlCore/checks.hpp>
#include <cstdio>
#include <set>

using namespace vl;

namespace blind_tests
{
  bool test_UID()
  {
    FILE* fout = NULL; 
#if 1 // set to 1 to write the uids to a text file.
    fout = fopen("uid.txt","wt");
#endif

    std::set<vl::UUID> str_set;
    std::string guid_str, guid_str2;
    vl::UUID guid, guid2;
    Random rnd;

    for (size_t x=0; x<5000; x++)
    {
      guid.generateVersion4(rnd);

      // check that no duplicate exists
      // VL_CHECK(str_set.find(guid) == str_set.end())
      if (str_set.find(guid) != str_set.end())
        return false;
      str_set.insert(guid);

      // check conversion to and from string
      guid.toStdString(guid_str);
      guid2.fromString(guid_str.c_str());
      // VL_CHECK(guid2 == guid)
      if (guid2 != guid)
        return false;
      guid2.toStdString(guid_str2);
      // VL_CHECK( guid_str == guid_str2 )
      if ( guid_str != guid_str2 )
        return false;

      // write to file
      if (fout)
      {
        fprintf(fout, "%s\n", guid_str.c_str());
      }
    }

    if (fout)
    {
      fclose(fout); fout = NULL;
    }

    return true;
  }
}

//-----------------------------------------------------------------------------
