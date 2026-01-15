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

#include <vlCore/half.hpp>
#include <limits>
#include <vector>

using namespace vl;

namespace blind_tests
{
  bool test_hfloat()
  {
    half  ha, hb;
    ha = 2.0f;
    hb = 4.0f;
    float fa = ha, fb;

    // basic operations
    if (fa != 2.0)
      return false;
    if (ha * hb != 8.0f)
      return false;
    if (ha + hb != 6.0f)
      return false;
    if (ha - hb != -2.0f)
      return false;
    if (hb / ha != 2.0f)
      return false;

    // check no loss of precision between 0 and +-(1<<11)
    for(int i=0; i<=(1<<11); ++i)
    {
      ha = +i;
      hb = -i;
      int ifa = (int)(float)ha;
      int ifb = (int)(float)hb;
      if (ifa != +i)
        return false;
      if (ifb != -i)
        return false;
    }

    // infinity
    ha =  half::infinity(); fa = ha; ha = fa; fa = ha;
    hb = -half::infinity(); fb = hb; hb = fb; fb = hb;
    if ( fa != +std::numeric_limits<float>::infinity())
      return false;
    if ( fb != -std::numeric_limits<float>::infinity())
      return false;

    ha = 3.14159265f;
    fa = ha; 
    if ( ha != fa )
      return false;

    std::vector<half> hvec;
    std::vector<float> fvec;
    const int count = 2000;
    hvec.resize(count);
    fvec.resize(count);

    float err = 0;

    // convertHalfToFloat()
    for (int i=0; i<count; ++i)
    {
      float ff = (i-count/2)*3.14159265358979323846f;
      hvec[i] = ff;
      err = err > fabs(ff-hvec[i]) ? err : fabs(ff-hvec[i]);
    }
    half::convertHalfToFloat(&hvec[0],&fvec[0],hvec.size());
    for (int i=0; i<count; ++i)
    {
      if (hvec[i] != fvec[i])
        return false;
    }
    if (err > 2.0f)
      return false;

    // convertFloatToHalf()
    err = 0;
    hvec.resize(0); hvec.resize(count);
    for (int i=0; i<count; ++i)
    {
      fvec[i] = (i-count/2)*3.14159265358979323846f;
    }
    half::convertFloatToHalf(&fvec[0],&hvec[0],hvec.size());
    for (int i=0; i<count; ++i)
    {
      err = err > fabs(fvec[i]-hvec[i]) ? err : fabs(fvec[i]-hvec[i]);
      if (hvec[i] != fvec[i])
        return false;
    }
    if (err > 2.0f)
      return false;

    // various compilation and conversion checks for vectors and matrices
    hvec3 v1, v2(1,2,3), v3(4,5,6);
    v1 = v2 + v3;
    v1 = v2 - v3;
    v1 = v2 * v3;
    v1 = v2 / v3;
    half l1 = v1.length();
    half l2 = v1.lengthSquared();
    v2 = v1 + (hvec3)fvec3(1,1,1);
    // hmat4 m = fmat4::getRotation( 90, 0, 1, 0 );
    hmat4 m;
    m.translate(1,1,1);
    // m.rotate(90, 0, 1, 0 );
    m.scale(10,10,10);
    m = (hmat4)fmat4::getRotation( 90, 0, 1, 0 );
    v1 = m * hvec3(1,0,0);
    if (v1.x() != 0 || v1.y() != 0 || v1.z() != -1.0f)
      return false;

    return true;
  }
}
