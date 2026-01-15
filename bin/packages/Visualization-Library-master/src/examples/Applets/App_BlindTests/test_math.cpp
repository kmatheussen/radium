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

#include <vlCore/Quaternion.hpp>
#include <vlCore/Say.hpp>
#include <vlCore/Log.hpp>
#include <set>
#include <cstdlib>

using namespace vl;

#define CONDITION(cond) \
  if (!(cond)) \
  { \
    vl::Log::print( Say("%s %n: condition \""#cond"\" failed.\n") << __FILE__ << __LINE__); \
    VL_CHECK(0); \
    return false; \
  }

namespace blind_tests
{
  bool test_quat();
  bool test_matr();
  bool test_glsl_math();

  bool test_math()
  {
    return test_matr() && test_quat() && test_glsl_math();
  }

  bool test_matr()
  {
    //-------------------------------------------------------------------------
    // Matrix compilation and functionality tests
    //-------------------------------------------------------------------------

    // row/column order
    fmat4 m;
    m.e(0,0) = 1.0; m.e(0,1) = 0.0; m.e(0,2) = 0.0; m.e(0,3) = 13.0;
    m.e(1,0) = 0.0; m.e(1,1) = 1.0; m.e(1,2) = 0.0; m.e(1,3) = 17.0;
    m.e(2,0) = 0.0; m.e(2,1) = 0.0; m.e(2,2) = 1.0; m.e(2,3) = 19.0;
    m.e(3,0) = 0.0; m.e(3,1) = 0.0; m.e(3,2) = 0.0; m.e(3,3) =  1.0;
    fvec4 v4 = m * fvec4(0,0,0,1);
    CONDITION(v4 == fvec4(13,17,19,1))
    fvec3 v3;
    fvec2 v2;
    m.setT( fvec3(23,26,29) );
    v4 = m * fvec4(0,0,0,1);
    CONDITION(v4 == fvec4(23,26,29,1))

    // rotazione & concatenazione
    m = fmat4::getRotation( 90, 0,0,1 );
    v4 = m * fvec4(1,0,0,1);
    v3 = m * fvec3(1,0,0);
    v2 = m * fvec2(1,0);
    CONDITION( distance( v4, fvec4(0,1,0,1) ) < 0.001f );
    CONDITION( distance( v3, fvec3(0,1,0)   ) < 0.001f );
    CONDITION( distance( v2, fvec2(0,1)     ) < 0.001f );

    m = fmat4::getRotation( 180, 1,0,0 ) * fmat4::getRotation( 90, 0,0,1 );
    v4 = m * fvec4(1,0,0,1);
    v3 = m * fvec3(1,0,0);
    v2 = m * fvec2(1,0);
    CONDITION( distance( v4, fvec4(0,-1,0,1) ) < 0.001f );
    CONDITION( distance( v3, fvec3(0,-1,0)   ) < 0.001f );
    CONDITION( distance( v2, fvec2(0,-1)     ) < 0.001f );

    // inversione
    fmat4 inv = m.getInverse();
    v4 = inv * m * fvec4(1,0,0,1);
    v3 = inv * m * fvec3(1,0,0);
    v2 = inv * m * fvec2(1,0);
    CONDITION( distance( v4, fvec4(1,0,0,1) ) < 0.001f );
    CONDITION( distance( v3, fvec3(1,0,0)   ) < 0.001f );
    CONDITION( distance( v2, fvec2(1,0)     ) < 0.001f );

    v4 = m * inv * fvec4(1,0,0,1);
    v3 = m * inv * fvec3(1,0,0);
    v2 = m * inv * fvec2(1,0);
    CONDITION( distance( v4, fvec4(1,0,0,1) ) < 0.001f );
    CONDITION( distance( v3, fvec3(1,0,0)   ) < 0.001f );
    CONDITION( distance( v2, fvec2(1,0)     ) < 0.001f );

    // pre-multiply
    m = fmat4::getRotation( -90, 0,0,1 ) * fmat4::getRotation( -180, 1,0,0 );
    v4 = fvec4(1,0,0,1) * m;
    v3 = fvec3(1,0,0) * m;
    v2 = fvec2(1,0) * m;
    CONDITION( distance( v4, fvec4(0,-1,0,1) ) < 0.001f );
    CONDITION( distance( v3, fvec3(0,-1,0)   ) < 0.001f );
    CONDITION( distance( v2, fvec2(0,-1)     ) < 0.001f );

    // getX/Y/Z/T()
    m = fmat4::getTranslation(13,17,19) * fmat4::getRotation( 90, 1,0,0 ) * fmat4::getRotation( 90, 0,0,1 );
    fvec3 x = m.getX();
    fvec3 y = m.getY();
    fvec3 z = m.getZ();
    CONDITION( m.getT() == fvec3(13,17,19) );
    CONDITION( distance(x, fvec3(0,0,+1)) < 0.001f );
    CONDITION( distance(y, fvec3(-1,0,0)) < 0.001f );
    CONDITION( distance(z, fvec3(0,-1,0)) < 0.001f );

    // transposition
    for(int i=0; i<4; ++i)
      for(int j=0; j<4; ++j)
        m.e(i,j) = (float)rand();
    fmat4 m2 = m;
    m2.transpose();
    for(int i=0; i<4; ++i)
    {
      for(int j=0; j<4; ++j)
      {
        CONDITION( m2.e(i,j) = m.e(j,i) );
      }
    }
    CONDITION( m2 == m.getTransposed() )

    // misc
    m = fmat4::getRotation( 90, 1,0,0 ) * fmat4::getRotation( 90, 0,0,1 ) * fmat4::getIdentity();
    m = m * m.getTransposed();
    v4 = m * fvec4(1,0,0,1);
    v3 = m * fvec3(1,0,0);
    v2 = m * fvec2(1,0);
    CONDITION( distance( v4, fvec4(1,0,0,1) ) < 0.001f );
    CONDITION( distance( v3, fvec3(1,0,0)   ) < 0.001f );
    CONDITION( distance( v2, fvec2(1,0)     ) < 0.001f );

    // operators compilation checks
    //m = fmat4() * fmat4();
    //m *= fmat4();
    m = fmat4(3.0f) + fmat4(4.0f);
    m += fmat4(7.0f);
    CONDITION( m == fmat4(14.0f) );
    m = fmat4(10.0f) - fmat4(3.0f);
    m -= fmat4(5.0f);
    CONDITION( m == fmat4(2.0f) );
    // m = fmat4() / fmat4();
    // m /= fmat4();
    m = fmat4(3.0f) * 10.0f;
    m *= 10.0f;
    CONDITION( m == fmat4(300.0f) );
    m = fmat4(0.0f) + 10.0f;
    m += 5.0f;
    CONDITION( m == fmat4().fill(+15.0f) );
    m = fmat4(0.0f) - 10.0f;
    m -= 5.0f;
    CONDITION( m == fmat4().fill(-15.0f) );
    m = fmat4(40.0f) / 10.0f;
    m /= 2.0f;
    CONDITION( m == fmat4(2.0f) );

    CONDITION( fmat4(1) == fmat4(1) );
    CONDITION( fmat4(1) != fmat4(2) );

    return true;
  }

  bool test_quat()
  {
    //-------------------------------------------------------------------------
    // Quaternion compilation and functionality tests
    //-------------------------------------------------------------------------

    std::set<fquat> fqset;
    fqset.insert(fquat());

    std::set<dquat> dqset;
    dqset.insert(dquat());

    dquat qd;
    fquat q1(1,2,3,4), q2( fvec4(1,2,3,4) );
    fquat q3(q1), q4(1,fvec3(2,3,4));
    q2 = fvec4(1,2,3,4);
    fvec4 v1;
    q1 = q2;
    q1 = (fquat)qd;
    qd = (dquat)q1;
    v1 = q1 * v1;
    v1 = q1.xyzw();
    q1 = v1;
    q1 = fquat::getSquad(0, q1,q1,q1,q1);

    q1 = fquat(4,3,2,1);
    q2 = fquat(4,3,2,1);
    CONDITION(q1 == q2 && !(q1 != q2))
    q2 = fquat(3,2,1,0);
    CONDITION(q1 != q2 && !(q1 == q2))

    q1 = fquat(1,2,3,4);
    q1.setZero();
    CONDITION( q1 == fquat(0,0,0,0) );

    q1 = fquat(1,2,3,4);
    q1.setNoRotation();
    CONDITION( q1 == fquat(0,0,0,1) );

    fvec3 v3; float angle = 0;
    q1.setFromAxisAngle(fvec3(0,1,0), 90);
    q1.toAxisAngle(v3, angle);
    CONDITION( distance(v3, fvec3(0,1,0)) < 0.001f );
    CONDITION( angle == 90 );

    q1.setFromVectors(fvec3(1,0,0), fvec3(-1,0,-1));
    q1.toAxisAngle(v3, angle);
    CONDITION( distance(v3, fvec3(0,1,0)) < 0.001f );
    CONDITION( angle == 135 );

    q1.setFromMatrix( fmat4::getRotation(0, 1, 1, 0) );
    q1.toAxisAngle(v3, angle);
    CONDITION( v3 == fvec3(0,0,0) )
    CONDITION( angle == 0 );

    fvec3 A3 = fquat::getFromAxisAngle(fvec3(1,1,0), 35) * fquat::getFromAxisAngle(fvec3(0,1,1), 35) * fquat::getFromAxisAngle(fvec3(1,0,1), 35) * fvec3(1,0,0);
    fvec3 B3 = fmat4::getRotation(35, fvec3(1,1,0)) * fmat4::getRotation(35, fvec3(0,1,1)) * fmat4::getRotation(35, fvec3(1,0,1)) * fvec3(1,0,0);
    CONDITION( distance(A3, B3) < 0.001f )

    fvec4 A4 = fquat::getFromAxisAngle(fvec3(1,1,0), 35) * fquat::getFromAxisAngle(fvec3(0,1,1), 35) * fquat::getFromAxisAngle(fvec3(1,0,1), 35) * fvec4(1,0,0,1);
    fvec4 B4 = fmat4::getRotation(35, fvec3(1,1,0)) * fmat4::getRotation(35, fvec3(0,1,1)) * fmat4::getRotation(35, fvec3(1,0,1)) * fvec4(1,0,0,1);
    CONDITION( distance(A4, B4) < 0.001f )

    q1 = fquat::getZero();
    fquat::getZero(q1);
    q1.setZero();

    q1 = fquat::getNoRotation();
    fquat::getNoRotation(q1);
    q1.setNoRotation();

    q1 = fquat::getFromVectors(fvec3(1,2,3), fvec3(3,2,1));
    fquat::getFromVectors(q1, fvec3(1,2,3), fvec3(3,2,1));
    q1.setFromVectors(fvec3(1,2,3), fvec3(3,2,1));

    fmat3 m3;
    q1 = fquat::getFromMatrix(m3);
    fquat::getFromMatrix(q1, m3);
    q1.setFromMatrix(m3);

    fmat4 m4;
    q1 = fquat::getFromMatrix(m4);
    fquat::getFromMatrix(q1, m4);
    q1.setFromMatrix(m4);

    q1 = fquat::getFromEulerXYZ(10,20,30);
    fquat::getFromEulerXYZ(q1, 10,20,30);
    q1.setFromEulerXYZ(10,20,30);

    q1 = fquat::getFromEulerZYX(10,20,30);
    fquat::getFromEulerZYX(q1, 10,20,30);
    q1.setFromEulerZYX(10,20,30);

    q1 = fquat::getFromAxisAngle( fvec3(1,2,3), 10 );
    fquat::getFromAxisAngle(q1, fvec3(1,2,3), 10);
    q1.setFromAxisAngle(fvec3(1,2,3), 10);

    q1.toMatrix3(m3);
    m3 = q1.toMatrix3();

    q1.toMatrix4(m4);
    m4 = q1.toMatrix4();

    CONDITION(fquat().getFromAxisAngle(fvec3(1,1,0),10).dot(fquat().getFromAxisAngle(fvec3(1,1,0),30)) > 0 );

    q1.normalize();
    q1.getNormalized(q2);
    q2 = q1.getNormalized();

    q1 = q2.getConjugate();
    q2.getConjugate(q1);

    q1 = q2.getInverse();
    q2.getInverse(q1);

    q1 = fquat::getSlerp( 0.5, q1, q2 );
    fquat::getSlerp( q1, 0.5, q1, q2 );

    q1 = fquat::getNlerp( 0.5, q1, q2 );
    fquat::getNlerp( q1, 0.5, q1, q2 );

    q1 = fquat::getSquad( 0.5, q1, q2, q1, q2 );
    fquat::getSquad( q1, 0.5, q1, q2, q1, q2 );

    {
      fquat q1 = fquat::getFromAxisAngle( fvec3(0,1,0), 45.0f*1.0f );
      fquat q2 = fquat::getFromAxisAngle( fvec3(0,1,0), 45.0f*3.0f );

      for(int i=0; i<=100; ++i)
      {
        float t = i / 100.0f;
        fquat qs = fquat::getSlerp( t, q1, q2 );
        fquat qn = fquat::getNlerp( t, q1, q2 );
        fmat4 m  = fmat4::getRotation( 45.0f*1.0f*(1-t) + 45.0f*3.0f*t, 0,1,0 );
        fvec3 v1 = qs * fvec3(1,0,0);
        fvec3 v2 = qn * fvec3(1,0,0);
        fvec3 v3 =  m * fvec3(1,0,0);
        CONDITION( distance(v1,v2) < 0.0175f );
        CONDITION( distance(v1,v3) < 0.001f  );
        CONDITION( distance(v2,v3) < 0.0175f );
      }
    }

    fvec3 faxes[] = { 
      fvec3(+1,+1, 0).normalize(), 
      fvec3( 0,+1,+1).normalize(), 
      fvec3(+1, 0,+1).normalize(), 
      fvec3(-1,-1, 0).normalize(), 
      fvec3( 0,-1,-1).normalize(), 
      fvec3(-1, 0,-1).normalize(), 
      fvec3(1,0,0).normalize(),
      fvec3(0,1,0).normalize(),
      fvec3(0,0,1).normalize(),
      fvec3(0,0,0)
    };

    for (int k=1; k<180/*1800*/; k+=1)
    {
      float i = k / 1.0f/*10.0f*/;
      for(int j=0; !faxes[j].isNull(); ++j )
      {
        fvec3 ax;
        fquat q;
        float alpha;
        float alpha_eps = 0.005f;
        float mat_eps = 0.01f;

        q.setFromMatrix( fmat4::getRotation(i, faxes[j]) );
        q.toAxisAngle(ax, alpha);
        CONDITION( distance(ax, faxes[j]) < 0.001f );
        CONDITION( fabs(alpha - i) < alpha_eps );
        CONDITION( q.toMatrix4().diff(fmat4::getRotation(i, faxes[j])) < mat_eps )

        q.setFromAxisAngle( faxes[j], i );
        q.toAxisAngle(ax, alpha);
        CONDITION( distance(ax, faxes[j]) < 0.001f );
        CONDITION( fabs(alpha - i) < alpha_eps );

        q.setFromMatrix( fmat4::getRotation(-i, faxes[j]) );
        q.toAxisAngle(ax, alpha);
        CONDITION( distance(ax, -faxes[j]) < 0.001f );
        CONDITION( fabs(alpha - i) < alpha_eps );
        CONDITION( q.toMatrix4().diff(fmat4::getRotation(-i, faxes[j])) < mat_eps )

        q.setFromAxisAngle( faxes[j], -i );
        q.toAxisAngle(ax, alpha);
        CONDITION( distance(ax, -faxes[j]) < 0.001f );
        CONDITION( fabs(alpha - i) < alpha_eps );

        q.setFromMatrix( fmat4::getRotation(i, -faxes[j]) );
        q.toAxisAngle(ax, alpha);
        CONDITION( distance(ax, -faxes[j]) < 0.001f );
        CONDITION( fabs(alpha - i) < alpha_eps );
        CONDITION( q.toMatrix4().diff(fmat4::getRotation(i, -faxes[j])) < mat_eps )

        q.setFromAxisAngle( -faxes[j], i );
        q.toAxisAngle(ax, alpha);
        CONDITION( distance(ax, -faxes[j]) < 0.001f );
        CONDITION( fabs(alpha - i) < alpha_eps );

        q.setFromMatrix( fmat4::getRotation(-i, -faxes[j]) );
        q.toAxisAngle(ax, alpha);
        CONDITION( distance(ax, faxes[j]) < 0.001f );
        CONDITION( fabs(alpha - i) < alpha_eps );
        CONDITION( q.toMatrix4().diff(fmat4::getRotation(-i, -faxes[j])) < mat_eps )

        q.setFromAxisAngle( -faxes[j], -i );
        q.toAxisAngle(ax, alpha);
        CONDITION( distance(ax, faxes[j]) < 0.001f );
        CONDITION( fabs(alpha - i) < alpha_eps );
      }
    }

    dvec3 daxes[] = { 
      dvec3(+1,+1, 0).normalize(), 
      dvec3( 0,+1,+1).normalize(), 
      dvec3(+1, 0,+1).normalize(), 
      dvec3(-1,-1, 0).normalize(), 
      dvec3( 0,-1,-1).normalize(), 
      dvec3(-1, 0,-1).normalize(), 
      dvec3(1,0,0).normalize(),
      dvec3(0,1,0).normalize(),
      dvec3(0,0,1).normalize(),
      dvec3(0,0,0)
    };

    for (int k=1; k<18000; k+=1)
    {
      double i = k / 100.0;
      for(int j=0; !daxes[j].isNull(); ++j )
      {
        dvec3 ax;
        dquat q;
        double alpha;
        double alpha_eps = 0.005f;
        double mat_eps = 0.01f;

        q.setFromMatrix( dmat4::getRotation(i, daxes[j]) );
        q.toAxisAngle(ax, alpha);
        CONDITION( distance(ax, daxes[j]) < 0.001f );
        CONDITION( fabs(alpha - i) < alpha_eps );
        CONDITION( q.toMatrix4().diff(dmat4::getRotation(i, daxes[j])) < mat_eps )

        q.setFromAxisAngle( daxes[j], i );
        q.toAxisAngle(ax, alpha);
        CONDITION( distance(ax, daxes[j]) < 0.001f );
        CONDITION( fabs(alpha - i) < alpha_eps );

        q.setFromMatrix( dmat4::getRotation(-i, daxes[j]) );
        q.toAxisAngle(ax, alpha);
        CONDITION( distance(ax, -daxes[j]) < 0.001f );
        CONDITION( fabs(alpha - i) < alpha_eps );
        CONDITION( q.toMatrix4().diff(dmat4::getRotation(-i, daxes[j])) < mat_eps )

        q.setFromAxisAngle( daxes[j], -i );
        q.toAxisAngle(ax, alpha);
        CONDITION( distance(ax, -daxes[j]) < 0.001f );
        CONDITION( fabs(alpha - i) < alpha_eps );

        q.setFromMatrix( dmat4::getRotation(i, -daxes[j]) );
        q.toAxisAngle(ax, alpha);
        CONDITION( distance(ax, -daxes[j]) < 0.001f );
        CONDITION( fabs(alpha - i) < alpha_eps );
        CONDITION( q.toMatrix4().diff(dmat4::getRotation(i, -daxes[j])) < mat_eps )

        q.setFromAxisAngle( -daxes[j], i );
        q.toAxisAngle(ax, alpha);
        CONDITION( distance(ax, -daxes[j]) < 0.001f );
        CONDITION( fabs(alpha - i) < alpha_eps );

        q.setFromMatrix( dmat4::getRotation(-i, -daxes[j]) );
        q.toAxisAngle(ax, alpha);
        CONDITION( distance(ax, daxes[j]) < 0.001f );
        CONDITION( fabs(alpha - i) < alpha_eps );
        CONDITION( q.toMatrix4().diff(dmat4::getRotation(-i, -daxes[j])) < mat_eps )

        q.setFromAxisAngle( -daxes[j], -i );
        q.toAxisAngle(ax, alpha);
        CONDITION( distance(ax, daxes[j]) < 0.001f );
        CONDITION( fabs(alpha - i) < alpha_eps );
      }
    }

    q1 = fquat::getFromAxisAngle(fvec3(1,1,1), 45);
    q1 = q1 * q1.getInverse();
    v3 = q1 * fvec3(1,1,1);
    CONDITION( distance(v3,fvec3(1,1,1)) < 0.001f )
    v1 = q1 * fvec4(1,1,1,1);
    CONDITION( distance(v1,fvec4(1,1,1,1)) < 0.001f )

    // 3x3 matrix inversion
    fmat3 m3x3;
    m3x3.translate(10, 10);
    m3x3.rotate(45);
    m3x3.translate(12, -19);
    m3x3.rotate(-17);
    m3x3.scale(3,-2);
    fmat3 m3x3_inverse = m3x3.getInverse();
    fmat3 m3x3_identity = m3x3 * m3x3_inverse;
    CONDITION( m3x3_identity.diff(fmat3::getIdentity()) < 0.00001f )

    return true;
  }

  bool test_glsl_math()
  {
    //-------------------------------------------------------------------------
    // For the moment we are just checking that it compiles
    //-------------------------------------------------------------------------

    fvec2 fv2;
    fvec3 fv3;
    fvec4 fv4;
    dvec2 dv2;
    dvec3 dv3;
    dvec4 dv4;
    ivec2 iv2;
    ivec3 iv3;
    ivec4 iv4;
    fmat4 fm4;
    dmat4 dm4;
    fmat3 fm3;
    dmat3 dm3;
    fmat2 fm2;
    dmat2 dm2;

    fv4 = vl::modf(fv4, fv4);
    dv4 = vl::modf(dv4, dv4);
    fv4 = vl::radians(fv4);
    dv4 = vl::radians(dv4);
    fv4 = vl::degrees(fv4);
    dv4 = vl::degrees(dv4);
    fv4 = vl::sin(fv4);
    dv4 = vl::sin(dv4);
    fv4 = vl::cos(fv4);
    dv4 = vl::cos(dv4);
    fv4 = vl::tan(fv4);
    dv4 = vl::tan(dv4);
    fv4 = vl::atan(fv4, fv4);
    dv4 = vl::atan(dv4, dv4);
    fv4 = vl::asin(fv4);
    dv4 = vl::asin(dv4);
    fv4 = vl::acos(fv4);
    dv4 = vl::acos(dv4);
    fv4 = vl::sinh(fv4);
    dv4 = vl::sinh(dv4);
    fv4 = vl::cosh(fv4);
    dv4 = vl::cosh(dv4);
    fv4 = vl::tanh(fv4);
    dv4 = vl::tanh(dv4);
    fv4 = vl::asinh(fv4);
    dv4 = vl::asinh(dv4);
    fv4 = vl::acosh(fv4);
    dv4 = vl::acosh(dv4);
    fv4 = vl::atanh(fv4);
    dv4 = vl::atanh(dv4);
    fv4 = vl::pow(fv4, fv4);
    dv4 = vl::pow(dv4, dv4);
    fv4 = vl::exp(fv4);
    dv4 = vl::exp(dv4);
    fv4 = vl::exp2(fv4);
    dv4 = vl::exp2(dv4);
    fv4 = vl::log(fv4);
    dv4 = vl::log(dv4);
    fv4 = vl::log10(fv4);
    dv4 = vl::log10(dv4);
    fv4 = vl::sqrt(fv4);
    dv4 = vl::sqrt(dv4);
    fv4 = vl::inversesqrt(fv4);
    dv4 = vl::inversesqrt(dv4);
    fv4 = vl::sqrt(fv4);
    dv4 = vl::sqrt(dv4);
    fv4 = vl::abs(fv4);
    dv4 = vl::abs(dv4);
    fv4 = vl::sign(fv4);
    dv4 = vl::sign(dv4);
    fv4 = vl::floor(fv4);
    dv4 = vl::floor(dv4);
    fv4 = vl::trunc(fv4);
    dv4 = vl::trunc(dv4);
    fv4 = vl::round(fv4);
    dv4 = vl::round(dv4);
    fv4 = vl::roundEven(fv4);
    dv4 = vl::roundEven(dv4);
    fv4 = vl::ceil(fv4);
    dv4 = vl::ceil(dv4);
    fv4 = vl::fract(fv4);
    dv4 = vl::fract(dv4);
    fv4 = vl::mod(fv4, 1.0f);
    dv4 = vl::mod(dv4, 1.0);
    fv4 = vl::mod(fv4, fv4);
    dv4 = vl::mod(dv4, dv4);
    fv4 = vl::modf(fv4, fv4);
    dv4 = vl::modf(dv4, dv4);
    fv4 = vl::mix(fv4, fv4, 1.0f);
    dv4 = vl::mix(dv4, dv4, 1.0);
    fv4 = vl::mix(fv4, fv4, fv4);
    dv4 = vl::mix(dv4, dv4, dv4);
    fv4 = vl::step(fv4, fv4);
    dv4 = vl::step(dv4, dv4);
    fv4 = vl::smoothstep(fv4, fv4, fv4);
    dv4 = vl::smoothstep(dv4, dv4, dv4);
    iv4 = vl::isnan(fv4);
    iv4 = vl::isnan(dv4);
    iv4 = vl::isinf(fv4);
    iv4 = vl::isinf(dv4);
    vl::length(fv4);
    vl::length(dv4);
    vl::distance(fv4, fv4);
    vl::distance(dv4, dv4);
    fv4 = vl::normalize(fv4);
    dv4 = vl::normalize(dv4);
    fv4 = vl::faceforward(fv4,fv4,fv4);
    dv4 = vl::faceforward(dv4,dv4,dv4);
    fv4 = vl::reflect(fv4,fv4);
    dv4 = vl::reflect(dv4,dv4);
    fv4 = vl::refract(fv4,fv4,1.0f);
    dv4 = vl::refract(dv4,dv4,1.0);
    fm4 = vl::matrixCompMult(fm4, fm4);
    dm4 = vl::matrixCompMult(dm4, dm4);
    fm4 = vl::outerProduct(fv4, fv4);
    dm4 = vl::outerProduct(dv4, dv4);
    fm4 = vl::transpose(fm4);
    dm4 = vl::transpose(dm4);
    iv4 = vl::lessThan(fv4, fv4);
    iv4 = vl::lessThan(dv4, dv4);
    iv4 = vl::lessThanEqual(fv4, fv4);
    iv4 = vl::lessThanEqual(dv4, dv4);
    iv4 = vl::greaterThan(fv4, fv4);
    iv4 = vl::greaterThan(dv4, dv4);
    iv4 = vl::greaterThanEqual(fv4, fv4);
    iv4 = vl::greaterThanEqual(dv4, dv4);
    iv4 = vl::equal(fv4, fv4);
    iv4 = vl::equal(dv4, dv4);
    iv4 = vl::notEqual(fv4, fv4);
    iv4 = vl::notEqual(dv4, dv4);
    fv4 = vl::dot(fv4, fv4);
    dv4 = vl::dot(dv4, dv4);

    fv3 = vl::modf(fv3, fv3);
    dv3 = vl::modf(dv3, dv3);
    fv3 = vl::radians(fv3);
    dv3 = vl::radians(dv3);
    fv3 = vl::degrees(fv3);
    dv3 = vl::degrees(dv3);
    fv3 = vl::sin(fv3);
    dv3 = vl::sin(dv3);
    fv3 = vl::cos(fv3);
    dv3 = vl::cos(dv3);
    fv3 = vl::tan(fv3);
    dv3 = vl::tan(dv3);
    fv3 = vl::atan(fv3, fv3);
    dv3 = vl::atan(dv3, dv3);
    fv3 = vl::asin(fv3);
    dv3 = vl::asin(dv3);
    fv3 = vl::acos(fv3);
    dv3 = vl::acos(dv3);
    fv3 = vl::sinh(fv3);
    dv3 = vl::sinh(dv3);
    fv3 = vl::cosh(fv3);
    dv3 = vl::cosh(dv3);
    fv3 = vl::tanh(fv3);
    dv3 = vl::tanh(dv3);
    fv3 = vl::asinh(fv3);
    dv3 = vl::asinh(dv3);
    fv3 = vl::acosh(fv3);
    dv3 = vl::acosh(dv3);
    fv3 = vl::atanh(fv3);
    dv3 = vl::atanh(dv3);
    fv3 = vl::pow(fv3, fv3);
    dv3 = vl::pow(dv3, dv3);
    fv3 = vl::exp(fv3);
    dv3 = vl::exp(dv3);
    fv3 = vl::exp2(fv3);
    dv3 = vl::exp2(dv3);
    fv3 = vl::log(fv3);
    dv3 = vl::log(dv3);
    fv3 = vl::log10(fv3);
    dv3 = vl::log10(dv3);
    fv3 = vl::sqrt(fv3);
    dv3 = vl::sqrt(dv3);
    fv3 = vl::inversesqrt(fv3);
    dv3 = vl::inversesqrt(dv3);
    fv3 = vl::sqrt(fv3);
    dv3 = vl::sqrt(dv3);
    fv3 = vl::abs(fv3);
    dv3 = vl::abs(dv3);
    fv3 = vl::sign(fv3);
    dv3 = vl::sign(dv3);
    fv3 = vl::floor(fv3);
    dv3 = vl::floor(dv3);
    fv3 = vl::trunc(fv3);
    dv3 = vl::trunc(dv3);
    fv3 = vl::round(fv3);
    dv3 = vl::round(dv3);
    fv3 = vl::roundEven(fv3);
    dv3 = vl::roundEven(dv3);
    fv3 = vl::ceil(fv3);
    dv3 = vl::ceil(dv3);
    fv3 = vl::fract(fv3);
    dv3 = vl::fract(dv3);
    fv3 = vl::mod(fv3, 1.0f);
    dv3 = vl::mod(dv3, 1.0);
    fv3 = vl::mod(fv3, fv3);
    dv3 = vl::mod(dv3, dv3);
    fv3 = vl::modf(fv3, fv3);
    dv3 = vl::modf(dv3, dv3);
    fv3 = vl::mix(fv3, fv3, 1.0f);
    dv3 = vl::mix(dv3, dv3, 1.0);
    fv3 = vl::mix(fv3, fv3, fv3);
    dv3 = vl::mix(dv3, dv3, dv3);
    fv3 = vl::step(fv3, fv3);
    dv3 = vl::step(dv3, dv3);
    fv3 = vl::smoothstep(fv3, fv3, fv3);
    dv3 = vl::smoothstep(dv3, dv3, dv3);
    iv3 = vl::isnan(fv3);
    iv3 = vl::isnan(dv3);
    iv3 = vl::isinf(fv3);
    iv3 = vl::isinf(dv3);
    vl::length(fv3);
    vl::length(dv3);
    vl::distance(fv3, fv3);
    vl::distance(dv3, dv3);
    fv3 = vl::normalize(fv3);
    dv3 = vl::normalize(dv3);
    fv3 = vl::faceforward(fv3,fv3,fv3);
    dv3 = vl::faceforward(dv3,dv3,dv3);
    fv3 = vl::reflect(fv3,fv3);
    dv3 = vl::reflect(dv3,dv3);
    fv3 = vl::refract(fv3,fv3,1.0f);
    dv3 = vl::refract(dv3,dv3,1.0);
    fm3 = vl::matrixCompMult(fm3, fm3);
    dm3 = vl::matrixCompMult(dm3, dm3);
    fm3 = vl::outerProduct(fv3, fv3);
    dm3 = vl::outerProduct(dv3, dv3);
    fm3 = vl::transpose(fm3);
    dm3 = vl::transpose(dm3);
    iv3 = vl::lessThan(fv3, fv3);
    iv3 = vl::lessThan(dv3, dv3);
    iv3 = vl::lessThanEqual(fv3, fv3);
    iv3 = vl::lessThanEqual(dv3, dv3);
    iv3 = vl::greaterThan(fv3, fv3);
    iv3 = vl::greaterThan(dv3, dv3);
    iv3 = vl::greaterThanEqual(fv3, fv3);
    iv3 = vl::greaterThanEqual(dv3, dv3);
    iv3 = vl::equal(fv3, fv3);
    iv3 = vl::equal(dv3, dv3);
    iv3 = vl::notEqual(fv3, fv3);
    iv3 = vl::notEqual(dv3, dv3);
    fv3 = vl::dot(fv3, fv3);
    dv3 = vl::dot(dv3, dv3);

    fv2 = vl::modf(fv2, fv2);
    dv2 = vl::modf(dv2, dv2);
    fv2 = vl::radians(fv2);
    dv2 = vl::radians(dv2);
    fv2 = vl::degrees(fv2);
    dv2 = vl::degrees(dv2);
    fv2 = vl::sin(fv2);
    dv2 = vl::sin(dv2);
    fv2 = vl::cos(fv2);
    dv2 = vl::cos(dv2);
    fv2 = vl::tan(fv2);
    dv2 = vl::tan(dv2);
    fv2 = vl::atan(fv2, fv2);
    dv2 = vl::atan(dv2, dv2);
    fv2 = vl::asin(fv2);
    dv2 = vl::asin(dv2);
    fv2 = vl::acos(fv2);
    dv2 = vl::acos(dv2);
    fv2 = vl::sinh(fv2);
    dv2 = vl::sinh(dv2);
    fv2 = vl::cosh(fv2);
    dv2 = vl::cosh(dv2);
    fv2 = vl::tanh(fv2);
    dv2 = vl::tanh(dv2);
    fv2 = vl::asinh(fv2);
    dv2 = vl::asinh(dv2);
    fv2 = vl::acosh(fv2);
    dv2 = vl::acosh(dv2);
    fv2 = vl::atanh(fv2);
    dv2 = vl::atanh(dv2);
    fv2 = vl::pow(fv2, fv2);
    dv2 = vl::pow(dv2, dv2);
    fv2 = vl::exp(fv2);
    dv2 = vl::exp(dv2);
    fv2 = vl::exp2(fv2);
    dv2 = vl::exp2(dv2);
    fv2 = vl::log(fv2);
    dv2 = vl::log(dv2);
    fv2 = vl::log10(fv2);
    dv2 = vl::log10(dv2);
    fv2 = vl::sqrt(fv2);
    dv2 = vl::sqrt(dv2);
    fv2 = vl::inversesqrt(fv2);
    dv2 = vl::inversesqrt(dv2);
    fv2 = vl::sqrt(fv2);
    dv2 = vl::sqrt(dv2);
    fv2 = vl::abs(fv2);
    dv2 = vl::abs(dv2);
    fv2 = vl::sign(fv2);
    dv2 = vl::sign(dv2);
    fv2 = vl::floor(fv2);
    dv2 = vl::floor(dv2);
    fv2 = vl::trunc(fv2);
    dv2 = vl::trunc(dv2);
    fv2 = vl::round(fv2);
    dv2 = vl::round(dv2);
    fv2 = vl::roundEven(fv2);
    dv2 = vl::roundEven(dv2);
    fv2 = vl::ceil(fv2);
    dv2 = vl::ceil(dv2);
    fv2 = vl::fract(fv2);
    dv2 = vl::fract(dv2);
    fv2 = vl::mod(fv2, 1.0f);
    dv2 = vl::mod(dv2, 1.0);
    fv2 = vl::mod(fv2, fv2);
    dv2 = vl::mod(dv2, dv2);
    fv2 = vl::modf(fv2, fv2);
    dv2 = vl::modf(dv2, dv2);
    fv2 = vl::mix(fv2, fv2, 1.0f);
    dv2 = vl::mix(dv2, dv2, 1.0);
    fv2 = vl::mix(fv2, fv2, fv2);
    dv2 = vl::mix(dv2, dv2, dv2);
    fv2 = vl::step(fv2, fv2);
    dv2 = vl::step(dv2, dv2);
    fv2 = vl::smoothstep(fv2, fv2, fv2);
    dv2 = vl::smoothstep(dv2, dv2, dv2);
    iv2 = vl::isnan(fv2);
    iv2 = vl::isnan(dv2);
    iv2 = vl::isinf(fv2);
    iv2 = vl::isinf(dv2);
    vl::length(fv2);
    vl::length(dv2);
    vl::distance(fv2, fv2);
    vl::distance(dv2, dv2);
    fv2 = vl::normalize(fv2);
    dv2 = vl::normalize(dv2);
    fv2 = vl::faceforward(fv2,fv2,fv2);
    dv2 = vl::faceforward(dv2,dv2,dv2);
    fv2 = vl::reflect(fv2,fv2);
    dv2 = vl::reflect(dv2,dv2);
    fv2 = vl::refract(fv2,fv2,1.0f);
    dv2 = vl::refract(dv2,dv2,1.0);
    fm2 = vl::matrixCompMult(fm2, fm2);
    dm2 = vl::matrixCompMult(dm2, dm2);
    fm2 = vl::outerProduct(fv2, fv2);
    dm2 = vl::outerProduct(dv2, dv2);
    fm2 = vl::transpose(fm2);
    dm2 = vl::transpose(dm2);
    iv2 = vl::lessThan(fv2, fv2);
    iv2 = vl::lessThan(dv2, dv2);
    iv2 = vl::lessThanEqual(fv2, fv2);
    iv2 = vl::lessThanEqual(dv2, dv2);
    iv2 = vl::greaterThan(fv2, fv2);
    iv2 = vl::greaterThan(dv2, dv2);
    iv2 = vl::greaterThanEqual(fv2, fv2);
    iv2 = vl::greaterThanEqual(dv2, dv2);
    iv2 = vl::equal(fv2, fv2);
    iv2 = vl::equal(dv2, dv2);
    iv2 = vl::notEqual(fv2, fv2);
    iv2 = vl::notEqual(dv2, dv2);
    fv2 = vl::dot(fv2, fv2);
    dv2 = vl::dot(dv2, dv2);

    fv3 = vl::cross(fv3, fv3);
    dv3 = vl::cross(dv3, dv3);

    return true;
  }
}
