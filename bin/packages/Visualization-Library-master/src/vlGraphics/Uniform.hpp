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

#ifndef Uniform_INCLUDE_ONCE
#define Uniform_INCLUDE_ONCE

#include <vlCore/vlnamespace.hpp>
#include <vlCore/Object.hpp>
#include <vlCore/Vector4.hpp>
#include <vlCore/Matrix4.hpp>
#include <vlGraphics/OpenGL.hpp>
#include <cstring>
#include <map>
#include <vector>
#include <algorithm>

namespace vl
{
  //------------------------------------------------------------------------------
  // Uniform
  //------------------------------------------------------------------------------
  /**
  * Wraps an OpenGL Shading Language uniform to be associated to a GLSLProgram (see vl::GLSLProgram documentation).
   *
   * \sa
   * - GLSLProgram
   * - Shader
   * - Actor
   * - UniformSet
  */
  class Uniform: public Object
  {
    VL_INSTRUMENT_CLASS(vl::Uniform, Object)

    friend class GLSLProgram;

  public:

    Uniform(): mType(UT_NONE)
    {
      VL_DEBUG_SET_OBJECT_NAME()
    }

    Uniform(const char* name): mType(UT_NONE) 
    {
      VL_DEBUG_SET_OBJECT_NAME()
      mName = name;
    }

    ref<Uniform> clone() const
    {
      ref<Uniform> uniform  = new Uniform;
      *uniform = *this;
      return uniform;
    }

    //! Returns the name of the uniform variable
    const std::string& name() const { return mName; }
    
    //! Returns the name of the uniform variable
    std::string& name() { return mName; }
    
    //! Sets the name of the uniform variable
    void setName(const char* name) { mName = name; }

    // generic array setters

    void setUniform1i(int count, const int* value) { initData(count*1); memcpy(&mData[0], value, sizeof(mData[0]) * mData.size()); mType = UT_INT;  VL_CHECK(Has_GLSL); }
    void setUniform2i(int count, const int* value) { initData(count*2); memcpy(&mData[0], value, sizeof(mData[0]) * mData.size()); mType = UT_INT_VEC2; VL_CHECK(Has_GLSL); }
    void setUniform3i(int count, const int* value) { initData(count*3); memcpy(&mData[0], value, sizeof(mData[0]) * mData.size()); mType = UT_INT_VEC3; VL_CHECK(Has_GLSL); }
    void setUniform4i(int count, const int* value) { initData(count*4); memcpy(&mData[0], value, sizeof(mData[0]) * mData.size()); mType = UT_INT_VEC4; VL_CHECK(Has_GLSL); }

    void setUniform1ui(int count, const unsigned int* value) { initData(count*1); memcpy(&mData[0], value, sizeof(mData[0]) * mData.size()); mType = UT_UNSIGNED_INT;  VL_CHECK(Has_GL_EXT_gpu_shader4||Has_GL_Version_3_0||Has_GL_Version_4_0); }
    void setUniform2ui(int count, const unsigned int* value) { initData(count*2); memcpy(&mData[0], value, sizeof(mData[0]) * mData.size()); mType = UT_UNSIGNED_INT_VEC2; VL_CHECK(Has_GL_EXT_gpu_shader4||Has_GL_Version_3_0||Has_GL_Version_4_0); }
    void setUniform3ui(int count, const unsigned int* value) { initData(count*3); memcpy(&mData[0], value, sizeof(mData[0]) * mData.size()); mType = UT_UNSIGNED_INT_VEC3; VL_CHECK(Has_GL_EXT_gpu_shader4||Has_GL_Version_3_0||Has_GL_Version_4_0); }
    void setUniform4ui(int count, const unsigned int* value) { initData(count*4); memcpy(&mData[0], value, sizeof(mData[0]) * mData.size()); mType = UT_UNSIGNED_INT_VEC4; VL_CHECK(Has_GL_EXT_gpu_shader4||Has_GL_Version_3_0||Has_GL_Version_4_0); }

    void setUniform1f(int count, const float* value) { initData(count*1); memcpy(&mData[0], value, sizeof(mData[0]) * mData.size()); mType = UT_FLOAT;  VL_CHECK(Has_GLSL); }
    void setUniform2f(int count, const float* value) { initData(count*2); memcpy(&mData[0], value, sizeof(mData[0]) * mData.size()); mType = UT_FLOAT_VEC2; VL_CHECK(Has_GLSL); }
    void setUniform3f(int count, const float* value) { initData(count*3); memcpy(&mData[0], value, sizeof(mData[0]) * mData.size()); mType = UT_FLOAT_VEC3; VL_CHECK(Has_GLSL); }
    void setUniform4f(int count, const float* value) { initData(count*4); memcpy(&mData[0], value, sizeof(mData[0]) * mData.size()); mType = UT_FLOAT_VEC4; VL_CHECK(Has_GLSL); }

    void setUniform1d(int count, const double* value) { initDouble(count*1); memcpy(&mData[0], value, sizeof(mData[0]) * mData.size()); mType = UT_DOUBLE;  VL_CHECK(Has_GL_Version_4_0); }
    void setUniform2d(int count, const double* value) { initDouble(count*2); memcpy(&mData[0], value, sizeof(mData[0]) * mData.size()); mType = UT_DOUBLE_VEC2; VL_CHECK(Has_GL_Version_4_0); }
    void setUniform3d(int count, const double* value) { initDouble(count*3); memcpy(&mData[0], value, sizeof(mData[0]) * mData.size()); mType = UT_DOUBLE_VEC3; VL_CHECK(Has_GL_Version_4_0); }
    void setUniform4d(int count, const double* value) { initDouble(count*4); memcpy(&mData[0], value, sizeof(mData[0]) * mData.size()); mType = UT_DOUBLE_VEC4; VL_CHECK(Has_GL_Version_4_0); }

    // generic matrix array setters

    void setUniformMatrix2f(int count, const float* value) { initData(count*2*2); memcpy(&mData[0], value, sizeof(mData[0]) * mData.size()); mType = UT_FLOAT_MAT2; VL_CHECK(Has_GLSL); }
    void setUniformMatrix3f(int count, const float* value) { initData(count*3*3); memcpy(&mData[0], value, sizeof(mData[0]) * mData.size()); mType = UT_FLOAT_MAT3; VL_CHECK(Has_GLSL); }
    void setUniformMatrix4f(int count, const float* value) { initData(count*4*4); memcpy(&mData[0], value, sizeof(mData[0]) * mData.size()); mType = UT_FLOAT_MAT4; VL_CHECK(Has_GLSL); }

    void setUniformMatrix2x3f(int count, const float* value) { initData(count*2*3); memcpy(&mData[0], value, sizeof(mData[0]) * mData.size()); mType = UT_FLOAT_MAT2x3; VL_CHECK(Has_GLSL_120_Or_More); }
    void setUniformMatrix3x2f(int count, const float* value) { initData(count*3*2); memcpy(&mData[0], value, sizeof(mData[0]) * mData.size()); mType = UT_FLOAT_MAT3x2; VL_CHECK(Has_GLSL_120_Or_More); }
    void setUniformMatrix2x4f(int count, const float* value) { initData(count*2*4); memcpy(&mData[0], value, sizeof(mData[0]) * mData.size()); mType = UT_FLOAT_MAT2x4; VL_CHECK(Has_GLSL_120_Or_More); }
    void setUniformMatrix4x2f(int count, const float* value) { initData(count*4*2); memcpy(&mData[0], value, sizeof(mData[0]) * mData.size()); mType = UT_FLOAT_MAT4x2; VL_CHECK(Has_GLSL_120_Or_More); }
    void setUniformMatrix3x4f(int count, const float* value) { initData(count*3*4); memcpy(&mData[0], value, sizeof(mData[0]) * mData.size()); mType = UT_FLOAT_MAT3x4; VL_CHECK(Has_GLSL_120_Or_More); }
    void setUniformMatrix4x3f(int count, const float* value) { initData(count*4*3); memcpy(&mData[0], value, sizeof(mData[0]) * mData.size()); mType = UT_FLOAT_MAT4x3; VL_CHECK(Has_GLSL_120_Or_More); }

    void setUniformMatrix2d(int count, const double* value) { initDouble(count*2*2); memcpy(&mData[0], value, sizeof(mData[0]) * mData.size()); mType = UT_DOUBLE_MAT2; VL_CHECK(Has_GL_Version_4_0); }
    void setUniformMatrix3d(int count, const double* value) { initDouble(count*3*3); memcpy(&mData[0], value, sizeof(mData[0]) * mData.size()); mType = UT_DOUBLE_MAT3; VL_CHECK(Has_GL_Version_4_0); }
    void setUniformMatrix4d(int count, const double* value) { initDouble(count*4*4); memcpy(&mData[0], value, sizeof(mData[0]) * mData.size()); mType = UT_DOUBLE_MAT4; VL_CHECK(Has_GL_Version_4_0); }

    void setUniformMatrix2x3d(int count, const double* value) { initDouble(count*2*3); memcpy(&mData[0], value, sizeof(mData[0]) * mData.size()); mType = UT_DOUBLE_MAT2x3; VL_CHECK(Has_GL_Version_4_0); }
    void setUniformMatrix3x2d(int count, const double* value) { initDouble(count*3*2); memcpy(&mData[0], value, sizeof(mData[0]) * mData.size()); mType = UT_DOUBLE_MAT3x2; VL_CHECK(Has_GL_Version_4_0); }
    void setUniformMatrix2x4d(int count, const double* value) { initDouble(count*2*4); memcpy(&mData[0], value, sizeof(mData[0]) * mData.size()); mType = UT_DOUBLE_MAT2x4; VL_CHECK(Has_GL_Version_4_0); }
    void setUniformMatrix4x2d(int count, const double* value) { initDouble(count*4*2); memcpy(&mData[0], value, sizeof(mData[0]) * mData.size()); mType = UT_DOUBLE_MAT4x2; VL_CHECK(Has_GL_Version_4_0); }
    void setUniformMatrix3x4d(int count, const double* value) { initDouble(count*3*4); memcpy(&mData[0], value, sizeof(mData[0]) * mData.size()); mType = UT_DOUBLE_MAT3x4; VL_CHECK(Has_GL_Version_4_0); }
    void setUniformMatrix4x3d(int count, const double* value) { initDouble(count*4*3); memcpy(&mData[0], value, sizeof(mData[0]) * mData.size()); mType = UT_DOUBLE_MAT4x3; VL_CHECK(Has_GL_Version_4_0); }

    // vector/matrix array setters

    void setUniform(int count, const int* value)   { setUniform1i(count, value); }
    void setUniform(int count, const ivec2* value) { setUniform2i(count, value->ptr()); }
    void setUniform(int count, const ivec3* value) { setUniform3i(count, value->ptr()); }
    void setUniform(int count, const ivec4* value) { setUniform4i(count, value->ptr()); }

    void setUniform(int count, const unsigned int* value) { setUniform1ui(count, value); }
    void setUniform(int count, const uvec2* value) { setUniform2ui(count, value->ptr()); }
    void setUniform(int count, const uvec3* value) { setUniform3ui(count, value->ptr()); }
    void setUniform(int count, const uvec4* value) { setUniform4ui(count, value->ptr()); }

    void setUniform(int count, const float* value)  { setUniform1f(count, value); }
    void setUniform(int count, const fvec2* value)  { setUniform2f(count, value->ptr()); }
    void setUniform(int count, const fvec3* value)  { setUniform3f(count, value->ptr()); }
    void setUniform(int count, const fvec4* value)  { setUniform4f(count, value->ptr()); }

    void setUniform(int count, const fmat2* value)  { setUniformMatrix2f(count, value->ptr()); }
    void setUniform(int count, const fmat3* value)  { setUniformMatrix3f(count, value->ptr()); }
    void setUniform(int count, const fmat4* value)  { setUniformMatrix4f(count, value->ptr()); }

    void setUniform(int count, const double* value) { setUniform1d(count, value); }
    void setUniform(int count, const dvec2* value)  { setUniform2d(count, value->ptr()); }
    void setUniform(int count, const dvec3* value)  { setUniform3d(count, value->ptr()); }
    void setUniform(int count, const dvec4* value)  { setUniform4d(count, value->ptr()); }

    void setUniform(int count, const dmat2* value)  { setUniformMatrix2d(count, value->ptr()); }
    void setUniform(int count, const dmat3* value)  { setUniformMatrix3d(count, value->ptr()); }
    void setUniform(int count, const dmat4* value)  { setUniformMatrix4d(count, value->ptr()); }

    // single value setters

    void setUniformI(const int& value)  { setUniform1i(1, &value); }
    void setUniform(const ivec2& value) { setUniform2i(1, value.ptr()); }
    void setUniform(const ivec3& value) { setUniform3i(1, value.ptr()); }
    void setUniform(const ivec4& value) { setUniform4i(1, value.ptr()); }

    void setUniformU(const unsigned int& value) { setUniform1ui(1, &value); }
    void setUniform(const uvec2& value) { setUniform2ui(1, value.ptr()); }
    void setUniform(const uvec3& value) { setUniform3ui(1, value.ptr()); }
    void setUniform(const uvec4& value) { setUniform4ui(1, value.ptr()); }

    void setUniformF(const float& value) { setUniform1f(1, &value); }
    void setUniform(const fvec2& value)  { setUniform2f(1, value.ptr()); }
    void setUniform(const fvec3& value)  { setUniform3f(1, value.ptr()); }
    void setUniform(const fvec4& value)  { setUniform4f(1, value.ptr()); }

    void setUniform(const fmat2& value) { setUniformMatrix2f(1, value.ptr()); }
    void setUniform(const fmat3& value) { setUniformMatrix3f(1, value.ptr()); }
    void setUniform(const fmat4& value) { setUniformMatrix4f(1, value.ptr()); }

    void setUniformD(const double& value) { setUniform1d(1, &value); }
    void setUniform(const dvec2& value)   { setUniform2d(1, value.ptr()); }
    void setUniform(const dvec3& value)   { setUniform3d(1, value.ptr()); }
    void setUniform(const dvec4& value)   { setUniform4d(1, value.ptr()); }

    void setUniform(const dmat2& value) { setUniformMatrix2d(1, value.ptr()); }
    void setUniform(const dmat3& value) { setUniformMatrix3d(1, value.ptr()); }
    void setUniform(const dmat4& value) { setUniformMatrix4d(1, value.ptr()); }

    // getters

    void getUniform(double* value) const { VL_CHECK(type() != UT_NONE); VL_CHECK(mData.size()); memcpy( value, &mData[0], sizeof(mData[0]) * mData.size()); }
    void getUniform(float* value) const { VL_CHECK(type() != UT_NONE); VL_CHECK(mData.size()); memcpy( value, &mData[0], sizeof(mData[0]) * mData.size()); }
    void getUniform(int* value) const { VL_CHECK(type() != UT_NONE); VL_CHECK(mData.size()); memcpy( value, &mData[0], sizeof(mData[0]) * mData.size()); }
    void getUniform(unsigned int* value) const { VL_CHECK(type() != UT_NONE); VL_CHECK(mData.size()); memcpy( value, &mData[0], sizeof(mData[0]) * mData.size()); }

    void getUniform(ivec2* value) const { getUniform(value->ptr()); }
    void getUniform(ivec3* value) const { getUniform(value->ptr()); }
    void getUniform(ivec4* value) const { getUniform(value->ptr()); }

    void getUniform(uvec2* value) const { getUniform(value->ptr()); }
    void getUniform(uvec3* value) const { getUniform(value->ptr()); }
    void getUniform(uvec4* value) const { getUniform(value->ptr()); }

    void getUniform(fvec2* value) const { getUniform(value->ptr()); }
    void getUniform(fvec3* value) const { getUniform(value->ptr()); }
    void getUniform(fvec4* value) const { getUniform(value->ptr()); }

    void getUniform(fmat2* value) const { getUniform(value->ptr()); }
    void getUniform(fmat3* value) const { getUniform(value->ptr()); }
    void getUniform(fmat4* value) const { getUniform(value->ptr()); }

    void getUniform(dvec2* value) const { getUniform(value->ptr()); }
    void getUniform(dvec3* value) const { getUniform(value->ptr()); }
    void getUniform(dvec4* value) const { getUniform(value->ptr()); }

    void getUniform(dmat2* value) const { getUniform(value->ptr()); }
    void getUniform(dmat3* value) const { getUniform(value->ptr()); }
    void getUniform(dmat4* value) const { getUniform(value->ptr()); }

    EUniformType type() const { return mType; }

    int count() const
    {
      if (mData.empty())
        return 0;

      switch(mType)
      {
        case UT_INT:      return singleCount();      
        case UT_INT_VEC2: return singleCount() / 2;      
        case UT_INT_VEC3: return singleCount() / 3;      
        case UT_INT_VEC4: return singleCount() / 4;

        case UT_UNSIGNED_INT:      return singleCount();      
        case UT_UNSIGNED_INT_VEC2: return singleCount() / 2;      
        case UT_UNSIGNED_INT_VEC3: return singleCount() / 3;      
        case UT_UNSIGNED_INT_VEC4: return singleCount() / 4;

        case UT_FLOAT:      return singleCount();
        case UT_FLOAT_VEC2: return singleCount() / 2;
        case UT_FLOAT_VEC3: return singleCount() / 3;
        case UT_FLOAT_VEC4: return singleCount() / 4;      

        case UT_FLOAT_MAT2: return singleCount() / (2*2);
        case UT_FLOAT_MAT3: return singleCount() / (3*3);      
        case UT_FLOAT_MAT4: return singleCount() / (4*4);      

        case UT_FLOAT_MAT2x3: return singleCount() / (2*3);
        case UT_FLOAT_MAT3x2: return singleCount() / (3*2);
        case UT_FLOAT_MAT2x4: return singleCount() / (2*4);
        case UT_FLOAT_MAT4x2: return singleCount() / (4*2);
        case UT_FLOAT_MAT3x4: return singleCount() / (3*4);
        case UT_FLOAT_MAT4x3: return singleCount() / (4*3);

        case UT_DOUBLE:      return doubleCount();
        case UT_DOUBLE_VEC2: return doubleCount() / 2;
        case UT_DOUBLE_VEC3: return doubleCount() / 3;
        case UT_DOUBLE_VEC4: return doubleCount() / 4;

        case UT_DOUBLE_MAT2: return doubleCount() / (2*2);
        case UT_DOUBLE_MAT3: return doubleCount() / (3*3);
        case UT_DOUBLE_MAT4: return doubleCount() / (4*4);

        case UT_DOUBLE_MAT2x3: return doubleCount() / (2*3);
        case UT_DOUBLE_MAT3x2: return doubleCount() / (3*2);
        case UT_DOUBLE_MAT2x4: return doubleCount() / (2*4);
        case UT_DOUBLE_MAT4x2: return doubleCount() / (4*2);
        case UT_DOUBLE_MAT3x4: return doubleCount() / (3*4);
        case UT_DOUBLE_MAT4x3: return doubleCount() / (4*3);

        default:
        VL_TRAP()
        return -1;
      }
    }

    void* rawData() { if (mData.empty()) return NULL; else return &mData[0]; }

    const void* rawData() const { if (mData.empty()) return NULL; else return &mData[0]; }

  protected:
    VL_COMPILE_TIME_CHECK( sizeof(int) == sizeof(float) )
    void initData(int count) { mData.resize(count); }
    void initDouble(int count) { mData.resize(count*2); }
    int singleCount() const { return (int)mData.size(); }
    int doubleCount() const { VL_CHECK((mData.size() & 0x1) == 0 ); return (int)(mData.size() >> 1); }
    const double* doubleData() const { VL_CHECK(!mData.empty()); VL_CHECK((mData.size() & 0x1) == 0 ); return (double*)&mData[0]; }
    const float* floatData() const { VL_CHECK(!mData.empty()); return (float*)&mData[0]; }
    const int* intData() const { VL_CHECK(!mData.empty()); return (int*)&mData[0]; }
    const unsigned int* uintData() const { VL_CHECK(!mData.empty()); return (unsigned int*)&mData[0]; }

    EUniformType mType;
    std::vector<int> mData;
    std::string mName;
  };
}

#endif
