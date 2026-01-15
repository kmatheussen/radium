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

#ifndef GLSL_INCLUDE_ONCE
#define GLSL_INCLUDE_ONCE

#include <vlGraphics/UniformSet.hpp>
#include <vlCore/glsl_math.hpp>
#include <vlGraphics/RenderState.hpp>
#include <vlCore/String.hpp>

namespace vl
{
  class Uniform;

  //------------------------------------------------------------------------------
  // UniformInfo
  //------------------------------------------------------------------------------
  //! Structure containing all the info regarding an active Uniform, see also GLSLProgram::activeUniforms()
  struct UniformInfo: public Object
  {
    UniformInfo(const char* name, EUniformType type, int size, int location)
    :Name(name), Type(type), Size(size), Location(location) {}

    std::string Name;  //!< The name of the uniform.
    EUniformType Type; //!< The type of the uniform (float, vec4, mat2x3, sampler2D etc.)
    int Size;          //!< The size of the uniform: 1 for non-arrays, >= 1 for arrays.
    int Location;      //!< Location of the uniform as retuned by glGetUniformLocation().
  };

  //------------------------------------------------------------------------------
  // AttribInfo
  //------------------------------------------------------------------------------
  //! Structure containing all the info regarding an active vertex attribute, see also GLSLProgram::activeAttribs()
  struct AttribInfo: public Object
  {
    AttribInfo(const char* name, EAttributeType type, int size, int location)
    :Name(name), Type(type), Size(size), Location(location) {}

    std::string Name;    //!< Name of the active attribute.
    EAttributeType Type; //!< Type as returned by http://www.opengl.org/sdk/docs/man4/xhtml/glGetActiveAttrib.xml
    int Size;            //!< The size of the attribute, in units of the type returned in Type.
    int Location;        //!< Location of the active attribute
  };

  //------------------------------------------------------------------------------
  // GLSLShader
  //------------------------------------------------------------------------------
  /** For internal use only. Base class for GLSLVertexShader, GLSLFragmentShader, GLSLGeometryShader, GLSLTessEvaluationShader and GLSLTessControlShader.
   *
   * \sa GLSLVertexShader, GLSLFragmentShader, GLSLGeometryShader, GLSLTessControlShader, GLSLTessEvaluationShader, GLSLProgram, Effect */
  class VLGRAPHICS_EXPORT GLSLShader: public Object
  {
    VL_INSTRUMENT_CLASS(vl::GLSLShader, Object)

  public:
    GLSLShader();

    GLSLShader(EShaderType type, const String& source_or_path);

    ~GLSLShader();

    void setType(EShaderType type) { mType = type; }

    EShaderType type() const { return mType; }

    //! Sets the sources for this shader and schedules a recompilation for it. If the string passed is a file path the source is loaded from it.
    void setSource( const String& source_or_path );

    //! Returns the sources for this shader
    const std::string& source() const { return mSource; }

    //! The path from which the shader was loaded
    void setPath(const char* path) { mPath = path; }

    //! The path from which the shader was loaded
    const std::string& path() const { return mPath; }

    //! Retrieves the shader source using glGetShaderSource()
    std::string getShaderSource() const;

    //! Compiles the shader, see also http://www.opengl.org/sdk/docs/man/xhtml/glCompileShader.xml for more information.
    //! This function also create the shader if handle() == 0 using the OpenGL function glCreateShader(), see also http://www.opengl.org/sdk/docs/man/xhtml/glCreateShader.xml
    bool compile();

    //! Returns true if the shader has been succesfully compiled.
    //! The check is done using the OpenGL function glGetShaderiv(), see also http://www.opengl.org/sdk/docs/man/xhtml/glGetShader.xml for more information.
    bool compileStatus() const;

    //! Returns a String object containing this shader's info log as returned by glGetShaderInfoLog(), see also http://www.opengl.org/sdk/docs/man/xhtml/glGetShaderInfoLog.xml for more information.
    String infoLog() const;

    //! Creates the shader using the OpenGL function glCreateShader(), see also http://www.opengl.org/sdk/docs/man/xhtml/glCreateShader.xml for more information.
    void createShader();

    //! Deletes the shader using the OpenGL function glDeleteShader(), see also http://www.opengl.org/sdk/docs/man/xhtml/glDeleteShader.xml for more information.
    void deleteShader();

    //! The handle of this OpenGL shader object as returned by glCreateShader()
    unsigned int handle() const { return mHandle; }

  protected:
    EShaderType mType;
    std::string mSource;
    std::string mPath;
    unsigned int mHandle;
    bool mCompiled;
  };
  //------------------------------------------------------------------------------
  /** Wraps a GLSL vertex shader to be bound to a GLSLProgram: the shader this shader will run on the programmable vertex processor.
   *
   * \sa GLSLFragmentShader, GLSLGeometryShader, GLSLTessControlShader, GLSLTessEvaluationShader, GLSLProgram, Effect */
  class GLSLVertexShader: public GLSLShader
  {
    VL_INSTRUMENT_CLASS(vl::GLSLVertexShader, GLSLShader)

  public:
    //! Constructor.
    //! \param source Vertex shader's source code or path to a text file containing the vertex shader's source code.
    GLSLVertexShader(const String& source=String()): GLSLShader(ST_VERTEX_SHADER, source)
    {
      #ifndef NDEBUG
        if (mObjectName.empty())
          mObjectName = className();
      #endif
    }
  };
  //------------------------------------------------------------------------------
  /** Wraps a GLSL fragment shader to be bound to a GLSLProgram: the shader this shader will run on the programmable fragment processor.
   *
   * \sa GLSLVertexShader, GLSLGeometryShader, GLSLTessControlShader, GLSLTessEvaluationShader, GLSLProgram, Effect */
  class GLSLFragmentShader: public GLSLShader
  {
    VL_INSTRUMENT_CLASS(vl::GLSLFragmentShader, GLSLShader)

  public:
    //! \param source Fragment shader's source code or path to a text file containing the fragment shader's source code.
    GLSLFragmentShader(const String& source=String()): GLSLShader(ST_FRAGMENT_SHADER, source)
    {
      #ifndef NDEBUG
        if (mObjectName.empty())
          mObjectName = className();
      #endif
    }
  };
  //------------------------------------------------------------------------------
  /** Wraps a GLSL geometry shader to be bound to a GLSLProgram: the shader this shader will run on the programmable geometry processor.
   *
   * \sa GLSLVertexShader, GLSLFragmentShader, GLSLTessControlShader, GLSLTessEvaluationShader, GLSLProgram, Effect */
  class GLSLGeometryShader: public GLSLShader
  {
    VL_INSTRUMENT_CLASS(vl::GLSLGeometryShader, GLSLShader)

  public:
    //! \param source Geometry shader's source code or path to a text file containing the geometry shader's source code.
    GLSLGeometryShader(const String& source=String()): GLSLShader(ST_GEOMETRY_SHADER, source)
    {
      #ifndef NDEBUG
        if (mObjectName.empty())
          mObjectName = className();
      #endif
    }
  };
  //------------------------------------------------------------------------------
  /** Wraps a GLSL tessellation control shader to be bound to a GLSLProgram: the shader this shader will run on the programmable tessellation processor in the control stage.
   *
   * \sa GLSLVertexShader, GLSLFragmentShader, GLSLGeometryShader, GLSLTessEvaluationShader, GLSLProgram, Effect */
  class GLSLTessControlShader: public GLSLShader
  {
    VL_INSTRUMENT_CLASS(vl::GLSLTessControlShader, GLSLShader)

  public:
    //! \param source Tessellation-control shader's source code or path to a text file containing the shader's source code.
    GLSLTessControlShader(const String& source=String()): GLSLShader(ST_TESS_CONTROL_SHADER, source)
    {
      #ifndef NDEBUG
        if (mObjectName.empty())
          mObjectName = className();
      #endif
    }
  };
  //------------------------------------------------------------------------------
  /** Wraps a GLSL tessellation evaluation shader to be bound to a GLSLProgram: this shader will run on the programmable tessellation processor in the evaluation stage.
   *
   * \sa GLSLVertexShader, GLSLFragmentShader, GLSLGeometryShader, GLSLTessControlShader, GLSLProgram, Effect */
  class GLSLTessEvaluationShader: public GLSLShader
  {
    VL_INSTRUMENT_CLASS(vl::GLSLTessEvaluationShader, GLSLShader)

  public:
    //! \param source Tessellation-evaluation shader's source code or path to a text file containing the shader's source code.
    GLSLTessEvaluationShader(const String& source=String()): GLSLShader(ST_TESS_EVALUATION_SHADER, source)
    {
      #ifndef NDEBUG
        if (mObjectName.empty())
          mObjectName = className();
      #endif
    }
  };
  //------------------------------------------------------------------------------
  // GLSLProgram
  //------------------------------------------------------------------------------
  /**
   * Wraps a GLSL program to which you can bind vertex, fragment and geometry shaders.
   *
   * \par Uniforms
   * You have 5 ways to set the value of a uniform:
   * -# call useProgram() to activate the GLSLProgram and directly call glUniform* (see also getUniformLocation()).
   * -# add a Uniform to the GLSLProgram UniformSet, see vl::GLSLProgram::getUniformSet().
   * -# add a Uniform to the Actor's UniformSet, see vl::Actor::getUniformSet().
   * -# add a Uniform to the Actor's Shader UniformSet, see vl::Shader::getUniformSet().
   * -# directly update the uniform value from ActorEventCallback::onActorRenderStarted() using the standard glUniform*() OpenGL functions.
   *    In this case you have to make sure that <i>all</i> the Actors using a given GLSLProgram/Shader write such uniform.
   *
   * \remarks
   * A Uniform must be setup using <i>one and only one</i> of the 5 previously mentioned methods.
   *
   * \par Attribute Location Bindings
   * In order to explicity specify which attribute index should be bound to which attribute name you can do one of the following.
   * -# call glBindAttribLocation() with the appropriate GLSLProgram::handle(). This is the most low level way of doing it.
   * -# call bindAttribLocation() as you would do normally with glBindAttribLocation() but with the difference the you don't need to specify the GLSL program handle.
   * -# create a list of attribute name/indices that will be automatically bound whenever the GLSLProgram is linked. In order to do so you can use the following functions 
   *    setAutoAttribLocations(), autoAttribLocations(), clearAutoAttribLocations(), addAutoAttribLocation(), removeAutoAttribLocation().
   *
   * Note that for option #1 and #2 you need to relink the GLSLProgram in order for the changes to take effect (linkProgram(force_relink=true)). 
   * Option #2 and #3 automatically schedule a re-link of the GLSL program. See also http://www.opengl.org/sdk/docs/man/xhtml/glBindAttribLocation.xml
   *
   * \sa
   * - GLSLVertexShader
   * - GLSLFragmentShader
   * - GLSLGeometryShader
   * - Shader
   * - Effect
   * - Actor::renderEventCallbacks()
  */
  class VLGRAPHICS_EXPORT GLSLProgram: public RenderStateNonIndexed
  {
    VL_INSTRUMENT_CLASS(vl::GLSLProgram, RenderStateNonIndexed)

    // applyUniform
    friend class Renderer;
  public:
    //! Constructor.
    GLSLProgram();

    //! Destructor. Calls deleteProgram().
    ~GLSLProgram();

    //! \internal
    virtual ERenderState type() const { return RS_GLSLProgram; }

    virtual ref<RenderState> clone() const
    {
      ref<GLSLProgram> rs = new GLSLProgram;
      *rs = *this;
      return rs;
    }

    GLSLProgram& operator=(const GLSLProgram& other);

    //! Calls glCreateProgram() in order to acquire a GLSL program handle, see also http://www.opengl.org/sdk/docs/man/xhtml/glCreateProgram.xml for more information.
    //! \note
    //! The program is created only if handle() == 0
    void createProgram();

    //! Deletes the GLSL program calling glDeleteProgram(handle()), see also http://www.opengl.org/sdk/docs/man/xhtml/glDeleteProgram.xml for more information.
    //! After this function handle() will return 0.
    void deleteProgram();

    //! The handle of the GLSL program as returned by glCreateProgram()
    //! \sa
    //! http://www.opengl.org/sdk/docs/man/xhtml/glCreateProgram.xml
    //! - createProgram()
    //! - deleteProgram()
    unsigned int handle() const { return mHandle; }

    //! Equivalent to glUseProgram(handle()), see also http://www.opengl.org/sdk/docs/man/xhtml/glUseProgram.xml for more information.
    bool useProgram() const;

    //! Calls useProgram()
    void apply(int index, const Camera*, OpenGLContext* ctx) const;

    //! Links the GLSLProgram calling glLinkProgram(handle()) only if the program needs to be linked.
    //! \sa
    //! - http://www.opengl.org/sdk/docs/man/xhtml/glLinkProgram.xml
    //! - scheduleRelinking()
    //! - linked()
    bool linkProgram(bool force_relink = false);

    bool linkStatus() const;

    //! Returns true if the program has been succesfully linked.
    bool linked() const { return mHandle && !mScheduleLink; }

    //! Schedules a relink of the GLSL program.
    void scheduleRelinking() { mScheduleLink = true; }

    /**
     * Attaches the GLSLShader to this GLSLProgram
     * \note
     * Attaching a shader triggers the compilation of the shader (if not already compiled) and relinking of the program.
    */
    bool attachShader(GLSLShader* shader);

    //! Detaches a GLSLShader from the GLSLShader (note: it does NOT schedule a relink of the program), see also http://www.opengl.org/sdk/docs/man/xhtml/glDetachShader.xml for more information.
    bool detachShader(GLSLShader* shader);

    //! Detaches all the shaders and deletes them (note that the GLSL Program remains still valid). 
    //! Use this function when your GLSL program compiled well, you don't want to re-link or re-compile it and you want to save 
    //! some memory by discarding unnecessary shaders objects.
    void discardAllShaders();

    //! Returns the info log of this GLSL program using the OpenGL function glGetProgramInfoLog(), see also http://www.opengl.org/sdk/docs/man/xhtml/glGetProgramInfoLog.xml for more information.
    String infoLog() const;

    //! Returns true if the validation of this GLSL program is succesful, see also http://www.opengl.org/sdk/docs/man/xhtml/glValidateProgram.xml for more information.
    bool validateProgram() const;

    /** Equivalent to glBindAttribLocation(handle(), index, name.c_str()) with the difference that this function will automatically create a GLSL program if none is present
      * and it will schedule a re-link since the new specified bindings take effect after linking the GLSL program.
      * \sa setAutoAttribLocations(), autoAttribLocations(), clearAutoAttribLocations(), 
      *     removeAutoAttribLocation(), addAutoAttribLocation() */
    void bindAttribLocation(unsigned int index, const char* name);

    /** Adds an attribute name / index pair to the automatic attribute location binding list. Calling this function will schedule a re-linking of the GLSL program.
    * \sa setAutoAttribLocations(), autoAttribLocations(), clearAutoAttribLocations(), 
    *     bindAttribLocation(), removeAutoAttribLocation() */
    void addAutoAttribLocation(int attr_index, const char* attr_name) { mAutoAttribLocation[attr_name] = attr_index; mScheduleLink = true; }

    /** Removes an attribute from the automatic attribute location binding list. Calling this function will schedule a re-linking of the GLSL program.
    * \sa setAutoAttribLocations(), autoAttribLocations(), clearAutoAttribLocations(), 
    *     bindAttribLocation(), addAutoAttribLocation() */
    void removeAutoAttribLocation(const char* attr_name) { mAutoAttribLocation.erase(attr_name); mScheduleLink = true; }

    /** Defines which \p attribute should be automatically bound to which \p attribute \p index at GLSL program linking time. Calling this function will schedule a re-linking of the GLSL program.
    * \sa autoAttribLocations(), clearAutoAttribLocations(), 
    *     bindAttribLocation(), removeAutoAttribLocation(), addAutoAttribLocation() */
    void setAutoAttribLocations(const std::map<std::string, int>& attrib_bindings) { mAutoAttribLocation = attrib_bindings; mScheduleLink = true; }

    /** Returns which \p attribute name should be automatically bound to which \p attribute \p index at GLSL program linking time.
    * \sa setAutoAttribLocations(), clearAutoAttribLocations(), 
    *     bindAttribLocation(), removeAutoAttribLocation(), addAutoAttribLocation() */
    const std::map<std::string, int>& autoAttribLocations() const { return  mAutoAttribLocation; }

    /** Clears the automatic attribute location binding list. See also setAutoAttribLocations() and autoAttribLocations().
    * \sa setAutoAttribLocations(), autoAttribLocations(), 
    *     bindAttribLocation(), removeAutoAttribLocation(), addAutoAttribLocation() */
    void clearAutoAttribLocations() { mAutoAttribLocation.clear(); }

    //! Eqivalento to glGetAttribLocation(handle(), name).
    //! \note The program must be linked before calling this function.
    int getAttribLocation(const char* name) const
    {
      VL_CHECK( Has_GLSL )
      if( !Has_GLSL )
        return -1;
      VL_CHECK(handle())
      VL_CHECK(linked())
      int location = glGetAttribLocation(handle(), name);
      return location;
    }

    //! Returns the number of GLSLShader objects bound to this GLSLProgram
    int shaderCount() const { return (int)mShaders.size(); }

    //! Returns the i-th GLSLShader objects bound to this GLSLProgram
    const GLSLShader* shader(int i) const { return mShaders[i].get(); }

    //! Returns the i-th GLSLShader objects bound to this GLSLProgram
    GLSLShader* shader(int i) { return mShaders[i].get(); }

    //! Removes all the previously linked shaders and schedules a relinking
    void detachAllShaders();

    // --------------- bind frag data location ---------------

    void bindFragDataLocation(int color_number, const char* name);

    void unbindFragDataLocation(const char* name);

    int fragDataLocation(const char* name) const;

    const std::map<std::string, int>& fragDataLocations() const { return mFragDataLocation; }

    // --------------- geometry shader ---------------

    //! See GL_ARB_geometry_shader4's GL_GEOMETRY_VERTICES_OUT_EXT
    void setGeometryVerticesOut(int vertex_count) 
    { 
      if (mGeometryVerticesOut != vertex_count)
      {
        scheduleRelinking(); 
        mGeometryVerticesOut = vertex_count; 
      }
    }

    //! See GL_ARB_geometry_shader4's GL_GEOMETRY_VERTICES_OUT_EXT
    int geometryVerticesOut() const { return mGeometryVerticesOut; }

    //! See GL_ARB_geometry_shader4's GL_GEOMETRY_INPUT_TYPE_EXT
    void setGeometryInputType(EGeometryInputType type) 
    { 
      if (mGeometryInputType != type)
      {
        scheduleRelinking(); 
        mGeometryInputType = type; 
      }
    }
    //! See GL_ARB_geometry_shader4's GL_GEOMETRY_INPUT_TYPE_EXT
    EGeometryInputType geometryInputType() const { return mGeometryInputType; }

    //! See GL_ARB_geometry_shader4's GL_GEOMETRY_OUTPUT_TYPE_EXT
    void setGeometryOutputType(EGeometryOutputType type) 
    { 
      if (mGeometryOutputType != type)
      {
        scheduleRelinking(); 
        mGeometryOutputType = type; 
      }
    }
    
    //! See GL_ARB_geometry_shader4's GL_GEOMETRY_OUTPUT_TYPE_EXT
    EGeometryOutputType geometryOutputType() const { return mGeometryOutputType; }

    // --------------- GLSL 4.x ---------------

    //! Indicate to the implementation the intention of the application to retrieve the program's binary representation 
    //! with glGetProgramBinary. The implementation may use this information to store information that may be useful for 
    //! a future query of the program's binary. See http://www.opengl.org/sdk/docs/man4/xhtml/glProgramParameter.xml
    void setProgramBinaryRetrievableHint(bool hint) { mProgramBinaryRetrievableHint = hint; }
    
    //! Indicate to the implementation the intention of the application to retrieve the program's binary representation 
    //! with glGetProgramBinary. The implementation may use this information to store information that may be useful for 
    //! a future query of the program's binary. See http://www.opengl.org/sdk/docs/man4/xhtml/glProgramParameter.xml
    bool programBinaryRetrievableHint() const { return mProgramBinaryRetrievableHint; }

    //! Indicates whether program can be bound to individual pipeline stages via glUseProgramStages, see also http://www.opengl.org/sdk/docs/man4/xhtml/glProgramParameter.xml
    //! \note Changing the program-separable attribute will schedule a relink of the GLSL program.
    void setProgramSeparable(bool separable) 
    { 
      if (mProgramSeparable != separable)
      {
        scheduleRelinking();
        mProgramSeparable = separable; 
      }
    }

    //! Indicates whether program can be bound to individual pipeline stages via glUseProgramStages, see also http://www.opengl.org/sdk/docs/man4/xhtml/glProgramParameter.xml
    bool programSeparable() const { return mProgramSeparable; }

    //! glGetProgramBinary wrapper: returns a binary representation of a program object's compiled and linked executable source, see also http://www.opengl.org/sdk/docs/man4/xhtml/glGetProgramBinary.xml
    bool getProgramBinary(GLenum& binary_format, std::vector<unsigned char>& binary) const;

    //! glProgramBinary wrapper: loads a program object with a program binary, see also http://www.opengl.org/sdk/docs/man4/xhtml/glProgramBinary.xml
    bool programBinary(GLenum binary_format, const std::vector<unsigned char>& binary) { return programBinary(binary_format, &binary[0], (int)binary.size()); }

    //! glProgramBinary wrapper: loads a program object with a program binary, see also http://www.opengl.org/sdk/docs/man4/xhtml/glProgramBinary.xml
    bool programBinary(GLenum binary_format, const void* binary, int length);

    // --------------- uniform variables ---------------

    /**
     * Applies a set of uniforms to the currently bound GLSL program.
     * This function expects the GLSLProgram to be already bound, see useProgram().
    */
    bool applyUniformSet(const UniformSet* uniforms) const;

    /**
    * Returns the binding index of the given uniform.
    */
    int getUniformLocation(const char* name) const
    {
      VL_CHECK( Has_GLSL )
      if( !Has_GLSL )
        return -1;
      VL_CHECK(linked())

      int location = glGetUniformLocation(handle(), name);
      return location;
    }

    // --------------- uniform variables: getters ---------------

    // general uniform getters: use these to access to all the types supported by your GLSL implementation,
    // and not only the ordinary fvec2, fvec3, fvec4, ivec2, ivec3, ivec4, fmat2, fmat3, fmat4

    //! Equivalent to glGetUniformfv(handle(), location, params)
    void getUniformfv(int location, float* params) const
    {
      VL_CHECK( Has_GLSL )
      if( !Has_GLSL )
        return;
      VL_CHECK(linked())
      VL_CHECK(handle())
      glGetUniformfv(handle(), location, params); VL_CHECK_OGL()
    }
    //! Equivalent to getUniformfv(getUniformLocation(name), params)
    void getUniformfv(const char* name, float* params) const { getUniformfv(getUniformLocation(name), params); }
    //! Equivalent to glGetUniformiv(handle(), location, params)
    void getUniformiv(int location, int* params) const
    {
      VL_CHECK( Has_GLSL )
      if( !Has_GLSL )
        return;
      VL_CHECK(linked())
      VL_CHECK(handle())
      glGetUniformiv(handle(), location, params); VL_CHECK_OGL()
    }
    //! Equivalent to getUniformiv(getUniformLocation(name)
    void getUniformiv(const char* name, int* params) const { getUniformiv(getUniformLocation(name), params); }

    // utility functions for fvec2, fvec3, fvec4, ivec2, ivec3, ivec4, fmat2, fmat3, fmat4

    void getUniform(int location, fvec2& vec) const { getUniformfv(location, vec.ptr()); }
    void getUniform(int location, fvec3& vec) const { getUniformfv(location, vec.ptr()); }
    void getUniform(int location, fvec4& vec) const { getUniformfv(location, vec.ptr()); }
    void getUniform(int location, fmat2& mat) const { getUniformfv(location, mat.ptr()); }
    void getUniform(int location, fmat3& mat) const { getUniformfv(location, mat.ptr()); }
    void getUniform(int location, fmat4& mat) const { getUniformfv(location, mat.ptr()); }
    void getUniform(int location, ivec2& vec) const { getUniformiv(location, vec.ptr()); }
    void getUniform(int location, ivec3& vec) const { getUniformiv(location, vec.ptr()); }
    void getUniform(int location, ivec4& vec) const { getUniformiv(location, vec.ptr()); }
    void getUniform(const char* name, fvec2& vec) const { getUniform(getUniformLocation(name), vec); }
    void getUniform(const char* name, fvec3& vec) const { getUniform(getUniformLocation(name), vec); }
    void getUniform(const char* name, fvec4& vec) const { getUniform(getUniformLocation(name), vec); }
    void getUniform(const char* name, fmat2& mat) const { getUniform(getUniformLocation(name), mat); }
    void getUniform(const char* name, fmat3& mat) const { getUniform(getUniformLocation(name), mat); }
    void getUniform(const char* name, fmat4& mat) const { getUniform(getUniformLocation(name), mat); }
    void getUniform(const char* name, ivec2& vec) const { getUniform(getUniformLocation(name), vec); }
    void getUniform(const char* name, ivec3& vec) const { getUniform(getUniformLocation(name), vec); }
    void getUniform(const char* name, ivec4& vec) const { getUniform(getUniformLocation(name), vec); }

    //! Returns a GLSLProgram's \p static UniformSet. \p Static uniforms are those uniforms whose value is constant across one rendering as opposed to Shader uniforms that change across Shaders and Actor uniforms that change across Actors.
    UniformSet* getUniformSet() { return mUniformSet.get(); }
    //! Returns a GLSLProgram's \p static UniformSet. \p Static uniforms are those uniforms whose value is constant across one rendering as opposed to Shader uniforms that change across Shaders and Actor uniforms that change across Actors.
    const UniformSet* getUniformSet() const { return mUniformSet.get(); }
    //! Sets a GLSLProgram's \p static UniformSet.
    void setUniformSet(UniformSet* uniforms) { mUniformSet = uniforms; }
    //! Utility function using getUniformSet(). Adds a Uniform to this program's \p static uniform set.
    void setUniform(Uniform* uniform) { if (!getUniformSet()) setUniformSet(new UniformSet); getUniformSet()->setUniform(uniform); }
    //! Utility function using getUniformSet(). Returns the specified Uniform. Returns NULL if there isn't such a Uniform
    Uniform* getUniform(const char* name) { if (!getUniformSet()) return NULL; return getUniformSet()->getUniform(name); }
    //! Utility function using getUniformSet(). Gets or creates the specified Uniform.
    Uniform* gocUniform(const char* name) { if (!getUniformSet()) setUniformSet(new UniformSet); return getUniformSet()->gocUniform(name); }
    //! Utility function using getUniformSet(). Erases the specified uniform.
    void eraseUniform(const char* name) { if(getUniformSet()) getUniformSet()->eraseUniform(name); }
    //! Utility function using getUniformSet(). Erases the specified uniform.
    void eraseUniform(const Uniform* uniform) { if(getUniformSet()) getUniformSet()->eraseUniform(uniform); }
    //! Utility function using getUniformSet(). Erases all the uniforms.
    void eraseAllUniforms() { if(getUniformSet()) getUniformSet()->eraseAllUniforms(); }

    //! Returns a map containing name, type, size and location of all the uniforms that were active last time the GLSL program was linked.
    //! - See also vl::GLSLProgram::activeUniformInfo(), vl::UniformInfo, http://www.opengl.org/sdk/docs/man4/xhtml/glGetActiveUniform.xml
    const std::map<std::string, ref<UniformInfo> >& activeUniforms() const { return mActiveUniforms; }

    //! Returns the info (name, type, size and location) regarding the specified uniform or NULL if such uniform is not currently active since last time the GLSL program was linked.
    //! - See also vl::GLSLProgram::activeUniforms(), vl::UniformInfo, http://www.opengl.org/sdk/docs/man4/xhtml/glGetActiveUniform.xml
    const UniformInfo* activeUniformInfo(const char* name) const 
    { 
      std::map<std::string, ref<UniformInfo> >::const_iterator it = mActiveUniforms.find(name);
      if (it == mActiveUniforms.end())
        return NULL;
      else
        return it->second.get();
    }

    //! Returns a map containing the info of all the attributes active since last time the GLSL program was linked.
    //! - See also vl::GLSLProgram::activeAttribInfo(), vl::AttribInfo, http://www.opengl.org/sdk/docs/man4/xhtml/glGetActiveAttrib.xml
    const std::map<std::string, ref<AttribInfo> >& activeAttribs() const { return mActiveAttribs; }

    //! Returns the info regarding the specified attribute or NULL if such attribute is not currently active since last time the GLSL program was linked.
    //! - See also vl::GLSLProgram::activeAttribs(), vl::AttribInfo, http://www.opengl.org/sdk/docs/man4/xhtml/glGetActiveAttrib.xml
    const AttribInfo* activeAttribInfo(const char* name) const 
    { 
      std::map<std::string, ref<AttribInfo> >::const_iterator it = mActiveAttribs.find(name);
      if (it == mActiveAttribs.end())
        return NULL;
      else
        return it->second.get();
    }

    //! Returns the binding location of the vl_ModelViewMatrix uniform variable or -1 if no such variable is used by the GLSLProgram
    int vl_ModelViewMatrix() const { return m_vl_ModelViewMatrix; }

    //! Returns the binding location of the vl_ProjectionMatrix uniform variable or -1 if no such variable is used by the GLSLProgram
    int vl_ProjectionMatrix() const  { return m_vl_ProjectionMatrix; }

    //! Returns the binding location of the vl_ModelViewProjectionMatrix uniform variable or -1 if no such variable is used by the GLSLProgram
    int vl_ModelViewProjectionMatrix() const  { return m_vl_ModelViewProjectionMatrix; }

    //! Returns the binding location of the vl_NormalMatrix uniform variable or -1 if no such variable is used by the GLSLProgram
    int vl_NormalMatrix() const { return m_vl_NormalMatrix; }

  private:
    void preLink();
    void postLink();

  protected:
    std::vector< ref<GLSLShader> > mShaders;
    std::map<std::string, int> mFragDataLocation;
    std::map<std::string, ref<UniformInfo> > mActiveUniforms;
    std::map<std::string, ref<AttribInfo> > mActiveAttribs;
    std::map<std::string, int> mAutoAttribLocation;
    ref<UniformSet> mUniformSet;
    unsigned int mHandle;
    bool mScheduleLink;

    // glProgramParameter
    int mGeometryVerticesOut;
    EGeometryInputType mGeometryInputType;
    EGeometryOutputType mGeometryOutputType;
    bool mProgramBinaryRetrievableHint;
    bool mProgramSeparable;

    int m_vl_ModelViewMatrix;
    int m_vl_ProjectionMatrix;
    int m_vl_ModelViewProjectionMatrix;
    int m_vl_NormalMatrix;
  };
}

#endif
