/**************************************************************************************/
/*                                                                                    */
/*  Copyright (c) 2005-2011, Michele Bosi.                                            */
/*  All rights reserved.                                                              */
/*                                                                                    */
/*  This file is part of Visualization Library                                        */
/*  http://www.visualizationlibrary.org                                               */
/*                                                                                    */
/*  Released under the OSI approved Simplified BSD License                            */
/*  http://www.opensource.org/licenses/bsd-license.php                                */
/*                                                                                    */
/**************************************************************************************/

varying vec3 L;
attribute vec3 tangent;
uniform vec3 light_obj_space_pos;
void main(void)
{
 gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;

 vec3 bitangent = cross( gl_Normal, tangent  );
 
 vec3 v = light_obj_space_pos.xyz-gl_Vertex.xyz;
 L.x = dot( tangent,   v );
 L.y = dot( bitangent, v );
 L.z = dot( gl_Normal, v );
 #if 0
  L = normalize(L); // normalized in the fragment shader
 #endif
 
 gl_FrontColor = gl_Color;
 gl_TexCoord[0] = gl_MultiTexCoord0;
}
