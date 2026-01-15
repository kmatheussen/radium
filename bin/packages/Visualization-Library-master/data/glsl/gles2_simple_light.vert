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

// VL matrices
uniform mat4 vl_ModelViewProjectionMatrix;
uniform mat4 vl_ModelViewMatrix;
uniform mat4 vl_NormalMatrix;
uniform mat4 vl_ProjectionMatrix;

// Other uniforms
uniform vec3 U_LightPosition;

// input
attribute vec4 A_Position;
attribute vec4 A_Normal;
attribute vec4 A_Color;

// output
varying vec4 V_Color;

// main function
void main(void)
{
	vec3 V = (vl_ModelViewMatrix * A_Position).xyz;
	vec3 N = (vl_NormalMatrix * A_Normal).xyz;
	vec3 L = normalize(U_LightPosition - V);
	float NdotL = dot(N, L);
	V_Color = A_Color * vec4(max(0.0, NdotL));

	// quick projection, see http://www.songho.ca/opengl/gl_projectionmatrix.html
	// we could actually pass the projection matrix as a vec4
	// gl_Position = vl_ProjectionMatrix * vec4(V, 0.0);
    gl_Position.x = vl_ProjectionMatrix[0].x * V.x;
    gl_Position.y = vl_ProjectionMatrix[1].y * V.y;
    gl_Position.z = vl_ProjectionMatrix[2].z * V.z + vl_ProjectionMatrix[2].w;
    gl_Position.w = -V.z;
}
