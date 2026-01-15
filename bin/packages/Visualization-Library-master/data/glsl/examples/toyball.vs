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

varying vec3 N; // normal in object space
varying vec3 L; // light direction in camera space from the vertex

void main()
{
	const vec4 BallCenter = vec4(0.0, 0.0, 0.0, 1.0); // ball center in modeling coordinates
	
	N = (gl_Vertex - BallCenter).xyz;
	L = normalize( gl_LightSource[0].position.xyz - (gl_ModelViewMatrix * gl_Vertex).xyz );
	gl_Position = ftransform();
}
