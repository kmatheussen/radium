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

varying float NdotL;
varying vec3 ReflectVec;
varying vec3 ViewVec;

void main()
{
	vec3 LightPos = gl_LightSource[0].position.xyz;

	vec3 ecPos = vec3( gl_ModelViewMatrix * gl_Vertex);
	vec3 tnorm = normalize(gl_NormalMatrix * gl_Normal);
	vec3 lightVec = normalize(LightPos - ecPos);
	ReflectVec = normalize(reflect(-lightVec, tnorm));
	ViewVec = normalize(-ecPos);
	NdotL = (dot(lightVec, tnorm) + 1.0) * 0.5;
	gl_Position = ftransform();
}
