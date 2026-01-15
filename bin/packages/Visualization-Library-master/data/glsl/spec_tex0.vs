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

void main(void)
{
	gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
	vec3 N = normalize(gl_NormalMatrix * gl_Normal);
	vec3 V = gl_ModelViewMatrix * gl_Vertex;
	vec3 L = normalize(gl_LightSource[0].position - V.xyz);
	vec3 H = normalize(L + vec3(0.0,0.0,1.0));

	// compute diffuse equation
	float NdotL = dot(N,L);
	vec4 diffuse = gl_Color * vec4(max(0.0,NdotL));

	float NdotH = max(0.0, dot(N,H));
	vec4 specular = vec4(0.0);
	const float specularExp = 128.0;
	if (NdotL > 0.0)
	  specular = vec4(pow(NdotH, specularExp));
	gl_FrontColor = diffuse + specular;
	gl_TexCoord[0] = gl_MultiTexCoord0;
}
