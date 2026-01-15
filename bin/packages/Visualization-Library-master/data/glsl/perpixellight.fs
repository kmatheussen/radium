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

// requires "perpixellight.vs"

varying vec3 N;
varying vec3 L;
void main(void)
{
	vec3 l = normalize(L);
	vec3 n = normalize(N);
	vec3 H = normalize(l + vec3(0.0,0.0,1.0));

	// compute diffuse equation
	float NdotL = dot(n,l);
	vec4 diffuse = gl_Color * vec4(max(0.0,NdotL));

	float NdotH = max(0.0, dot(n,H));
	vec4 specular = vec4(0.0);
	const float specularExp = 128.0;
	if (NdotL > 0.0)
	  specular = vec4(pow(NdotH, specularExp));

	gl_FragColor.rgb = (diffuse + specular).rgb;
	gl_FragColor.a = 1.0;
}