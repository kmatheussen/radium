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
uniform sampler2D sampler0;
void main(void)
{
	vec3 l = normalize(L);
	vec3 N = texture2D(sampler0, gl_TexCoord[0].st, 0.0).rgb * 2.0 - 1.0;
	#if 0
	 vec3 n = normalize(N);
	#else
	 vec3 n = N;
	#endif

	// compute diffuse equation
	float NdotL = dot(n,l);
	vec4 diffuse = gl_Color * vec4(max(0.0,NdotL));

	gl_FragColor.rgb = diffuse.rgb;

	gl_FragColor.a = 1.0;
}