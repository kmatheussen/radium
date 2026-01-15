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
	const float quantize = 3.0;
	vec3 l = normalize(L);
	vec3 n = normalize(N);
	vec3 H = normalize(l + vec3(0.0,0.0,1.0));

	// compute diffuse equation
	float NdotL = dot(n,l);
	NdotL = floor(NdotL*quantize)/quantize;
	vec3 diffuse = gl_Color.rgb * vec3(abs(NdotL));

	float NdotH = max(0.0, dot(n,H));
	vec3 specular = vec3(0.0);
	const float specularExp = 128.0;
	if (NdotL > 0.0)
	  specular = vec3(pow(NdotH, specularExp));

	specular = floor(specular*quantize)/quantize;

	vec3 basecolor = vec3(0.2,0.2,0.2);
	vec3 color = clamp( (diffuse + specular + basecolor), 0.0, 1.0);
	gl_FragColor.rgb = color;
	gl_FragColor.a = 1.0;
}
