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

varying vec3 N;
varying vec3 L;

vec3 heat(int i)
{
	if (i==0)
		return vec3(0.0,0.0,1.0);

	if (i==1)
		return vec3(0.0,1.0,0.0);

	if (i==2)
		return vec3(1.0,1.0,0.0);
	else
		return vec3(1.0,0.0,0.0);
}

void main(void)
{
	const float quantize = 3.0;
	vec3 n = normalize(L);
	vec3 l = normalize(N);
	vec3 H = normalize(l + vec3(0.0,0.0,1.0));

	// compute diffuse equation
	float NdotL = dot(n,l);
	vec4 diffuse = gl_FrontMaterial.diffuse * vec4(max(0.0,NdotL));

	float NdotH = max(0.0, dot(n,H));
	vec4 specular = vec4(0.0);
	const float specularExp = 128.0;
	if (NdotL > 0.0)
	  specular = vec4(pow(NdotH, specularExp));

	specular.rgb = floor(specular.rgb*quantize)/quantize;
	specular.a = 1.0;

	vec4 color = clamp( (diffuse + specular), 0.0, 1.0);

	float gray = dot( color.rgb, vec3(0.299,0.587,0.114) );
	if (gray >= 1.0)
		gray = 0.999;
	if (gray < 0.0)
		gray = 0.0;

	// vec3 heat[] = { vec3(0.0,0.0,1.0), vec3(0.0,1.0,0.0), vec3(1.0,1.0,0.0), vec3(1.0,0.0,0.0) };
	int col1 = int(gray*3.0);
	int col2 = col1 + 1;
	float t = fract(gray*3.0);
	// color.rgb = heat[col1]*(1.0-t) + heat[col2]*t;
	color.rgb = heat(col1)*(1.0-t) + heat(col2)*t;
	color.a = 1.0;

	gl_FragData[0] = color;

	H = normalize(l + vec3(0.0,0.0,1.0));

	// compute diffuse equation
	NdotL = dot(n,l);
	diffuse = gl_FrontMaterial.diffuse * vec4(max(0.0,NdotL));

	NdotH = max(0.0, dot(n,H));
	specular = vec4(0.0);
	if (NdotL > 0.0)
	  specular = vec4(pow(NdotH, specularExp));

	gl_FragData[1] = vec4( (diffuse + specular).rgb, 1.0 );
}