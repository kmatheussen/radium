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

varying vec3 N;
varying vec3 L;
void main(void)
{
 vec3 l = normalize(N);
 vec3 n = normalize(L);
 vec3 H = normalize(l + vec3(0.0,0.0,1.0));

 // compute diffuse equation
 float NdotL = dot(n,l);
 // vec4 diffuse = gl_FrontMaterial.diffuse * vec4(max(0.0,NdotL));
 vec4 diffuse = gl_FrontMaterial.diffuse * vec4(abs(NdotL));

 float NdotH = max(0.0, dot(n,H));
 vec4 specular = vec4(0.0);
 const float specularExp = 128.0;
 if (NdotL > 0.0)
    specular = vec4(pow(NdotH, specularExp));

 vec4 color = clamp( (diffuse + specular), 0.0, 1.0);

 float gray = dot( color.rgb, vec3(0.299,0.587,0.114) );
 if (gray >= 1.0)
 		gray = 0.999;
 if (gray < 0.0)
 		gray = 0.0;

 int col1 = int(gray*3.0);
 int col2 = col1 + 1;
 float t = fract(gray*3.0);
 color.rgb = heat(col1)*(1.0-t) + heat(col2)*t;
 color.a = 1.0;

 gl_FragColor = color;
}
