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

// inspired by http://http.developer.nvidia.com/GPUGems/gpugems_ch05.html

// requires "noisebump.vs"
// requires "noise3D.glsl"

varying vec3 V;
varying vec3 N;
varying vec3 L;

float snoise(vec3 v);

// const float base_freq = 0.05; // DINO
// const float base_freq = 0.0004; // CAT
// const float base_freq = 15.0; // COW
// const float base_freq = 12.5; // HORSE
// const float base_freq = 50.0; // DRAGON
// const float base_freq = 15.0; // CANE
// const float base_freq = 1.7; // HAND
const float base_freq = 2.9; // SPIDER

float F(vec3 v)
{
	vec3 offset = vec3(1.0, 1.1, 1.0);
	
	// return snoise(v * base_freq);
	vec4 noisevec;
	noisevec.x = snoise(offset + v * base_freq*1.0) * 8.0;
	noisevec.y = snoise(offset + v * base_freq*2.0) * 2.0;
	noisevec.z = snoise(offset + v * base_freq*4.0) * 1.0;
	noisevec.w = snoise(offset + v * base_freq*8.0) * 0.5;

	float intensity = noisevec[0] + 
					  noisevec[1] + 
					  noisevec[2] +
					  noisevec[3] ;
					  
	intensity = abs(intensity);
					  
	intensity = (intensity / 15.0 + 1.0) / 2.0;
	// intensity = intensity / 15.0;
	// intensity = intensity * intensity;
	intensity -= 0.5;
	intensity *= 2.0;

	return sin(cos(intensity*3.54159265)*3.14159265);

	// noisevec = noisevec / 15.0; // range = -1 .. +1
	// noisevec = (noisevec / 15.0 + 1.0) / 2.0; // range = 0 .. +1

	// float intensity = abs(noisevec[0]) + 
					  // abs(noisevec[1]) + 
					  // abs(noisevec[2]) +
					  // abs(noisevec[3]);
					  
	// return intensity;
}

void main(void)
{
	const vec4 Color1 = vec4(0.2, 0.0, 0.0, 1.0);
	const vec4 Color2 = vec4(1.0, 1.0, 1.0, 1.0);
	
	const float eps = 0.0005;

	float F0 = F(V);
	float Fx = F(vec3(V.x+eps, V.y, V.z));
	float Fy = F(vec3(V.x, V.y+eps, V.z));
	float Fz = F(vec3(V.x, V.y, V.z+eps));
	vec3 dF = vec3( (Fx-F0)/eps, (Fy-F0)/eps, (Fz-F0)/eps );

	dF = normalize(gl_NormalMatrix * dF) * 0.33;
	
	vec3 l = normalize(L);
	vec3 n = normalize( normalize(N) - dF );
	// vec3 n = normalize( N );
	vec3 H = normalize(l + vec3(0.0,0.0,1.0));

	// compute diffuse equation
	float NdotL = dot(n,l);
	vec4 diffuse = mix(Color1, Color2, F0) * vec4(max(0.0,NdotL));
	// vec4 diffuse = vec4(max(0.0,NdotL));

	float NdotH = max(0.0, dot(n,H));
	vec4 specular = vec4(0.0);
	const float specularExp = 128.0;
	if (NdotL > 0.0)
	  specular = vec4(pow(NdotH, specularExp));

	gl_FragColor.rgb = (diffuse + specular).rgb;
	gl_FragColor.a = 1.0;
}