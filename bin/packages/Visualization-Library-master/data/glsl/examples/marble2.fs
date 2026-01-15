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

// inspired by the "Orange Book" (http://www.3dshaders.com)

// requires "noise.vs"
// requires "noise3D.glsl"

varying float LightIntensity;
varying vec3 MCposition;

float snoise(vec3 v);

void main(void)
{
	const vec3 Color1 = vec3(0.8, 0.8, 0.8);
	const vec3 Color2 = vec3(0.2, 0.2, 0.5);
	// const float base_freq = 0.005; // DINO
	// const float base_freq = 0.0004; // CAT
	// const float base_freq = 0.5; // COW
	// const float base_freq = 0.5; // HORSE
	// const float base_freq = 8.3; // DRAGON
	// const float base_freq = 1.0; // CANE
	// const float base_freq = 0.2; // HAND
	const float base_freq = 0.2; // SPIDER

	vec4 noisevec;
	noisevec.x = snoise(MCposition * base_freq*1.0) * 8.0;
	noisevec.y = snoise(MCposition * base_freq*2.0) * 4.0;
	noisevec.z = snoise(MCposition * base_freq*4.0) * 2.0;
	noisevec.w = snoise(MCposition * base_freq*8.0) * 1.0;
	// noisevec = (noisevec / 8.0 + 1.0) / 2.0;
	noisevec = noisevec / 8.0;
	// noisevec = noisevec * noisevec;

	float intensity = abs(noisevec[0] - 0.20) + 
	                  abs(noisevec[1] - 0.10) + 
					  abs(noisevec[2] - 0.05) +
					  abs(noisevec[3] - 0.025);
	
	// intensity = intensity *  intensity;
	// intensity -= 0.3;
	
	float sineval = sin(MCposition.y * 12.0 + intensity * 8.0) * 0.5 + 0.5;
	
	vec3 color = mix(Color1, Color2, sineval) * LightIntensity;
	gl_FragColor = vec4(color, 1.0);
}
