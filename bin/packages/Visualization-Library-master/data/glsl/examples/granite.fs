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

// requires "noise.vs"
// requires "noise3D.glsl"

varying float LightIntensity;
varying vec3 MCposition;

float snoise(vec3 v);

void main(void)
{
	const vec3 Color1 = vec3(0.35, 0.3, 0.2);
	const vec3 Color2 = vec3(0.7, 0.7, 0.7);

	// const float base_freq = 0.01; // DINO
	// const float base_freq = 0.0004; // CAT
	// const float base_freq = 2.0; // COW
	// const float base_freq = 0.3; // HORSE
	// const float base_freq = 7.3; // DRAGON
	// const float base_freq = 1.5; // CANE
	// const float base_freq = 0.2; // HAND
	// const float base_freq = 2.1; // RINO
	const float base_freq = 0.2; // SPIDER

	
	vec4 noisevec;
	noisevec.x = snoise(MCposition * base_freq*1.0) * 8.0;
	noisevec.y = snoise(MCposition * base_freq*2.0) * 4.0;
	noisevec.z = snoise(MCposition * base_freq*4.0) * 2.0;
	noisevec.w = snoise(MCposition * base_freq*8.0) * 1.0;
	// noisevec = (noisevec / 8.0 + 1.0) / 2.0;
	noisevec = noisevec / 8.0;

	// alternate granite
	// float intensity = min(1.0, noisevec[2] * 12.0 + noisevec[3]*12);
	float intensity = min(1.0, noisevec[3] * 12.0);
	
	// vec3 color = vec3(intensity * LightIntensity);
	vec3 color = mix(Color1, Color2, intensity) * LightIntensity;
	gl_FragColor = vec4(color, 1.0);
}
