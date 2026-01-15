// adaptation from the "Orange Book" (http://www.3dshaders.com)

// requires "noise.vs"
// requires "noise3D.glsl"

varying float LightIntensity;
varying vec3 MCposition;

float snoise(vec3 v);

void main(void)
{
	const vec3 LightWood = vec3(0.6, 0.3, 0.1);
	const vec3 DarkWood = vec3(0.4, 0.2, 0.07);
	const vec3 NoiseScale = vec3(0.5, 0.1, 0.1);
	const float Noisiness = 5.0;
	const float RingFreq = 3.5;
	const float LightGrains = 1.0;
	const float DarkGrains = 0.0;
	const float GrainThreshold = 0.3;

	// const float base_freq = 0.005; // DINO
	// const float base_freq = 0.0004; // CAT
	// const float base_freq = 0.7; // COW
	// const float base_freq = 0.5; // HORSE
	// const float base_freq = 4; // DRAGON
	// const float base_freq = 1.0; // CANE
	// const float base_freq = 0.2; // HAND
	const float base_freq = 0.2; // SPIDER

	vec4 noisevec;
	noisevec.x = snoise(MCposition * NoiseScale * base_freq*1.0) * 8.0 * Noisiness;
	noisevec.y = snoise(MCposition * NoiseScale * base_freq*2.0) * 4.0 * Noisiness;
	noisevec.z = snoise(MCposition * NoiseScale * base_freq*4.0) * 2.0 * Noisiness;
	noisevec.w = snoise(MCposition * NoiseScale * base_freq*8.0) * 1.0 * Noisiness;
	// noisevec = (noisevec / 8.0 + 1.0) / 2.0;
	noisevec = noisevec / 8.0;

	vec3 location = MCposition + noisevec.xyz;
	float dist = sqrt( location.x * location.x + location.y * location.y ) * RingFreq;
	
	float r = fract(dist + noisevec[0] + noisevec[1] + noisevec[2]) * 2.0;
	
	if (r > 1.0)
		r = 2.0 - r;
	
	vec3 color = mix(LightWood, DarkWood, r);
	
	noisevec[2] *= r;
	
	if (r<GrainThreshold)
		color += LightWood * LightGrains * noisevec[2];
	else
		color += DarkWood * DarkGrains * noisevec[2];
		
	color *= LightIntensity;	
	gl_FragColor = vec4(color, 1.0);
}

