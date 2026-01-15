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

/* color control raycast example */

varying vec3 frag_position;     // in object space
uniform sampler3D volume_texunit;
uniform sampler1D trfunc_texunit;
uniform float trfunc_delta;
uniform vec3 eye_position;      // camera position in object space
uniform float sample_step;      // step used to advance the sampling ray
uniform float val_threshold;

void main(void)
{
	const float brightness = 10.0;
	// NOTE: ray direction goes from frag_position to eye_position, i.e. back to front
	vec3 ray_dir = normalize(eye_position - frag_position);
	vec3 ray_pos = gl_TexCoord[0].xyz; // the current ray position
	vec3 pos111 = vec3(1.0, 1.0, 1.0);
	vec3 pos000 = vec3(0.0, 0.0, 0.0);

	vec4 frag_color = vec4(0.0, 0.0, 0.0, 0.0);
	vec4 color;
	do
	{
		// note: 
		// - ray_dir * sample_step can be precomputed
		// - we assume the volume has a cube-like shape

		ray_pos += ray_dir * sample_step;

		// break out if ray reached the end of the cube.
		if (any(greaterThan(ray_pos,pos111)))
			break;

		if (any(lessThan(ray_pos,pos000)))
			break;

		float density = texture3D(volume_texunit, ray_pos).r;

		color.rgb = texture1D(trfunc_texunit, val_threshold + density).rgb;
		color.a   = density * sample_step * brightness;
		frag_color.rgb = frag_color.rgb * (1.0 - color.a) + color.rgb * color.a;
	}
	while(true);

	if (frag_color == vec4(0.0,0.0,0.0,0.0))
		discard;
	else
		gl_FragColor = vec4(frag_color.rgb,1.0);
}
// Have fun!