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

// This shader maps the volume value to the transfer function plus computes the
// gradient and lighting on the fly. This shader is to be used with IF_LUMINANCE
// image volumes.

#define LIGHTING_ALPHA_THRESHOLD 0.02

varying vec3 frag_position;     // in object space
uniform sampler3D volume_texunit;
uniform sampler3D gradient_texunit;
uniform sampler1D trfunc_texunit;
uniform float trfunc_delta;
uniform vec3 light_position[4]; // light positions in object space
uniform bool light_enable[4];   // light enable flags
uniform vec3 eye_position;      // camera position in object space
uniform float val_threshold;
uniform vec3 gradient_delta; // for on-the-fly gradient computation
uniform bool precomputed_gradient; // whether the gradient has been precomputed or not

// computes a simplified lighting equation
vec3 blinn_phong(vec3 N, vec3 V, vec3 L, int light)
{
	// material properties
	// you might want to put this into a bunch or uniforms
	vec3 Ka = vec3(1.0, 1.0, 1.0);
	vec3 Kd = vec3(1.0, 1.0, 1.0);
	vec3 Ks = vec3(1.0, 1.0, 1.0);
	float shininess = 50.0;

	// diffuse coefficient
	float diff_coeff = max(dot(L,N),0.0);

	// specular coefficient
	vec3 H = normalize(L+V);
	float spec_coeff = pow(max(dot(H,N), 0.0), shininess);
	if (diff_coeff <= 0.0) 
		spec_coeff = 0.0;

	// final lighting model
	return  Ka * gl_LightSource[light].ambient.rgb + 
			Kd * gl_LightSource[light].diffuse.rgb  * diff_coeff + 
			Ks * gl_LightSource[light].specular.rgb * spec_coeff ;
}

void main(void)
{
	// sample the LUMINANCE value
	float val = texture3D(volume_texunit, gl_TexCoord[0].xyz ).r;
	
	// all the pixels whose val is less than val_threshold are discarded
	if (val < val_threshold)
		discard;
	
	// sample the transfer function
	
	// to properly sample the texture clamp bewteen trfunc_delta...1.0-trfunc_delta
	float clamped_val = trfunc_delta+(1.0-2.0*trfunc_delta)*val;
	vec4 color = texture1D(trfunc_texunit, clamped_val);
	vec3 color_tmp = vec3(0.0, 0.0, 0.0);

	// compute the gradient and lighting only if the pixel is visible "enough"
	if (color.a > LIGHTING_ALPHA_THRESHOLD)
	{
		vec3 N;
		if (precomputed_gradient)
		{
			// retrieve pre-computed gradient
			N  = normalize( (texture3D(gradient_texunit, gl_TexCoord[0].xyz).xyz - vec3(0.5,0.5,0.5))*2.0 );
		}
		else
		{
			// on-the-fly gradient computation: slower but requires less memory (no gradient texture required).
			vec3 sample1, sample2;
			sample1.x = texture3D(volume_texunit, gl_TexCoord[0].xyz-vec3(gradient_delta.x,0.0,0.0) ).r;
			sample2.x = texture3D(volume_texunit, gl_TexCoord[0].xyz+vec3(gradient_delta.x,0.0,0.0) ).r;
			sample1.y = texture3D(volume_texunit, gl_TexCoord[0].xyz-vec3(0.0,gradient_delta.y,0.0) ).r;
			sample2.y = texture3D(volume_texunit, gl_TexCoord[0].xyz+vec3(0.0,gradient_delta.y,0.0) ).r;
			sample1.z = texture3D(volume_texunit, gl_TexCoord[0].xyz-vec3(0.0,0.0,gradient_delta.z) ).r;
			sample2.z = texture3D(volume_texunit, gl_TexCoord[0].xyz+vec3(0.0,0.0,gradient_delta.z) ).r;
			N  = normalize( sample1 - sample2 );
		}

		vec3 V  = normalize(eye_position - frag_position);
		for(int i=0; i<4; ++i)
		{
			if (light_enable[i])
			{
				vec3 L = normalize(light_position[i] - frag_position);
				color_tmp.rgb += color.rgb * blinn_phong(N,V,L,i);
			}
		}
	}
	else
		color_tmp = color.rgb;

	gl_FragColor = vec4(color_tmp,color.a);
}
// Have fun!