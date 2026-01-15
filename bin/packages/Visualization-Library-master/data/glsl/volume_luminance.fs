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

// This shader directly maps the volume value to the transfer function with no
// lighting. This shader is to be used with IF_LUMINANCE image volumes.

uniform sampler3D volume_texunit;
uniform sampler1D trfunc_texunit;
uniform float     trfunc_delta;
void main(void)
{
	// sample the LUMINANCE value
	
	float val = texture3D(volume_texunit, gl_TexCoord[0].xyz ).x;
	
	// sample the transfer function
	
	// to properly sample the texture clamp bewteen trfunc_delta...1.0-trfunc_delta
	float clamped_val = trfunc_delta+(1.0-2.0*trfunc_delta)*val;
	vec4 rgba = texture1D(trfunc_texunit, clamped_val );

	gl_FragColor = rgba;
}
