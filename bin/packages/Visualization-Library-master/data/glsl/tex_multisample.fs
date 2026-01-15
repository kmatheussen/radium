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

#version 150 compatibility

uniform sampler2DMS ms_texture;

void main(void)
{
	const int samples = 4;

	ivec2 texcoord = ivec2( vec2(textureSize(ms_texture)) * gl_TexCoord[0].st);
	vec4 tex_color = vec4(0.0, 0.0, 0.0, 0.0);
	for(int i=0; i<samples; ++i)
		tex_color += texelFetch(ms_texture, texcoord, i);
	tex_color /= samples;

	gl_FragColor = gl_Color * tex_color;
}