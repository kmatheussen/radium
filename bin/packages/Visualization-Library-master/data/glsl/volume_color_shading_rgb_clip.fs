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

uniform sampler3D sampler0;
uniform float alpha_bias;
varying vec4 WorldPosition;
void main(void)
{
	// cylindrical clipping
	float dist = length( WorldPosition.xy - vec2(0.0,2.0) );
	if (dist < 2.0)
	  discard;

	vec3 col = texture3D(sampler0, gl_TexCoord[0].xyz ).rgb;
	gl_FragColor.rgb = col;
	float gray = dot( col, vec3(0.299,0.587,0.114) );
	gl_FragColor.a = gray * alpha_bias;
}