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

#version 400

uniform sampler2D tex_diffuse;
in vec2 TexCoord;
out vec4 FragColor;

void main(void)
{
	vec3 diffuse = texture(tex_diffuse, TexCoord).rgb;
	FragColor = vec4( diffuse, 1.0);
}
