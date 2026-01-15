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
void main(void)
{
	vec4 color = texture3D(sampler0, gl_TexCoord[0].xyz );
	if (color.a >= alpha_bias) 
	{
		// color.a *= alpha_bias;
		gl_FragColor = color;
	}
	else
		discard;
}
