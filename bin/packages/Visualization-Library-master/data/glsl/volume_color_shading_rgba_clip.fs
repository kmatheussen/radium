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
varying vec4 WorldPosition;
void main(void)
{
	// cylindrical clipping
	float dist = length( WorldPosition.xy - vec2(0.0,2.0) );
	if (dist < 2.0)
	  discard;

	gl_FragColor = texture3D(sampler0, gl_TexCoord[0].xyz );
}