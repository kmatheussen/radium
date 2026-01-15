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

uniform sampler1D sampler0;
void main(void)
{
	float gray = dot( gl_Color.xyz + gl_SecondaryColor.xyz, vec3(0.299,0.587,0.114) );
	gl_FragColor = texture1D(sampler0, gray, 0);
}
