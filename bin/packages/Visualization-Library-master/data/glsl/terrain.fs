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

uniform sampler2D terrain_tex;
uniform sampler2D detail_tex;
void main(void)
{
	gl_FragColor = texture2D(terrain_tex, gl_TexCoord[0].st ) * texture2D(detail_tex, gl_TexCoord[1].st );
}
