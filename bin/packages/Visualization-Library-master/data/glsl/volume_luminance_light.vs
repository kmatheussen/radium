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

// Simply passes the vertex frag_position and texture coordinate to the fragment shader. 
// It also passes the vertex coord in object space to perform per-pixel lighting.

varying vec3 frag_position; // in object space

void main(void)
{
	frag_position = gl_Vertex.xyz;
	gl_TexCoord[0] = gl_MultiTexCoord0;
	gl_Position = ftransform();
}
// Have fun!