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

#version 120

#extension GL_ARB_geometry_shader4 : enable

void main(void)
{
	vec4 A = gl_PositionIn[0];
	vec4 B = gl_PositionIn[1];
	vec4 C = gl_PositionIn[2];
	vec4 D = (A+B) * 0.5;
	vec4 E = (B+C) * 0.5;
	vec4 F = (C+A) * 0.5;
	vec4 G = (A+B+C) * 0.33333;
	vec3 N = normalize(cross(B.xyz-A.xyz, C.xyz-A.xyz)) * length(B.xyz-A.xyz);
	G += vec4(-2.0*N,0);

	gl_Position    = A; gl_FrontColor  = gl_FrontColorIn[0]; EmitVertex();
	gl_Position    = D; gl_FrontColor  = gl_FrontColorIn[2]; EmitVertex();
	gl_Position    = F; gl_FrontColor  = gl_FrontColorIn[1]; EmitVertex();
	EndPrimitive();

	gl_Position    = F; gl_FrontColor  = gl_FrontColorIn[0]; EmitVertex();
	gl_Position    = E; gl_FrontColor  = gl_FrontColorIn[2]; EmitVertex();
	gl_Position    = C; gl_FrontColor  = gl_FrontColorIn[1]; EmitVertex();
	EndPrimitive();

	gl_Position    = E; gl_FrontColor  = gl_FrontColorIn[0]; EmitVertex();
	gl_Position    = D; gl_FrontColor  = gl_FrontColorIn[2]; EmitVertex();
	gl_Position    = B; gl_FrontColor  = gl_FrontColorIn[1]; EmitVertex();
	EndPrimitive();

	gl_Position    = D; gl_FrontColor  = gl_FrontColorIn[0]; EmitVertex();
	gl_Position    = G; gl_FrontColor  = gl_FrontColorIn[2]; EmitVertex();
	gl_Position    = F; gl_FrontColor  = gl_FrontColorIn[1]; EmitVertex();
	EndPrimitive();

	gl_Position    = F; gl_FrontColor  = gl_FrontColorIn[0]; EmitVertex();
	gl_Position    = G; gl_FrontColor  = gl_FrontColorIn[2]; EmitVertex();
	gl_Position    = E; gl_FrontColor  = gl_FrontColorIn[1]; EmitVertex();
	EndPrimitive();

	gl_Position    = E; gl_FrontColor  = gl_FrontColorIn[0]; EmitVertex();
	gl_Position    = G; gl_FrontColor  = gl_FrontColorIn[2]; EmitVertex();
	gl_Position    = D; gl_FrontColor  = gl_FrontColorIn[1]; EmitVertex();
	EndPrimitive();
}
