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

#version 400 compatibility

layout(triangles) in;
layout(triangle_strip, max_vertices = 3) out;

in vec3 tesPosition[3];
in vec4 tesColor[3];

vec3 shrink(vec3 v, vec3 center, float amount)
{
	return normalize(center-v)*amount + v;
}

void main(void)
{
	vec4 A = gl_ModelViewMatrix * vec4( tesPosition[0], 1.0 );
	vec4 B = gl_ModelViewMatrix * vec4( tesPosition[1], 1.0 );
	vec4 C = gl_ModelViewMatrix * vec4( tesPosition[2], 1.0 );
	vec3 N = normalize( cross( B.xyz-A.xyz, C.xyz-A.xyz) );
	vec3 center = ( A.xyz + B.xyz + C.xyz ) / 3.0;
	A.xyz = shrink(A.xyz, center, 0.01);
	B.xyz = shrink(B.xyz, center, 0.01);
	C.xyz = shrink(C.xyz, center, 0.01);

	// simple flat shading from the center of the triangle
	vec3 L = normalize( gl_LightSource[0].position.xyz - center );
	vec4 NdotL = vec4( max(0.0, dot(N, L) ) );

	gl_FrontColor = tesColor[0] * NdotL;
	gl_Position = gl_ProjectionMatrix * A; EmitVertex();

	gl_FrontColor = tesColor[1] * NdotL;
	gl_Position = gl_ProjectionMatrix * B; EmitVertex();

	gl_FrontColor = tesColor[2] * NdotL;
	gl_Position = gl_ProjectionMatrix * C; EmitVertex();

	EndPrimitive();
}
