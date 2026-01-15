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

// #version 120
#extension GL_ARB_draw_instanced: enable
#extension GL_EXT_gpu_shader4: enable
uniform mat4 model_view_matrix[100];
uniform mat3 normal_matrix[100];
void main(void)
{
	vec4 pos = model_view_matrix[gl_InstanceID] * gl_Vertex;

	gl_Position = gl_ProjectionMatrix * pos;
	vec3 N = normalize(normal_matrix[gl_InstanceID] * gl_Normal);
	vec3 V = pos.xyz;
	vec3 L = normalize(gl_LightSource[0].position.xyz - V.xyz);
	// compute diffuse equation
	float NdotL = dot(N,L);
	gl_FrontColor = gl_Color * vec4(max(0.0,NdotL));
}
