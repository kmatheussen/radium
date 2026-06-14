/*
#version 440

layout(location = 0) in vec4 position;
layout(location = 1) in vec3 color;

layout(location = 0) out vec3 v_color;

layout(std140, binding = 0) uniform buf {
    mat4 mvp;
    float opacity;
};

void main()
{
    v_color = color;
    gl_Position = mvp * position;
}
*/

#version 440

layout(location = 0) in vec2 pos;
layout(location = 1) in vec4 color;

layout(location = 0) out vec4 v_color;

layout(std140, binding = 0) uniform buf {
    mat4 mvp;
    float yscroll;
};

void main()
{
	v_color = vec4(color.rgb * color.a, color.a);
	gl_Position = mvp * vec4(pos.x, pos.y - yscroll, 0, 1);
}
