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
    float yscroll;
};

void main()
{
	vec4 gakk = color;
	//gakk.rgb *= 0.5;
	//gakk.a = 0.5;
	gakk.rgb *= gakk.a;
	//color.rgb *= color.a;
	v_color = gakk; //color; //gakk; //color; //vec3(1,0,0);//color;
	//gl_Position = vec4(pos, 0.0, 1.0); //vec4(pos, 0.0, 1.0);
	gl_Position = vec4(pos.x, pos.y+yscroll, 0, 1); //vec4(pos, 0.0, 1.0);
	//FragColor = v_color;
}
