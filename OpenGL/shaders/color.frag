/*
#version 440

layout(location = 0) in vec3 v_color;

layout(location = 0) out vec4 fragColor;

layout(std140, binding = 0) uniform buf {
    mat4 mvp;
    float opacity;
};

void main()
{
    fragColor = vec4(v_color * opacity, opacity);
}

*/


#version 440

layout(location = 0) in vec4 v_color;

layout(location = 0) out vec4 c;

void main() {
	vec4 gakk = v_color;
	//gakk.rgb *= 0.5;
	//gakk.a = 0.5;
	//gakk.rgb *= gakk.a;

	//c = vec4(0.5, 0, 0.5, 1);
	c = gakk; //v_color; //vec4(v_color);
	//FragColor = v_color;
}
