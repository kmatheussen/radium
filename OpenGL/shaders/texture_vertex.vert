#version 440
layout(location = 0) in vec2 a_position;
layout(location = 1) in vec2 a_texcoord;
layout(location = 2) in vec4 a_color;
layout(location = 0) out vec2 v_texcoord;
layout(location = 1) out vec4 v_color;

layout(std140, binding = 1) uniform buf {
    mat4 matrix;
};

// Didn't work putting matrix+yscroll into the same binding. I have no idea why, it works for the triangle buffers with the exact same data, but it should make no practical difference in processing time.
layout(std140, binding = 2) uniform buf2 {
	float yscroll;
};

void main() {
    gl_Position = matrix * vec4(a_position.x, a_position.y - yscroll, 0.0, 1.0);
    v_texcoord = a_texcoord;
    v_color = a_color;
}
