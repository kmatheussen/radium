#version 440
layout(location = 0) in vec2 v_texcoord;
layout(location = 1) in vec4 v_color;
layout(location = 0) out vec4 fragColor;
layout(binding = 0) uniform sampler2D u_texture;

void main() {
    vec4 texColor = texture(u_texture, v_texcoord);
    fragColor = texColor * v_color;
}
