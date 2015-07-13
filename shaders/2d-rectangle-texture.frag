#version 330

out vec4 outputColor;

uniform sampler2D rectangle_texture;

void main() {
  outputColor = texture(rectangle_texture, vec2(0.5,0.5));
}
