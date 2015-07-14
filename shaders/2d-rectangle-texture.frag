#version 330

out vec4 outputColor;

uniform sampler2D rectangle_texture;

in vec2 texture_coordinate;

void main() {
  outputColor = texture(rectangle_texture, texture_coordinate);
}
