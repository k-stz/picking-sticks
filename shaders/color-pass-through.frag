#version 330

out vec4 outputColor;

uniform sampler2D test_texture;

void main() {
  outputColor = vec4(0.0f, 1.0f, 1.0f, 1.0f);
}
