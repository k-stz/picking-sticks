#version 330

in vec4 interp_color;

out vec4 outputColor;

uniform sampler1D test_texture;

void main() {
  // texture(test_texture, 0).r <- returns a float!
  // outputColor = sqrt(interp_color); // simple gamma correction
  outputColor = sqrt(vec4(texture(test_texture, 0.5).r)); // simple gamma correction
}
