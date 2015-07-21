#version 330

out vec4 outputColor;

uniform sampler2D rectangle_texture;

in vec2 colorCoord;

//in vec2 texture_coordinate;

void main() {
  if (texture(rectangle_texture, colorCoord).a == 0.0)
  discard;
  
  outputColor = texture(rectangle_texture, colorCoord);
}
