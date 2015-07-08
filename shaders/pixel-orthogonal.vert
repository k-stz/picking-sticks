

#version 330

layout(location = 0) in vec4 position;

uniform int window_width;
uniform int window_height;

void main () {
  gl_Position = position;
}
