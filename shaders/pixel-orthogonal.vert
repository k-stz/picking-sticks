#version 330

layout(location = 0) in vec4 position;

uniform int window_width;
uniform int window_height;

void main () {

  // aspect ratio scale
  vec4 window_scaled = vec4(2 * position.x / window_width,
		     2 * position.y / window_height,
		     position.z,
		     position.w);
  // make bottom left the new origin
    gl_Position =  window_scaled - vec4(1.0, 1.0, 0.0, 0.0);
}
