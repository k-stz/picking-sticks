#version 330

layout(location = 0) in vec4 position;

uniform int window_width;
uniform int window_height;

void main () {
  // TODO: pixel->coordinate space scaling
  vec4 to_origin = vec4(position.x / 200.0,
		      position.y / 200.0,
		      position.z / 200.0,
		      1);
  gl_Position = to_origin  + vec4(-1.0, -1.0, 0.0, 1.0);

  //  gl_Position = vec4(0.0, 0.0, 0.0, 0.0);
}
