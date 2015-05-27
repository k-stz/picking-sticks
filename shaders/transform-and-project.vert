// perform a single transformation and set up for perspective divide

#version 330

layout(location = 0) in vec4 position;

uniform mat4 model_to_clip;
uniform mat4 perspective_matrix;

void main () {
  gl_Position = perspective_matrix * model_to_clip * position;
}
