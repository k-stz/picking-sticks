// simple passthrough shader; no transformation take place

#version 330

layout(location = 0) in vec4 position;

void main () {
  gl_Position = position + vec4(-1.0, -1.0, 0.0, 0.0);
}
