#version 330

// simple fragment shader assigning the same outputColor on all fragments
out vec4 outputColor;

void main() {
  outputColor = vec4(0.0f, 1.0f, 1.0f, 1.0f);
}
