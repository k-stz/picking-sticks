#version 330

// simple fragment shader assigning the same outputColor on all fragments
out vec4 outputColor;

uniform vec4 color; // TODO try if _ -> - lispification take place

void main() {
  outputColor = color;
}
