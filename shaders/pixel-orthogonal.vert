#version 330

layout(location = 0) in vec4 position;
layout(location = 1) in vec2 texCoord;

uniform int window_width;
uniform int window_height;

out vec2 colorCoord;

// remove once colorCoord works:
out vec2 texture_coordinate;

void main () {

  // aspect ratio scale
  vec4 window_scaled = vec4(2 * position.x / window_width,
			    2 * position.y / window_height,
			    position.z,
			    position.w);
  // make bottom left the new origin
  gl_Position =  window_scaled - vec4(1.0, 1.0, 0.0, 0.0);

  // texture coordinate:
  vec2 offset = vec2(0.0,0.0);

  int x = int(mod(gl_VertexID, 6));
  switch(x)
    {
    case 0:
      offset = vec2(1.0,0.0);
      break;
    case 1:
      offset = vec2(1.0,1.0);
      break;
    case 2:
      offset = vec2(0.0,1.0);
      break;
    case 3:
      offset = vec2(0.0,1.0);
      break;
    case 4:
      offset = vec2(0.0,0.0);
      break;
    case 5:
      offset = vec2(1.0,0.0);
      break;
    }
  texture_coordinate = offset;

  // remove the above once this works:
  colorCoord = texCoord;
}
