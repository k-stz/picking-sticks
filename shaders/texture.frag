
#version 330

smooth in vec4 interp_color;

out vec4 outputColor;

// The _GLSL sampler_ a special type in opengl, they represent a texture that is bound
// to the context
// every sampler type corresponds to a texture type:
// :texture-1d -> sampler1D etc.
// furthermore: isampler1D would represent a texture that returns integers
// while just "sampler1D" defaults to a float returning texture
uniform sampler1D test_texture;

void main() {
  // texture(test_texture, 0).r <- returns a float!
  // outputColor = sqrt(interp_color); // simple gamma correction
  // '.r' here refers to the texture component. Since this is a 1d-texture that's it
  // other component including r: RGBA

  // the 2nd parameter to texture() is the _texture coordinate_ since this is a 1d-texture/sampler1D
  // the texture coordinate also has only one dimension
  // the texture() function expects texture coordinates to be normalized (!) i.e. on the range [0,1]
  // maps to all the texels within the texture [0, texture-size]!!
  // Return value: a vec4, here vec4(<depends> 0 0 1) <- because we only set the RED component!!
  //  float i = texture(test_texture, 0.0).r;
  // _texture sampling_: "fetching data from a texture at a particular location is called sampling"!
  // hence texture() does the sampling here!!

  // texture() returns a vec4, and .r accesses the "red" component
  // the other components are intuitively g,b and a!!!

  // this is equivalent to the one below!! This is important to understand!
  //  outputColor = vec4(texture(test_texture, 0.5).r, 0.0, 0.0, 1.0); 
  outputColor = texture(test_texture, interp_color.x);
  //  outputColor = texture(test_texture, 0.5);

}
