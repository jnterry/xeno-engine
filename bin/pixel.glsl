#version 330

out vec4 out_color;
varying vec3 color;

void main(){
  out_color = vec4(color,1);
}
