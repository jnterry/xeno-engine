#version 130

in vec3 pos;
in vec3 vert_color;

varying vec3 color;
uniform mat4 mvpMatrix;

void main(){
  color = vert_color;
  gl_Position = mvpMatrix * vec4(pos,1);
}
