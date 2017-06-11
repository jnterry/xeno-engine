#version 130

in vec3 pos;
in vec3 vert_color;
in vec3 vert_normal;

varying vec3 color;
varying vec3 normal;
varying vec3 world_position;

uniform mat4 mvpMatrix;

void main(){
	color = vec3(1,1,1);//vert_color;
	normal = vert_normal;
	gl_Position = mvpMatrix * vec4(pos,1);
	world_position = gl_Position.xyz;
}
