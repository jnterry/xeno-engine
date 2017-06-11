#version 130

in vec3 vert_pos;
in vec3 vert_color;
in vec3 vert_normal;

varying vec3 color;
varying vec3 normal;
varying vec3 world_position;

uniform mat4 mvp_mat;
uniform mat4 model_mat;

void main(){
	color = vec3(1,1,1);//vert_color;
	normal = vert_normal;
	gl_Position = mvp_mat * vec4(vert_pos,1);
	world_position = (model_mat * vec4(vert_pos,1)).xyz;
}
