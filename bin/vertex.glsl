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
	color = vert_color;
	normal = normalize(model_mat * vec4(vert_normal,1)).xyz;
	gl_Position = mvp_mat * vec4(vert_pos,1);
	world_position = (model_mat * vec4(vert_pos,1)).xyz;
}
