#version 430

layout(location = 0) in vec3 vert_pos;
layout(location = 1) in vec3 vert_normal;
layout(location = 2) in vec3 vert_color;

varying vec3 color;
varying vec3 normal;
varying vec3 world_position;
varying vec3 model_position;

uniform mat4 mvp_mat;
uniform mat4 model_mat;

void main(){
	color = vert_color;

	vec4 vert_normal_4 = vec4(vert_normal, 1) * model_mat;
	vert_normal_4 /= vert_normal_4.w;

	normal = normalize(vert_normal_4.xyz);

	gl_Position = vec4(vert_pos,1) * mvp_mat;

	model_position = vert_pos;//(vec4(vert_pos,1) * model_mat).xyz;
	world_position = (vec4(vert_pos,1) * model_mat).xyz;
}
