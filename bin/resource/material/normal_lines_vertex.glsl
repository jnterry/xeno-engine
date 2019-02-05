#version 430

layout (location = 0) in vec3 vert_pos;
layout (location = 1) in vec3 vert_normal;

out VS_OUT {
	vec3 position;
	vec3 normal;
} vs_out;

void main(){
	vs_out.position = vert_pos;
	vs_out.normal   = vert_normal;
}
