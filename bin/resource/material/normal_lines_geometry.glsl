#version 330 core

layout (triangles) in;
layout (line_strip, max_vertices = 6) out;

in VS_OUT {
    vec3 position;
    vec3 normal;
} gs_in[];

uniform mat4 mvp_mat;
uniform mat4 model_mat;

void emitLine(vec4 start, vec4 dir, float len){
	gl_Position = start;
	EmitVertex();
	gl_Position = start + (dir * len);
	EmitVertex();
	EndPrimitive();
}

const float MAGNITUDE = 0.1;

void main(){

	for(int i = 0; i < gs_in.length(); ++i){
		vec3 p3 = gs_in[i].position.xyz;
		vec3 n3 = gs_in[i].normal.xyz;

		vec4 p4 = vec4(p3, 1) * mvp_mat;
		//vec4 n4 = normalize(vec4(n3, 1) * mvp_mat);
		vec4 n4 = vec4(n3, 1);

		emitLine(p4, n4, MAGNITUDE);
	}
}
