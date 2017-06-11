#version 130

varying vec3 color;
varying vec3 world_position;
varying vec3 normal;

out vec4 out_color;

uniform vec4 ambient_light   = vec4(0.3, 0.3, 0.3, 1.0);
uniform vec3 camera_position = vec3(0,0,0);

uniform vec3 point_light_pos   = vec3(5,0,0);
uniform vec4 point_light_color = vec4(1,0,0,1); // xyz is rgb, w is intensity

void main(){
	vec4 total_light = ambient_light;

	float diffuse_factor = dot(normal, (point_light_pos - world_position));

	out_color = vec4(color,1) * total_light;
}
