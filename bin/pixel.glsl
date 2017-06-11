#version 130

varying vec3 color;
varying vec3 world_position;
varying vec3 normal;

out vec4 out_color;

uniform vec4 ambient_light   = vec4(0.3, 0.3, 0.3, 1.0);
uniform vec3 camera_position = vec3(0,0,0);

uniform vec3 point_light_pos   = vec3(5,0,0);
uniform vec4 point_light_color = vec4(1,0,0,1); // xyz is rgb, w is intensity
uniform vec3 point_light_attenuation = vec3(0,1,0);
uniform vec3 emissive_color = vec3(0,0,0);

void main(){
	vec4 total_light = ambient_light;

	vec3  light_dir  = (world_position - point_light_pos);
	float light_dist = length(light_dir);
	light_dir /= light_dist;
	float diffuse_factor = dot(normal, light_dir);


	float attenuation = point_light_attenuation.z
		              + point_light_attenuation.y * light_dist
		              + point_light_attenuation.x * light_dist * light_dist
		              + 0.00001; // don't div by 0
	if(diffuse_factor > 0){
		total_light += (vec4(point_light_color.xyz, 1.0) * point_light_color.w * diffuse_factor) / attenuation;
	}

	total_light.xyz += emissive_color;

	out_color = vec4(color,1) * total_light;
}
