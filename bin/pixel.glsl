#version 130

varying vec3 color;
varying vec3 world_position;
varying vec3 normal;

out vec4 out_color;

uniform vec3 ambient_light   = vec3(0.01, 0.01, 0.01);
uniform vec3 camera_position = vec3(0,0,0);

uniform vec3 point_light_pos         = vec3(0,0,0);
uniform vec4 point_light_color       = vec4(1,0,0,1); // xyz is rgb, w is intensity
uniform vec3 point_light_attenuation = vec3(0.0,0.3,0);
uniform vec4 emissive_color = vec4(0,0,0,0);

vec3 calcLight(vec4 light_color, vec3 dir){
	vec3 result = vec3(0,0,0);

	float diffuse_factor = dot(normal, dir);
	if(diffuse_factor > 0){
		result += light_color.xyz * diffuse_factor;

		//closer these are the more specular you get as eye is closer to the reflected beam
		vec3 dir_to_cam = normalize(camera_position - world_position);
		vec3 reflection_dir = normalize(reflect(dir, normal)); // expected specular reflection dir
		float specular_factor = dot(dir_to_cam,reflection_dir); //cosine of the angle between directions, 1 when angle is 0, gets less as angle increases
		specular_factor = pow(specular_factor, 2); //raise to specified power
		if(specular_factor > 0){
			result += light_color.xyz * specular_factor;
		}
	}

	return result * light_color.w;
}

vec3 calcPointLight(vec4 light_col, vec3 light_pos, vec3 light_attenuation){
	vec3  light_dir  = (world_position - light_pos);
	float light_dist = length(light_dir);
	light_dir /= light_dist;

	float attenuation = light_attenuation.z
		              + light_attenuation.y * light_dist
		              + light_attenuation.x * light_dist * light_dist
		              + 0.00001; // don't div by 0

	return calcLight(light_col, light_dir) / attenuation;
}

void main(){
	vec3 total_light = ambient_light;
	total_light += calcPointLight(point_light_color, point_light_pos, point_light_attenuation);
	total_light += calcLight(vec4(1,1,1,0.3), normalize(vec3(0,-1,-1)));
	total_light += emissive_color.xyz * emissive_color.w;

	vec3 normal_col = (normalize(normal) + vec3(1,1,1)) / 2.0;

	//out_color = vec4(normal_col,1);
	out_color = vec4(color * total_light, 1);
	//out_color = vec4(1,0,0,1);
}
