#version 430

varying vec3 color;
varying vec3 world_position;
varying vec3 normal;
varying vec3 model_position;

out vec4 out_color;

uniform vec3 ambient_light   = vec3(0.1, 0.1, 0.1);
uniform vec3 camera_position = vec3(0,0,0);

uniform vec3 point_light_pos         = vec3(0,0,0);
uniform vec4 point_light_color       = vec4(1,0,0,1); // xyz is rgb, w is intensity
uniform vec3 point_light_attenuation = vec3(0.0,0.3,0);
uniform vec4 emissive_color = vec4(0,0,0,0);

uniform vec4      diffuse_color = vec4(1,1,1,1);
uniform sampler2D diffuse_map;


vec3 calcPointLight(vec3 camera_position,
                    vec3 surface_position,
                    vec3 surface_normal,
                    vec3 light_pos,
                    vec4 light_col,
                    vec3 light_attenuation);
vec3 calcDirectionLight(vec3 camera_position,
                        vec3 surface_position,
                        vec3 surface_normal,
                        vec4 light_color,
                        vec3 light_dir);

void main(){
	vec3 total_light = ambient_light;
	total_light += calcPointLight(camera_position,
	                              world_position, normal,
	                              point_light_pos, point_light_color,
	                              point_light_attenuation);
	total_light += calcDirectionLight(camera_position,
	                                  world_position, normal,
	                                  vec4(1,1,1,0.1),
	                                  normalize(vec3(0,-1,-1)));
	total_light += emissive_color.xyz * emissive_color.w;

	vec3 normal_col = (normal + vec3(1,1,1)) / 2.0;


	vec3 pixel_diffuse_color = color.rgb * diffuse_color.rgb * texture(diffuse_map, model_position.xz).rgb;

	//out_color = vec4(normal_col,1);
	out_color = vec4(pixel_diffuse_color * total_light, 1);
	//out_color = vec4(pixel_diffuse_color,1);
	//out_color = vec4(1,0,0,1);
}
