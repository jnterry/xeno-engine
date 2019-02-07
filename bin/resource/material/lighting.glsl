#version 430

/// \brief Computing a direction lights influence on some point
/// \param dir_to_cam     Direction from surface to camera
/// \param surface_normal The fragment's surface normal in world space
/// \param light_color    The color and intensity of the light
/// \param light_dir      The direction in which the light rays are travelling
///                       (IE: from light source to fragment being illuminated)

vec3 calcDirectionLight(vec3 camera_position,
                        vec3 surface_position,
                        vec3 surface_normal,
                        vec4 light_color,
                        vec3 light_dir){
	vec3 result = vec3(0,0,0);

	float diffuse_factor = dot(surface_normal, light_dir);
	if(diffuse_factor > 0.0f){
		result += light_color.xyz * diffuse_factor;

		// The closer dir_to_cam and reflection_dir are the greater the specular
		// component as eye is closer to the reflected beam

		vec3 dir_to_cam     = normalize(camera_position - surface_position);
		vec3 reflection_dir = normalize(reflect(light_dir, surface_normal));

		// cosine of the angle between direction vectors,
		// 1 when angle is 0, decreases as angle between directions increases
		float specular_factor = dot(dir_to_cam,reflection_dir);

		//raise to specified power (this is specular_exponent, IE: how shiny material is)
		if(specular_factor > 0){
			specular_factor = pow(specular_factor, 2);
			result += light_color.xyz * specular_factor;
		}
	}

	return result * light_color.w;
}

/// \brief Computes the overall lighting impact of a point light
///
/// \param surface_position The location of the fragment being illuminated
/// in world space
/// \param surface_normal The fragment's surface normal in world space
/// \param light_pos -> Position of the point light
/// \param light_col -> Color and intensity of the point light
/// \param light_attenuation -> Parameters describing the lights falloff over distance
vec3 calcPointLight(vec3 camera_position,
                    vec3 surface_position,
                    vec3 surface_normal,
                    vec3 light_pos,
                    vec4 light_col,
                    vec3 light_attenuation){
	vec3  light_dir  = (surface_position - light_pos);
	float light_dist = length(light_dir);
	light_dir /= light_dist;

	float attenuation = light_attenuation.z
		              + light_attenuation.y * light_dist
		              + light_attenuation.x * light_dist * light_dist
		              + 0.00001; // don't div by 0

	return calcDirectionLight(camera_position,
	                          surface_position, surface_normal,
	                          light_col, light_dir
	                         ) / attenuation;
}
