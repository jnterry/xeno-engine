////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains defintions of FragmentShader related functions
///
/// \ingroup sren
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_SREN_FRAGMENTSHADER_CPP
#define XEN_SREN_FRAGMENTSHADER_CPP

#include <xen/sren/FragmentShader.hpp>
#include <xen/core/array.hpp>
#include <xen/graphics/RenderCommand3d.hpp>

/////////////////////////////////////////////////////////////////////
/// \brief Performs fragment shader computations - IE: lighting
/// \param uniforms     Uniform variables being passed to the fragment shader
/// \param pos_world    The position of the point being filled in world space
/// \param normal_world The normal of the point being filled in world space
/// \param color        The diffuse color of the point being filled
/////////////////////////////////////////////////////////////////////
xen::Color4f _defaultFragmentShader(const xsr::FragmentUniforms& uniforms,
                                    Vec3r                              pos_world,
                                    Vec3r                              normal_world,
                                    xen::Color4f                       color,
                                    Vec2f                              uvs){

	xen::Color3f total_light = uniforms.ambient_light;
	total_light += (uniforms.emissive_color.rgb * uniforms.emissive_color.a);

	for(u32 i = 0; i < xen::size(uniforms.lights); ++i){
		if(uniforms.lights[i].type != xen::LightSource3d::POINT){
			printf("WARN: Unsupported light type in rasterizer\n");
			continue;
		}

		real dist_sq_world = xen::distanceSq(pos_world, uniforms.lights[i].point.position);

		total_light += xsr::computeLightInfluenceSimple
			( uniforms.lights[i].color,
			  uniforms.lights[i].attenuation,
			  dist_sq_world
			  );
	}

	for(u32 i = 0; i < 3; ++i){
		if(total_light.elements[i] > 1){
			total_light.elements[i] = 1;
		}
	}

	xen::Color4f result = uniforms.diffuse_color;
	result     *= color;
	result     *= xsr::sampleTexture(uniforms.textures[0], uvs);
	result.rgb *= total_light;

	return result;
}

xen::Color4f _fragmentShaderAllWhite(const xsr::FragmentUniforms& uniforms,
                                     Vec3r                              pos_world,
                                     Vec3r                              normal_world,
                                     xen::Color4f                       color,
                                     Vec2f                              uvs){
	return xen::Color::WHITE4f;
}

xen::Color4f _fragmentShaderDiffuseColor(const xsr::FragmentUniforms& uniforms,
                                     Vec3r                              pos_world,
                                     Vec3r                              normal_world,
                                     xen::Color4f                       color,
                                     Vec2f                              uvs){
	return uniforms.diffuse_color;
}

xsr::FragmentShader xsr::FragmentShader_Default      = &_defaultFragmentShader;
xsr::FragmentShader xsr::FragmentShader_AllWhite     = &_fragmentShaderAllWhite;
xsr::FragmentShader xsr::FragmentShader_DiffuseColor = &_fragmentShaderDiffuseColor;

xen::Color3f xsr::computeLightInfluencePhong(Vec3r        light_pos,
                                               xen::Color4f light_color,
                                               Vec3f        attenuation_coefficents,
                                               real         distance_sq,
                                               Vec3r        eye_pos,
                                               Vec3r        pos_world,
                                               Vec3r        normal_world,
                                               real         specular_exponent,
                                               real         specular_intensity){

	normal_world *= -1_r; // :TODO: why?

	light_color.rgb *= light_color.a;

	// Direction in which photons are travelling from the light
	// source in order to hit this point
	Vec3r light_dir = xen::normalized(pos_world - light_pos);

	float diffuse_factor = xen::dot(normal_world, light_dir);
	if(diffuse_factor < 0){ return Vec3f::Origin; }

	Vec3f diffuse_color = light_color.rgb * diffuse_factor;

	// Compare the reflection direction to the surface_to_eye direction. If
	// they are equal we get mirror like bright specular reflection. The
	// greater the angle the less the reflection
	Vec3r surface_to_eye = xen::normalized(eye_pos - pos_world);
	Vec3r reflection_dir = xen::reflect(light_dir, normal_world);

	float specular_factor = xen::dot(surface_to_eye, reflection_dir);

	Vec3f specular_color = Vec3f::Origin;
	if (specular_factor > 0) {
		specular_factor = pow(specular_factor, specular_exponent);
		specular_color = light_color.rgb * specular_factor * specular_intensity;
	}

	return diffuse_color + specular_color;
} // end of computeLightInfulencePhong

xen::Color3f xsr::computeLightInfluenceSimple(xen::Color4f light_color,
                                                Vec3f        attenuation_coefficents,
                                                real         distance_sq){
	float attenuation = (attenuation_coefficents.x * 1.0 +
	                     attenuation_coefficents.y * xen::sqrt(distance_sq) +
	                     attenuation_coefficents.z * distance_sq
	                     );

	return (light_color / attenuation).rgb * light_color.w;
} // end of computeLightInfluenceSimple

void xsr::setPerCommandFragmentUniforms(xsr::FragmentUniforms& uniforms,
                                          const xen::MaterialParams&   material,
                                          const Mat4r&      m_mat,
                                          const Mat4r&      vp_mat){
	uniforms.m_matrix           = m_mat;
	uniforms.vp_matrix          = vp_mat;
	uniforms.diffuse_color      = material.color;
	uniforms.emissive_color     = material.emissive_color;
	uniforms.specular_exponent  = material.specular_exponent;
	uniforms.specular_intensity = material.specular_intensity;
}

#endif
