////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Defines fragment shaders for use by the torus demo
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_EXAMPLES_TORUS_FRAGMENTSHADERS_CPP
#define XEN_EXAMPLES_TORUS_FRAGMENTSHADERS_CPP

#include <xen/sren/FragmentShader.hpp>

xen::Color4f FragmentShader_Normals(const xen::sren::FragmentUniforms& uniforms,
                                    Vec3r                              pos_world,
                                    Vec3r                              normal_world,
                                    xen::Color4f                       color){
	return xen::mkVec(((Vec3f)normal_world + (Vec3f{1,1,1}) / 2.0f), 1.0f);
}

xen::Color4f FragmentShader_Positions(const xen::sren::FragmentUniforms& uniforms,
                                      Vec3r                              pos_world,
                                      Vec3r                              normal_world,
                                      xen::Color4f                       color){
	return xen::mkVec(((Vec3f)pos_world), 1.0f);
}

xen::Color4f FragmentShader_Phong(const xen::sren::FragmentUniforms& uniforms,
                                  Vec3r                              pos_world,
                                  Vec3r                              normal_world,
                                  xen::Color4f                       color){
  xen::Color3f total_light = uniforms.ambient_light;
  total_light += (uniforms.emissive_color.rgb * uniforms.emissive_color.a);

  for(u32 i = 0; i < xen::size(uniforms.lights); ++i){
	  if(uniforms.lights[i].type != xen::LightSource3d::POINT){
		  printf("WARN: Unsupported light type in rasterizer\n");
		  continue;
	  }

	  real dist_sq_world = xen::distanceSq(pos_world, uniforms.lights[i].point.position);

	  total_light += xen::sren::computeLightInfluencePhong
		  ( uniforms.lights[i].point.position,
		    uniforms.lights[i].color,
		    uniforms.lights[i].attenuation,
		    dist_sq_world,
		    uniforms.camera.position,
		    pos_world, normal_world,
		    uniforms.specular_exponent,
		    uniforms.specular_intensity
		    );
  }

  for(u32 i = 0; i < 3; ++i){
	  if(total_light.elements[i] > 1){
		  total_light.elements[i] = 1;
	  }
  }

  xen::Color4f result = xen::Color4f::Origin;
  result.rgb = uniforms.diffuse_color.rgb * color.rgb * total_light;
  result.a   = 1;

  return result;
}

#endif
