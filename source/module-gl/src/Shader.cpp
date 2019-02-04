////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \brief Contains implementation of Shader for opengl
///
/// \ingroup gl
////////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GL_SHADER_GL_CPP
#define XEN_GL_SHADER_GL_CPP

#include <xen/core/memory/ArenaLinear.hpp>
#include <xen/core/memory/ArenaPool.hpp>
#include <xen/math/vector_types.hpp>
#include <xen/math/matrix_types.hpp>
#include <xen/core/File.hpp>
#include <xen/kernel/log.hpp>

#include "ModuleGl.hxx"
#include "Shader.hxx"
#include "gl_header.hxx"

namespace {
	GLuint compileShader(GLenum stage, const char* source){
		// create shader
		GLuint result = XEN_CHECK_GL_RETURN(glCreateShader(stage));

		// load source and compile
		XEN_CHECK_GL(glShaderSource(result, 1, &source, NULL));
		XEN_CHECK_GL(glCompileShader(result));

		return result;
	}

	xgl::ShaderProgram* initShaderProgram(xgl::ShaderProgram* result,
	                                      const char* vertex_source,
	                                      const char* pixel_source
	                                     ){
		result->vertex_shader = compileShader(GL_VERTEX_SHADER,   vertex_source);
		result->pixel_shader  = compileShader(GL_FRAGMENT_SHADER, pixel_source);

		result->program = XEN_CHECK_GL_RETURN(glCreateProgram());

		XEN_CHECK_GL(glAttachShader(result->program, result->vertex_shader  ));
		XEN_CHECK_GL(glAttachShader(result->program, result->pixel_shader));

		XEN_CHECK_GL(glLinkProgram(result->program));

		XEN_CHECK_GL(glDetachShader(result->program, result->vertex_shader  ));
		XEN_CHECK_GL(glDetachShader(result->program, result->pixel_shader));

		////////////////////////////////////////////////////
		// Query Shader Interface
		// :TODO: We should probably store the vertex layout as well as the uniform
		// data, but that requires supporting arbitrary vertex layouts in the public
		// api...
		GLint attrib_count;

		char tmp[256];

		XEN_CHECK_GL(glGetProgramiv(result->program, GL_ACTIVE_ATTRIBUTES, &attrib_count));
		XenLogDone("Created shader program");
		XenLogInfo("- Vertex spec has %i attributes", attrib_count);
		for(int i = 0; i < attrib_count; ++i){

			GLint  attrib_size;
			GLenum attrib_type;

			XEN_CHECK_GL(glGetActiveAttrib(result->program, i,
			                               XenArrayLength(tmp), NULL,
			                               &attrib_size, &attrib_type,
			                               tmp));

			GLint attrib_location = XEN_CHECK_GL_RETURN(glGetAttribLocation(result->program, tmp));

			XenLogInfo("  - %2i: %24s, size: %i, type: %6i, location: %2i",
			           i, tmp, attrib_size, attrib_type, attrib_location);
		}
		////////////////////////////////////////////////////

		XEN_CHECK_GL(glGetProgramiv(result->program, GL_ACTIVE_UNIFORMS, &result->uniform_count));

		GLchar tmp_name_buffer[512];

		xen::MemoryTransaction transaction(xgl::gl_state->primary_arena);

		//////////////////////////////////////////////////////
		// Allocate dynamic sized memory for the ShaderProgram
		u64 data_block_size = result->uniform_count * (
			sizeof(xen::StringHash) + sizeof(xen::MetaType*) + sizeof(GLint) + sizeof(GLenum)
		);
		void* data_block = xen::reserveBytes(xgl::gl_state->primary_arena, data_block_size);

		result->uniform_locations = (GLint*)data_block;
		xen::ptrAdvance(&data_block, sizeof(GLint) * result->uniform_count);
		result->uniform_types = (const xen::MetaType**)data_block;
		xen::ptrAdvance(&data_block, sizeof(xen::MetaType*) * result->uniform_count);
		result->uniform_gl_types = (GLenum*)data_block;
		xen::ptrAdvance(&data_block, sizeof(GLenum) * result->uniform_count);
		result->uniform_name_hashes = (xen::StringHash*)data_block;
		//////////////////////////////////////////////////////


		//////////////////////////////////////////////////////
		// Retrieve information regarding program uniforms
		XenLogInfo("- Program has %i uniforms", result->uniform_count);
		for(GLint i = 0; i < result->uniform_count; ++i){
			GLint   array_length;
			GLint   name_bytes_written;
			GLenum  type;

			XEN_CHECK_GL(
				glGetActiveUniform(result->program, i,
				                   XenArrayLength(tmp_name_buffer), &name_bytes_written,
				                   &array_length, &type, tmp_name_buffer)
			);

			result->uniform_name_hashes[i] = xen::hash(tmp_name_buffer);
			result->uniform_gl_types[i]    = type;
			result->uniform_locations[i]   = XEN_CHECK_GL_RETURN(
				glGetUniformLocation(result->program, tmp_name_buffer)
			);

			switch(type){
				// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
				// This switch must have same cases as that in ~applyMaterial()~
				// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
			case GL_FLOAT:
				XenLogInfo("  - %2i : float       : %s", i, tmp_name_buffer);
				result->uniform_types[i] = &xen::meta_type<float>::type;
				break;
			case GL_FLOAT_VEC2:
				XenLogInfo("  - %2i : Vec2f       : %s", i, tmp_name_buffer);
				result->uniform_types[i] = &xen::meta_type<Vec2f>::type;
				break;
			case GL_FLOAT_VEC3:
				XenLogInfo("  - %2i : Vec3f       : %s", i, tmp_name_buffer);
				result->uniform_types[i] = &xen::meta_type<Vec3f>::type;
				break;
			case GL_FLOAT_VEC4:
				XenLogInfo("  - %2i : Vec4f       : %s", i, tmp_name_buffer);
				result->uniform_types[i] = &xen::meta_type<Vec4f>::type;
				break;
			case GL_DOUBLE:
				XenLogInfo("  - %2i : double      : %s", i, tmp_name_buffer);
				result->uniform_types[i] = &xen::meta_type<double>::type;
				break;
			case GL_DOUBLE_VEC2:
				XenLogInfo("  - %2i : Vec2d       : %s", i, tmp_name_buffer);
				result->uniform_types[i] = &xen::meta_type<Vec2d>::type;
				break;
			case GL_DOUBLE_VEC3:
				XenLogInfo("  - %2i : Vec3d       : %s", i, tmp_name_buffer);
				result->uniform_types[i] = &xen::meta_type<Vec3d>::type;
				break;
			case GL_DOUBLE_VEC4:
				XenLogInfo("  - %2i : Vec4d       : %s", i, tmp_name_buffer);
				result->uniform_types[i] = &xen::meta_type<Vec4d>::type;
				break;
			case GL_INT:
				XenLogInfo("  - %2i : double      : %s", i, tmp_name_buffer);
				result->uniform_types[i] = &xen::meta_type<int>::type;
				break;
			case GL_INT_VEC2:
				XenLogInfo("  - %2i : Vec2s       : %s", i, tmp_name_buffer);
				result->uniform_types[i] = &xen::meta_type<Vec2s>::type;
				break;
			case GL_INT_VEC3:
				XenLogInfo("  - %2i : Vec3s       : %s", i, tmp_name_buffer);
				result->uniform_types[i] = &xen::meta_type<Vec3s>::type;
				break;
			case GL_INT_VEC4:
				XenLogInfo("  - %2i : Vec4s       : %s", i, tmp_name_buffer);
				result->uniform_types[i] = &xen::meta_type<Vec4s>::type;
				break;
			case GL_BOOL:
				XenLogInfo("  - %2i : bool        : %s", i, tmp_name_buffer);
				result->uniform_types[i] = &xen::meta_type<bool>::type;
				break;
			case GL_FLOAT_MAT4:
				XenLogInfo("  - %2i : Mat4<float> : %s", i, tmp_name_buffer);
				result->uniform_types[i] = &xen::meta_type<xen::Matrix<4,4,float>>::type;
				break;
			case GL_DOUBLE_MAT4:
				XenLogInfo("  - %2i : Mat4<double> %s", i, tmp_name_buffer);
				result->uniform_types[i] = &xen::meta_type<xen::Matrix<4,4,double>>::type;
				break;
			case GL_SAMPLER_2D:
				XenLogInfo("  - %2i : Sampler2d %s", i, tmp_name_buffer);
				result->uniform_types[i] = &xen::meta_type<xen::Texture>::type;
				break;
			default:
				XenLogError("ShaderProgram contained uniform '%s' which has an unsupported type: %i",
				            tmp_name_buffer, type);
					XEN_CHECK_GL(glDeleteShader (result->vertex_shader));
					XEN_CHECK_GL(glDeleteShader (result->pixel_shader ));
					XEN_CHECK_GL(glDeleteProgram(result->program      ));
				return nullptr;
			}
		}


		transaction.commit();
		return result;
	}
}

bool xgl::isOkay(xgl::ShaderProgram* shader){
	GLint status_vert, status_pixel, status_prog;

	XEN_CHECK_GL(glGetShaderiv (shader->vertex_shader, GL_COMPILE_STATUS, &status_vert ));
	XEN_CHECK_GL(glGetShaderiv (shader->pixel_shader,  GL_COMPILE_STATUS, &status_pixel));
	XEN_CHECK_GL(glGetProgramiv(shader->program,       GL_LINK_STATUS,    &status_prog ));

	return status_vert == GL_TRUE && status_pixel == GL_TRUE && status_prog == GL_TRUE;
}

char* xgl::getErrors(xgl::ShaderProgram* shader, xen::ArenaLinear& arena){
	if(isOkay(shader)){
		return pushString(arena, "No errors");
	}

	char* result = (char*)arena.next_byte;
	GLint bytes_written;

	xen::pushStringNoTerminate(arena, "Vertex Shader:\n");
	XEN_CHECK_GL(glGetShaderInfoLog(shader->vertex_shader, xen::getBytesRemaining(arena),
			                                &bytes_written, (GLchar*)arena.next_byte));
	xen::ptrAdvance(&arena.next_byte, bytes_written);

	xen::pushStringNoTerminate(arena, "Pixel Shader:\n");
	XEN_CHECK_GL(glGetShaderInfoLog(shader->pixel_shader, xen::getBytesRemaining(arena),
	                                &bytes_written, (GLchar*)arena.next_byte));
	xen::ptrAdvance(&arena.next_byte, bytes_written);

	xen::pushStringNoTerminate(arena, "Program:\n");
	XEN_CHECK_GL(glGetProgramInfoLog(shader->program, xen::getBytesRemaining(arena),
	                                 &bytes_written, (GLchar*)arena.next_byte));
	xen::ptrAdvance(&arena.next_byte, bytes_written);

	return result;
}

void xgl::useShader(xgl::ShaderProgram* shader){
	if(shader == nullptr){
		glUseProgram(0);
	} else {
		glUseProgram(shader->program);
	}
}

xen::Shader xgl::createShader(const xen::ShaderSource& source){
	if(source.glsl_vertex_path == nullptr ||
	   source.glsl_fragment_path == nullptr){
		// return handle to the default shader
		return xen::makeGraphicsHandle<xen::Shader::HANDLE_ID>(0, 0);
	}

	u32 slot = xen::reserveSlot(xgl::gl_state->pool_shader);
	xen::Shader result = xen::makeGraphicsHandle<xen::Shader::HANDLE_ID>(slot, 0);

	XenTempArena(scratch, 8196);

	xen::FileData vertex_src = loadFileAndNullTerminate(scratch, source.glsl_vertex_path);
	xen::FileData pixel_src  = loadFileAndNullTerminate(scratch, source.glsl_fragment_path);

	auto sprog = initShaderProgram(xgl::getShaderImpl(result),
	                               (char*)&vertex_src[0],
	                               (char*)&pixel_src[0]
	                              );

	if(!xgl::isOkay(sprog)){
		resetArena(scratch);
		const char* errors = xgl::getErrors(sprog, scratch);
		XenLogError("Shader Errors:\n%s", errors);
		XenBreak();
	} else {
		XenLogDone("Shader compiled successfully");
	}

	return result;
}

void xgl::destroyShader(xen::Shader shader){
	if(shader._id == 0){
		// never kill the default shader
		return;
	}

	xgl::ShaderProgram* sprog = xgl::getShaderImpl(shader);

	XEN_CHECK_GL(glDeleteShader (sprog->vertex_shader));
	XEN_CHECK_GL(glDeleteShader (sprog->pixel_shader ));
	XEN_CHECK_GL(glDeleteProgram(sprog->program      ));

	xen::freeSlot(xgl::gl_state->pool_shader, shader._id);
}

xgl::ShaderProgram* xgl::getShaderImpl(xen::Shader shader){
	return &xgl::gl_state->pool_shader.slots[shader._id].item;
}

const xen::Material* xgl::createMaterial(const xen::ShaderSource& source,
                                         const xen::MaterialParameterSource* params,
                                         u64 param_count){

	////////////////////////////////////////////////////////////////////
	// Create Shader and Material instance
	//
	// :TODO: we should reuse existing shaders created from the same source
	// wherever possible...
	xen::Shader shader = xgl::createShader(source);
	xgl::ShaderProgram* sprog = xgl::getShaderImpl(shader);

	xgl::Material* result = xen::reserveType<xgl::Material>(xgl::gl_state->pool_material);
	result->program = sprog;

	result->uniform_sources = xen::reserveTypeArray<xen::MaterialParameterSource::Kind>(
		xgl::gl_state->primary_arena, sprog->uniform_count
	);
	result->uniform_param_offsets = xen::reserveTypeArray<u32>(
		xgl::gl_state->primary_arena, sprog->uniform_count
	);
	////////////////////////////////////////////////////////////////////


	////////////////////////////////////////////////////////////////////
	// Warn on ignored parameters
	for(u64 i = 0; i < param_count; ++i){
		xen::StringHash hash = xen::hash(params[i].name);
		bool found = false;
		for(int j = 0; j < sprog->uniform_count && !found; ++j){
			found |= sprog->uniform_name_hashes[j] == hash;
		}
		if(!found){
			XenLogWarn("Ignoring material parameter source '%s' since no corresponding variable found in shader",
			           params[i].name);
		}
	}
	////////////////////////////////////////////////////////////////////

	////////////////////////////////////////////////////////////////////
	// Build array in material instance that maps from uniform index to
	// MaterialParameterSource
	u16 variable_count = 0;
	for(GLint i = 0; i < sprog->uniform_count; ++i){
		result->uniform_sources[i] = xen::MaterialParameterSource::Variable;

		for(u64 j = 0; j < param_count; ++j){
			if(sprog->uniform_name_hashes[i] == xen::hash(params[j].name)){
				XenAssert(result->uniform_sources[i] == xen::MaterialParameterSource::Variable,
				          "Program contains multiple variables with same hash!"
				);
				result->uniform_sources[i] = params[j].kind;
			}
		}

		if(result->uniform_sources[i] == xen::MaterialParameterSource::Variable) {
			++variable_count;
		}
	}
	////////////////////////////////////////////////////////////////////


	////////////////////////////////////////////////////////////////////
	// Build the meta type describing any free parameters
	unsigned meta_type_size = (sizeof(xen::MetaType) +
	                           sizeof(xen::MetaTypeField) * variable_count);
	result->parameters = (xen::MetaType*)(
		xen::reserveBytes(xgl::gl_state->primary_arena, meta_type_size)
	);
	xen::clearToZero(result->parameters, meta_type_size);

	result->parameters->name = "GlShaderParameterPack";
	result->parameters->field_count = variable_count;

	int param_index = 0;
	for(GLint i = 0; i < sprog->uniform_count; ++i){
		if(result->uniform_sources[i] != xen::MaterialParameterSource::Variable){
			continue;
		}

		xen::MetaTypeField* field = &result->parameters->fields[param_index++];

		field->type.base = sprog->uniform_types[i];
		field->offset    = result->parameters->size;
		field->name_hash = sprog->uniform_name_hashes[i];

		result->uniform_param_offsets[i] = field->offset;

		result->parameters->size += sprog->uniform_types[i]->size;

		int align = result->parameters->size % alignof(int);
		if(align != 0){
			result->parameters->size += alignof(int) - align;
		}
	}
	////////////////////////////////////////////////////////////////////

	return result;
}
void xgl::destroyMaterial(const xen::Material*){
}

// Used as dummy uniform source by applyMaterial
u64 all_zeros[16] = { 0 };

const void* getUniformDataSource(const xen::RenderCommand3d&    cmd,
                                 const xen::RenderParameters3d& params,
                                 const xen::Aabb2u& viewport,
                                 int uniform_index){

	// :TODO: rather than doing all the calculations here we should instead
	// delay rendering until we have all commands, and then precompute all of
	// these once, then do rendering step
	// This would remove all of the pushing to thread scratch

	const xgl::Material* material = (const xgl::Material*)cmd.material;

	switch(material->uniform_sources[uniform_index]){
	case xen::MaterialParameterSource::Variable: {
		u08* data = (u08*)cmd.material_params;
		if(data == nullptr){ return &all_zeros; };
		return &data[material->uniform_param_offsets[uniform_index]];
	}
	case xen::MaterialParameterSource::ModelMatrix:
		return &cmd.model_matrix;
	case xen::MaterialParameterSource::ViewMatrix: {
	  Mat4r* vmat = xen::reserveType<Mat4r>(xen::getThreadScratchSpace());
		// opengl has (0,0) at bottom left, we expect it to be at top left so flip y
		*vmat = (getViewMatrix(params.camera) * xen::Scale3d(1, -1, 1));
		return vmat;
	}
	case xen::MaterialParameterSource::ProjectionMatrix: {
		Mat4r* pmat = xen::reserveType<Mat4r>(xen::getThreadScratchSpace());
		*pmat = (getProjectionMatrix(params.camera,
		                             (Vec2r)(viewport.max - viewport.min)));
		return pmat;
	}
	case xen::MaterialParameterSource::MvpMatrix: {
		Mat4r* mvp = xen::reserveType<Mat4r>(xen::getThreadScratchSpace());
		*mvp = (
			cmd.model_matrix *
			getViewMatrix(params.camera) *
			xen::Scale3d(1, -1, 1) *
			getProjectionMatrix(params.camera, (Vec2r)(viewport.max - viewport.min))
		);
		return mvp;
	}
	case xen::MaterialParameterSource::CameraWorldPosition:
		return &params.camera.position;
	case xen::MaterialParameterSource::CameraLookDirection:
	  return &params.camera.look_dir;
	case xen::MaterialParameterSource::CameraUpDir:
		return &params.camera.up_dir;
	case xen::MaterialParameterSource::AmbientLightColor:
		return &params.ambient_light;
	case xen::MaterialParameterSource::PointLightPosition:
		if(&params.lights.size == 0){ return &all_zeros; }
		return &params.lights[0].point.position;
	case xen::MaterialParameterSource::PointLightColor:
		return &params.lights[0].color;
	case xen::MaterialParameterSource::PointLightAttenuation:
	  return &params.lights[0].attenuation;
	case xen::MaterialParameterSource::TextureChannel0: {
		static const int ZERO = 0; return &ZERO;
	}
	case xen::MaterialParameterSource::TextureChannel1: {
		static const int ONE = 1; return &ONE;
	}
	case xen::MaterialParameterSource::TextureChannel2: {
		static const int TWO = 2; return &TWO;
	}
	case xen::MaterialParameterSource::TextureChannel3: {
		static const int THREE = 3; return &THREE;
	}
	}
	XenInvalidCodePath("Should hit a switch case!");
	return nullptr;
}

void xgl::applyMaterial(const xen::RenderCommand3d& cmd,
                        const xen::RenderParameters3d& params,
                        const xen::Aabb2u& viewport){
	const xgl::Material* mat  = (xgl::Material*)cmd.material;
	xgl::ShaderProgram* sprog = mat->program;

	XEN_CHECK_GL(glUseProgram(sprog->program));

	for(GLint i = 0; i < sprog->uniform_count; ++i){
		int loc = sprog->uniform_locations[i];

		const void* data = getUniformDataSource(cmd, params, viewport, i);
		switch(sprog->uniform_gl_types[i]){
			// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
			// This switch must have same cases as that in ~applyMaterial()~
			// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		case GL_FLOAT       : XGL_CHECK(glUniform1fv(loc, 1, ( GLfloat*)data)); break;
		case GL_FLOAT_VEC2  : XGL_CHECK(glUniform2fv(loc, 1, ( GLfloat*)data)); break;
		case GL_FLOAT_VEC3  : XGL_CHECK(glUniform3fv(loc, 1, ( GLfloat*)data)); break;
		case GL_FLOAT_VEC4  : XGL_CHECK(glUniform4fv(loc, 1, ( GLfloat*)data)); break;
		case GL_DOUBLE      : XGL_CHECK(glUniform1dv(loc, 1, (GLdouble*)data)); break;
		case GL_DOUBLE_VEC2 : XGL_CHECK(glUniform2dv(loc, 1, (GLdouble*)data)); break;
		case GL_DOUBLE_VEC3 : XGL_CHECK(glUniform3dv(loc, 1, (GLdouble*)data)); break;
		case GL_DOUBLE_VEC4 : XGL_CHECK(glUniform4dv(loc, 1, (GLdouble*)data)); break;
		case GL_INT         : XGL_CHECK(glUniform1iv(loc, 1, (   GLint*)data)); break;
		case GL_INT_VEC2    : XGL_CHECK(glUniform2iv(loc, 1, (   GLint*)data)); break;
		case GL_INT_VEC3    : XGL_CHECK(glUniform3iv(loc, 1, (   GLint*)data)); break;
		case GL_INT_VEC4    : XGL_CHECK(glUniform4iv(loc, 1, (   GLint*)data)); break;
		case GL_BOOL        : XGL_CHECK(glUniform1iv(loc, 1, (   GLint*)data)); break;
		case GL_FLOAT_MAT4  :
			XEN_CHECK_GL(glUniformMatrix4fv(loc, 1, GL_TRUE, (GLfloat*)data));
			break;
		case GL_DOUBLE_MAT4 :
			XEN_CHECK_GL(glUniformMatrix4dv(loc, 1, GL_TRUE, (GLdouble*)data));
			break;
		case GL_SAMPLER_2D:
			XEN_CHECK_GL(glUniform1i(loc, *(GLint*)data)); break;
			break;
		}
	}

	for(u64 i = 0; i < XenArrayLength(cmd.textures); ++i){
		XEN_CHECK_GL(glActiveTexture(GL_TEXTURE0 + i));
		XEN_CHECK_GL(glBindTexture(GL_TEXTURE_2D, xgl::getTextureImpl(cmd.textures[i])->id));
	}
}




#endif
