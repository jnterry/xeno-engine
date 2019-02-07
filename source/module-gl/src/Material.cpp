////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \brief Contains implementation of Material for opengl
///
/// \ingroup gl
////////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GL_MATERIAL_CPP
#define XEN_GL_MATERIAL_CPP

#include <xen/core/memory/ArenaLinear.hpp>
#include <xen/core/memory/ArenaPool.hpp>
#include <xen/math/vector_types.hpp>
#include <xen/math/matrix_types.hpp>
#include <xen/core/File.hpp>
#include <xen/kernel/log.hpp>

#include "ModuleGl.hxx"
#include "Material.hxx"
#include "gl_header.hxx"

bool fillShaderProgramMetaData(xgl::ShaderProgram* result){
	////////////////////////////////////////////////////
	// Vertex attributes
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

	xen::MemoryTransaction transaction(xgl::state->primary_arena);

	//////////////////////////////////////////////////////
	// Allocate dynamic sized memory for the ShaderProgram
	u64 data_block_size = result->uniform_count * (
		sizeof(xen::StringHash) + sizeof(xen::MetaType*) + sizeof(GLint) + sizeof(GLenum)
	);
	void* data_block = xen::reserveBytes(xgl::state->primary_arena, data_block_size);

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
			return false;
		}
	}

	transaction.commit();
	return true;
}

GLuint compileShader(GLenum stage, const char* source, const char* filename){
	// create shader
	GLuint result = XGL_CHECKRET(glCreateShader(stage));

	// load source and compile
	XGL_CHECK(glShaderSource(result, 1, &source, NULL));
	XGL_CHECK(glCompileShader(result));

	GLint status;
  XGL_CHECK(glGetShaderiv(result, GL_COMPILE_STATUS, &status));

  if(status != GL_TRUE){
	  xen::ArenaLinear& scratch = xen::getThreadScratchSpace();
	  xen::MemoryTransaction trans(scratch);
	  XEN_CHECK_GL(glGetShaderInfoLog(result,
	                                  xen::getBytesRemaining(scratch),
	                                  nullptr,
	                                  (GLchar*)scratch.next_byte));
	  XenLogError("Errors occurred compiling shader '%s':\n%s",
	              filename, (char*)scratch.next_byte);
  }

	return result;
}

bool attachShaderSources(GLint program,
                         xen::Array<const char*> sources,
                         GLint stage){

	xen::ArenaLinear& scratch = xen::getThreadScratchSpace();
	xen::MemoryTransaction transaction_scratch(scratch); // do not commit this!

	// :TODO: should has file names and reuse existing shaders where possible

	bool success = true;
	for(unsigned i = 0; i < sources.size; ++i){
		transaction_scratch.rollback();
		xen::FileData source_code = xen::loadFileAndNullTerminate(scratch, sources[i]);

		if(source_code.size == 0){
			XenLogError("Failed to load shader source file: %s", sources[i]);
			success = false;
			continue;
		}

		GLuint shader = compileShader(stage, (const char*)&source_code[0], sources[i]);

		XGL_CHECK(glAttachShader(program, shader));
	}

	return success;
}

void destroyShaderProgram(xgl::ShaderProgram* sprog){
	// :TODO: we should also delete attached shaders (but probably want ref counting
	// in order to reuse shaders from same source files)
	XEN_CHECK_GL(glDeleteProgram(sprog->program));

	xen::freeType(xgl::state->pool_shader, sprog);
}

bool linkShaderProgram(GLint program){
	XEN_CHECK_GL(glLinkProgram(program));

	GLint link_status;
	XEN_CHECK_GL(glGetProgramiv(program, GL_LINK_STATUS, &link_status ));

	if(link_status == GL_TRUE){
		XenLogDone("Successfully linked shader program");
		return true;
	}

	xen::ArenaLinear& scratch = xen::getThreadScratchSpace();
	xen::MemoryTransaction scratch_transaction(scratch);
	XEN_CHECK_GL(glGetProgramInfoLog(program, xen::getBytesRemaining(scratch),
	                                 nullptr, (GLchar*)scratch.next_byte));
	XenLogError("Failed to link shader:\n%s", (const char*)scratch.next_byte);
	return false;

}

xgl::ShaderProgram* createShaderProgram(const xen::MaterialCreationParameters& params){
	xgl::ShaderProgram* result = xen::reserveType(xgl::state->pool_shader);


	// :TODO: reuse an existing shader program if one exists with same source files
	result->program = XEN_CHECK_GL_RETURN(glCreateProgram());

	bool success = true;

	success &= attachShaderSources(result->program, params.vertex_sources,   GL_VERTEX_SHADER);
	success &= attachShaderSources(result->program, params.geometry_sources, GL_GEOMETRY_SHADER);
	success &= attachShaderSources(result->program, params.pixel_sources,    GL_FRAGMENT_SHADER);

	if(!success){
		XenLogWarn("Errors occurred while compiling shaders, attempting to link...");
	}

	if(!linkShaderProgram(result->program)){
		destroyShaderProgram(result);
		return nullptr;
	}

	if(!fillShaderProgramMetaData(result)){
		destroyShaderProgram(result);
		return nullptr;
	}

	return result;
}

const xen::Material* xgl::createMaterial(const xen::MaterialCreationParameters& data){
	////////////////////////////////////////////////////////////////////
	// Create Shader and Material instance
	xgl::ShaderProgram* sprog = createShaderProgram(data);
	if(sprog == nullptr){ return nullptr; }

	xgl::Material* result = xen::reserveType<xgl::Material>(xgl::state->pool_material);
	result->program = sprog;

	result->uniform_sources = xen::reserveTypeArray<xen::MaterialParameterSource::Kind>(
		xgl::state->primary_arena, sprog->uniform_count
	);
	result->uniform_param_offsets = xen::reserveTypeArray<u32>(
		xgl::state->primary_arena, sprog->uniform_count
	);
	////////////////////////////////////////////////////////////////////


	////////////////////////////////////////////////////////////////////
	// Warn on ignored parameters
	for(u64 i = 0; i < data.parameter_sources.size; ++i){
		xen::StringHash hash = xen::hash(data.parameter_sources[i].name);
		bool found = false;
		for(int j = 0; j < sprog->uniform_count && !found; ++j){
			found |= sprog->uniform_name_hashes[j] == hash;
		}
		if(!found){
			XenLogWarn("Ignoring material parameter source '%s' since no corresponding variable found in shader",
			           data.parameter_sources[i].name);
		}
	}
	////////////////////////////////////////////////////////////////////

	////////////////////////////////////////////////////////////////////
	// Build array in material instance that maps from uniform index to
	// MaterialParameterSource
	u16 variable_count = 0;
	for(GLint i = 0; i < sprog->uniform_count; ++i){
		result->uniform_sources[i] = xen::MaterialParameterSource::Variable;

		for(u64 j = 0; j < data.parameter_sources.size; ++j){
			if(sprog->uniform_name_hashes[i] == xen::hash(data.parameter_sources[j].name)){
				XenAssert(result->uniform_sources[i] == xen::MaterialParameterSource::Variable,
				          "Program contains multiple variables with same hash!"
				);
				result->uniform_sources[i] = data.parameter_sources[j].kind;
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
		xen::reserveBytes(xgl::state->primary_arena, meta_type_size)
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
void xgl::destroyMaterial(const xen::Material* mat){
	// :TODO: reuse shaders between multiple materials where applicable
	destroyShaderProgram(((xgl::Material*)mat)->program);

	// :TODO: we want to free dynamic storage used for material, but its all
	// in the primary arena which we don't want to reset.

	xen::freeType(xgl::state->pool_material, ((xgl::Material*)mat));

}

// Used as dummy uniform source by applyMaterial
u64 all_zeros[16] = { 0 };

const void* getUniformDataSource(const xgl::Material* material,
                                 const xen::RenderCommand3d&    cmd,
                                 const xen::RenderParameters3d& params,
                                 const xen::Aabb2u& viewport,
                                 int uniform_index){

	// :TODO: rather than doing all the calculations here we should instead
	// delay rendering until we have all commands, and then precompute all of
	// these once, then do rendering step
	// This would remove all of the pushing to thread scratch

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
	case xen::MaterialParameterSource::VpMatrix: {
		Mat4r* vp = xen::reserveType<Mat4r>(xen::getThreadScratchSpace());
		*vp = (
			getViewMatrix(params.camera) *
			xen::Scale3d(1, -1, 1) *
			getProjectionMatrix(params.camera, (Vec2r)(viewport.max - viewport.min))
		);
		return vp;
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
		if(params.lights.size == 0){ return &all_zeros; }
		return &params.lights[0].point.position;
	case xen::MaterialParameterSource::PointLightColor:
		if(params.lights.size == 0){ return &all_zeros; }
		return &params.lights[0].color;
	case xen::MaterialParameterSource::PointLightAttenuation:
		if(params.lights.size == 0){ return &all_zeros; }
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
	case xen::MaterialParameterSource::Runtime: {
		return &xgl::state->kernel_time;
	}
	}
	XenInvalidCodePath("Should hit a switch case!");
	return nullptr;
}

void xgl::applyMaterial(const xgl::Material* mat,
                        const xen::RenderCommand3d& cmd,
                        const xen::RenderParameters3d& params,
                        const xen::Aabb2u& viewport){

	xgl::ShaderProgram* sprog = mat->program;

	XEN_CHECK_GL(glUseProgram(sprog->program));

	for(GLint i = 0; i < sprog->uniform_count; ++i){
		int loc = sprog->uniform_locations[i];

		const void* data = getUniformDataSource(mat, cmd, params, viewport, i);
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

const char* _default_material_vertex = (
	"#version 330\n" \
	"\n" \
	"layout(location = 0) in vec3 vert_pos;\n" \
	"layout(location = 1) in vec3 vert_normal;\n" \
	"layout(location = 2) in vec4 vert_color;\n" \
	"\n" \
	"uniform mat4 mvp_mat;\n" \
	"\n" \
	"smooth out vec4 color;\n"\
	"\n"\
	"void main(){\n" \
	"	 gl_Position = vec4(vert_pos,1) * mvp_mat;\n" \
	"	 color       = vert_color;\n" \
	"}\n" \
);

const char* _default_material_fragment = (
	"#version 330\n" \
	"\n" \
	"smooth in vec4 color;\n" \
	"\n" \
	"out vec4 out_color;\n" \
	"\n" \
	"void main() {\n" \
	"  out_color = color;\n" \
	"}\n" \
);


/// \brief Should be called once at startup to create the default material
/// to be used by the engine if no other is specified
const xgl::Material* xgl::initDefaultMaterial(){
	////////////////////////////////////////////////////////////////////
	// Create shader program
	xgl::ShaderProgram* sprog = xen::reserveType<xgl::ShaderProgram>(xgl::state->pool_shader);
	sprog->program = glCreateProgram();

	GLuint shader_vertex = compileShader(
		GL_VERTEX_SHADER, _default_material_vertex, "_default_material_vertex"
	);
	GLuint shader_fragment = compileShader(
		GL_FRAGMENT_SHADER, _default_material_fragment, "_default_material_fragment"
	);

	XGL_CHECK(glAttachShader(sprog->program, shader_vertex));
	XGL_CHECK(glAttachShader(sprog->program, shader_fragment));

	if(!linkShaderProgram(sprog->program))      { return nullptr; }
	if(!fillShaderProgramMetaData(sprog)){ return nullptr; }
	////////////////////////////////////////////////////////////////////


	////////////////////////////////////////////////////////////////////
	// Create material
	xgl::Material* mat = xen::reserveType<xgl::Material>(xgl::state->pool_material);
	mat->program = sprog;

	mat->uniform_sources = xen::reserveTypeArray<xen::MaterialParameterSource::Kind>(
		xgl::state->primary_arena, sprog->uniform_count
	);
	XenAssert(sprog->uniform_count == 1, "Expected default program to have single uniform (mvp)");
	mat->uniform_sources[0] = xen::MaterialParameterSource::MvpMatrix;
	mat->uniform_param_offsets = nullptr;
	////////////////////////////////////////////////////////////////////

	return mat;
}



#endif
