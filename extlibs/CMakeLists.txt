use_cxx11()

##########################################
# SFML
##########################################
set(SFML_BUILD_EXAMPLES OFF)
set(SFML_BUILD_DOCS     OFF)
add_subdirectory(sfml2.4.2)

##########################################
# SDL2
##########################################
# Extract archive
execute_process(COMMAND cmake -E tar xf SDL2-2.0.7.zip WORKING_DIRECTORY ${CMAKE_CURRENT_LIST_DIR})
# Build libraries
add_subdirectory(SDL2-2.0.7)

##########################################
# assimp
##########################################
# Extract archive
execute_process(COMMAND cmake -E tar xf assimp-3.3.1.zip WORKING_DIRECTORY ${CMAKE_CURRENT_LIST_DIR})
# Build libraries
set(ASSIMP_BUILD_ASSIMP_TOOLS OFF)
set(ASSIMP_BUILD_SAMPLES      OFF)
set(ASSIMP_BUILD_TESTS        OFF)
add_subdirectory(assimp-3.3.1)
target_compile_definitions(assimp PRIVATE ASSIMP_BUILD_BOOST_WORKAROUND=1)

##########################################
# GLEW
##########################################
# Extract archive
execute_process(COMMAND cmake -E tar xf glew-2.1.0.zip WORKING_DIRECTORY ${CMAKE_CURRENT_LIST_DIR})
# Build librares
add_subdirectory(glew-2.1.0)
