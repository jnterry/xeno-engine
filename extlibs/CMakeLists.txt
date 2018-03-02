use_cxx11()

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
