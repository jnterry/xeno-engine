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
