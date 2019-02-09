# Xeno Engine

  Work in progress "back to c style" game engine

[![Build Status](https://travis-ci.org/jnterry/xeno-engine.svg?branch=master)](https://travis-ci.org/jnterry/xeno-engine.svg?branch=master)

# Build Steps

The build is managed by CMake, hence the project can be built from command line or in an IDE of your choice by following these steps:

1. Clone Repository
2. Create a directory `build` in root of repository
3. Navigate to build directory in terminal
4. Run the command `cmake ..` from the build directory
5. Build the project:
    - On Linux CMake will generate make files by default, so just run `make` in
      the build directory
    - On Windows CMake will generate Visual Studio Project Files by default,
      open these and build as usual
6. Produced binaries will be placed in the `bin` folder, run them from there so they can find their resource files

Note that Xeno Engine relies on some modern c++ syntax, minimum supported compiler versions are:
- GCC 7
- MSVC 15.7
