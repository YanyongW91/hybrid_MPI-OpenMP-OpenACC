# Set the minimum CMake version
cmake_minimum_required(VERSION 3.10)

# Project name and language
project(MyFortranProject LANGUAGES Fortran)

# Specify the source files
file(GLOB_RECURSE SOURCES src/*.f90)


# Create an executable
add_executable(hybrid_prog ${SOURCES})
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_SOURCE_DIR}/bin")


# Enable OpenMP (this should add the OpenMP flags automatically)
find_package(OpenMP REQUIRED)
# Link OpenMP library to the target
target_link_libraries(hybrid_prog PRIVATE OpenMP::OpenMP_Fortran)

# Include MPI (ensure that mpif.h is available)
find_package(MPI REQUIRED)
target_include_directories(hybrid_prog PRIVATE ${MPI_INCLUDE_PATH})
target_link_libraries(hybrid_prog PRIVATE ${MPI_LIBRARIES})

# Add compile options for the target
target_compile_options(hybrid_prog PRIVATE
    -O2        # Optimization level 2
    -g         # Debugging symbols
    -fopenmp   # Enable OpenMP (if needed)
)

# Optionally, set Fortran flags (e.g., optimization, debugging)
target_compile_options(hybrid_prog PRIVATE -O2 -g)

message(STATUS "CMAKE_RUNTIME_OUTPUT_DIRECTORY = ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}")

message(STATUS "CMAKE_SOURCE_DIR = ${CMAKE_SOURCE_DIR}")
