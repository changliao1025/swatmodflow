cmake_minimum_required(VERSION 2.8)
project(swatmodflow Fortran)
set(CMAKE_VERBOSE_MAKEFILE ON)
set(CMAKE_BUILD_TYPE Debug)

file(GLOB swat_src   swat/*.f    swat/*.f90)

file(GLOB modflow_src     modflow/*.f    modflow/*.f90)

file(GLOB rt3d_src    rt3d/*.f rt3d/*.f90)

file(GLOB smrt_src    smrt/*.f    smrt/*.f90)

file(GLOB other_src    other/*.f    other/*.f90)

set(SOURCE_FILES ${swat_src} ${modflow_src} ${rt3d_src} ${smrt_src} ${other_src} ) 

add_executable(swatmodflow ${SOURCE_FILES})

set_source_files_properties(
  ${SOURCE_FILES}
  PROPERTIES COMPILE_FLAGS -ffree-line-length-none
             COMPILE_FLAGS -ffixed-line-length-none)


set(INSTALL_DIR ${CMAKE_CURRENT_SOURCE_DIR}/../bin)
install(TARGETS swatmodflow RUNTIME DESTINATION ${INSTALL_DIR})

