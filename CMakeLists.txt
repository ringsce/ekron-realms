cmake_minimum_required(VERSION 3.22)
project(RealmsEngine LANGUAGES C)

# Set default to Release
if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release)
endif()

# Platform-specific FPC flags
set(FPC_FLAGS "-Mobjfpc -Scgi -O2 -g -gl -Fu../ -Fu../kayte -Fu../vulkan")

# Detect platform
if(WIN32)
  set(PLATFORM "win32")
elseif(APPLE)
  set(PLATFORM "mac")
elseif(UNIX)
  set(PLATFORM "linux")
endif()

# Gather Pascal sources
file(GLOB_RECURSE COMMON_SOURCES
  ../qcommon/*.pas
  ../client/*.pas
  ../server/*.pas
  ../game/*.pas
)

file(GLOB_RECURSE VULKAN_SOURCES
  ../vulkan/*.pas
)

file(GLOB_RECURSE KAYTE_SOURCES
  ../kayte/*.pas
)

# Platform-specific sources
if(${PLATFORM} STREQUAL "win32")
  file(GLOB_RECURSE PLATFORM_SOURCES ../win32/*.pas)
elseif(${PLATFORM} STREQUAL "linux")
  file(GLOB_RECURSE PLATFORM_SOURCES ../linux/*.pas)
elseif(${PLATFORM} STREQUAL "mac")
  file(GLOB_RECURSE PLATFORM_SOURCES ../mac/*.pas)
endif()

# Final source list
set(ALL_SOURCES
  ${COMMON_SOURCES}
  ${VULKAN_SOURCES}
  ${KAYTE_SOURCES}
  ${PLATFORM_SOURCES}
)

# Pascal output directory
set(OUTPUT_DIR ${CMAKE_BINARY_DIR}/bin)
file(MAKE_DIRECTORY ${OUTPUT_DIR})

# Add custom command to invoke FPC
add_custom_command(
  OUTPUT ${OUTPUT_DIR}/realms
  COMMAND fpc ${FPC_FLAGS} -FE${OUTPUT_DIR} -o${OUTPUT_DIR}/realms ../launcher.pas
  DEPENDS ${ALL_SOURCES}
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
  COMMENT "Building Realms Engine with Free Pascal"
)

# Add executable target (dummy for CMake, real work is done by custom command)
add_custom_target(realms ALL DEPENDS ${OUTPUT_DIR}/realms)
