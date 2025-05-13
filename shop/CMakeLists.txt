
cmake_minimum_required(VERSION 3.14)
project(ShopGuiApiTemplate)

set(CMAKE_CXX_STANDARD 17)

# Set Vulkan path manually or find_package
find_package(Vulkan REQUIRED)

# Set ImGui and Crow directories if not using a package manager
add_subdirectory(gui)
add_subdirectory(api)

# Include directories
include_directories(${Vulkan_INCLUDE_DIR})
