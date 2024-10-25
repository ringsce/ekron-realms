cmake_minimum_required(VERSION 3.29.0)
project(EkronRealmsFPS)

# Specify CMake modules path for custom find modules
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake/Modules")

# Detect Free Pascal and Kayte
find_package(FreePascal REQUIRED)
find_package(Kayte REQUIRED)

# Set source files (example list, add your actual Pascal and Kayte files)
set(SOURCE_FILES
    src/main.pas
    src/some_kayte_script.kayte
)

# Create a custom target for Kayte files, assuming it compiles into bytecode
# or can be directly interpreted, if necessary.
add_custom_target(KayteScripts ALL
    COMMAND ${KAYTE_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/src/some_kayte_script.kayte
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/src/some_kayte_script.kayte
)

# Set platform-specific compiler flags
if (CMAKE_SYSTEM_NAME STREQUAL "iOS")
    # iOS build settings
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -target arm64-apple-ios")
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -framework UIKit -framework Foundation")
    message(STATUS "Configured for iOS platform")
elseif (CMAKE_SYSTEM_NAME STREQUAL "Android")
    # Android build settings
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -target aarch64-linux-android")
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -landroid -llog")
    message(STATUS "Configured for Android platform")
elseif (CMAKE_SYSTEM_NAME STREQUAL "Darwin")
    # macOS build settings (assuming Apple silicon if arm64)
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -target arm64-apple-macos")
    message(STATUS "Configured for macOS platform")
elseif (CMAKE_SYSTEM_NAME STREQUAL "Linux")
    # Linux (aarch64) build settings
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -target aarch64-linux-gnu")
    message(STATUS "Configured for Linux (aarch64) platform")
endif()

# Add Free Pascal as a language for compiling .pas files
enable_language(Pascal)

# Add Pascal executable
add_executable(ekron_realms ${SOURCE_FILES})
target_link_libraries(ekron_realms ${FPC_LIBRARIES})
add_dependencies(ekron_realms KayteScripts)

# Installation (if applicable)
install(TARGETS ekron_realms DESTINATION bin)

