cmake_minimum_required(VERSION 3.29.0)
project(EkronRealmsFPS)

# Specify CMake modules path for custom find modules
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake/Modules")

# Detect Free Pascal and Kayte
find_package(FreePascal REQUIRED)
find_package(Kayte REQUIRED)

# Set source files (Pascal and Kayte)
set(SOURCE_FILES
    src/main.pas
    src/some_kayte_script.kayte
)

# Create a custom target for Kayte scripts
add_custom_target(KayteScripts ALL
    COMMAND ${KAYTE_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/src/some_kayte_script.kayte
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/src/some_kayte_script.kayte
)

# Set platform-specific compiler and linker flags
if (CMAKE_SYSTEM_NAME STREQUAL "iOS")
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -target arm64-apple-ios")
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -framework UIKit -framework Foundation")
    message(STATUS "Configured for iOS")
elseif (CMAKE_SYSTEM_NAME STREQUAL "Android")
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -target aarch64-linux-android")
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -landroid -llog")
    message(STATUS "Configured for Android")
elseif (CMAKE_SYSTEM_NAME STREQUAL "Darwin")
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -target arm64-apple-macos")
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -framework Cocoa")
    message(STATUS "Configured for macOS")
elseif (CMAKE_SYSTEM_NAME STREQUAL "Linux")
    if(CMAKE_SYSTEM_PROCESSOR STREQUAL "aarch64")
        set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -target aarch64-linux-gnu")
        message(STATUS "Configured for Linux (aarch64)")
    else()
        set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -m64")
        message(STATUS "Configured for Linux (x86_64)")
    endif()
elseif (CMAKE_SYSTEM_NAME STREQUAL "Windows")
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -m64")
    message(STATUS "Configured for Windows")
endif()

# Enable Pascal language
enable_language(Pascal)

# Add Pascal executable
add_executable(ekron_realms ${SOURCE_FILES})
target_link_libraries(ekron_realms ${FPC_LIBRARIES})
add_dependencies(ekron_realms KayteScripts)

# Install target
install(TARGETS ekron_realms DESTINATION bin)

