cmake_minimum_required(VERSION 3.29.0)
project(EkronRealms LANGUAGES CXX Pascal)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Specify CMake modules path for custom find modules
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake/Modules")

# Include custom Pascal modules
include(CMakeDeterminePascalCompiler)
include(CMakePascalInformation)

# Detect Free Pascal and Kayte
find_package(FreePascal REQUIRED)
find_package(Kayte REQUIRED)

# Set source files (Pascal and Kayte)
set(SOURCE_FILES
    qcommon/Cmd.pas
    qcommon/CModel.pas
    qcommon/Common.pas
    qcommon/CPas.pas
    qcommon/crc.pas
    qcommon/CVar.pas
    qcommon/Delphi_cdecl_printf.pas
    qcommon/DelphiTypes.pas
    qcommon/Files.pas
    qcommon/md4.pas
    qcommon/net_chan.pas
    qcommon/PMoveUnit.pas
    qcommon/QCommon.inc
    qcommon/qfiles.pas
    mac/sys_mac.pas
    mac/snd_mac.pas
    mac/vid_mac.pas
    mac/OpenGL2.pas
    vulkan/vulkan.pas
    vulkan/vulkanrender.pas
    vulkan/vulkanutils.pas
    null/sys_null.pas
    null/vid_null.pas
    Jedi/opengl2.pas
    Jedi/OpenGL12.pas
    ui/HUD.kayte
    ui/map.kayte
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

# Add Pascal executable
add_pascal_executable(ekron_realms "${SOURCE_FILES}")
target_link_libraries(ekron_realms ${FPC_LIBRARIES})
add_dependencies(ekron_realms KayteScripts)

# Install targets
install(TARGETS ekron_realms
        RUNTIME DESTINATION bin
        LIBRARY DESTINATION lib
        ARCHIVE DESTINATION lib)

# CPack configuration for macOS
set(CPACK_GENERATOR "productbuild")  # Use "productbuild" for newer macOS versions
set(CPACK_PACKAGE_NAME "EkronRealms")
set(CPACK_PACKAGE_VERSION "1.0.0")
set(CPACK_PACKAGE_VENDOR "Your Organization")
set(CPACK_PACKAGE_CONTACT "Your Name <your.email@example.com>")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "A combined FreePascal and C++17 project")
set(CPACK_PACKAGE_INSTALL_DIRECTORY "EkronRealms")

# macOS-specific settings
set(CPACK_PACKAGE_FILE_NAME "${CPACK_PACKAGE_NAME}-${CPACK_PACKAGE_VERSION}-macos-arm64")
set(CPACK_PACKAGE_ICON "${CMAKE_SOURCE_DIR}/resources/icon.icns")  # Optional: Add an icon for the installer
set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_SOURCE_DIR}/LICENSE.txt")  # Optional: Add a license file
set(CPACK_RESOURCE_FILE_README "${CMAKE_SOURCE_DIR}/README.txt")    # Optional: Add a readme file

# Set the architecture to arm64 for macOS Silicon
set(CPACK_OSX_ARCHITECTURES "arm64")

# Include CPack
include(CPack)