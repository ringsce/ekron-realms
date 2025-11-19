cmake_minimum_required(VERSION 3.16 FATAL_ERROR)
project(Realms VERSION 1.0.0 LANGUAGES CXX NONE)

# Add the cmake module path to find FindFPC.cmake and FindLazarus.cmake
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")
find_package(KayteIDE REQUIRED)
find_package(KCC REQUIRED)
# =============================================================================
# Find Free Pascal Compiler
# =============================================================================
find_package(FPC REQUIRED)

if(FPC_FOUND)
    message(STATUS "=== Free Pascal Compiler ===")
    message(STATUS "  Executable: ${FPC_EXECUTABLE}")
    message(STATUS "  Version: ${FPC_VERSION}")
    message(STATUS "  Target CPU: ${FPC_TARGET_CPU}")
    message(STATUS "  Target OS: ${FPC_TARGET_OS}")
    if(APPLE AND FPC_MACOS_ARCH)
        message(STATUS "  Architecture: ${FPC_MACOS_ARCH}")
    endif()
    message(STATUS "")
endif()

# =============================================================================
# Find Lazarus IDE (Optional)
# =============================================================================
find_package(Lazarus)

if(LAZARUS_FOUND)
    message(STATUS "=== Lazarus IDE ===")
    message(STATUS "  Root: ${LAZARUS_ROOT_DIR}")
    message(STATUS "  Version: ${LAZARUS_VERSION}")
    message(STATUS "  Platform: ${LAZARUS_PLATFORM}")
    message(STATUS "  lazbuild: ${LAZBUILD_EXECUTABLE}")
    message(STATUS "  LCL: ${LAZARUS_LCL_DIR}")
    if(FPC_FROM_LAZARUS)
        message(STATUS "  Bundled FPC: ${FPC_FROM_LAZARUS}")
    endif()
    message(STATUS "")
endif()

# =============================================================================
# Utility Functions
# =============================================================================
# Placeholder function for checking SDL compatibility (to fix "Unknown CMake command" error)
function(check_sdl_compatibility)
    if(SDL2_FOUND AND SDL3_FOUND)
        message(WARNING "Both SDL2 and SDL3 bindings were found.
Ensure your Pascal code links to only one set of headers to prevent conflicts.")
    endif()
endfunction()


# =============================================================================
# Find SDL2 for Pascal (Optional)
# =============================================================================
find_package(SDL2) # This is where the find_package call is

# =============================================================================
# Find SDL3 for Pascal (Optional)
# =============================================================================
find_package(SDL3)


# =============================================================================
# Load External Project Module for KayteIDE C++ Build
# =============================================================================
include(ExternalProject)


# =============================================================================
# Build KayteIDE C++ Project
# =============================================================================
set(KAYTEIDE_CMAKE_FILE "${KAYTEIDE_DIR}/CMakeLists.txt")

if(KAYTEIDE_FOUND AND EXISTS ${KAYTEIDE_CMAKE_FILE})
    message(STATUS "=== Building KayteIDE C++ Project with CMake (Final command fix) ===")
    
    # Define build and install directories for the external project
    set(KAYTEIDE_BUILD_DIR "${CMAKE_CURRENT_BINARY_DIR}/KayteIDE_build")
    set(KAYTEIDE_INSTALL_DIR "${CMAKE_CURRENT_BINARY_DIR}/KayteIDE_install_staging")
    
    # Define the expected executable name (e.g., kayteide or kayteide.exe)
    set(KAYTEIDE_EXECUTABLE_FILE "kayteide${CMAKE_EXECUTABLE_SUFFIX}")
    set(KAYTEIDE_BUILD_OUTPUT "${KAYTEIDE_BUILD_DIR}/${CMAKE_BUILD_TYPE}/${KAYTEIDE_EXECUTABLE_FILE}")
    
    # NOTE: The build output path is often under a build-type subdirectory (Debug, Release) for C++ projects.
    # We will adjust the output path accordingly. If your C++ project builds directly into ${KAYTEIDE_BUILD_DIR}, use that path instead.
    if(NOT EXISTS ${KAYTEIDE_BUILD_OUTPUT})
        set(KAYTEIDE_BUILD_OUTPUT "${KAYTEIDE_BUILD_DIR}/${KAYTEIDE_EXECUTABLE_FILE}")
    endif()
    
    set(KAYTEIDE_STAGING_OUTPUT "${KAYTEIDE_INSTALL_DIR}/${KAYTEIDE_EXECUTABLE_FILE}")

    # Use ExternalProject_Add to configure and build the C++ project
    ExternalProject_Add(kayteide_cpp_build
        SOURCE_DIR ${KAYTEIDE_DIR}
        BINARY_DIR ${KAYTEIDE_BUILD_DIR}
        CMAKE_ARGS
            -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
            -DCMAKE_INSTALL_PREFIX=${KAYTEIDE_INSTALL_DIR}
        
        # Use the project's own install rules
        BUILD_COMMAND ${CMAKE_COMMAND} --build <BINARY_DIR>
        INSTALL_COMMAND ${CMAKE_COMMAND} --install <BINARY_DIR>
    )


    # Create a simple, user-friendly target 'kayteide' that depends on the build
    add_custom_target(kayteide DEPENDS kayteide_cpp_build)
    message(STATUS "  ✓ kayteide C++ build target created. Output staged in ${KAYTEIDE_INSTALL_DIR}")

    # Set variable for the installation rules
    set(KAYTEIDE_FINAL_INSTALL_PATH ${KAYTEIDE_INSTALL_DIR})

elseif(KAYTEIDE_FOUND)
    message(STATUS "KayteIDE files found but C++ CMakeLists.txt not found. Skipping C++ build.")
endif()


# =============================================================================
# Build KCC C Compiler Project
# =============================================================================
set(KCC_CMAKE_FILE "${KCC_DIR}/CMakeLists.txt")

if(KCC_FOUND AND EXISTS ${KCC_CMAKE_FILE})
    message(STATUS "=== Building KCC C Compiler Project with CMake (Fixing Install Prefix) ===")
    
    # Define build and install directories for the external project
    set(KCC_BUILD_DIR "${CMAKE_CURRENT_BINARY_DIR}/KCC_build")
    set(KCC_INSTALL_DIR "${CMAKE_CURRENT_BINARY_DIR}/KCC_install_staging")
    
    # Use ExternalProject_Add to configure, build, and let the external project install
    ExternalProject_Add(kcc_cpp_build
        SOURCE_DIR ${KCC_DIR}
        BINARY_DIR ${KCC_BUILD_DIR}
        # INSTALL_DIR is still needed for stamping, but the prefix below is key
        INSTALL_DIR ${KCC_INSTALL_DIR} 
        
        # FIX: Explicitly set CMAKE_INSTALL_PREFIX for the external project
        # This directs the external project's internal 'install' target to our staging folder.
        CMAKE_ARGS
            -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
            -DCMAKE_INSTALL_PREFIX:PATH=${KCC_INSTALL_DIR}
            
        # The external project's native 'install' step will now run to KCC_INSTALL_DIR.
        # This is much cleaner than using manual POST_BUILD_COMMANDs.
    )
    
    # Create a simple, user-friendly target 'kcc' that depends on the build
    add_custom_target(kcc DEPENDS kcc_cpp_build)
    message(STATUS "  ✓ kcc C compiler build target created. Output staged in ${KCC_INSTALL_DIR}")

    # Set variable for the installation rules
    set(KCC_FINAL_INSTALL_PATH ${KCC_INSTALL_DIR})

elseif(KCC_FOUND)
    message(STATUS "KCC files found but C++ CMakeLists.txt not found. Skipping KCC build.")
endif()

# Install kcc executable and contents
if(TARGET kcc)
    # The output is the staged install directory of the C++ project.
    if(APPLE)
        # On macOS, install contents to 'Contents/tools/KCC' inside the bundle
        install(DIRECTORY 
            ${KCC_FINAL_INSTALL_PATH}/
            DESTINATION "Contents/tools/KCC"
            COMPONENT applications
            OPTIONAL
        )
        message(STATUS "  ✓ Install rule for KCC (macOS bundle) created.")
    else()
        # Standard install destination for other platforms (e.g., bin/KCC)
        install(DIRECTORY
            ${KCC_FINAL_INSTALL_PATH}/
            DESTINATION bin/KCC
            COMPONENT applications
            OPTIONAL
        )
        message(STATUS "  ✓ Install rule for KCC (standard) created.")
    endif()
endif()

# =============================================================================
# Compiler Flags Configuration
# =============================================================================

# Common flags for all builds
set(FPC_COMMON_FLAGS
	-O-                      # Change this from -O2 or -O0 to -O- (No Optimization)
    -g                       # Generate debug info
    -gl                      # Use line info unit
    -Xs                      # Link with static libraries
    -XX                      # Smart linking
    -CX                      # Create smartlinked units
)

# Debug build flags
set(FPC_DEBUG_FLAGS
    -g                       # Debug info
    -gh                      # Use heaptrace unit
    -Cr                      # Range checking
    -Co                      # Overflow checking
    -Ci                      # I/O checking
    -Sa                      # Include assertion code
    -Criot                   # All runtime checks
)

# Release build flags
set(FPC_RELEASE_FLAGS
    -O3                      # Maximum optimization
    -Xs                      # Strip symbols
    -XX                      # Smart linking
)

# Select flags based on build type
if(CMAKE_BUILD_TYPE STREQUAL "Debug")
    set(FPC_BUILD_FLAGS ${FPC_DEBUG_FLAGS})
    message(STATUS "Build type: Debug (with runtime checks)")
elseif(CMAKE_BUILD_TYPE STREQUAL "Release")
    set(FPC_BUILD_FLAGS ${FPC_RELEASE_FLAGS})
    message(STATUS "Build type: Release (optimized)")
else()
    set(FPC_BUILD_FLAGS ${FPC_COMMON_FLAGS})
    message(STATUS "Build type: Default")
endif()

# =============================================================================
# Directory Configuration
# =============================================================================
set(SRC_DIR "${CMAKE_CURRENT_SOURCE_DIR}/src")
set(UNIT_DIR "${CMAKE_CURRENT_SOURCE_DIR}/units")
set(PROJECTS_DIR "${CMAKE_CURRENT_SOURCE_DIR}/Projects")

# Add Lazarus LCL units if available
if(LAZARUS_FOUND)
    set(LAZARUS_UNIT_PATHS
        ${LAZARUS_LCL_DIR}
        ${LAZARUS_COMPONENTS_DIR}
        ${LAZARUS_IDEINTF_DIR}
    )
endif()

# Add SDL2 units if available
if(SDL2_FOUND)
    list(APPEND LAZARUS_UNIT_PATHS ${SDL2_INCLUDE_DIRS})
endif()

# Add SDL3 units if available
if(SDL3_FOUND)
    list(APPEND LAZARUS_UNIT_PATHS ${SDL3_INCLUDE_DIRS})
endif()

# =============================================================================
# Detect Project Files
# =============================================================================
# Install kayteide C++ project output
if(TARGET kayteide)
    # The output is the staged install directory of the C++ project.
    if(APPLE)
        # On macOS, install contents to 'Contents/tools/KayteIDE' inside the bundle
        install(DIRECTORY 
            ${KAYTEIDE_FINAL_INSTALL_PATH}/
            DESTINATION "Contents/tools/KayteIDE"
            COMPONENT applications
            OPTIONAL
        )
        message(STATUS "  ✓ Install rule for KayteIDE (macOS bundle) created.")
    else()
        # Standard install destination for other platforms (e.g., bin/KayteIDE)
        install(DIRECTORY
            ${KAYTEIDE_FINAL_INSTALL_PATH}/
            DESTINATION bin/KayteIDE
            COMPONENT applications
            OPTIONAL
        )
        message(STATUS "  ✓ Install rule for KayteIDE (standard) created.")
    endif()
endif()

message(STATUS "")
message(STATUS "=== Detecting Project Files ===")

# Check for Realms Lazarus project (.lpi)
set(REALMS_LPI_FILE "${PROJECTS_DIR}/realms.lpi")
if(EXISTS ${REALMS_LPI_FILE})
    set(REALMS_LPI_FOUND TRUE)
    message(STATUS "  Found Lazarus project: ${REALMS_LPI_FILE}")
else()
    set(REALMS_LPI_FOUND FALSE)
    message(STATUS "  Lazarus project not found: ${REALMS_LPI_FILE}")
endif()

# Check for Realms Pascal program (.lpr or .dpr)
set(REALMS_LPR_FILE "")
if(EXISTS "${PROJECTS_DIR}/realms.lpr")
    set(REALMS_LPR_FILE "${PROJECTS_DIR}/realms.lpr")
elseif(EXISTS "${PROJECTS_DIR}/realms.dpr")
    set(REALMS_LPR_FILE "${PROJECTS_DIR}/realms.dpr")
endif()

if(REALMS_LPR_FILE)
    set(REALMS_LPR_FOUND TRUE)
    message(STATUS "  Found Pascal program: ${REALMS_LPR_FILE}")
else()
    set(REALMS_LPR_FOUND FALSE)
    message(STATUS "  Pascal program not found: ${PROJECTS_DIR}/realms.lpr or .dpr")
endif()

message(STATUS "")

# =============================================================================
# Build Realms Project
# =============================================================================

if(TARGET kayteide)
    message(STATUS "  ✓ kayteide - KayteIDE application")
endif()

if(TARGET kcc)
    message(STATUS "  ✓ kcc- Kayte C Compiler")
endif()

# Get relative paths for project files
if(REALMS_LPI_FOUND)
    file(RELATIVE_PATH LPI_PROJECT_PATH ${CMAKE_CURRENT_SOURCE_DIR} ${REALMS_LPI_FILE})
endif()

if(REALMS_LPR_FOUND)
    file(RELATIVE_PATH LPR_PROJECT_PATH ${CMAKE_CURRENT_SOURCE_DIR} ${REALMS_LPR_FILE})
endif()

# Option 1: Build with Lazarus (if available) - Prioritize .lpi
if(LAZARUS_FOUND AND LAZBUILD_EXECUTABLE)
    set(LAZ_PROJECT_FILE "")

    # Priority 1: Use .lpi file if it exists
    if(REALMS_LPI_FOUND)
        message(STATUS "=== Building Realms with Lazarus (.lpi) ===")
        set(LAZ_PROJECT_FILE ${LPI_PROJECT_PATH})
        set(REALMS_BUILD_METHOD "Lazarus IDE (lazbuild) - .lpi")
    
    # Priority 2: Use .lpr/.dpr file as fallback
    elseif(REALMS_LPR_FOUND)
        message(STATUS "=== Building Realms with Lazarus (.lpr/.dpr) ===")
        set(LAZ_PROJECT_FILE ${LPR_PROJECT_PATH})
        set(REALMS_BUILD_METHOD "Lazarus IDE (lazbuild) - .lpr/.dpr")
    else()
        set(REALMS_BUILD_METHOD "Lazarus found but no project file")
    endif()

    # Add targets if a project file was found
    if(LAZ_PROJECT_FILE)
        add_lazarus_project(realms
            PROJECT_FILE ${LAZ_PROJECT_FILE}
            BUILD_MODE Default
            OUTPUT_NAME realms
        )
        
        add_lazarus_project(realms_debug
            PROJECT_FILE ${LAZ_PROJECT_FILE}
            BUILD_MODE Default
            OUTPUT_NAME realms_debug
        )
        
        add_lazarus_project(realms_verbose
            PROJECT_FILE ${LAZ_PROJECT_FILE}
            BUILD_MODE Default
            COMPILE_FLAGS
                --lazarusdir=${LAZARUS_ROOT_DIR}
                --verbose
                --skip-dependencies
            OUTPUT_NAME realms_verbose
        )
    endif()

# Option 2: Fallback to FPC directly (if .lpr/.dpr exists and Lazarus not used)
elseif(REALMS_LPR_FOUND)
    message(STATUS "=== Building Realms with FPC (Lazarus not found/used) ===")
    
    file(GLOB REALMS_SOURCES "${PROJECTS_DIR}/*.pas" "${PROJECTS_DIR}/*.pp")
    
    add_fpc_executable(realms
        SOURCES
            ${LPR_PROJECT_PATH}
            ${REALMS_SOURCES}
        INCLUDE_DIRS
            ${PROJECTS_DIR}
            ${UNIT_DIR}
        UNITS
            ${LAZARUS_UNIT_PATHS}
        COMPILE_FLAGS
            ${FPC_COMMON_FLAGS}
            ${FPC_BUILD_FLAGS}
            -Mobjfpc
            -Scghi
            -CX
            -vewnhibq
        OUTPUT_NAME realms
    )
    
    add_fpc_executable(realms_debug
        SOURCES
            ${LPR_PROJECT_PATH}
            ${REALMS_SOURCES}
        INCLUDE_DIRS
            ${PROJECTS_DIR}
            ${UNIT_DIR}
        UNITS
            ${LAZARUS_UNIT_PATHS}
        COMPILE_FLAGS
            ${FPC_DEBUG_FLAGS}
            -Mobjfpc
            -Scghi
            -vewnhibq
        OUTPUT_NAME realms_debug
    )
    
    set(REALMS_BUILD_METHOD "FPC Compiler (direct)")

else()
    message(WARNING "Realms project files not found!")
    message(WARNING "  Expected: ${PROJECTS_DIR}/realms.lpi, .lpr, or .dpr")
    message(WARNING "  Skipping Realms build targets")
    set(REALMS_BUILD_METHOD "Not available")
endif()

# =============================================================================
# Additional Example Targets (Optional)
# =============================================================================

# Example 1: Simple Console Application (if main.pas exists)
if(EXISTS "${SRC_DIR}/main.pas")
    add_fpc_executable(console_app
        SOURCES
            src/main.pas
        COMPILE_FLAGS
            ${FPC_COMMON_FLAGS}
            ${FPC_BUILD_FLAGS}
            -Mobjfpc
        OUTPUT_NAME console_app
    )
endif()

# Example 2: Application with Multiple Units (if files exist)
if(EXISTS "${SRC_DIR}/program.pas")
    file(GLOB MULTI_UNIT_SOURCES "${SRC_DIR}/*.pas")
    add_fpc_executable(multi_unit_app
        SOURCES
            ${MULTI_UNIT_SOURCES}
        INCLUDE_DIRS
            ${UNIT_DIR}
        COMPILE_FLAGS
            ${FPC_COMMON_FLAGS}
            ${FPC_BUILD_FLAGS}
            -Mobjfpc
            -Scgi
            -vewnhi
        OUTPUT_NAME multi_unit_app
    )
endif()

# =============================================================================
# Cross-Platform Configuration
# =============================================================================
if(FPC_TARGET_OS STREQUAL "android")
    # For Android cross-compilation (e.g., fpc -Tandroid -Paarch64)
    set(PLATFORM_FLAGS -dANDROID)
    set(PLATFORM_UNITS ${UNIT_DIR}/android)
    set(PLATFORM_NAME "Android")
    if(FPC_TARGET_CPU)
        set(PLATFORM_NAME "Android (${FPC_TARGET_CPU})")
    endif()
elseif(FPC_TARGET_OS STREQUAL "ios" OR FPC_TARGET_OS STREQUAL "iphoneos")
    # For iOS cross-compilation (e.g., fpc -Tios -Paarch64)
    set(PLATFORM_FLAGS -diOS)
    set(PLATFORM_UNITS ${UNIT_DIR}/ios)
    set(PLATFORM_NAME "iOS")
    if(FPC_TARGET_CPU)
        set(PLATFORM_NAME "iOS (${FPC_TARGET_CPU})")
    endif()
elseif(WIN32)
    set(PLATFORM_FLAGS -dWINDOWS)
    set(PLATFORM_UNITS ${UNIT_DIR}/windows)
    set(PLATFORM_NAME "Windows")
elseif(APPLE)
    # This handles macOS build on a macOS host.
    set(PLATFORM_FLAGS -dMACOS)
    set(PLATFORM_UNITS ${UNIT_DIR}/macos)
    set(PLATFORM_NAME "macOS")
    if(FPC_TARGET_ARCH STREQUAL "aarch64")
        list(APPEND PLATFORM_FLAGS -dARM64)
        set(PLATFORM_NAME "macOS (Apple Silicon)")
    endif()
else()
    # Default for all other targets (usually Linux host builds)
    set(PLATFORM_FLAGS -dLINUX)
    set(PLATFORM_UNITS ${UNIT_DIR}/linux)
    set(PLATFORM_NAME "Linux")
endif()

# =============================================================================
# Installation Rules
# =============================================================================

# Install realms executable (works for both Lazarus and FPC builds)
if(TARGET realms)
    # For Lazarus builds, the executable is a custom target output
    # We need to install the file directly, not the target
    install(PROGRAMS
        ${CMAKE_CURRENT_BINARY_DIR}/realms${CMAKE_EXECUTABLE_SUFFIX}
        DESTINATION bin
        COMPONENT applications
        OPTIONAL
    )
endif()

if(TARGET realms_debug)
    install(PROGRAMS
        ${CMAKE_CURRENT_BINARY_DIR}/realms_debug${CMAKE_EXECUTABLE_SUFFIX}
        DESTINATION bin
        COMPONENT debug
        OPTIONAL
    )
endif()

if(TARGET console_app)
    install(PROGRAMS
        ${CMAKE_CURRENT_BINARY_DIR}/console_app${CMAKE_EXECUTABLE_SUFFIX}
        DESTINATION bin
        COMPONENT applications
        OPTIONAL
    )
endif()

# Install documentation
install(FILES
    README.md
    DESTINATION share/doc/realms
    OPTIONAL
    COMPONENT documentation
)

install(FILES
    LICENSE
    DESTINATION share/doc/realms
    OPTIONAL
    COMPONENT documentation
)

# Install project files (for development)
install(DIRECTORY ${PROJECTS_DIR}/
    DESTINATION share/realms/projects
    OPTIONAL
    COMPONENT development
    FILES_MATCHING
        PATTERN "*.lpi"
        PATTERN "*.lpr"
        PATTERN "*.dpr"
        PATTERN "*.pas"
        PATTERN "*.lfm"
        PATTERN "*.lrs"
    PATTERN "backup" EXCLUDE
    PATTERN "lib" EXCLUDE
)

# =============================================================================
# Clean Target - Remove Pascal-specific files
# =============================================================================
set_directory_properties(PROPERTIES
    ADDITIONAL_MAKE_CLEAN_FILES
        "*.ppu;*.o;*.a;*.or;*.res;*.compiled;*.rsj;*.lps;*.dbg;backup/*;lib/*"
)

# Custom clean target for deep cleaning
add_custom_target(distclean
    COMMAND ${CMAKE_COMMAND} -E remove_directory ${CMAKE_BINARY_DIR}/lib
    COMMAND ${CMAKE_COMMAND} -E remove_directory ${CMAKE_BINARY_DIR}/backup
    COMMAND ${CMAKE_COMMAND} -E remove_directory ${PROJECTS_DIR}/lib
    COMMAND ${CMAKE_COMMAND} -E remove_directory ${PROJECTS_DIR}/backup
    COMMAND ${CMAKE_COMMAND} -E remove *.ppu *.o *.a *.or *.res *.compiled
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
    COMMENT "Deep cleaning all build artifacts"
)

# =============================================================================
# SDL Update Targets
# =============================================================================
if(SDL2_FOUND)
    add_custom_target(update_sdl2
        COMMAND ${CMAKE_COMMAND} -E echo "Updating SDL2 Pascal bindings..."
        COMMAND git -C ${SDL2_DOWNLOAD_DIR} pull
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
        COMMENT "Updating SDL2 Pascal bindings from GitHub"
    )
endif()

if(SDL3_FOUND)
    add_custom_target(update_sdl3
        COMMAND ${CMAKE_COMMAND} -E echo "Updating SDL3 Pascal bindings..."
        COMMAND git -C ${SDL3_DOWNLOAD_DIR} pull
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
        COMMENT "Updating SDL3 Pascal bindings from GitHub"
    )
endif()

# =============================================================================
# Testing Support (Optional)
# =============================================================================
enable_testing()

# Test Realms if it was built
if(TARGET realms)
    add_test(
        NAME test_realms_version
        COMMAND realms --version
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    )
    set_tests_properties(test_realms_version PROPERTIES
        TIMEOUT 10
        LABELS "realms;version"
    )
endif()

# Test console app if it exists
if(TARGET console_app)
    add_test(
        NAME test_console_app
        COMMAND console_app
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    )
endif()

# =============================================================================
# Package Configuration (Optional)
# =============================================================================
set(CPACK_PACKAGE_NAME "Realms")
set(CPACK_PACKAGE_VERSION "${PROJECT_VERSION}")
set(CPACK_PACKAGE_VENDOR "Your Name")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "Realms - A FreePascal/Lazarus Application")
set(CPACK_PACKAGE_DESCRIPTION_FILE "${CMAKE_CURRENT_SOURCE_DIR}/README.md")
set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/LICENSE")
set(CPACK_RESOURCE_FILE_README "${CMAKE_CURRENT_SOURCE_DIR}/README.md")

# Platform-specific package generators
if(WIN32)
    set(CPACK_GENERATOR "ZIP;NSIS")
    set(CPACK_NSIS_DISPLAY_NAME "Realms Application")
    set(CPACK_NSIS_PACKAGE_NAME "Realms")
    set(CPACK_NSIS_ENABLE_UNINSTALL_BEFORE_INSTALL ON)
elseif(APPLE)
    set(CPACK_GENERATOR "DragNDrop;TGZ")
    set(CPACK_DMG_VOLUME_NAME "Realms")
    set(CPACK_DMG_FORMAT "UDZO")
else()
    set(CPACK_GENERATOR "TGZ;DEB;RPM")
    set(CPACK_DEBIAN_PACKAGE_MAINTAINER "Your Name <your.email@example.com>")
    set(CPACK_DEBIAN_PACKAGE_SECTION "games")
    set(CPACK_RPM_PACKAGE_LICENSE "MIT")
    set(CPACK_RPM_PACKAGE_GROUP "Applications/Games")
endif()

# Components
set(CPACK_COMPONENTS_ALL applications debug documentation development)
set(CPACK_COMPONENT_APPLICATIONS_DISPLAY_NAME "Realms Application")
set(CPACK_COMPONENT_DEBUG_DISPLAY_NAME "Debug Binaries")
set(CPACK_COMPONENT_DOCUMENTATION_DISPLAY_NAME "Documentation")
set(CPACK_COMPONENT_DEVELOPMENT_DISPLAY_NAME "Development Files")

include(CPack)

# =============================================================================
# Build Information Summary
# =============================================================================
message(STATUS "")
message(STATUS "=======================================================")
message(STATUS "=== Build Configuration Summary ===")
message(STATUS "=======================================================")
message(STATUS "  Project: ${PROJECT_NAME} ${PROJECT_VERSION}")
message(STATUS "  Platform: ${PLATFORM_NAME}")
message(STATUS "  Build type: ${CMAKE_BUILD_TYPE}")
message(STATUS "  Source dir: ${CMAKE_CURRENT_SOURCE_DIR}")
message(STATUS "  Binary dir: ${CMAKE_CURRENT_BINARY_DIR}")
message(STATUS "  Install prefix: ${CMAKE_INSTALL_PREFIX}")
message(STATUS "")
message(STATUS "=== Realms Project ===")
message(STATUS "  Build method: ${REALMS_BUILD_METHOD}")
if(REALMS_LPI_FOUND)
    message(STATUS "  Lazarus project: FOUND (${REALMS_LPI_FILE})")
else()
    message(STATUS "  Lazarus project: NOT FOUND")
endif()
if(REALMS_LPR_FOUND)
    message(STATUS "  Pascal program: FOUND (${REALMS_LPR_FILE})")
else()
    message(STATUS "  Pascal program: NOT FOUND (checked for .lpr/.dpr)")
endif()
message(STATUS "")
message(STATUS "=== SDL Support ===")
if(SDL2_FOUND)
    message(STATUS "  SDL2: FOUND")
    message(STATUS "    Units: ${SDL2_INCLUDE_DIRS}")
    if(SDL2_VERSION)
        message(STATUS "    Version: ${SDL2_VERSION}")
    endif()
else()
    message(STATUS "  SDL2: NOT FOUND")
endif()
if(SDL3_FOUND)
    message(STATUS "  SDL3: FOUND")
    message(STATUS "    Units: ${SDL3_INCLUDE_DIRS}")
    if(SDL3_VERSION)
        message(STATUS "    Version: ${SDL3_VERSION}")
    endif()
else()
    message(STATUS "  SDL3: NOT FOUND")
endif()
message(STATUS "")
message(STATUS "=== Compiler Configuration ===")
message(STATUS "  FPC: ${FPC_EXECUTABLE}")
message(STATUS "  FPC Version: ${FPC_VERSION}")
message(STATUS "  Common flags: ${FPC_COMMON_FLAGS}")
message(STATUS "  Build flags: ${FPC_BUILD_FLAGS}")
message(STATUS "  Platform flags: ${PLATFORM_FLAGS}")
message(STATUS "")
message(STATUS "=== Available Targets ===")
if(TARGET realms)
    message(STATUS "  ✓ realms - Realms application (Release)")
endif()
if(TARGET realms_debug)
    message(STATUS "  ✓ realms_debug - Realms application (Debug)")
endif()
if(TARGET realms_verbose)
    message(STATUS "  ✓ realms_verbose - Realms application (Verbose)")
endif()
if(TARGET console_app)
    message(STATUS "  ✓ console_app - Simple console application")
endif()
if(TARGET multi_unit_app)
    message(STATUS "  ✓ multi_unit_app - Multi-unit application")
endif()
message(STATUS "")
message(STATUS "=== Usage ===")
message(STATUS "  Configure: cmake -DCMAKE_BUILD_TYPE=Release ..")
message(STATUS "  Build all: cmake --build .")
message(STATUS "  Build specific: cmake --build . --target realms")
message(STATUS "  Install: cmake --install .")
message(STATUS "  Test: ctest --output-on-failure")
message(STATUS "  Package: cpack")
message(STATUS "  Clean: cmake --build . --target distclean")
if(SDL2_FOUND OR SDL3_FOUND)
    message(STATUS "")
    message(STATUS "=== SDL Update Commands ===")
    if(SDL2_FOUND)
        message(STATUS "  Update SDL2: cmake --build . --target update_sdl2")
    endif()
    if(SDL3_FOUND)
        message(STATUS "  Update SDL3: cmake --build . --target update_sdl3")
    endif()
endif()
message(STATUS "=======================================================")
message(STATUS "")


# =============================================================================
# Platform Flags Configuration
# =============================================================================
set(PLATFORM_FLAGS "")

# --- 1. Linux Support (amd64 and arm64) ---
if(CMAKE_SYSTEM_NAME MATCHES "Linux")
    # Determine the target architecture
    if(CMAKE_SYSTEM_PROCESSOR MATCHES "aarch64|arm64")
        set(FPC_TARGET_CPU "aarch64") # For Linux ARM64
        set(PLATFORM_FLAGS "${PLATFORM_FLAGS} -Taarch64")
        message(STATUS "  Targeting Linux ARM64 (-Taarch64)")
    else()
        set(FPC_TARGET_CPU "x86_64") # For Linux AMD64
        set(PLATFORM_FLAGS "${PLATFORM_FLAGS} -Tx86_64")
        message(STATUS "  Targeting Linux AMD64 (-Tx86_64)")
    endif()

# --- 2. macOS Support (Universal or Single-Arch) ---
elseif(APPLE)
    # Universal Binary Support
    if(BUILD_UNIVERSAL_MACOS)
        # FPC's recommended method for multi-architecture targets on Darwin is to use -Xarch.
        set(PLATFORM_FLAGS "${PLATFORM_FLAGS} -Paarch64 -Xarch=x86_64")
        message(STATUS "  Targeting macOS UNIVERSAL (arm64 + amd64) binary.")
    else()
        # Single-Arch Build (Default behavior)
        if(FPC_MACOS_ARCH MATCHES "arm64")
            set(FPC_TARGET_CPU "aarch64")
            set(PLATFORM_FLAGS "${PLATFORM_FLAGS} -Paarch64")
            message(STATUS "  Targeting macOS ARM64 (Apple Silicon) single-architecture.")
        else()
            set(FPC_TARGET_CPU "x86_64")
            set(PLATFORM_FLAGS "${PLATFORM_FLAGS} -Px86_64")
            message(STATUS "  Targeting macOS AMD64 (Intel) single-architecture.")
        endif()
    endif()

    # FIX: FPC Internal Error 200609171 on Darwin
    # This forces DWARF-2 debugging to fix the known compiler bug on macOS.
    set(PLATFORM_FLAGS "${PLATFORM_FLAGS} -Tdarwin -gw2")
    message(STATUS "  Applying fix: Forcing DWARF-2 debug info (-gw2).")
# -----------------------------------------------------------------------------
    
# --- 3. Windows Support (Default - not explicitly requested, but good for completeness) ---
elseif(WIN32)
    set(PLATFORM_FLAGS "${PLATFORM_FLAGS} -Twindows")
    
    # Check for the desired target CPU architecture
    if("${CMAKE_SYSTEM_PROCESSOR}" MATCHES "arm64|aarch64")
        set(FPC_TARGET_CPU "aarch64")
        message(STATUS "  Targeting Windows ARM64 (-Twindows -Paarch64)")
    else()
        # Default to x86_64 for general 64-bit Windows builds
        set(FPC_TARGET_CPU "x86_64")
        message(STATUS "  Targeting Windows x86_64 (-Twindows -Px86_64)")
    endif()
endif()

message(STATUS "  Final PLATFORM_FLAGS: ${PLATFORM_FLAGS}")