# FindSDL3.cmake - Find SDL3 for Pascal bindings
# This module fetches and finds SDL3-for-Pascal bindings
#
# Usage:
#   find_package(SDL3)
#
# Variables defined by this module:
#   SDL3_FOUND              - System has SDL3 for Pascal
#   SDL3_PASCAL_DIR         - SDL3 Pascal bindings directory
#   SDL3_INCLUDE_DIRS       - SDL3 Pascal units directory
#   SDL3_LIBRARIES          - SDL3 native libraries (if found)
#   SDL3_VERSION            - Version of SDL3 (if detectable)
#   SDL3_UNITS              - List of SDL3 Pascal unit files

include(FetchContent)

# =============================================================================
# Configuration
# =============================================================================
set(SDL3_PASCAL_REPO "https://github.com/PascalGameDevelopment/SDL3-for-Pascal.git")
set(SDL3_TOOLS_DIR "${CMAKE_SOURCE_DIR}/tools")
set(SDL3_DOWNLOAD_DIR "${SDL3_TOOLS_DIR}/SDL3-for-Pascal")

message(STATUS "Looking for SDL3 Pascal bindings...")

# =============================================================================
# Fetch SDL3-for-Pascal if not already present
# =============================================================================
if(NOT EXISTS "${SDL3_DOWNLOAD_DIR}")
    message(STATUS "SDL3 Pascal bindings not found, fetching from GitHub...")
    
    FetchContent_Declare(
        SDL3_Pascal
        GIT_REPOSITORY ${SDL3_PASCAL_REPO}
        GIT_TAG main
        GIT_SHALLOW TRUE
        SOURCE_DIR ${SDL3_DOWNLOAD_DIR}
    )
    
    FetchContent_MakeAvailable(SDL3_Pascal)
    
    if(EXISTS "${SDL3_DOWNLOAD_DIR}")
        message(STATUS "SDL3 Pascal bindings downloaded successfully")
    else()
        message(FATAL_ERROR "Failed to download SDL3 Pascal bindings")
    endif()
else()
    message(STATUS "SDL3 Pascal bindings found at: ${SDL3_DOWNLOAD_DIR}")
endif()

# =============================================================================
# Detect SDL3 Pascal bindings structure
# =============================================================================
set(SDL3_PASCAL_DIR "${SDL3_DOWNLOAD_DIR}")

# Find units directory - check multiple possible locations
set(POSSIBLE_UNIT_DIRS
    "${SDL3_PASCAL_DIR}/units"
    "${SDL3_PASCAL_DIR}/SDL3"
    "${SDL3_PASCAL_DIR}/src"
    "${SDL3_PASCAL_DIR}/source"
    "${SDL3_PASCAL_DIR}/bindings"
    "${SDL3_PASCAL_DIR}"
)

foreach(dir ${POSSIBLE_UNIT_DIRS})
    if(EXISTS "${dir}/sdl3.pas" OR EXISTS "${dir}/SDL3.pas")
        set(SDL3_INCLUDE_DIRS "${dir}")
        break()
    endif()
endforeach()

# If still not found, search recursively
if(NOT SDL3_INCLUDE_DIRS)
    file(GLOB_RECURSE SDL3_UNIT_FILE "${SDL3_PASCAL_DIR}/sdl3.pas" "${SDL3_PASCAL_DIR}/SDL3.pas")
    if(SDL3_UNIT_FILE)
        list(GET SDL3_UNIT_FILE 0 FIRST_UNIT)
        get_filename_component(SDL3_INCLUDE_DIRS "${FIRST_UNIT}" DIRECTORY)
    endif()
endif()

# =============================================================================
# Find SDL3 Pascal unit files
# =============================================================================
if(SDL3_INCLUDE_DIRS)
    file(GLOB SDL3_UNITS 
        "${SDL3_INCLUDE_DIRS}/*.pas"
        "${SDL3_INCLUDE_DIRS}/*.pp"
    )
    
    list(LENGTH SDL3_UNITS SDL3_UNITS_COUNT)
    message(STATUS "Found ${SDL3_UNITS_COUNT} SDL3 Pascal units in ${SDL3_INCLUDE_DIRS}")
endif()

# =============================================================================
# Find native SDL3 libraries (optional, for linking info)
# =============================================================================
if(WIN32)
    # Windows - look for DLLs in common locations
    find_library(SDL3_LIBRARY
        NAMES SDL3 sdl3
        PATHS
            "${SDL3_PASCAL_DIR}/lib/win32"
            "${SDL3_PASCAL_DIR}/lib/win64"
            "${SDL3_PASCAL_DIR}/lib/x86"
            "${SDL3_PASCAL_DIR}/lib/x64"
            "${SDL3_PASCAL_DIR}/bin"
            "C:/SDL3/lib"
            "$ENV{PROGRAMFILES}/SDL3/lib"
        PATH_SUFFIXES lib bin
    )
    
    # Find DLL files
    find_file(SDL3_DLL
        NAMES SDL3.dll
        PATHS
            "${SDL3_PASCAL_DIR}/lib/win32"
            "${SDL3_PASCAL_DIR}/lib/win64"
            "${SDL3_PASCAL_DIR}/bin"
            "C:/SDL3/bin"
        PATH_SUFFIXES bin
    )
    
elseif(APPLE)
    # macOS - look for frameworks or dylibs
    find_library(SDL3_LIBRARY
        NAMES SDL3
        PATHS
            "${SDL3_PASCAL_DIR}/lib/macos"
            "${SDL3_PASCAL_DIR}/lib"
            /Library/Frameworks
            /usr/local/lib
            /opt/homebrew/lib
            /opt/local/lib
        PATH_SUFFIXES lib
    )
    
else()
    # Linux - look for shared libraries
    find_library(SDL3_LIBRARY
        NAMES SDL3 sdl3
        PATHS
            "${SDL3_PASCAL_DIR}/lib/linux"
            "${SDL3_PASCAL_DIR}/lib"
            /usr/lib
            /usr/local/lib
            /usr/lib/x86_64-linux-gnu
            /usr/lib/aarch64-linux-gnu
        PATH_SUFFIXES lib lib64
    )
endif()

if(SDL3_LIBRARY)
    set(SDL3_LIBRARIES ${SDL3_LIBRARY})
    message(STATUS "Found SDL3 native library: ${SDL3_LIBRARY}")
else()
    message(STATUS "SDL3 native library not found (Pascal bindings will use dynamic loading)")
endif()

# =============================================================================
# Try to detect SDL3 version
# =============================================================================
if(SDL3_INCLUDE_DIRS)
    # Try to find version in Pascal unit file
    if(EXISTS "${SDL3_INCLUDE_DIRS}/sdl3.pas")
        file(STRINGS "${SDL3_INCLUDE_DIRS}/sdl3.pas" SDL3_VERSION_LINE
            REGEX "SDL_MAJOR_VERSION.*=.*[0-9]"
            LIMIT_COUNT 1
        )
        if(SDL3_VERSION_LINE)
            string(REGEX MATCH "[0-9]+" SDL3_VERSION_MAJOR "${SDL3_VERSION_LINE}")
        endif()
        
        file(STRINGS "${SDL3_INCLUDE_DIRS}/sdl3.pas" SDL3_VERSION_LINE
            REGEX "SDL_MINOR_VERSION.*=.*[0-9]"
            LIMIT_COUNT 1
        )
        if(SDL3_VERSION_LINE)
            string(REGEX MATCH "[0-9]+" SDL3_VERSION_MINOR "${SDL3_VERSION_LINE}")
        endif()
        
        file(STRINGS "${SDL3_INCLUDE_DIRS}/sdl3.pas" SDL3_VERSION_LINE
            REGEX "SDL_PATCHLEVEL.*=.*[0-9]"
            LIMIT_COUNT 1
        )
        if(SDL3_VERSION_LINE)
            string(REGEX MATCH "[0-9]+" SDL3_VERSION_PATCH "${SDL3_VERSION_LINE}")
        endif()
        
        if(SDL3_VERSION_MAJOR AND SDL3_VERSION_MINOR)
            if(SDL3_VERSION_PATCH)
                set(SDL3_VERSION "${SDL3_VERSION_MAJOR}.${SDL3_VERSION_MINOR}.${SDL3_VERSION_PATCH}")
            else()
                set(SDL3_VERSION "${SDL3_VERSION_MAJOR}.${SDL3_VERSION_MINOR}")
            endif()
        endif()
    endif()
endif()

# =============================================================================
# Handle standard find_package arguments
# =============================================================================
include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(SDL3
    REQUIRED_VARS SDL3_PASCAL_DIR SDL3_INCLUDE_DIRS
    VERSION_VAR SDL3_VERSION
    FAIL_MESSAGE "Could not find SDL3 Pascal bindings. Check your internet connection."
)

mark_as_advanced(
    SDL3_PASCAL_DIR
    SDL3_INCLUDE_DIRS
    SDL3_LIBRARY
)

# =============================================================================
# Print information if found
# =============================================================================
if(SDL3_FOUND)
    message(STATUS "")
    message(STATUS "=== SDL3 for Pascal ===")
    message(STATUS "  Bindings directory: ${SDL3_PASCAL_DIR}")
    message(STATUS "  Units directory: ${SDL3_INCLUDE_DIRS}")
    message(STATUS "  Number of units: ${SDL3_UNITS_COUNT}")
    if(SDL3_VERSION)
        message(STATUS "  Version: ${SDL3_VERSION}")
    else()
        message(STATUS "  Version: 3.x (SDL3 Preview/Development)")
    endif()
    if(SDL3_LIBRARIES)
        message(STATUS "  Native library: ${SDL3_LIBRARIES}")
    endif()
    if(SDL3_DLL)
        message(STATUS "  SDL3 DLL: ${SDL3_DLL}")
    endif()
    message(STATUS "")
endif()

# =============================================================================
# Helper function to add SDL3 to FPC projects
# =============================================================================
function(target_link_sdl3 target_name)
    if(NOT SDL3_FOUND)
        message(FATAL_ERROR "SDL3 not found. Cannot link to target ${target_name}")
    endif()
    
    # This function can be used to document that a target uses SDL3
    # The actual linking is handled by FPC through unit paths
    set_property(TARGET ${target_name} 
        APPEND PROPERTY SDL3_ENABLED TRUE
    )
    
    # Store SDL3 units path for the target
    set_property(TARGET ${target_name}
        APPEND PROPERTY SDL3_UNITS_PATH "${SDL3_INCLUDE_DIRS}"
    )
    
    message(STATUS "Target ${target_name} will use SDL3 Pascal bindings")
endfunction()

# =============================================================================
# Helper function to check SDL2/SDL3 compatibility
# =============================================================================
function(check_sdl_compatibility)
    if(SDL2_FOUND AND SDL3_FOUND)
        message(STATUS "")
        message(STATUS "=== SDL Version Notice ===")
        message(WARNING "Both SDL2 and SDL3 bindings are available")
        message(STATUS "SDL2 and SDL3 are NOT binary compatible")
        message(STATUS "Make sure your code uses the correct version")
        message(STATUS "SDL2 units: ${SDL2_INCLUDE_DIRS}")
        message(STATUS "SDL3 units: ${SDL3_INCLUDE_DIRS}")
        message(STATUS "")
    endif()
endfunction()

# =============================================================================
# Update CMake module for easy download updates
# =============================================================================
function(update_sdl3_bindings)
    message(STATUS "Updating SDL3 Pascal bindings...")
    
    if(EXISTS "${SDL3_DOWNLOAD_DIR}/.git")
        execute_process(
            COMMAND git pull
            WORKING_DIRECTORY ${SDL3_DOWNLOAD_DIR}
            RESULT_VARIABLE GIT_RESULT
            OUTPUT_VARIABLE GIT_OUTPUT
            ERROR_VARIABLE GIT_ERROR
        )
        
        if(GIT_RESULT EQUAL 0)
            message(STATUS "SDL3 Pascal bindings updated successfully")
            message(STATUS "${GIT_OUTPUT}")
        else()
            message(WARNING "Failed to update SDL3 Pascal bindings: ${GIT_ERROR}")
        endif()
    else()
        message(WARNING "SDL3 directory is not a git repository. Cannot update.")
        message(STATUS "Delete ${SDL3_DOWNLOAD_DIR} and reconfigure to re-download")
    endif()
endfunction()

# =============================================================================
# Platform-specific installation hints
# =============================================================================
if(SDL3_FOUND AND NOT SDL3_LIBRARIES)
    message(STATUS "")
    message(STATUS "=== SDL3 Runtime Installation ===")
    message(STATUS "NOTE: SDL3 is currently in preview/development")
    
    if(WIN32)
        message(STATUS "On Windows, you need SDL3.dll at runtime.")
        message(STATUS "Download SDL3 preview from: https://github.com/libsdl-org/SDL/releases")
        message(STATUS "Look for preview/development releases")
        message(STATUS "Place SDL3.dll next to your executable or in System32")
    elseif(APPLE)
        message(STATUS "On macOS, build SDL3 from source or use preview releases:")
        message(STATUS "  git clone https://github.com/libsdl-org/SDL.git")
        message(STATUS "  cd SDL && mkdir build && cd build")
        message(STATUS "  cmake .. && make && sudo make install")
        message(STATUS "Or check Homebrew for SDL3 when available:")
        message(STATUS "  brew install sdl3 (when released)")
    else()
        message(STATUS "On Linux, build SDL3 from source:")
        message(STATUS "  git clone https://github.com/libsdl-org/SDL.git")
        message(STATUS "  cd SDL && mkdir build && cd build")
        message(STATUS "  cmake .. && make && sudo make install")
        message(STATUS "Or check your package manager for SDL3 when available:")
        message(STATUS "  Ubuntu/Debian: sudo apt install libsdl3-dev (when released)")
        message(STATUS "  Fedora: sudo dnf install SDL3-devel (when released)")
        message(STATUS "  Arch: sudo pacman -S sdl3 (when released)")
    endif()
    
    message(STATUS "")
    message(STATUS "SDL3 Resources:")
    message(STATUS "  GitHub: https://github.com/libsdl-org/SDL")
    message(STATUS "  Migration Guide: https://github.com/libsdl-org/SDL/blob/main/docs/README-migration.md")
    message(STATUS "")
endif()

# =============================================================================
# SDL3 Migration Helper
# =============================================================================
function(sdl3_migration_info)
    message(STATUS "")
    message(STATUS "=== SDL2 to SDL3 Migration Notes ===")
    message(STATUS "Major changes in SDL3:")
    message(STATUS "  - Improved GPU API")
    message(STATUS "  - Better gamepad support")
    message(STATUS "  - Streamlined audio API")
    message(STATUS "  - Enhanced window management")
    message(STATUS "  - Many functions renamed for consistency")
    message(STATUS "")
    message(STATUS "For full migration guide, see:")
    message(STATUS "  https://github.com/libsdl-org/SDL/blob/main/docs/README-migration.md")
    message(STATUS "")
endfunction()