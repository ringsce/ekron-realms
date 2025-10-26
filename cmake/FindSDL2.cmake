# FindSDL2.cmake - Find SDL2 for Pascal bindings
# This module fetches and finds SDL2-for-Pascal bindings
#
# Usage:
#   find_package(SDL2)
#
# Variables defined by this module:
#   SDL2_FOUND              - System has SDL2 for Pascal
#   SDL2_PASCAL_DIR         - SDL2 Pascal bindings directory
#   SDL2_INCLUDE_DIRS       - SDL2 Pascal units directory
#   SDL2_LIBRARIES          - SDL2 native libraries (if found)
#   SDL2_VERSION            - Version of SDL2 (if detectable)
#   SDL2_UNITS              - List of SDL2 Pascal unit files

include(FetchContent)

# =============================================================================
# Configuration
# =============================================================================
set(SDL2_PASCAL_REPO "https://github.com/PascalGameDevelopment/SDL2-for-Pascal.git")
set(SDL2_TOOLS_DIR "${CMAKE_SOURCE_DIR}/tools")
set(SDL2_DOWNLOAD_DIR "${SDL2_TOOLS_DIR}/SDL2-for-Pascal")

message(STATUS "Looking for SDL2 Pascal bindings...")

# =============================================================================
# Fetch SDL2-for-Pascal if not already present
# =============================================================================
if(NOT EXISTS "${SDL2_DOWNLOAD_DIR}")
    message(STATUS "SDL2 Pascal bindings not found, fetching from GitHub...")
    
    FetchContent_Declare(
        SDL2_Pascal
        GIT_REPOSITORY ${SDL2_PASCAL_REPO}
        GIT_TAG main
        GIT_SHALLOW TRUE
        SOURCE_DIR ${SDL2_DOWNLOAD_DIR}
    )
    
    FetchContent_MakeAvailable(SDL2_Pascal)
    
    if(EXISTS "${SDL2_DOWNLOAD_DIR}")
        message(STATUS "SDL2 Pascal bindings downloaded successfully")
    else()
        message(FATAL_ERROR "Failed to download SDL2 Pascal bindings")
    endif()
else()
    message(STATUS "SDL2 Pascal bindings found at: ${SDL2_DOWNLOAD_DIR}")
endif()

# =============================================================================
# Detect SDL2 Pascal bindings structure
# =============================================================================
set(SDL2_PASCAL_DIR "${SDL2_DOWNLOAD_DIR}")

# Find units directory - check multiple possible locations
set(POSSIBLE_UNIT_DIRS
    "${SDL2_PASCAL_DIR}/units"
    "${SDL2_PASCAL_DIR}/SDL2"
    "${SDL2_PASCAL_DIR}/src"
    "${SDL2_PASCAL_DIR}"
)

foreach(dir ${POSSIBLE_UNIT_DIRS})
    if(EXISTS "${dir}/sdl2.pas" OR EXISTS "${dir}/SDL2.pas")
        set(SDL2_INCLUDE_DIRS "${dir}")
        break()
    endif()
endforeach()

# If still not found, search recursively
if(NOT SDL2_INCLUDE_DIRS)
    file(GLOB_RECURSE SDL2_UNIT_FILE "${SDL2_PASCAL_DIR}/sdl2.pas" "${SDL2_PASCAL_DIR}/SDL2.pas")
    if(SDL2_UNIT_FILE)
        list(GET SDL2_UNIT_FILE 0 FIRST_UNIT)
        get_filename_component(SDL2_INCLUDE_DIRS "${FIRST_UNIT}" DIRECTORY)
    endif()
endif()

# =============================================================================
# Find SDL2 Pascal unit files
# =============================================================================
if(SDL2_INCLUDE_DIRS)
    file(GLOB SDL2_UNITS 
        "${SDL2_INCLUDE_DIRS}/*.pas"
        "${SDL2_INCLUDE_DIRS}/*.pp"
    )
    
    list(LENGTH SDL2_UNITS SDL2_UNITS_COUNT)
    message(STATUS "Found ${SDL2_UNITS_COUNT} SDL2 Pascal units in ${SDL2_INCLUDE_DIRS}")
endif()

# =============================================================================
# Find native SDL2 libraries (optional, for linking info)
# =============================================================================
if(WIN32)
    # Windows - look for DLLs in common locations
    find_library(SDL2_LIBRARY
        NAMES SDL2 sdl2
        PATHS
            "${SDL2_PASCAL_DIR}/lib/win32"
            "${SDL2_PASCAL_DIR}/lib/win64"
            "${SDL2_PASCAL_DIR}/lib/x86"
            "${SDL2_PASCAL_DIR}/lib/x64"
            "${SDL2_PASCAL_DIR}/bin"
            "C:/SDL2/lib"
            "$ENV{PROGRAMFILES}/SDL2/lib"
        PATH_SUFFIXES lib bin
    )
    
    # Find DLL files
    find_file(SDL2_DLL
        NAMES SDL2.dll
        PATHS
            "${SDL2_PASCAL_DIR}/lib/win32"
            "${SDL2_PASCAL_DIR}/lib/win64"
            "${SDL2_PASCAL_DIR}/bin"
            "C:/SDL2/bin"
        PATH_SUFFIXES bin
    )
    
elseif(APPLE)
    # macOS - look for frameworks or dylibs
    find_library(SDL2_LIBRARY
        NAMES SDL2
        PATHS
            "${SDL2_PASCAL_DIR}/lib/macos"
            "${SDL2_PASCAL_DIR}/lib"
            /Library/Frameworks
            /usr/local/lib
            /opt/homebrew/lib
            /opt/local/lib
        PATH_SUFFIXES lib
    )
    
else()
    # Linux - look for shared libraries
    find_library(SDL2_LIBRARY
        NAMES SDL2 sdl2
        PATHS
            "${SDL2_PASCAL_DIR}/lib/linux"
            "${SDL2_PASCAL_DIR}/lib"
            /usr/lib
            /usr/local/lib
            /usr/lib/x86_64-linux-gnu
            /usr/lib/aarch64-linux-gnu
        PATH_SUFFIXES lib lib64
    )
endif()

if(SDL2_LIBRARY)
    set(SDL2_LIBRARIES ${SDL2_LIBRARY})
    message(STATUS "Found SDL2 native library: ${SDL2_LIBRARY}")
else()
    message(STATUS "SDL2 native library not found (Pascal bindings will use dynamic loading)")
endif()

# =============================================================================
# Try to detect SDL2 version
# =============================================================================
if(SDL2_INCLUDE_DIRS)
    # Try to find version in Pascal unit file
    if(EXISTS "${SDL2_INCLUDE_DIRS}/sdl2.pas")
        file(STRINGS "${SDL2_INCLUDE_DIRS}/sdl2.pas" SDL2_VERSION_LINE
            REGEX "SDL_MAJOR_VERSION.*=.*[0-9]"
            LIMIT_COUNT 1
        )
        if(SDL2_VERSION_LINE)
            string(REGEX MATCH "[0-9]+" SDL2_VERSION_MAJOR "${SDL2_VERSION_LINE}")
        endif()
        
        file(STRINGS "${SDL2_INCLUDE_DIRS}/sdl2.pas" SDL2_VERSION_LINE
            REGEX "SDL_MINOR_VERSION.*=.*[0-9]"
            LIMIT_COUNT 1
        )
        if(SDL2_VERSION_LINE)
            string(REGEX MATCH "[0-9]+" SDL2_VERSION_MINOR "${SDL2_VERSION_LINE}")
        endif()
        
        if(SDL2_VERSION_MAJOR AND SDL2_VERSION_MINOR)
            set(SDL2_VERSION "${SDL2_VERSION_MAJOR}.${SDL2_VERSION_MINOR}")
        endif()
    endif()
endif()

# =============================================================================
# Handle standard find_package arguments
# =============================================================================
include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(SDL2
    REQUIRED_VARS SDL2_PASCAL_DIR SDL2_INCLUDE_DIRS
    VERSION_VAR SDL2_VERSION
    FAIL_MESSAGE "Could not find SDL2 Pascal bindings. Check your internet connection."
)

mark_as_advanced(
    SDL2_PASCAL_DIR
    SDL2_INCLUDE_DIRS
    SDL2_LIBRARY
)

# =============================================================================
# Print information if found
# =============================================================================
if(SDL2_FOUND)
    message(STATUS "")
    message(STATUS "=== SDL2 for Pascal ===")
    message(STATUS "  Bindings directory: ${SDL2_PASCAL_DIR}")
    message(STATUS "  Units directory: ${SDL2_INCLUDE_DIRS}")
    message(STATUS "  Number of units: ${SDL2_UNITS_COUNT}")
    if(SDL2_VERSION)
        message(STATUS "  Version: ${SDL2_VERSION}")
    endif()
    if(SDL2_LIBRARIES)
        message(STATUS "  Native library: ${SDL2_LIBRARIES}")
    endif()
    if(SDL2_DLL)
        message(STATUS "  SDL2 DLL: ${SDL2_DLL}")
    endif()
    message(STATUS "")
endif()

# =============================================================================
# Helper function to add SDL2 to FPC projects
# =============================================================================
function(target_link_sdl2 target_name)
    if(NOT SDL2_FOUND)
        message(FATAL_ERROR "SDL2 not found. Cannot link to target ${target_name}")
    endif()
    
    # This function can be used to document that a target uses SDL2
    # The actual linking is handled by FPC through unit paths
    set_property(TARGET ${target_name} 
        APPEND PROPERTY SDL2_ENABLED TRUE
    )
    
    # Store SDL2 units path for the target
    set_property(TARGET ${target_name}
        APPEND PROPERTY SDL2_UNITS_PATH "${SDL2_INCLUDE_DIRS}"
    )
    
    message(STATUS "Target ${target_name} will use SDL2 Pascal bindings")
endfunction()

# =============================================================================
# Update CMake module for easy download updates
# =============================================================================
function(update_sdl2_bindings)
    message(STATUS "Updating SDL2 Pascal bindings...")
    
    if(EXISTS "${SDL2_DOWNLOAD_DIR}/.git")
        execute_process(
            COMMAND git pull
            WORKING_DIRECTORY ${SDL2_DOWNLOAD_DIR}
            RESULT_VARIABLE GIT_RESULT
            OUTPUT_VARIABLE GIT_OUTPUT
            ERROR_VARIABLE GIT_ERROR
        )
        
        if(GIT_RESULT EQUAL 0)
            message(STATUS "SDL2 Pascal bindings updated successfully")
        else()
            message(WARNING "Failed to update SDL2 Pascal bindings: ${GIT_ERROR}")
        endif()
    else()
        message(WARNING "SDL2 directory is not a git repository. Cannot update.")
        message(STATUS "Delete ${SDL2_DOWNLOAD_DIR} and reconfigure to re-download")
    endif()
endfunction()

# =============================================================================
# Platform-specific installation hints
# =============================================================================
if(SDL2_FOUND AND NOT SDL2_LIBRARIES)
    message(STATUS "")
    message(STATUS "=== SDL2 Runtime Installation ===")
    
    if(WIN32)
        message(STATUS "On Windows, you need SDL2.dll at runtime.")
        message(STATUS "Download from: https://github.com/libsdl-org/SDL/releases")
        message(STATUS "Place SDL2.dll next to your executable or in System32")
    elseif(APPLE)
        message(STATUS "On macOS, install SDL2 via Homebrew:")
        message(STATUS "  brew install sdl2")
    else()
        message(STATUS "On Linux, install SDL2 via package manager:")
        message(STATUS "  Ubuntu/Debian: sudo apt install libsdl2-2.0-0 libsdl2-dev")
        message(STATUS "  Fedora: sudo dnf install SDL2 SDL2-devel")
        message(STATUS "  Arch: sudo pacman -S sdl2")
    endif()
    
    message(STATUS "")
endif()