# cmake/Modules/CMakeDeterminePascalCompiler.cmake

# Find the FreePascal compiler
find_program(FPC_COMPILER fpc
    PATHS
        /usr/local/bin
        /opt/homebrew/bin  # Homebrew on Apple Silicon
        /usr/bin
    DOC "FreePascal Compiler"
)

if (FPC_COMPILER)
    # Set the Pascal compiler
    set(CMAKE_Pascal_COMPILER "${FPC_COMPILER}" CACHE FILEPATH "FreePascal Compiler")
    set(CMAKE_Pascal_COMPILER_ID "FreePascal")
    message(STATUS "Found FreePascal compiler: ${FPC_COMPILER}")
else()
    message(FATAL_ERROR "FreePascal compiler (fpc) not found. Please install FreePascal.")
endif()