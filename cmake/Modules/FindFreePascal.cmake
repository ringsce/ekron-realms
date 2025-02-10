# Find Free Pascal Compiler (FPC)
find_program(FPC_EXECUTABLE fpc
    PATHS
    /usr/bin
    /usr/local/bin
    /opt/homebrew/bin
    /opt/local/bin
    $ENV{FPC_HOME}
    DOC "Path to the Free Pascal Compiler"
)

# If found, set variables
if (FPC_EXECUTABLE)
    get_filename_component(FPC_PATH ${FPC_EXECUTABLE} PATH)
    set(FPC_FOUND TRUE)
    set(FPC_COMPILER ${FPC_EXECUTABLE} CACHE FILEPATH "Free Pascal Compiler")
    message(STATUS "Found Free Pascal Compiler: ${FPC_EXECUTABLE}")
else()
    message(FATAL_ERROR "Free Pascal Compiler (fpc) not found! Install it or set FPC_HOME.")
endif()

mark_as_advanced(FPC_EXECUTABLE)

