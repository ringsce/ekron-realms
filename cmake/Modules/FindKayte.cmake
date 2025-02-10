# Find Kayte Compiler
find_program(KAYTE_EXECUTABLE kayte
    PATHS
    /usr/bin
    /usr/local/bin
    /opt/homebrew/bin
    $ENV{KAYTE_HOME}
    DOC "Path to the Kayte Compiler"
)

if (KAYTE_EXECUTABLE)
    get_filename_component(KAYTE_PATH ${KAYTE_EXECUTABLE} PATH)
    set(KAYTE_FOUND TRUE)
    set(KAYTE_COMPILER ${KAYTE_EXECUTABLE} CACHE FILEPATH "Kayte Compiler")
    message(STATUS "Found Kayte Compiler: ${KAYTE_EXECUTABLE}")
else()
    message(FATAL_ERROR "Kayte Compiler not found! Install it or set KAYTE_HOME.")
endif()

mark_as_advanced(KAYTE_EXECUTABLE)
