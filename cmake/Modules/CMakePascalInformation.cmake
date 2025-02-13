# cmake/Modules/CMakePascalInformation.cmake

# Set FreePascal compiler flags
set(FPC_FLAGS "-S2 -Sh -O2 -XX -Xs")
if (APPLE AND CMAKE_OSX_ARCHITECTURES MATCHES "arm64")
    # Add macOS Silicon-specific flags
    set(FPC_FLAGS "${FPC_FLAGS} -Parm")
endif()

# Define a function to compile Pascal files
function(add_pascal_executable TARGET_NAME SOURCE_FILES)
    set(OUTPUT_PATH "${CMAKE_BINARY_DIR}/${TARGET_NAME}")
    add_custom_command(
        OUTPUT ${OUTPUT_PATH}
        COMMAND ${CMAKE_Pascal_COMPILER} ${FPC_FLAGS} -o${OUTPUT_PATH} ${SOURCE_FILES}
        DEPENDS ${SOURCE_FILES}
        COMMENT "Compiling Pascal executable: ${TARGET_NAME}"
    )
    add_custom_target(${TARGET_NAME}_target ALL DEPENDS ${OUTPUT_PATH})
endfunction()