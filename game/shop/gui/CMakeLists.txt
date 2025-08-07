
add_executable(shop_gui main.cpp)
target_link_libraries(shop_gui Vulkan::Vulkan)
# Link MoltenVK on macOS
if(APPLE)
    find_library(MOLTENVK MoltenVK REQUIRED)
    target_link_libraries(shop_gui ${MOLTENVK})
endif()
