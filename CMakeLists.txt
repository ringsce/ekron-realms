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

    win32/sys_win.pas
    win32/vid_dll.pas
    win32/snd_win.pas
    win32/in_win.pas
    win32/q_shwin.pas
    win32/net_wins.pas
    win32\cd_win.pas
    win32\vid_menu.pas
    win32\ConProc.pas
  sys_linux   in '../linux/sys_linux.pas',
  vid_so      in '../linux/vid_so.pas',
  snd_sdl     in '../linux/snd_sdl.pas',
  in_linux    in '../linux/in_linux.pas',
  q_shlinux   in '../linux/q_shlinux.pas',
  net_udp     in '../linux/net_udp.pas',
  cd_sdl      in '../linux/cd_sdl.pas',
  vid_menu    in '../linux/vid_menu.pas',
  glob        in '../linux/glob.pas',
  rw_linux_h  in '../linux/rw_linux_h.pas',
  sys_mac   in '../mac/sys_mac.pas',
  vid_macos in '../mac/vid_macos.pas',
  snd_mac   in '../mac/snd_mac.pas',
  in_mac    in '../mac/in_mac.pas',
  q_shmac   in '../mac/q_shmac.pas',
  net_udp   in '../mac/net_udp.pas',
  cd_sdl    in '../mac/cd_sdl.pas',
  vid_menu  in '../mac/vid_menu.pas',
  glob      in '../mac/glob.pas',
  rw_mac_h  in '../mac/rw_mac_h.pas',
  MoltenVK  in '../mac/MoltenVK.pas', // Add MoltenVK unit
  Vulkan     in '../vulkan/vulkan.pas',
  VulkanUtils in '../vulkan/VulkanUtils.pas',
  VulkanRender in '../vulkan/VulkanRender.pas',
  kayte in '../kayte/kayte.pas',
  kayteparser in ' ../kayte/kayteparser.pas',
  platformAsm in '../game/platformasm.pas',
  launcher in 'launcher.pas',
  qfiles    in '..\qcommon\qfiles.pas',
  crc       in '..\qcommon\crc.pas',
  CPas      in '..\qcommon\CPas.pas',
  cmd       in '..\qcommon\cmd.pas',
  Common    in '..\qcommon\Common.pas',
  CVar      in '..\qcommon\CVar.pas',
  Files     in '..\qcommon\Files.pas',
  CModel    in '..\qcommon\CModel.pas',
  MD4       in '..\qcommon\MD4.pas',
  PMoveUnit in '..\qcommon\PMoveUnit.pas',
  net_chan  in '..\qcommon\net_chan.pas',
  gzio      in '..\qcommon\gzio.pas',
  Delphi_cdecl_printf in '..\qcommon\Delphi_cdecl_printf.pas',
  q_shared  in '..\game\q_shared.pas',
  m_flash   in '..\game\m_flash.pas',
  GameUnit  in '..\game\GameUnit.pas',
  cl_main   in '..\client\cl_main.pas',
  Client    in '..\client\Client.pas',
  ref       in '..\client\ref.pas',
  menu      in '..\client\menu.pas',
  Sound_h   in '..\client\Sound_h.pas',
  Console   in '..\client\Console.pas',
  cl_scrn   in '..\client\cl_scrn.pas',
  vid_h     in '..\client\vid_h.pas',
  keys      in '..\client\keys.pas',
  snd_loc   in '..\client\snd_loc.pas',
  cl_input  in '..\client\cl_input.pas',
  cl_cin    in '..\client\cl_cin.pas',
  snd_dma   in '..\client\snd_dma.pas',
  cl_ents   in '..\client\cl_ents.pas',
  cl_pred   in '..\client\cl_pred.pas',
  cl_view   in '..\client\cl_view.pas',
  cl_parse  in '..\client\cl_parse.pas',
  Qmenu     in '..\client\Qmenu.pas',
  cl_tent   in '..\client\cl_tent.pas',
  cl_fx     in '..\client\cl_fx.pas',
  cl_newfx  in '..\client\cl_newfx.pas',
  snd_mix   in '..\client\snd_mix.pas',
  snd_mem   in '..\client\snd_mem.pas',
  cl_inv    in '..\client\cl_inv.pas',
  server    in '..\server\server.pas',
  sv_game   in '..\server\sv_game.pas',
  sv_init   in '..\server\sv_init.pas',
  sv_ccmds  in '..\server\sv_ccmds.pas',
  Sv_main   in '..\server\sv_main.pas',
  sv_send   in '..\server\sv_send.pas',
  sv_ents   in '..\server\sv_ents.pas',
  sv_user   in '..\server\sv_user.pas',
  sv_world  in '..\server\sv_world.pas',
  DelphiTypes  in '..\qcommon\DelphiTypes.pas',
  q_shared_add in '..\game\q_shared_add.pas',
  game_add     in '..\game\game_add.pas';
  
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