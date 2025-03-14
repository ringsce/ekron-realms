# Project name
PROJECT = ekron_realms

# Compiler settings
FPC = fpc
FLAGS = -Mobjfpc -Scgi -vewn -O2 -gl -Xs -k"-dead_strip"

# macOS-specific settings
ifeq ($(shell uname -s), Darwin)
    ARCH = aarch64-darwin
    LCL_PATHS = -Fu/usr/local/share/lazarus/lcl/units/$(ARCH) \
                -Fu/usr/local/share/lazarus/lcl/units/$(ARCH)/cocoa \
                -Fu/usr/local/share/lazarus/packager/units/$(ARCH)
    SYS_PATH = -Fu./mac  # macOS-specific units
    NET_UNIT = net_udp
endif

# Linux ARM64 settings
ifeq ($(shell uname -m), aarch64)
    ifneq ($(filter Linux, $(shell uname -s)),)
        ARCH = aarch64-linux
        LCL_PATHS = -Fu/usr/share/lazarus/lcl/units/$(ARCH) \
                    -Fu/usr/share/lazarus/lcl/units/$(ARCH)/gtk2 \
                    -Fu/usr/share/lazarus/packager/units/$(ARCH)
        SYS_PATH = -Fu./linux  # Linux-specific units
        NET_UNIT = net_udp
    endif
endif

# Directories
PROJECTS_DIR = Projects
GAME_SRC = $(PROJECTS_DIR)/realms.dpr
LAUNCHER_SRC = launcher.pas

# Targets
all: quake2d launcher

# Compile Quake2D game
quake2d:
	$(FPC) $(FLAGS) $(LCL_PATHS) $(SYS_PATH) $(GAME_SRC) -o$(PROJECTS_DIR)/quake2d

# Compile Launcher
launcher:
	$(FPC) $(FLAGS) $(LCL_PATHS) $(SYS_PATH) $(LAUNCHER_SRC) -o$(PROJECT)

# Ensure necessary .pas files exist
check:
	@echo "Checking required source files..."
	@test -f $(GAME_SRC) || (echo "Error: Missing $(GAME_SRC)!" && exit 1)
	@test -f $(LAUNCHER_SRC) || (echo "Error: Missing $(LAUNCHER_SRC)!" && exit 1)
	@ls $(SYS_PATH)/*.pas >/dev/null 2>&1 || (echo "Error: No Pascal files found in $(SYS_PATH)!" && exit 1)

# Cleanup
clean:
	rm -f $(PROJECTS_DIR)/quake2d $(PROJECT) *.o *.ppu *.a *.compiled *.log $(SYS_PATH)/*.o $(SYS_PATH)/*.ppu

# Run Launcher
run: launcher
	./launcher

.PHONY: all clean run quake2d launcher check
