# Project name
PROJECT = ekron_realms

# Compiler settings
FPC = fpc
FLAGS = -Mobjfpc -Scgi -vewn -O2 -gl -Xs -k"-dead_strip"
LCL_PATHS = -Fu/usr/local/share/lazarus/lcl/units/aarch64-darwin \
            -Fu/usr/local/share/lazarus/lcl/units/aarch64-darwin/cocoa \
            -Fu/usr/local/share/lazarus/packager/units/aarch64-darwin \
            -Fu./mac  # Ensure macOS-specific units are included

# Directories
PROJECTS_DIR = Projects
GAME_SRC = $(PROJECTS_DIR)/quake2d.dpr
LAUNCHER_SRC = launcher.pas

# Targets
all: quake2d launcher

# Compile Quake2D game
quake2d:
	$(FPC) $(FLAGS) -Fu./mac $(GAME_SRC) -o$(PROJECTS_DIR)/quake2d

# Compile Launcher
launcher:
	$(FPC) $(FLAGS) $(LCL_PATHS) -Fu./mac $(LAUNCHER_SRC) -olauncher

# Ensure necessary .pas files exist
check:
	@echo "Checking required source files..."
	@test -f $(GAME_SRC) || (echo "Error: Missing $(GAME_SRC)!" && exit 1)
	@test -f $(LAUNCHER_SRC) || (echo "Error: Missing $(LAUNCHER_SRC)!" && exit 1)
	@ls ./mac/*.pas >/dev/null 2>&1 || (echo "Error: No Pascal files found in ./mac!" && exit 1)

# Cleanup
clean:
	rm -f $(PROJECTS_DIR)/quake2d launcher *.o *.ppu *.a *.compiled *.log ./mac/*.o ./mac/*.ppu

# Run Launcher
run: launcher
	./launcher

.PHONY: all clean run quake2d launcher check
