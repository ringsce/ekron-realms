# Project name
PROJECT = ekron_realms

# Compiler settings
FPC = fpc
FLAGS = -Mobjfpc -Scgi -vewn -O2 -gl -Xs -k"-dead_strip"
LCL_PATHS = -Fu/usr/local/share/lazarus/lcl/units/aarch64-darwin \
            -Fu/usr/local/share/lazarus/lcl/units/aarch64-darwin/cocoa \
            -Fu/usr/local/share/lazarus/packager/units/aarch64-darwin

# Directories
PROJECTS_DIR = Projects
GAME_SRC = $(PROJECTS_DIR)/quake2d.dpr
LAUNCHER_SRC = launcher.pas

# Targets
all: quake2d launcher

# Compile Quake2D game
quake2d:
	$(FPC) $(FLAGS) $(GAME_SRC) -o$(PROJECTS_DIR)/quake2d

# Compile Launcher
launcher:
	$(FPC) $(FLAGS) $(LCL_PATHS) $(LAUNCHER_SRC) -o$(PROJECT)

# Cleanup
clean:
	rm -f $(PROJECTS_DIR)/quake2d $(PROJECT) *.o *.ppu *.a *.compiled *.log

# Run Launcher
run: $(PROJECT)
	./$(PROJECT)

.PHONY: all clean run quake2d launcher
