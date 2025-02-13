# Makefile for ShopWindow and ekron_realms (Cross-Platform)

# Compilers
FPC = fpc
CXX = g++

# Detect the operating system
ifeq ($(OS),Windows_NT)
    PLATFORM = Windows
else
    PLATFORM = $(shell uname -s)
endif

# Platform-specific configurations
ifeq ($(PLATFORM),Windows)
    # Windows settings
    FPC_FLAGS = -MObjFPC -Scgi -O2 -g -vewnhi
    FPC_FLAGS_EKRON = -S2 -Sh -O2 -XX -Xs
    CXX_FLAGS = -std=c++17 -O2 -Wall -Wextra -static
    SDL2_LIBS = -lSDL2 -lSDL2main -lopengl32 -lglu32
    SDL3_LIBS = -lSDL3 -lopengl32 -lglu32
    EXE_EXT = .exe
else ifeq ($(PLATFORM),Darwin)
    # macOS settings
    FPC_FLAGS = -MObjFPC -Scgi -O2 -g -vewnhi -Parm
    FPC_FLAGS_EKRON = -S2 -Sh -O2 -XX -Xs -Parm
    CXX_FLAGS = -std=c++17 -O2 -Wall -Wextra -arch arm64
    SDL2_LIBS = -lSDL2 -framework OpenGL -framework GLUT
    SDL3_LIBS = -lSDL3 -framework OpenGL -framework GLUT
    EXE_EXT =
else
    # Linux settings
    FPC_FLAGS = -MObjFPC -Scgi -O2 -g -vewnhi
    FPC_FLAGS_EKRON = -S2 -Sh -O2 -XX -Xs
    CXX_FLAGS = -std=c++17 -O2 -Wall -Wextra
    SDL2_LIBS = -lSDL2 -lGL -lGLU
    SDL3_LIBS = -lSDL3 -lGL -lGLU
    EXE_EXT =
endif

# Directories
SRC_DIR = src
OBJ_DIR = obj
BIN_DIR = bin

# Source files for ekron_realms
PASCAL_SOURCES = $(wildcard $(SRC_DIR)/*.pas) $(wildcard $(SRC_DIR)/*/*.pas)
CPP_SOURCES = $(wildcard $(SRC_DIR)/*.cpp) $(wildcard $(SRC_DIR)/*/*.cpp)

# Object files for ekron_realms
PASCAL_OBJS = $(patsubst $(SRC_DIR)/%.pas, $(OBJ_DIR)/%.o, $(PASCAL_SOURCES))
CPP_OBJS = $(patsubst $(SRC_DIR)/%.cpp, $(OBJ_DIR)/%.o, $(CPP_SOURCES))

# Executable for ekron_realms
EKRON_TARGET = $(BIN_DIR)/ekron_realms$(EXE_EXT)

# Source file for ShopWindow
SHOPWINDOW_SRC = ShopWindow.pas
SHOPWINDOW_EXEC = $(BIN_DIR)/ShopWindowApp$(EXE_EXT)

# SDL configuration
USE_SDL2 = -dUSE_SDL2
USE_SDL3 = -dUSE_SDL3

# Default target
all: ekron_realms shopwindow_sdl2

# Build ekron_realms
ekron_realms: $(EKRON_TARGET)

# Link the final executable for ekron_realms
$(EKRON_TARGET): $(PASCAL_OBJS) $(CPP_OBJS)
	@mkdir -p $(BIN_DIR)
	$(CXX) $(CXX_FLAGS) $^ -o $@

# Compile Pascal files for ekron_realms
$(OBJ_DIR)/%.o: $(SRC_DIR)/%.pas
	@mkdir -p $(dir $@)
	$(FPC) $(FPC_FLAGS_EKRON) -o$@ $<

# Compile C++ files for ekron_realms
$(OBJ_DIR)/%.o: $(SRC_DIR)/%.cpp
	@mkdir -p $(dir $@)
	$(CXX) $(CXX_FLAGS) -c $< -o $@

# Build ShopWindow with SDL2
shopwindow_sdl2: $(SHOPWINDOW_EXEC)

# Compile ShopWindow with SDL2
$(SHOPWINDOW_EXEC): $(SHOPWINDOW_SRC)
	@mkdir -p $(BIN_DIR)
	$(FPC) $(FPC_FLAGS) $(USE_SDL2) $(SDL2_LIBS) -o$@ $<

# Build ShopWindow with SDL3
shopwindow_sdl3: $(SHOPWINDOW_EXEC)

# Compile ShopWindow with SDL3
$(SHOPWINDOW_EXEC): $(SHOPWINDOW_SRC)
	@mkdir -p $(BIN_DIR)
	$(FPC) $(FPC_FLAGS) $(USE_SDL3) $(SDL3_LIBS) -o$@ $<

# Clean build artifacts
clean:
	rm -rf $(OBJ_DIR) $(BIN_DIR)

.PHONY: all ekron_realms shopwindow_sdl2 shopwindow_sdl3 clean