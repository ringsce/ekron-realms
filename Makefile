# Makefile for ShopWindow

# Compiler and flags
FPC = fpc
SRC = ShopWindow.pas
EXEC = ShopWindowApp

# Compiler flags
FPC_FLAGS = -MObjFPC -Scgi -O2 -g -vewnhi

# SDL configuration
USE_SDL2 = -dUSE_SDL2
USE_SDL3 = -dUSE_SDL3

# Libraries and platform-specific flags
SDL2_LIBS = -lSDL2
SDL3_LIBS = -lSDL3
GL_LIBS = -lGL -lGLU

# Targets
all: sdl2

sdl2: $(SRC)
	$(FPC) $(FPC_FLAGS) $(USE_SDL2) $(SDL2_LIBS) $(GL_LIBS) -o$(EXEC) $(SRC)

sdl3: $(SRC)
	$(FPC) $(FPC_FLAGS) $(USE_SDL3) $(SDL3_LIBS) $(GL_LIBS) -o$(EXEC) $(SRC)

clean:
	rm -f $(EXEC) *.o *.ppu


