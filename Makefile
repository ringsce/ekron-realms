# Makefile for Ekron Realms (Updated with x86_64 targets and refactored variables)

FPC := fpc
PROJECT := ekron
SRC_ENGINE := engine
SRC_GAME := game

# Define Output Directories
BIN_MAC_ARM := bin/macos-arm64
BIN_LINUX_ARM := bin/linux-arm64
BIN_MAC_X64 := bin/macos-x86_64
BIN_LINUX_X64 := bin/linux-x86_64

# Define Common Compiler Flags
FPC_FLAGS := -MObjFPC -Scgi -O2
UNITS := -Fu$(SRC_ENGINE) -Fu$(SRC_GAME)

# The 'all' target now builds all four versions (arm64 and x86_64 for both OS)
all: macos-arm64 linux-arm64 macos-x86_64 linux-x86_64

# ----------------- ARM64 Targets (Refactored) -----------------
macos-arm64:
	mkdir -p $(BIN_MAC_ARM)
	$(FPC) $(FPC_FLAGS) -Tdarwin -Parm64 -o$(BIN_MAC_ARM)/$(PROJECT) $(PROJECT).lpr $(UNITS)

linux-arm64:
	mkdir -p $(BIN_LINUX_ARM)
	$(FPC) $(FPC_FLAGS) -Tlinux -Parm64 -o$(BIN_LINUX_ARM)/$(PROJECT) $(PROJECT).lpr $(UNITS)

# ----------------- x86_64 Targets (NEW) -----------------
macos-x86_64:
	mkdir -p $(BIN_MAC_X64)
	$(FPC) $(FPC_FLAGS) -Tdarwin -Px86_64 -o$(BIN_MAC_X64)/$(PROJECT) $(PROJECT).lpr $(UNITS)

linux-x86_64:
	mkdir -p $(BIN_LINUX_X64)
	$(FPC) $(FPC_FLAGS) -Tlinux -Px86_64 -o$(BIN_LINUX_X64)/$(PROJECT) $(PROJECT).lpr $(UNITS)

# ----------------- Clean Target -----------------
clean:
	rm -rf bin/* *.o *.ppu *.a