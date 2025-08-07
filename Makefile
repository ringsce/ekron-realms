# Makefile for Ekron Realms

FPC := fpc
PROJECT := ekron
SRC_ENGINE := engine
SRC_GAME := game
BIN_MAC := bin/macos-arm64
BIN_LINUX := bin/linux-arm64

UNITS := -Fu$(SRC_ENGINE) -Fu$(SRC_GAME)

all: macos linux

macos:
	mkdir -p $(BIN_MAC)
	$(FPC) -MObjFPC -Scgi -O2 -Tdarwin -Parm64 -o$(BIN_MAC)/$(PROJECT) $(PROJECT).lpr $(UNITS)

linux:
	mkdir -p $(BIN_LINUX)
	$(FPC) -MObjFPC -Scgi -O2 -Tlinux -Parm64 -o$(BIN_LINUX)/$(PROJECT) $(PROJECT).lpr $(UNITS)

clean:
	rm -rf bin/* *.o *.ppu *.a

