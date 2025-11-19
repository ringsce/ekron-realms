FROM ubuntu:22.04

# Install base build tools and FPC
# Installs FPC and cross-compilation tools for Linux ARM64 (aarch64) and Windows 64-bit (mingw-w64).
# These packages provide the necessary compilers and units for the cross-targets.
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    curl git unzip build-essential \
    fpc binutils-aarch64-linux-gnu binutils-mingw-w64 \
    fp-compiler-windows fp-units-mingw-w64 fp-units-base fp-units-fcl \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Copy source code
COPY . .

# =============================================================
# Build Targets
# =============================================================

# Build Linux ARM64
# FPC Flags: -Tlinux (Target OS) -Parm64 (Target CPU)
RUN mkdir -p output/linux/arm64 && \
    fpc -Mdelphi -Tlinux -Parm64 -O2 -FuProjects/units -ooutput/linux/arm64/realms Projects/realms.lpr

# Build Windows x86_64 (Original 64-bit target, now explicit)
# FPC Flags: -Twin64 (Target OS) -Px86_64 (Target CPU)
RUN mkdir -p output/windows/x86_64 && \
    fpc -Mdelphi -Twin64 -Px86_64 -O2 -FuProjects/units -ooutput/windows/x86_64/realms.exe Projects/realms.lpr

# Build Windows ARM64 (NEWLY ADDED)
# FPC Flags: -Twin64 (Target OS) -Paarch64 (Target CPU)
RUN mkdir -p output/windows/arm64 && \
    fpc -Mdelphi -Twin64 -Paarch64 -O2 -FuProjects/units -ooutput/windows/arm64/realms.exe Projects/realms.lpr

CMD ["bash"]