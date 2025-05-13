FROM ubuntu:22.04

# Install essentials
RUN apt-get update && \
    apt-get install -y curl git unzip build-essential \
    fpc binutils-aarch64-linux-gnu binutils-mingw-w64 \
    fp-compiler-windows fp-units-mingw-w64 fp-units-base fp-units-fcl

WORKDIR /app
COPY . .

# Build Linux ARM64
RUN mkdir -p output/linux && \
    fpc -Mdelphi -Tlinux -Parm64 -O2 -FuProjects/units -ooutput/linux/realms Projects/realms.dpr

# Build Windows 64-bit
RUN mkdir -p output/windows && \
    fpc -Mdelphi -Twin64 -O2 -FuProjects/units -ooutput/windows/realms.exe Projects/realms.dpr

CMD ["bash"]

