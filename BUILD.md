# Building Realms

This document describes how to build the Realms project using CMake on Windows, macOS, and Linux.

## Table of Contents

- [Prerequisites](#prerequisites)
- [Quick Start](#quick-start)
- [Platform-Specific Instructions](#platform-specific-instructions)
  - [macOS](#macos)
  - [Windows 11](#windows-11)
  - [Linux](#linux)
- [Build Types](#build-types)
- [Build Targets](#build-targets)
- [Advanced Configuration](#advanced-configuration)
- [Troubleshooting](#troubleshooting)

---

## Prerequisites

### Required

- **CMake** 3.16 or higher
- **Free Pascal Compiler (FPC)** 3.2.0 or higher
- **Lazarus IDE** 2.0.0 or higher (optional, but recommended)

### Installation

#### macOS

```bash
# Using Homebrew
brew install cmake
brew install fpc
brew install lazarus

# Or using MacPorts
sudo port install cmake
sudo port install fpc
sudo port install lazarus
```

#### Windows 11

1. Download and install [CMake](https://cmake.org/download/)
2. Download and install [Lazarus](https://www.lazarus-ide.org/index.php?page=downloads) (includes FPC)
   - Recommended: Use the 64-bit installer
   - Default location: `C:\lazarus`

Alternatively, install FPC standalone from [FreePascal.org](https://www.freepascal.org/download.html)

#### Linux (Debian/Ubuntu)

```bash
sudo apt update
sudo apt install cmake build-essential
sudo apt install fpc
sudo apt install lazarus

# Or from source
# https://www.freepascal.org/download.html
```

#### Linux (Fedora/RHEL)

```bash
sudo dnf install cmake gcc
sudo dnf install fpc
sudo dnf install lazarus
```

#### Linux (Arch)

```bash
sudo pacman -S cmake
sudo pacman -S fpc
sudo pacman -S lazarus
```

---

## Quick Start

```bash
# 1. Clone the repository (or navigate to project directory)
cd /path/to/realms

# 2. Create build directory
mkdir build
cd build

# 3. Configure the project
cmake -DCMAKE_BUILD_TYPE=Release ..

# 4. Build
cmake --build .

# 5. Run
./realms          # Linux/macOS
.\realms.exe      # Windows
```

---

## Platform-Specific Instructions

### macOS

#### Intel Macs

```bash
mkdir build && cd build
cmake -DCMAKE_BUILD_TYPE=Release ..
cmake --build .
./realms
```

#### Apple Silicon (M1/M2/M3/M4)

```bash
mkdir build && cd build

# CMake will automatically detect ARM64 architecture
cmake -DCMAKE_BUILD_TYPE=Release ..
cmake --build .
./realms
```

**Note**: If you have both Intel and Apple Silicon FPC installed, CMake will use the native architecture by default.

#### Creating macOS App Bundle (Optional)

```bash
# Build and package
cmake --build .
cpack -G DragNDrop

# This creates a .dmg file in the build directory
```

### Windows 11

#### Using Command Prompt

```cmd
REM Create build directory
mkdir build
cd build

REM Configure (64-bit)
cmake -G "MinGW Makefiles" -DCMAKE_BUILD_TYPE=Release ..

REM Or using Visual Studio generator
cmake -G "Visual Studio 17 2022" -DCMAKE_BUILD_TYPE=Release ..

REM Build
cmake --build . --config Release

REM Run
realms.exe
```

#### Using PowerShell

```powershell
# Create build directory
New-Item -ItemType Directory -Force -Path build
Set-Location build

# Configure
cmake -DCMAKE_BUILD_TYPE=Release ..

# Build
cmake --build . --config Release

# Run
.\realms.exe
```

#### Using Developer Command Prompt

If you have Visual Studio installed:

```cmd
mkdir build && cd build
cmake -G "NMake Makefiles" -DCMAKE_BUILD_TYPE=Release ..
nmake
realms.exe
```

#### Creating Windows Installer

```cmd
cmake --build . --config Release
cpack -G NSIS

REM This creates an installer .exe in the build directory
```

### Linux

#### Standard Build

```bash
mkdir build && cd build
cmake -DCMAKE_BUILD_TYPE=Release ..
cmake --build . -j$(nproc)  # Use all CPU cores
./realms
```

#### Install System-Wide

```bash
mkdir build && cd build
cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=/usr/local ..
cmake --build .
sudo cmake --install .

# Now you can run from anywhere
realms
```

#### Creating Linux Packages

```bash
# DEB package (Debian/Ubuntu)
cmake --build .
cpack -G DEB

# RPM package (Fedora/RHEL)
cpack -G RPM

# TGZ archive
cpack -G TGZ
```

---

## Build Types

The project supports three build types:

### Release (Recommended for end users)

```bash
cmake -DCMAKE_BUILD_TYPE=Release ..
```

- Maximum optimization (`-O3`)
- Stripped symbols
- Smallest binary size
- Best performance

### Debug (Recommended for developers)

```bash
cmake -DCMAKE_BUILD_TYPE=Debug ..
```

- Debug symbols (`-g`)
- Runtime checks (range, overflow, I/O)
- Heap trace enabled
- Assertions enabled
- Easier debugging with gdb/lldb

### Default (No optimization flags specified)

```bash
cmake ..
```

- Basic optimization (`-O2`)
- Debug info included
- Balanced build

---

## Build Targets

The project provides several build targets:

### Main Targets

| Target | Description |
|--------|-------------|
| `realms` | Main application (Release mode) |
| `realms_debug` | Debug version with runtime checks |
| `realms_verbose` | Verbose build output (Lazarus only) |

### Build Specific Target

```bash
# Build only the main release version
cmake --build . --target realms

# Build only the debug version
cmake --build . --target realms_debug

# Build all targets
cmake --build .
```

### Optional Targets

| Target | Description |
|--------|-------------|
| `console_app` | Example console application (if src/main.pas exists) |
| `multi_unit_app` | Example multi-unit app (if src/program.pas exists) |

### Utility Targets

```bash
# Clean build artifacts
cmake --build . --target clean

# Deep clean (removes all Pascal temp files)
cmake --build . --target distclean
```

---

## Advanced Configuration

### Specifying Lazarus Location

If CMake can't find Lazarus automatically:

```bash
cmake -DLAZARUS_ROOT_DIR=/path/to/lazarus ..
```

### Specifying FPC Location

If CMake can't find FPC automatically:

```bash
cmake -DFPC_EXECUTABLE=/path/to/fpc ..
```

### Custom Install Prefix

```bash
# Install to custom directory
cmake -DCMAKE_INSTALL_PREFIX=/opt/realms ..
cmake --build .
cmake --install .
```

### Verbose Build Output

```bash
# See actual compiler commands
cmake --build . --verbose

# Or with make
make VERBOSE=1
```

### Parallel Building

```bash
# Linux/macOS - use all CPU cores
cmake --build . -j$(nproc)

# Windows - use 8 cores
cmake --build . -j 8
```

### Cross-Compilation

The build system automatically detects the target platform, but you can manually specify:

```bash
# For Windows target (from Linux with cross-compiler)
cmake -DCMAKE_SYSTEM_NAME=Windows ..

# For specific architectures
cmake -DFPC_TARGET_CPU=x86_64 ..
cmake -DFPC_TARGET_CPU=aarch64 ..
```

---

## Testing

The project includes CTest support:

```bash
# Run all tests
ctest

# Run tests with verbose output
ctest --output-on-failure

# Run specific test
ctest -R test_realms_version

# Run tests in parallel
ctest -j$(nproc)
```

---

## Packaging

Create distribution packages:

```bash
# Configure and build first
cmake -DCMAKE_BUILD_TYPE=Release ..
cmake --build .

# Create package
cpack

# Or specify generator
cpack -G ZIP        # ZIP archive (all platforms)
cpack -G NSIS       # Windows installer
cpack -G DragNDrop  # macOS DMG
cpack -G DEB        # Debian package
cpack -G RPM        # RPM package
```

### Package Components

You can install specific components:

```bash
# Install only the application
cmake --install . --component applications

# Install with debug binaries
cmake --install . --component debug

# Install development files
cmake --install . --component development

# Install documentation
cmake --install . --component documentation
```

---

## Troubleshooting

### CMake Can't Find FPC

**Problem**: `Could not find FPC`

**Solution**:
```bash
# Find your FPC installation
which fpc          # Linux/macOS
where fpc          # Windows

# Specify it explicitly
cmake -DFPC_EXECUTABLE=/usr/local/bin/fpc ..
```

### CMake Can't Find Lazarus

**Problem**: `Could not find Lazarus IDE`

**Solution**:
```bash
# Lazarus is optional - the project will build with FPC alone
# But if you want to use Lazarus:
cmake -DLAZARUS_ROOT_DIR=/path/to/lazarus ..

# Common Lazarus locations:
# macOS: /Applications/Lazarus.app/Contents/MacOS
# Windows: C:/lazarus
# Linux: /usr/lib/lazarus or /usr/share/lazarus
```

### Build Fails with "File not found"

**Problem**: Missing project files

**Solution**: Ensure you have the correct project structure:
```
Realms/
├── CMakeLists.txt
├── cmake/
│   ├── FindFPC.cmake
│   └── FindLazarus.cmake
└── Projects/
    ├── realms.lpi  # Lazarus project
    └── realms.lpr  # Pascal program
```

### Compilation Errors

**Problem**: FPC compilation fails

**Solution**:
```bash
# Check FPC version (needs 3.2.0+)
fpc -iV

# Try verbose build to see actual error
cmake --build . --verbose

# Try building with FPC directly to see the actual error
cd Projects
fpc realms.lpr
```

### Wrong Architecture on macOS

**Problem**: Building for wrong architecture (Intel vs ARM)

**Solution**:
```bash
# Check what FPC is targeting
fpc -iTP  # Shows target processor
fpc -iTO  # Shows target OS

# If wrong, you may need to reinstall FPC for your architecture
arch      # Shows your current architecture
uname -m  # Shows machine hardware name
```

### Windows: "lazbuild not found"

**Problem**: Lazarus not in PATH

**Solution**:
```cmd
REM Add Lazarus to PATH temporarily
set PATH=%PATH%;C:\lazarus

REM Or install it in PATH permanently via System Settings
REM Or specify in CMake:
cmake -DLAZBUILD_EXECUTABLE=C:/lazarus/lazbuild.exe ..
```

### Linux: Permission Denied

**Problem**: Can't execute built binary

**Solution**:
```bash
# Make it executable
chmod +x realms

# Or run with explicit path
./realms
```

---

## Project Structure

Understanding the project layout:

```
Realms/
├── CMakeLists.txt              # Main CMake configuration
├── BUILD.md                    # This file
├── README.md                   # Project readme
├── LICENSE                     # License file
├── cmake/
│   ├── FindFPC.cmake          # FPC detection module
│   └── FindLazarus.cmake      # Lazarus detection module
├── Projects/                   # Main project files (note: capital P)
│   ├── realms.lpi             # Lazarus project
│   ├── realms.lpr             # Pascal program
│   └── *.pas                  # Additional units
├── src/                       # Optional additional sources
├── units/                     # Optional unit directories
└── build/                     # Build directory (created by you)
```

---

## Getting Help

If you encounter issues:

1. Check this BUILD.md file
2. Check the CMake output for warnings/errors
3. Try building with verbose output: `cmake --build . --verbose`
4. Check FPC/Lazarus versions: `fpc -iV` and check Lazarus About dialog
5. Open an issue on the project repository with:
   - Your OS and version
   - CMake version (`cmake --version`)
   - FPC version (`fpc -iV`)
   - Lazarus version (if using)
   - Full error message
   - CMake configuration output

---

## Additional Resources

- [CMake Documentation](https://cmake.org/documentation/)
- [Free Pascal Documentation](https://www.freepascal.org/docs.html)
- [Lazarus Documentation](https://wiki.lazarus.freepascal.org/)
- [FPC Compiler Options](https://www.freepascal.org/docs-html/user/user.html)