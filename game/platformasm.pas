unit PlatformASM;

{$mode objfpc}{$H+}
{$ASMMODE AARCH64} // Use ARM64 Assembly Mode

interface

function GetCPUArch: string;
procedure FastMemCopy(dest, src: Pointer; count: SizeInt);

implementation

uses
  SysUtils;

{ Detects the platform at runtime }
function GetCPUArch: string;
begin
  {$IFDEF DARWIN}
  Result := 'Apple Silicon (macOS ARM64)';
  {$ELSEIF DEFINED(LINUX) AND DEFINED(CPUAARCH64)}
  Result := 'Linux ARM64';
  {$ELSE}
  Result := 'Unknown Architecture';
  {$ENDIF}
end;

{ Optimized Memory Copy using ARM64 Assembly }
procedure FastMemCopy(dest, src: Pointer; count: SizeInt);
asm
  // X0 = dest, X1 = src, X2 = count
  CBZ X2, @Exit       // If count is zero, exit
  SUBS X2, X2, #16    // Check if more than 16 bytes remain
  B.LT @SmallCopy     // If less, do small copy

@Loop:
  LDP Q0, Q1, [X1], #32  // Load 32 bytes from source
  STP Q0, Q1, [X0], #32  // Store 32 bytes to destination
  SUBS X2, X2, #32       // Decrease count by 32 bytes
  B.GE @Loop            // Repeat if enough data remains

@SmallCopy:
  CMP X2, #0
  B.EQ @Exit

  LDRB W3, [X1], #1  // Load byte from source
  STRB W3, [X0], #1  // Store byte to destination
  SUBS X2, X2, #1    // Decrease count
  B.NE @SmallCopy   // Repeat if bytes remain

@Exit:
end;

end.

