unit delphi_cdecl_printf;

interface

uses
  GameUnit,
  q_shared,
  sv_game,
  sv_send,
  SysUtils // No comma here. The comma will be added conditionally below.
  {$IF Defined(WINDOWS)}
    , vid_dll in '../win32/vid_dll.pas' // Comma added if WINDOWS is defined
  {$ELSEIF Defined(DARWIN)}
    , vid_so in '../mac/vid_macos.pas'    // Comma added if DARWIN is defined
  {$ELSEIF Defined(UNIX)}
    , vid_so in '../unix/vid_so.pas' // Assuming a common 'unix' subdir. Adjust path if different.
                                   // Comma added if UNIX is defined
  {$ENDIF}
  ; // This semicolon now correctly terminates the entire uses clause.
// Forward declarations of actual external print functions (Pascal-style varargs)
// These are the functions that the assembly/pure-Pascal wrappers will call.
// Ensure these functions are actually defined in their respective units (vid_dll, sv_game, etc.)
// with `array of const` or `varargs` parameters.
// If they are not declared here, you might get 'Identifier not found' errors.
// For example, if VID_Printf is defined in vid_dll.pas:
// procedure VID_Printf(APrint_Level: Cardinal; const AFormat: PChar; Args: array of const); cdecl; external;

// Declared in interface of their respective units. We only use them here.
// You might need to add their *actual* declarations in the interface section here
// if they are not exposed properly through their units for direct Pascal calls.
// For now, assuming they exist and are correctly linked by the compiler when called.

// From vid_dll.pas
// (Assuming these are the *actual* functions that take array of const or varargs directly)
// These are the public facing functions that the C code would call.
// The '_cdecl' suffix in this unit indicates they are C-callable wrappers.
// Inside this unit, we then call the "real" Pascal functions like VID_Printf, VID_Error etc.
// from vid_dll etc.
procedure VID_Printf_cdecl(APrint_Level: LongInt; AFormat: PChar); cdecl;
procedure VID_Error_cdecl(AError_Level: LongInt; AFormat: PChar); cdecl;

// From server units
procedure SV_BroadcastPrintf_cdecl(Level: Integer; AFormat: PChar); cdecl;
procedure PF_centerprintf_cdecl(ent: edict_p; fmt: PChar); cdecl;
procedure PF_cprintf_cdecl(ent: edict_p; level: Integer; fmt: PChar); cdecl;
procedure PF_dprintf_cdecl(fmt: PChar); cdecl;

implementation

// Important: When calling actual functions like VID_Printf, VID_Error, SV_BroadcastPrintf,
// PF_centerprintf, PF_cprintf, PF_dprintf, ensure they are declared either:
// 1. In the interface of a unit included in the 'uses' clause.
// 2. As 'external' procedures/functions here if they are in a C library.
// For a Quake conversion, they are usually procedures declared in other Pascal units
// (like vid_dll, sv_game, etc.) that themselves might wrap C calls.
// We'll assume these Pascal functions exist and take the 'array of const' parameter.
// Example:
// procedure VID_Printf(APrint_Level: Cardinal; AFormat: PChar; Args: array of const); cdecl; external;
// If not, you will need to add their Pascal declarations (with array of const) here.

// Helper function to scan format string for arguments
function ScanFormatText(AText: PChar; var APos: Integer; var ALen: Integer): Integer;
var
  Len: Integer;
  State: Integer;
  EndPos: Integer;
begin
  Result := 0;
  State := 0;
  Len := StrLen(AText); // Get length of PChar
  EndPos := APos;
  ALen := 0;

  // Use 0-based indexing for PChar. Loop while within string bounds and no specifier found.
  while (APos < Len) and (Result = 0) do
  begin
    case State of
      0: // Looking for '%'
        if AText[APos] = '%' then
          State := 1;
      1: // Found '%' - now looking for identifier or specifiers
        // Check for '%%' which means a literal '%'
        if AText[APos] = '%' then
        begin
          // It's '%%', so reset state and continue search
          State := 0;
          Result := 0; // Not a format specifier for an argument
        end
        else
        begin
          // Check the character after '%' (APos is still at '%')
          case AText[APos] of // AText[APos] is now the character *after* the initial '%'
            'd', 'i', 'x': Result := 1; // Integer, signed/hex
            'u': Result := 2; // Unsigned integer
            'e', 'f', 'g', 'n', 'm': Result := 3; // Floating point
            'p': Result := 4; // Pointer
            's': Result := 5; // String
            // Skip format flags like '0'..'9', '.', ':', '*', '-'
            '0'..'9', '.', ':', '*', '-': ;
          else
            // Not a valid format specifier, reset state to look for next '%'
            State := 0;
            Result := 0; // Not a format specifier for an argument
          end;
        end;
    end; // case State

    // If a format specifier was found (Result <> 0), calculate its length.
    // Otherwise, advance APos to continue scanning.
    if Result <> 0 then
      ALen := (APos - EndPos) + 1 // Length includes '%' and the specifier itself
    else
      Inc(APos); // Move to the next character to scan
  end;
end;

// Helper function to format the string by extracting parameters from stack
function FormatString(AFormat: PChar; AParams: Cardinal): string;
var
  P, L: Integer;
  S: string;
  // Use a mutable buffer for the format string, as 'i' to 'd' conversion modifies it.
  // Assuming MAX_FORMAT_STRING_LEN is defined elsewhere or use a reasonable fixed size.
  // If not, a larger dynamic array or pointer-based allocation might be needed.
  MutableFormat: array[0..255] of Char;
begin
  S := '';
  StrCopy(MutableFormat, AFormat); // Make a mutable copy of the format string

  P := 0; // Current position in MutableFormat (0-based for PChar)
  L := 0; // Length of the matched format specifier (e.g., '%d' -> 2)

  try
    while (True) do
    begin
      // Scan for the next format specifier from current position P
      case ScanFormatText(@MutableFormat[0], P, L) of
        0: // No more format specifiers found
          begin
            // Append any remaining plain text from the mutable format string
            if P < StrLen(@MutableFormat[0]) then // If there's text after the last specifier
              S := S + Copy(string(@MutableFormat[0]), P + 1, StrLen(@MutableFormat[0]) - P);
            Break; // Exit the loop
          end;
        1: // Integer (d, i, x)
          begin
            // If the specifier was 'i', change it to 'd' in the mutable copy
            if (MutableFormat[P] = 'i') then
              MutableFormat[P] := 'd';

            // Format the segment. Copy needs 1-based indexing for string.
            S := S + Format(Copy(string(@MutableFormat[0]), P + 1, L), [Integer(Pointer(AParams)^)]);
            Inc(AParams, SizeOf(Integer)); // Advance pointer on stack
          end;
        2: // Unsigned decimal (u)
          begin
            S := S + Format(Copy(string(@MutableFormat[0]), P + 1, L), [Cardinal(Pointer(AParams)^)]);
            Inc(AParams, SizeOf(Cardinal));
          end;
        3: // Floating point (e, f, g, n, m)
          begin
            S := S + Format(Copy(string(@MutableFormat[0]), P + 1, L), [Double(Pointer(AParams)^)]);
            Inc(AParams, SizeOf(Double));
          end;
        4: // Pointer (p)
          begin
            S := S + Format(Copy(string(@MutableFormat[0]), P + 1, L), [Cardinal(Pointer(AParams)^)]);
            Inc(AParams, SizeOf(Cardinal)); // Pointers are usually 4 bytes (Cardinal/LongWord) on 32-bit
          end;
        5: // String (s)
          begin
            S := S + Format(Copy(string(@MutableFormat[0]), P + 1, L), [string(PChar(Pointer(AParams)^))]);
            Inc(AParams, SizeOf(Pointer)); // PChar is usually 4 bytes (Pointer) on 32-bit
          end;
      end;
      // Advance P to the start of the next segment to scan
      P := P + L;
    end;
  except
    on E: Exception do // Catch any formatting or memory access errors
      S := S + 'Converter : Internal error: ' + E.Message;
  end;
  Result := S;
end;

// --- Helper procedures to bridge C-cdecl varargs to Pascal FormatString ---

// Handles functions with 0 fixed parameters before varargs
procedure Proc_ZeroParamAndString(ARoutine: Integer; AFormat: PChar; AParams: Cardinal); cdecl;
var
  S: string;
begin
  S := FormatString(AFormat, AParams);
  case ARoutine of
    1: PF_dprintf('%s', [S]); // Example: Calls a Pascal PF_dprintf
  else
    // Handle unknown routine ID or log an error
  end;
end;

// Handles functions with 1 fixed parameter (APrint_Level) before varargs
procedure Proc_OneParamAndString(ARoutine: Integer; APrint_Level: Cardinal; AFormat: PChar; AParams: Cardinal); cdecl;
var
  S: string;
begin
  S := FormatString(AFormat, AParams);
  case ARoutine of
    1: VID_Printf(APrint_Level, '%s', [S]); // Calls a Pascal VID_Printf
    2: VID_Error(APrint_Level, '%s', [S]);  // Calls a Pascal VID_Error
    3: SV_BroadcastPrintf(APrint_Level, '%s', [S]); // Calls a Pascal SV_BroadcastPrintf
    4: PF_centerprintf(Pointer(APrint_Level), '%s', [S]); // APrint_Level holds 'ent' for this routine
  else
    // Handle unknown routine ID or log an error
  end;
end;

// Handles functions with 2 fixed parameters (Param1, APrint_Level) before varargs
procedure Proc_TwoParamAndString(ARoutine: Integer; Param1: Cardinal; APrint_Level: Cardinal; AFormat: PChar; AParams: Cardinal); cdecl;
var
  S: string;
begin
  S := FormatString(AFormat, AParams);
  case ARoutine of
    1: PF_cprintf(Pointer(Param1), APrint_level, '%s', [S]); // Calls a Pascal PF_cprintf
  else
    // Handle unknown routine ID or log an error
  end;
end;

// --- C-callable wrappers using inline assembly (for x86/x86_64) or pure Pascal (for ARM64/other) ---

// PF_dprintf_cdecl
procedure PF_dprintf_cdecl(fmt: PChar); cdecl;
{$IFDEF CPUX86}
asm
// Stack Layout for cdecl (32-bit x86):
// EBP + 8: fmt (AFormat)
// EBP + 12: First variable argument
  PUSH    EAX
  PUSH    EBX
  PUSH    ECX
  PUSH    EDI

  MOV     EAX,EBP             // Get stack frame pointer
  ADD     EAX,$0C             // Point to first variable parameter (EBP + 12)

  PUSH    EAX                 // Pass pointer to parameters (AParams)
  PUSH    DWORD PTR [EBP+$08] // Pass format string (AFormat)
  PUSH    $00000001           // Routine ID (1 for PF_dprintf)
  CALL    Proc_ZeroParamAndString // Call the Pascal helper
  ADD     ESP,$0C             // Clean up pushed arguments (3 * 4 bytes)

  POP     EDI
  POP     ECX
  POP     EBX
  POP     EAX
end;
{$ELSE}
// Portable Pascal fallback for non-x86 architectures (e.g., ARM64, RISC-V)
begin
  // The first variable argument directly follows the 'fmt' parameter on the stack.
  // PtrUInt(@fmt) gives the address of the 'fmt' parameter.
  // Adding SizeOf(Pointer) moves to the address *after* 'fmt', which is the first var-arg.
  Proc_ZeroParamAndString(1, fmt, PtrUInt(@fmt) + SizeOf(Pointer));
end;
{$ENDIF}


// PF_cprintf_cdecl
procedure PF_cprintf_cdecl(ent: edict_p; level: integer; fmt: PChar); cdecl;
{$IFDEF CPUX86}
asm
// Stack Layout for cdecl (32-bit x86):
// EBP + 8: ent
// EBP + 12: level
// EBP + 16: fmt (AFormat)
// EBP + 20: First variable argument
  PUSH    EAX
  PUSH    EBX
  PUSH    ECX
  PUSH    EDI

  MOV     EAX,EBP             // Get stack frame pointer
  ADD     EAX,$14             // Point to first variable parameter (EBP + 20)

  PUSH    EAX                 // Pass pointer to parameters (AParams)
  PUSH    DWORD PTR [EBP+$10] // Pass format string (fmt / AFormat)
  PUSH    DWORD PTR [EBP+$0C] // Pass level
  PUSH    DWORD PTR [EBP+$08] // Pass ent
  PUSH    $00000001           // Routine ID (1 for PF_cprintf)
  CALL    Proc_TwoParamAndString // Call the Pascal helper
  ADD     ESP,$14             // Clean up pushed arguments (5 * 4 bytes)

  POP     EDI
  POP     ECX
  POP     EBX
  POP     EAX
end;
{$ELSE}
// Portable Pascal fallback for non-x86 architectures
begin
  Proc_TwoParamAndString(1, Cardinal(ent), Cardinal(level), fmt,
    PtrUInt(@fmt) + SizeOf(Pointer));
end;
{$ENDIF}


// PF_centerprintf_cdecl
procedure PF_centerprintf_cdecl(ent: edict_p; fmt: PChar); cdecl;
{$IFDEF CPUX86}
asm
// Stack Layout for cdecl (32-bit x86):
// EBP + 8: ent
// EBP + 12: fmt (AFormat)
// EBP + 16: First variable argument
  PUSH    EAX
  PUSH    EBX
  PUSH    ECX
  PUSH    EDI

  MOV     EAX,EBP             // Get stack frame pointer
  ADD     EAX,$10             // Point to first variable parameter (EBP + 16)

  PUSH    EAX                 // Pass pointer to parameters (AParams)
  PUSH    DWORD PTR [EBP+$0C] // Pass format string (fmt / AFormat)
  PUSH    DWORD PTR [EBP+$08] // Pass ent (APrint_Level in Proc_OneParamAndString)
  PUSH    $00000004           // Routine ID (4 for PF_centerprintf)
  CALL    Proc_OneParamAndString // Call the Pascal helper
  ADD     ESP,$10             // Clean up pushed arguments (4 * 4 bytes)

  POP     EDI
  POP     ECX
  POP     EBX
  POP     EAX
end;
{$ELSE}
// Portable Pascal fallback for non-x86 architectures
begin
  Proc_OneParamAndString(4, Cardinal(ent), fmt,
    PtrUInt(@fmt) + SizeOf(Pointer));
end;
{$ENDIF}


// SV_BroadcastPrintf_cdecl
procedure SV_BroadcastPrintf_cdecl(Level: Integer; AFormat: PChar); cdecl;
{$IFDEF CPUX86}
asm
// Stack Layout for cdecl (32-bit x86):
// EBP + 8: Level (APrint_Level)
// EBP + 12: AFormat
// EBP + 16: First variable argument
  PUSH    EAX
  PUSH    EBX
  PUSH    ECX
  PUSH    EDI

  MOV     EAX,EBP             // Get stack frame pointer
  ADD     EAX,$10             // Point to first variable parameter (EBP + 16)

  PUSH    EAX                 // Pass pointer to parameters (AParams)
  PUSH    DWORD PTR [EBP+$0C] // Pass format string (AFormat)
  PUSH    DWORD PTR [EBP+$08] // Pass Level (APrint_Level)
  PUSH    $00000003           // Routine ID (3 for SV_BroadcastPrintf)
  CALL    Proc_OneParamAndString // Call the Pascal helper
  ADD     ESP,$10             // Clean up pushed arguments (4 * 4 bytes)

  POP     EDI
  POP     ECX
  POP     EBX
  POP     EAX
end;
{$ELSE}
// Portable Pascal fallback for non-x86 architectures
begin
  Proc_OneParamAndString(3, Cardinal(Level), AFormat,
    PtrUInt(@AFormat) + SizeOf(Pointer));
end;
{$ENDIF}


// VID_Printf_cdecl
procedure VID_Printf_cdecl(APrint_Level: LongInt; AFormat: PChar); cdecl;
{$IFDEF CPUX86}
asm
// Stack Layout for cdecl (32-bit x86):
// EBP + 8: APrint_Level
// EBP + 12: AFormat
// EBP + 16: First variable argument
  PUSH    EAX
  PUSH    EBX
  PUSH    ECX
  PUSH    EDI

  MOV     EAX,EBP             // Get stack frame pointer
  ADD     EAX,$10             // Point to first variable parameter (EBP + 16)

  PUSH    EAX                 // Pass pointer to parameters (AParams)
  PUSH    DWORD PTR [EBP+$0C] // Pass format string (AFormat)
  PUSH    DWORD PTR [EBP+$08] // Pass APrint_Level
  PUSH    $00000001           // Routine ID (1 for VID_Printf)
  CALL    Proc_OneParamAndString // Call the Pascal helper
  ADD     ESP,$10             // Clean up pushed arguments (4 * 4 bytes)

  POP     EDI
  POP     ECX
  POP     EBX
  POP     EAX
end;
{$ELSE}
// Portable Pascal fallback for non-x86 architectures
begin
  Proc_OneParamAndString(1, Cardinal(APrint_Level), AFormat,
    PtrUInt(@AFormat) + SizeOf(Pointer));
end;
{$ENDIF}


// VID_Error_cdecl
procedure VID_Error_cdecl(AError_Level: LongInt; AFormat: PChar); cdecl;
{$IFDEF CPUX86}
asm
// Stack Layout for cdecl (32-bit x86):
// EBP + 8: AError_Level (APrint_Level)
// EBP + 12: AFormat
// EBP + 16: First variable argument
  PUSH    EAX
  PUSH    EBX
  PUSH    ECX
  PUSH    EDI

  MOV     EAX,EBP             // Get stack frame pointer
  ADD     EAX,$10             // Point to first variable parameter (EBP + 16)

  PUSH    EAX                 // Pass pointer to parameters (AParams)
  PUSH    DWORD PTR [EBP+$0C] // Pass format string (AFormat)
  PUSH    DWORD PTR [EBP+$08] // Pass AError_Level (APrint_Level)
  PUSH    $00000002           // Routine ID (2 for VID_Error)
  CALL    Proc_OneParamAndString // Call the Pascal helper
  ADD     ESP,$10             // Clean up pushed arguments (4 * 4 bytes)

  POP     EDI
  POP     ECX
  POP     EBX
  POP     EAX
end;
{$ELSE}
// Portable Pascal fallback for non-x86 architectures
begin
  Proc_OneParamAndString(2, Cardinal(AError_Level), AFormat,
    PtrUInt(@AFormat) + SizeOf(Pointer));
end;
{$ENDIF}

end.
