(*
    Delphi_cdecl_printf.pas

    Juha Hartikainen (juha@linearteam.org):
    - This unit includes implementations for all exported C-functions with variable
      arguments. As there is not compatible method for them in Delphi, we need to
      dig out the parameters from stack by inline assembler.

      Code here is based on Michael Skovslund's example.

*)
unit delphi_cdecl_printf;

interface

uses
  GameUnit,
  q_shared,
  sv_game,
  sv_send,
  SysUtils,
  {$IF Defined(WINDOWS)}
    vid_dll,
  {$ELSEIF Defined(DARWIN)}
    vid_so,  // Use vid_so for macOS dynamic libraries
  {$ELSEIF Defined(UNIX)}
    vid_so;
  {$ENDIF}


 // From vid_dll.pas
  procedure VID_Printf_cdecl (APrint_Level: Integer; AFormat: PChar); cdecl;
  procedure VID_Error_cdecl  (AError_Level: Integer; AFormat: PChar); cdecl;

  // From server units
  procedure SV_BroadcastPrintf_cdecl(Level: Integer; AFormat: PChar); cdecl;
  procedure PF_centerprintf_cdecl  (ent: edict_p; fmt: PChar); cdecl;
  procedure PF_cprintf_cdecl       (ent: edict_p; level: Integer; fmt: PChar); cdecl;
  procedure PF_dprintf_cdecl       (fmt: PChar); cdecl;

implementation

function ScanFormatText(AText: PChar; var APos, ALen: Integer): Integer;
  var
    Len, EndPos: Integer;
    State: Integer;
  begin
    Result := 0;
    State  := 0;
    Len    := StrLen(AText);
    EndPos := APos;
    ALen   := 0;

    while (APos < Len) and (Result = 0) do
    begin
      case AText[APos] of
        '%': State := 1;
      else
        State := 0;
      end;

      if (State = 1) then
        case AText[APos + 1] of
          'd','i','x'            : Result := 1;
          'u'                    : Result := 2;
          'e','f','g','n','m'    : Result := 3;
          'p'                    : Result := 4;
          's'                    : Result := 5;
          // flags 0-9 . : * - are skipped
        end;

      if Result <> 0 then
        ALen := (APos - EndPos) + 2          // '%' plus specifier
      else
        Inc(APos);
    end;
  end;

function FormatString(AFormat: PChar; AParams: Cardinal): string;
var
  P, Len: Integer;
  LP: Integer;
  S: string;
  // No need for Tmp: string if AFormat is directly used as PChar
  // If you need to modify the format string for 'i' to 'd' conversion,
  // you must create a mutable copy.
  MutableFormat: array[0..255] of Char; // Or dynamically allocate if format strings can be very long
begin
  S := '';
  StrCopy(MutableFormat, AFormat); // Make a mutable copy

  P := 0; // Start P at 0 for PChar indexing
  LP := P;
  try
    while (True) do
    begin
      case ScanFormatText(MutableFormat, P, Len) of // Pass the mutable copy
        0:                                    // No more params to convert
          begin
            // Append remaining part of the string
            if LP < StrLen(MutableFormat) then // Check if there's anything left to copy
              S := S + Copy(string(MutableFormat), LP + 1, (StrLen(MutableFormat) - LP)); // Delphi string Copy is 1-based
            Break;
          end;
        1:                                    // decimal (d, i, x)
          begin
            // Check for 'i' and change to 'd' if necessary in the mutable copy
            if (MutableFormat[P + 1] = 'i') then // Check the character *after* '%'
              MutableFormat[P + 1] := 'd';

            // Extract the format specifier substring (e.g., "%d", "%.2f")
            // Delphi's Format expects 1-based indexing for the format string.
            S := S + Format(Copy(string(MutableFormat), P + 1, Len), [Integer(Pointer(AParams)^)]);
            Inc(AParams, SizeOf(Integer));
            LP := P + Len; // Update LP to the position after the processed format specifier
          end;
        2:                                    // unsigned decimal
          begin
            S := S + Format(Copy(string(MutableFormat), P + 1, Len), [Cardinal(Pointer(AParams)^)]);
            Inc(AParams, SizeOf(Cardinal));
            LP := P + Len;
          end;
        3:                                    // floating point
          begin
            S := S + Format(Copy(string(MutableFormat), P + 1, Len), [Double(Pointer(AParams)^)]);
            Inc(AParams, SizeOf(Double));
            LP := P + Len;
          end;
        4:                                    // pointer
          begin
            S := S + Format(Copy(string(MutableFormat), P + 1, Len), [Cardinal(Pointer(AParams)^)]);
            Inc(AParams, SizeOf(Cardinal));
            LP := P + Len;
          end;
        5:                                    // string
          begin
            S := S + Format(Copy(string(MutableFormat), P + 1, Len), [string(PChar(Pointer(AParams)^))]);
            Inc(AParams, SizeOf(Pointer));
            LP := P + Len;
          end;
      end;
      // After processing a format specifier, advance P for the next scan
      P := LP;
    end;
  except
    S := S + 'Converter : Internal error.';
  end;
  Result := S;
end;

procedure Proc_ZeroParamAndString(ARoutine: Integer;
  AFormat: PChar; AParams: Cardinal); cdecl;
var
  S: string;
begin
  S := FormatString(AFormat, AParams);
  case ARoutine of
    1:                                      // PF_dprintf
      PF_dprintf('%s', [S]);
  end;
end;

procedure Proc_OneParamAndString(ARoutine: Integer; APrint_Level: Cardinal;
  AFormat: PChar; AParams: Cardinal); cdecl;
var
  S: string;
begin
  S := FormatString(AFormat, AParams);
  case ARoutine of
    1:                                      // VID_Printf
      VID_Printf(APrint_Level, '%s', [S]);
    2:                                      // VID_Error
      VID_Error(APrint_Level, '%s', [S]);
    3:                                      // SV_BroadcastPrintf
      SV_BroadcastPrintf(APrint_Level, '%s', [S]);
    4:                                      // PF_centerprintf
      PF_centerprintf(Pointer(APrint_Level), '%s', [S]);
  end;
end;

procedure Proc_TwoParamAndString(ARoutine: Integer; Param1: Cardinal; APrint_Level: Cardinal;
  AFormat: PChar; AParams: Cardinal); cdecl;
var
  S: string;
begin
  S := FormatString(AFormat, AParams);
  case ARoutine of
    1:                                      // PF_cprintf
      PF_cprintf(Pointer(Param1), APrint_level, '%s', [S]);
  end;
end;

procedure PF_dprintf_cdecl(fmt: PChar); cdecl;
asm
// ASM statement produces push ebp
// Stack now: ebp
//         +4: return adr.
//         +8: AFormat
//        +12: First param.
  PUSH    EAX             // Store register
  PUSH    EBX             // Store register
  PUSH    ECX             // Store register
  PUSH    EDI             // Store register

  MOV     EAX,EBP           // Get stack pointer
  ADD     EAX,$0C           // Point to first variable parameter

  PUSH    EAX             // Store pointer to parameters
  PUSH    DWORD PTR [EBP+$08]       // store format string
  PUSH    $00000001           // Indicate SV_BroadCastPrintf
  CALL    Proc_ZeroParamAndString     // use the VarArgs in delphi routine
  ADD     ESP,$0C           // pop params off the stack

  POP     EDI             // restore register
  POP     ECX             // restore register
  POP     EBX             // restore register
  POP     EAX             // restore register
end;

procedure PF_cprintf_cdecl(ent: edict_p; level: integer; fmt: PChar); cdecl;
asm
// ASM statement produces push ebp
// Stack now: ebp
//         +4: return adr.
//         +8: ent
//        +12: level
//        +16: AFormat
//        +20: First param.
  PUSH    EAX             // Store register
  PUSH    EBX             // Store register
  PUSH    ECX             // Store register
  PUSH    EDI             // Store register

  MOV     EAX,EBP           // Get stack pointer
  ADD     EAX,$14           // Point to first variable parameter

  PUSH    EAX             // Store pointer to parameters
  PUSH    DWORD PTR [EBP+$10]       // store format string
  PUSH    DWORD PTR [EBP+$0C]       // store level
  PUSH    DWORD PTR [EBP+$08]       // store ent
  PUSH    $00000001           // Indicate PF_cprintf
  CALL    Proc_TwoParamAndString      // use the VarArgs in delphi routine
  ADD     ESP,$14           // pop params off the stack

  POP     EDI             // restore register
  POP     ECX             // restore register
  POP     EBX             // restore register
  POP     EAX             // restore register
end;

procedure PF_centerprintf_cdecl(ent: edict_p; fmt: PChar); cdecl;
asm
// ASM statement produces push ebp
// Stack now: ebp
//         +4: return adr.
//         +8: ent
//        +12: AFormat
//        +16: First param.
  PUSH    EAX             // Store register
  PUSH    EBX             // Store register
  PUSH    ECX             // Store register
  PUSH    EDI             // Store register

  MOV     EAX,EBP           // Get stack pointer
  ADD     EAX,$10           // Point to first variable parameter

  PUSH    EAX             // Store pointer to parameters
  PUSH    DWORD PTR [EBP+$0C]       // store format string
  PUSH    DWORD PTR [EBP+$08]       // store ent
  PUSH    $00000004           // Indicate SV_BroadCastPrintf
  CALL    Proc_OneParamAndString      // use the VarArgs in delphi routine
  ADD     ESP,$10           // pop params off the stack

  POP     EDI             // restore register
  POP     ECX             // restore register
  POP     EBX             // restore register
  POP     EAX             // restore register
end;

procedure SV_BroadcastPrintf_cdecl(Level: Integer; AFormat: PChar); cdecl;
asm
// ASM statement produces push ebp
// Stack now: ebp
//         +4: return adr.
//         +8: APrint_Level
//        +12: AFormat
//        +16: First param.
  PUSH    EAX             // Store register
  PUSH    EBX             // Store register
  PUSH    ECX             // Store register
  PUSH    EDI             // Store register

  MOV     EAX,EBP           // Get stack pointer
  ADD     EAX,$10           // Point to first variable parameter

  PUSH    EAX             // Store pointer to parameters
  PUSH    DWORD PTR [EBP+$0C]       // store format string
  PUSH    DWORD PTR [EBP+$08]       // store print_level
  PUSH    $00000003           // Indicate SV_BroadCastPrintf
  CALL    Proc_OneParamAndString      // use the VarArgs in delphi routine
  ADD     ESP,$10           // pop params off the stack

  POP     EDI             // restore register
  POP     ECX             // restore register
  POP     EBX             // restore register
  POP     EAX             // restore register
end;

procedure VID_Printf_cdecl(APrint_Level: Integer; AFormat: PChar); cdecl;
asm
// ASM statement produces push ebp
// Stack now: ebp
//         +4: return adr.
//         +8: APrint_Level
//        +12: AFormat
//        +16: First param.
  PUSH    EAX             // Store register
  PUSH    EBX             // Store register
  PUSH    ECX             // Store register
  PUSH    EDI             // Store register

  MOV     EAX,EBP           // Get stack pointer
  ADD     EAX,$10           // Point to first variable parameter

  PUSH    EAX             // Store pointer to parameters
  PUSH    DWORD PTR [EBP+$0C]       // store format string
  PUSH    DWORD PTR [EBP+$08]       // store print_level
  PUSH    $00000001           // Indicate VID_Printf
  CALL    Proc_OneParamAndString      // use the VarArgs in delphi routine
  ADD     ESP,$10           // pop params off the stack

  POP     EDI             // restore register
  POP     ECX             // restore register
  POP     EBX             // restore register
  POP     EAX             // restore register
end;

procedure VID_Error_cdecl(AError_Level: Integer; AFormat: PChar); cdecl;
asm
// ASM statement produces push ebp
// Stack now: ebp
//         +4: return adr.
//         +8: APrint_Level
//        +12: AFormat
//        +16: First param.
  PUSH    EAX             // Store register
  PUSH    EBX             // Store register
  PUSH    ECX             // Store register
  PUSH    EDI             // Store register

  MOV     EAX,EBP           // Get stack pointer
  ADD     EAX,$10           // Point to first variable parameter

  PUSH    EAX             // Store pointer to parameters
  PUSH    DWORD PTR [EBP+$0C]       // store format string
  PUSH    DWORD PTR [EBP+$08]       // store print_level
  PUSH    $00000002           // Indicate VID_Error
  CALL    Proc_OneParamAndString      // use the VarArgs in delphi routine
  ADD     ESP,$10           // pop params off the stack

  POP     EDI             // restore register
  POP     ECX             // restore register
  POP     EBX             // restore register
  POP     EAX             // restore register
end;

end.
