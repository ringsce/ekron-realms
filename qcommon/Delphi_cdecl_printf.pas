(*
   Delphi_cdecl_printf.pas

   Juha Hartikainen (juha@linearteam.org):
   - This unit includes implementations for all exported C-functions with variable
     arguments. As there is not compatible method for them in Delphi, we need to
     dig out the parameters from stack by inline assembler.

     Code here is based on Michael Skovslund's example.

*)
unit Delphi_cdecl_printf;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  GameUnit,
  q_shared;

// From vid_dll.pas
procedure VID_Printf_cdecl(APrint_Level: Integer; AFormat: PChar); cdecl;
procedure VID_Error_cdecl(AError_Level: Integer; AFormat: PChar); cdecl;
// From server units
procedure SV_BroadcastPrintf_cdecl(Level: Integer; AFormat: PChar); cdecl;
procedure PF_centerprintf_cdecl(ent: edict_p; fmt: PChar); cdecl;
procedure PF_cprintf_cdecl(ent: edict_p; level: integer; fmt: PChar); cdecl;
procedure PF_dprintf_cdecl(fmt: PChar); cdecl;

implementation

uses
{$IFnDEF FPC}
  vid_so,
{$ELSE}
{$ENDIF}
  sv_game,
  sv_send,
  {$IFDEF WIN32}
  vid_dll,
  {$ELSE}
  {$ENDIF}
  SysUtils;

function ScanFormatText(AText: string; var APos: Integer; var ALen: Integer): Integer;
var
  Len: Integer;
  State: Integer;
  EndPos: Integer;
begin
  Result := 0;
  State := 0;
  Len := Length(AText);
  EndPos := APos;
  ALen := 0;
  while (APos <= Len) and (Result = 0) do
  begin
    case State of
      0:                                // looking for '%'.
        if AText[APos] = '%' then
        begin
          State := 1;
        end;
      1:                                // looking for identifier
        begin
          case AText[APos] of
            'i':                        // decimal in C but not for delphi, so this must be patched by caller.
              Result := 1;
            'd':                        // decimal
              Result := 1;
            'u':                        // unsigned decimal
              Result := 2;
            'e':                        // floating point
              Result := 3;
            'f':                        // fixed, floting point.
              Result := 3;
            'g':                        // floating point (floor)
              Result := 3;
            'n':                        // floating point
              Result := 3;
            'm':                        // money, floating point
              Result := 3;
            'p':                        // pointer
              Result := 4;
            's':                        // string (PChar)
              Result := 5;
            'x':                        // integer convert to hex
              Result := 1;
            '0'..'9',                   // format specifiers (optional)
            '.', ':', '*', '-': ;       // they must be skipped so we can identify the type
          else
            begin
              // not a know identifier so skip it
              State := 0;               // start looking for new "%"
            end;
          end;
        end;
    end;
    if Result <> 0 then
    begin
      ALen := (APos - EndPos) + 1;
    end;
    Inc(APos);
  end;
end;

function FormatString(AFormat: PChar; AParams: Cardinal): string;
var
  P, Len: Integer;
  LP: Integer;
  S, Tmp: string;
begin
  S := '';
  Tmp := AFormat;
  P := 1;
  LP := P;
  try
    while (True) do
    begin
      case ScanFormatText(Tmp, P, Len) of
        0:                              // No more params to convert
          begin
            if LP <> P then
              S := S + Copy(Tmp, LP, (P - LP));
            Break;
          end;
        1:                              // decimal
          begin                         // this could be a 'i' identifier.
            if AFormat[P - 2] = 'i' then
            begin                       // if so, change it to 'd'
              Tmp[P - 1] := 'd';
              if AFormat[P - 1] = 'd' then // to be sure check and see if next char is 'd'
                Tmp[P] := ' ';          // if so, remove it.
            end;
            S := S + Format(Copy(Tmp, LP, Len), [Integer(Pointer(AParams)^)]);
            Inc(AParams, SizeOf(Integer));
            LP := P;
          end;
        2:                              // unsigned decimal
          begin
            S := S + Format(Copy(Tmp, LP, Len), [Cardinal(Pointer(AParams)^)]);
            Inc(AParams, SizeOf(Cardinal));
            LP := P;
          end;
        3:                              // floating point
          begin
            S := S + Format(Copy(Tmp, LP, Len), [Double(Pointer(AParams)^)]);
            Inc(AParams, SizeOf(Double));
            LP := P;
          end;
        4:                              // pointer
          begin
            S := S + Format(Copy(Tmp, LP, Len), [Cardinal(Pointer(AParams)^)]);
            Inc(AParams, SizeOf(Cardinal));
            LP := P;
          end;
        5:                              // string
          begin
            S := S + Format(Copy(Tmp, LP, Len), [string(PChar(Pointer(AParams)^))]);
            Inc(AParams, SizeOf(Pointer));
            LP := P;
          end;
      end;
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
    1:                                  // PF_dprintf
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
    1:                                  // VID_Printf
      {$IFDEF AARCH64}
      VID_Printf(APrint_Level, PChar(S));
      {$ELSE}
      VID_Printf(APrint_Level, '%s', [S]);
      {$ENDIF}
    2:                                  // VID_Error
      {$IFDEF AARCH64}
      VID_Error(APrint_Level, PChar(S));
      {$ELSE}
      VID_Error(APrint_Level, '%s', [S]);
      {$ENDIF}
    3:                                  // SV_BroadcastPrintf
      {$IFDEF AARCH64}
      SV_BroadcastPrintf(APrint_Level, PChar(S));
      {$ELSE}
      SV_BroadcastPrintf(APrint_Level, '%s', [S]);
      {$ENDIF}
    4:                                  // PF_centerprintf
      {$IFDEF AARCH64}
      PF_centerprintf(Pointer(APrint_Level), PChar(S));
      {$ELSE}
      PF_centerprintf(Pointer(APrint_Level), '%s', [S]);
      {$ENDIF}
  end;
end;

procedure Proc_TwoParamAndString(ARoutine: Integer; Param1: Cardinal; APrint_Level: Cardinal;
  AFormat: PChar; AParams: Cardinal); cdecl;
var
  S: string;
begin
  S := FormatString(AFormat, AParams);
  case ARoutine of
    1:                                  // PF_cprintf
      PF_cprintf(Pointer(Param1), APrint_level, '%s', [S]);
  end;
end;

procedure PF_dprintf_cdecl(fmt: PChar); cdecl;
asm
// ARM64 assembly
  STP     X29, X30, [SP, #-16]!  // Save frame pointer and link register
  MOV     X29, SP                 // Set up frame pointer

  ADD     X0, X29, #16            // Point to first variable parameter
  LDR     X1, [X29, #8]           // Load format string
  MOV     X2, #1                  // Indicate PF_dprintf
  BL      Proc_ZeroParamAndString // Call the VarArgs in Delphi routine

  LDP     X29, X30, [SP], #16     // Restore frame pointer and link register
  RET
end;

procedure PF_cprintf_cdecl(ent: edict_p; level: integer; fmt: PChar); cdecl;
asm
// ARM64 assembly
  STP     X29, X30, [SP, #-16]!  // Save frame pointer and link register
  MOV     X29, SP                 // Set up frame pointer

  ADD     X0, X29, #24            // Point to first variable parameter
  LDR     X1, [X29, #16]          // Load format string
  LDR     X2, [X29, #12]          // Load level
  LDR     X3, [X29, #8]           // Load ent
  MOV     X4, #1                  // Indicate PF_cprintf
  BL      Proc_TwoParamAndString  // Call the VarArgs in Delphi routine

  LDP     X29, X30, [SP], #16     // Restore frame pointer and link register
  RET
end;

procedure PF_centerprintf_cdecl(ent: edict_p; fmt: PChar); cdecl;
asm
// ARM64 assembly
  STP     X29, X30, [SP, #-16]!  // Save frame pointer and link register
  MOV     X29, SP                 // Set up frame pointer

  ADD     X0, X29, #16            // Point to first variable parameter
  LDR     X1, [X29, #12]          // Load format string
  LDR     X2, [X29, #8]           // Load ent
  MOV     X3, #4                  // Indicate PF_centerprintf
  BL      Proc_OneParamAndString  // Call the VarArgs in Delphi routine

  LDP     X29, X30, [SP], #16     // Restore frame pointer and link register
  RET
end;

procedure SV_BroadcastPrintf_cdecl(Level: Integer; AFormat: PChar); cdecl;
asm
// ARM64 assembly
  STP     X29, X30, [SP, #-16]!  // Save frame pointer and link register
  MOV     X29, SP                 // Set up frame pointer

  ADD     X0, X29, #16            // Point to first variable parameter
  LDR     X1, [X29, #12]          // Load format string
  LDR     X2, [X29, #8]           // Load print_level
  MOV     X3, #3                  // Indicate SV_BroadCastPrintf
  BL      Proc_OneParamAndString  // Call the VarArgs in Delphi routine

  LDP     X29, X30, [SP], #16     // Restore frame pointer and link register
  RET
end;

procedure VID_Printf_cdecl(APrint_Level: Integer; AFormat: PChar); cdecl;
asm
// ARM64 assembly
  STP     X29, X30, [SP, #-16]!  // Save frame pointer and link register
  MOV     X29, SP                 // Set up frame pointer

  ADD     X0, X29, #16            // Point to first variable parameter
  LDR     X1, [X29, #12]          // Load format string
  LDR     X2, [X29, #8]           // Load print_level
  MOV     X3, #1                  // Indicate VID_Printf
  BL      Proc_OneParamAndString  // Call the VarArgs in Delphi routine

  LDP     X29, X30, [SP], #16     // Restore frame pointer and link register
  RET
end;

procedure VID_Error_cdecl(AError_Level: Integer; AFormat: PChar); cdecl;
asm
// ARM64 assembly
  STP     X29, X30, [SP, #-16]!  // Save frame pointer and link register
  MOV     X29, SP                 // Set up frame pointer

  ADD     X0, X29, #16            // Point to first variable parameter
  LDR     X1, [X29, #12]          // Load format string
  LDR     X2, [X29, #8]           // Load print_level
  MOV     X3, #2                  // Indicate VID_Error
  BL      Proc_OneParamAndString  // Call the VarArgs in Delphi routine

  LDP     X29, X30, [SP], #16     // Restore frame pointer and link register
  RET
end;

end.
