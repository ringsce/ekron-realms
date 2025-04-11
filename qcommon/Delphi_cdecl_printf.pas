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

procedure VID_Printf(Level: Cardinal; Msg: PChar); cdecl;
procedure VID_Error(Level: Cardinal; Msg: PChar); cdecl;
procedure SV_BroadcastPrintf(Level: Cardinal; Msg: PChar); cdecl;
procedure PF_centerprintf(Target: Pointer; Msg: PChar); cdecl;
procedure VID_Printf_safe(Level: Integer; fmt: PChar; const Args: array of const); cdecl;
procedure VID_Error_safe(Level: Integer; fmt: PChar; const Args: array of const); cdecl;
procedure SV_BroadcastPrintf_safe(Level: Integer; fmt: PChar; const Args: array of const); cdecl;
procedure PF_centerprintf_safe(ent: edict_p; fmt: PChar; const Args: array of const); cdecl;
procedure PF_cprintf_safe(ent: edict_p; level: Integer; fmt: PChar; const Args: array of const); cdecl;
procedure PF_dprintf_safe(fmt: PChar; const Args: array of const); cdecl;

procedure PF_dprintf_safe(fmt: PChar; const Args: array of const); cdecl;
begin
  PF_dprintf(PChar(Format(fmt, Args)));
end;



procedure Proc_OneParamAndString(ARoutine: Integer; APrint_Level: Cardinal;
  AFormat: PChar; AParams: Cardinal); cdecl;
var
  S: AnsiString;
begin
  S := FormatString(AFormat, AParams);

  case ARoutine of
    1: // VID_Printf
      VID_Printf(APrint_Level, PChar(S));

    2: // VID_Error
      VID_Error(APrint_Level, PChar(S));

    3: // SV_BroadcastPrintf
      SV_BroadcastPrintf(APrint_Level, PChar(S));

    4: // PF_centerprintf
      PF_centerprintf(Pointer(APrint_Level), PChar(S));
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

procedure PF_dprintf_cdecl(fmt: PChar); cdecl; overload;
var
  dummyInt: LongInt;
  fmtLength: LongWord;
begin
  fmtLength := 1; // Dummy value, could be StrLen(fmt) or similar
  dummyInt := 0;

  // Correct parameter count and match declaration
  Proc_ZeroParamAndString(dummyInt, fmt, fmtLength);
end;


//procedure Proc_OneParamAndString(ent: edict_p; fmt: PChar; indicator: Integer); cdecl; overload;
// Safe string formatting using array of const
function FormatStringSafe(const AFormat: string; const Args: array of const): string;
begin
  try
    Result := Format(AFormat, Args);
  except
    Result := '[Format Error]';
  end;
end;

// Implementation of each printf-style function
procedure VID_Printf_safe(Level: Integer; fmt: PChar; const Args: array of const); cdecl;
begin
  VID_Printf(Level, PChar(FormatStringSafe(fmt, Args)));
end;

procedure VID_Error_safe(Level: Integer; fmt: PChar; const Args: array of const); cdecl;
begin
  VID_Error(Level, PChar(FormatStringSafe(fmt, Args)));
end;

procedure SV_BroadcastPrintf_safe(Level: Integer; fmt: PChar; const Args: array of const); cdecl;
begin
  SV_BroadcastPrintf(Level, PChar(FormatStringSafe(fmt, Args)));
end;

procedure PF_centerprintf_safe(ent: edict_p; fmt: PChar; const Args: array of const); cdecl;
begin
  PF_centerprintf(ent, PChar(FormatStringSafe(fmt, Args)));
end;

procedure PF_cprintf_safe(ent: edict_p; level: Integer; fmt: PChar; const Args: array of const); cdecl;
begin
  PF_cprintf(ent, level, PChar(FormatStringSafe(fmt, Args)));
end;

procedure PF_dprintf_safe(fmt: PChar; const Args: array of const); cdecl;
begin
  PF_dprintf(PChar(FormatStringSafe(fmt, Args)));
end;

end.
