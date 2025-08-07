unit CVar;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface
uses
  {$IFDEF WIN32}
  Windows,
  {$ENDIF}
  SysUtils, // For StrComp, StrLen, StrAlloc, StrCopy, Assigned, FillChar (instead of FillByte sometimes)
  Strings,  // For StrNew (used in CopyString)
  // Memory,   // Z_Malloc, Z_Free - external declarations are usually fine, but if they are functions, this unit helps.
  // Assuming these provide necessary types/functions, but ensure they don't redefine cvar_p/Tcvar_t
  Q_Shared in '../game/Q_Shared.pas',
  CPas,
  //cmd in '..\qcommon\cmd.pas',
  q_shared_add in '../game/q_shared_add.pas';

// --- IMPORTANT: Define the canonical cvar_t record here (or in Q_Shared/q_shared_add and use it) ---
// I'm defining it here to make CVar.pas self-contained for compilation purposes,
// but ideally, this should live in Q_Shared or q_shared_add if it's truly a shared type.
type
  // Define the pointer type first, referring to the (yet-to-be-defined) record
  cvar_p = ^Tcvar_t;
  qboolean = Boolean;


  // Now, fully define the record type. This is what 'cvar_t' refers to.
  Tcvar_t = record
    name: PChar;
    string_: PChar;
    latched_string: PChar; // For CVAR_LATCH vars
    flags: Integer;
    modified: Boolean;    // set each time the cvar is changed
    value: Single;
    next: cvar_p;         // This is now valid because cvar_p is known
  end;

  // Define pcchar for consistency
  pcchar = PChar; // PChar is generally the correct type for C-style strings

  // Definition for TCmdFunction (needed by Cmd_AddCommand)
  TCmdFunction = procedure; cdecl;

const
  CVAR_USERINFO   = $0001;
  CVAR_SERVERINFO = $0002;
  // These were used later in the code, define them here
  CVAR_ARCHIVE    = $0001; // Or whatever its actual value is in Quake2 (often 1)
  CVAR_NOSET      = $0008;
  CVAR_LATCH      = $0010; // Often 16 for latching behavior
  MAX_INFO_STRING = 512; // Example value, confirm actual value from Q_Shared.h if possible

  // In the implementation section (e.g., just before the first function implementation)
var
  info_buffer_global: array[0..MAX_INFO_STRING - 1] of Char; // This is your persistent global buffer

// memory management (external declarations for engine functions)
function Z_Malloc(size: SizeInt): Pointer; cdecl; external; // Added overload for safety
procedure Z_Free(ptr: Pointer); cdecl; external;

// string operations (external declarations, but you have an internal CopyString implementation)
// If CopyString is truly external, this line should be uncommented.
// If it's internal, keep the implementation below and remove 'external'.
// function CopyString(str: PChar): PChar; cdecl; external;

function atof(p: pcchar): Single; cdecl; external; // Quake2 atof

// file system
procedure FS_SetGamedir(dir: PChar); cdecl; external;
procedure FS_ExecAutoexec; cdecl; external;

// console
procedure Com_Printf(const fmt: pcchar; args: array of const); cdecl; external; // Added overload for safety
function Com_ServerState: Integer; cdecl; external;
function Cmd_Argc: Integer; cdecl; external;
function Cmd_Argv(index: Integer): pcchar; cdecl; external;
procedure Com_sprintf(Dest: PChar; DestSize: Integer; const Fmt: PChar; const Args: array of const); cdecl; external; // Added as it's used
procedure Info_SetValueForKey(info: PChar; key: PChar; value: PChar); cdecl; external; // Added as it's used

// external command registration (used by Cvar_Init)
procedure Cmd_AddCommand(name: PChar; func: TCmdFunction); cdecl; external; // Add external

// --- helpers implemented inside this unit ---------------------------
function Cvar_InfoValidate(p: pcchar): qboolean; // Changed return type to qboolean
function Cvar_FindVar(p: pcchar): cvar_p;
function Cvar_Get(var_name, var_value: pcchar; flags: Integer): cvar_p;
function Cvar_Set(var_name, value: PChar): cvar_p; cdecl;
function Cvar_ForceSet(var_name, value: PChar): cvar_p; cdecl;
function Cvar_FullSet(var_name, value: PChar; flags: Integer): cvar_p; cdecl;
procedure Cvar_SetValue(var_name: PChar; value: Single); cdecl;
function Cvar_VariableValue(var_name: PChar): Single; cdecl;
(*function Cvar_VariableString(var_name: PChar): PChar; cdecl;
function Cvar_CompleteVariable(partial: PChar): PChar; cdecl;
*)
procedure Cvar_GetLatchedVars; cdecl;
function Cvar_Command: qboolean; cdecl;
procedure Cvar_WriteVariables(path: PChar); cdecl;
procedure Cvar_Init; cdecl;
function Cvar_Userinfo_: PChar; cdecl;
function Cvar_Serverinfo_: PChar; cdecl;

var
  cvar_vars: cvar_p = nil; { global linked list }
  userinfo_modified: qboolean; // global variable used in Cvar_Set2


implementation // THIS IS THE ONLY IMPLEMENTATION KEYWORD

{ --- Internal CopyString implementation --- }
function CopyString(const S: pcchar): pcchar;
begin
  Result := StrNew(S); // StrNew allocates new memory and copies the string
end;

(*
============
Cvar_InfoValidate
  Rejects strings that contain \  "  or ;
============
*)
function Cvar_InfoValidate(p: pcchar): qboolean;
begin
  if (StrPos(p, PChar('\')) <> nil)  or   // back-slash
     (StrPos(p, PChar('"')) <> nil)  or   // double-quote
     (StrPos(p, PChar(';')) <> nil) then  // semicolon
    Result := False
  else
    Result := True;
end;


(*
============
Cvar_FindVar
============
*)
// static cvar_t *Cvar_FindVar (char *var_name)
function Cvar_FindVar(p: pcchar): cvar_p; // Changed 'var_name' to 'p'
var
  var_: cvar_p;
begin
  var_ := cvar_vars;
  while (var_ <> nil) do
  begin
    // Use 'p' instead of 'var_name'
    if (StrComp(p, var_^.name) = 0) then // Fixed: use 'p'
    begin
      Result := var_;
      Exit;
    end;
    var_ := var_^.next;
  end;
  Result := nil;
end;


function Cvar_BitInfo(bit: Integer): PChar;
// Remove the 'var info_buffer: ...' line from here
var
  var_: cvar_p; // Keep this local variable
begin
  // Fill the GLOBAL buffer, not a local one
  FillChar(info_buffer_global, SizeOf(info_buffer_global), 0);

  var_ := cvar_vars;
  while (var_ <> nil) do
  begin
    if (var_^.flags and bit) <> 0 then
      // Use the global buffer here
      Info_SetValueForKey(info_buffer_global, var_^.name, var_^.string_);
    var_ := var_^.next;
  end;

  // Return a PChar pointing to the start of the GLOBAL buffer.
  // This pointer will remain valid after the function returns.
  Result := @info_buffer_global[0];
end;
(*============
Cvar_Get
If the variable already exists, the value will not be set
The flags will be or'ed in if the variable exists.
============
*)
function Cvar_Get(var_name, var_value: pcchar; flags: Integer): cvar_p;
var
  var_: cvar_p;
begin
  Result := nil;

  if (flags and (CVAR_USERINFO or CVAR_SERVERINFO) <> 0) then
  begin
    if not Cvar_InfoValidate(var_name) then
    begin
      Com_Printf('invalid info cvar name'#10, []); // Pass empty array for []
      Exit;
    end;
  end;

  var_ := Cvar_FindVar(var_name);
  if Assigned(var_) then
  begin
    var_^.flags := var_^.flags or flags;
    Result := var_;
    Exit;
  end;

  if (var_value = nil) then // Check for nil PChar
    Exit;

  if (flags and (CVAR_USERINFO or CVAR_SERVERINFO) <> 0) then
  begin
    if not Cvar_InfoValidate(var_value) then
    begin
      Com_Printf('invalid info cvar value'#10, []); // Pass empty array for []
      Exit;
    end;
  end;

  { allocate and zero-fill new cvar record }
  var_ := Z_Malloc(SizeOf(Tcvar_t)); // Use Tcvar_t for SizeOf
  FillByte(var_^, SizeOf(Tcvar_t), 0); // FillByte requires actual size, not just pointer size

  var_^.name := CopyString(var_name);
  var_^.string_ := CopyString(var_value);
  var_^.modified := True;
  var_^.value := atof(var_^.string_);
  var_^.flags := flags; // Flags should be set here

  { link into global list }
  var_^.next := cvar_vars;
  cvar_vars := var_;

  Result := var_;
end;

(*
============
Cvar_Set2
============
*)
function Cvar_Set2(var_name, value: PChar; force: qboolean): cvar_p;
var
  var_: cvar_p;
begin
  var_ := Cvar_FindVar(var_name);
  if (var_ = nil) then
  begin                                   // create it
    Result := Cvar_Get(var_name, value, 0);
    Exit;
  end;

  if (var_^.flags and (CVAR_USERINFO or CVAR_SERVERINFO) <> 0) then
  begin
    if not Cvar_InfoValidate(value) then
    begin
      Com_Printf('invalid info cvar value'#10, []);
      Result := var_;
      Exit;
    end;
  end;

  if (not force) then
  begin
    if ((var_^.flags and CVAR_NOSET) <> 0) then
    begin
      Com_Printf('%s is write protected.'#10, [var_name]);
      Result := var_;
      Exit;
    end;

    if (var_^.flags and CVAR_LATCH) <> 0 then
    begin
      if (var_^.latched_string <> nil) then
      begin
        if (StrComp(value, var_^.latched_string) = 0) then
        begin
          Result := var_;
          Exit;
        end;
        Z_Free(var_^.latched_string);
      end
      else
      begin
        if (StrComp(value, var_^.string_) = 0) then
        begin
          Result := var_;
          Exit;
        end;
      end;

      if (Com_ServerState <> 0) then
      begin
        Com_Printf('%s will be changed for next game.'#10, [var_name]);
        var_^.latched_string := CopyString(value);
      end
      else
      begin
        Z_Free(var_^.string_); // Free old string before overwriting
        var_^.string_ := CopyString(value);
        var_^.value := atof(var_^.string_);
        if (StrComp(var_^.name, 'game') = 0) then
        begin
          FS_SetGamedir(var_^.string_);
          FS_ExecAutoexec;
        end;
      end;
      Result := var_;
      Exit;
    end;
  end
  else // force is true
  begin
    if (var_^.latched_string <> nil) then
    begin
      Z_Free(var_^.latched_string);
      var_^.latched_string := nil;
    end;
  end;

  if (StrComp(value, var_^.string_) = 0) then
  begin
    Result := var_;                                // not changed
    Exit;
  end;

  var_^.modified := True;

  if (var_^.flags and CVAR_USERINFO) <> 0 then
    userinfo_modified := True;                     // transmit at next opportunity

  Z_Free(var_^.string_);                          // free the old value string

  var_^.string_ := CopyString(value);
  var_^.value := atof(var_^.string_);
  Result := var_;
end;

(*
============
Cvar_ForceSet
============
*)
function Cvar_ForceSet(var_name, value: PChar): cvar_p;
begin
  Result := Cvar_Set2(var_name, value, True);
end;

(*
============
Cvar_Set
============
*)
function Cvar_Set(var_name, value: PChar): cvar_p;
begin
  Result := Cvar_Set2(var_name, value, False);
end;

(*
============
Cvar_FullSet
============
*)
function Cvar_FullSet(var_name, value: PChar; flags: Integer): cvar_p;
var
  var_: cvar_p;
begin
  var_ := Cvar_FindVar(var_name);
  if (var_ = nil) then
  begin                                   // create it
    Result := Cvar_Get(var_name, value, flags);
    Exit;
  end;

  var_^.modified := True;

  if (var_^.flags and CVAR_USERINFO) <> 0 then
    userinfo_modified := True;                     // transmit at next opportunity

  Z_Free(var_^.string_);                          // free the old value string

  var_^.string_ := CopyString(value);
  var_^.value := atof(var_^.string_);
  var_^.flags := flags;

  Result := var_;
end;

(*
============
Cvar_SetValue
============
*)
procedure Cvar_SetValue(var_name: PChar; value: Single);
var
  val: array[0..31] of Char;
  i: Integer;
begin
  if (value = Round(value)) then
    Com_sprintf(val, SizeOf(val), '%d', [Round(value)]) // SizeOf(val) is correct for array of Char
  else
    Com_sprintf(val, SizeOf(val), '%f', [value]);

  // Juha: Hack to make sure that we use DOT as decimal separator, since
  // Delphi takes it from Windows international settings.
  for i := 0 to 31 do
    if val[i] = ',' then
      val[i] := '.';
  Cvar_Set(var_name, val);
end;

(*
============
Cvar_GetLatchedVars

Any variables with latched values will now be updated
============
*)
procedure Cvar_GetLatchedVars;
var
  var_: cvar_p;
begin
  var_ := cvar_vars;
  while (var_ <> nil) do
  begin
    if (var_^.latched_string = nil) then // Dereference latched_string
    begin
      var_ := var_^.next; // Dereference next
      Continue;
    end;
    Z_Free(var_^.string_); // Dereference string_
    var_^.string_ := var_^.latched_string; // Dereference
    var_^.latched_string := nil; // Dereference
    var_^.value := atof(var_^.string_); // Dereference
    if (StrComp(var_^.name, 'game') = 0) then // Dereference
    begin
      FS_SetGamedir(var_^.string_); // Dereference
      FS_ExecAutoexec;
    end;
    var_ := var_^.next; // Dereference
  end;
end;

(*
========
Cvar_VariableValue *)
function Cvar_VariableValue(var_name: PChar): Single; cdecl;
var
  var_: cvar_p;
begin
  var_ := Cvar_FindVar(var_name);
  if Assigned(var_) then
    Result := var_.value
  else
    Result := 0;
end;

(*
============
Cvar_Command

Handles variable inspection and changing from the console
============
*)
function Cvar_Command: qboolean;
var
  v: cvar_p;
begin
  // check variables
  v := Cvar_FindVar(Cmd_Argv(0));
  if (v = nil) then
  begin
    Result := False;
    Exit;
  end;

  // perform a variable print or set
  if (Cmd_Argc = 1) then
  begin
    Com_Printf('"%s" is "%s"'#10, [v^.name, v^.string_]); // Dereference v
    Result := True;
    Exit;
  end;

  Cvar_Set(v^.name, Cmd_Argv(1)); // Dereference v
  Result := true;
end;

(*
============
Cvar_Set_f

Allows setting and defining of arbitrary cvars from console
============
*)
procedure Cvar_Set_f; cdecl;
var
  c: Integer;
  flags: Integer;
begin
  c := Cmd_Argc;
  if (c <> 3) and (c <> 4) then
  begin
    Com_Printf('usage: set <variable> <value> [u / s]'#10, []);
    Exit;
  end;

  if (c = 4) then
  begin
    if (StrComp(Cmd_Argv(3), 'u') = 0) then
      flags := CVAR_USERINFO
    else if (StrComp(Cmd_Argv(3), 's') = 0) then
      flags := CVAR_SERVERINFO
    else
    begin
      Com_Printf('flags can only be ''u'' or ''s'''#10, []);
      Exit;
    end;
    Cvar_FullSet(Cmd_Argv(1), Cmd_Argv(2), flags);
  end
  else
    Cvar_Set(Cmd_Argv(1), Cmd_Argv(2));
end;

(*
============
Cvar_WriteVariables

Appends lines containing "set variable value" for all variables
with the archive flag set to true.
============
*)
procedure Cvar_WriteVariables(path: PChar);
var
  var_: cvar_p;
  buffer: array[0..1024 - 1] of Char;
  f: Integer;
begin
  f := FileOpen(path, fmOpenWrite); // Assuming FileOpen and FileCreate/FileSeek/FileWrite are defined globally or in uses
  if f = -1 then
  begin
    f := FileCreate(path);
  end
  else
    FileSeek(f, 0, 2); // 2 means seek from end
  var_ := cvar_vars;
  while (var_ <> nil) do
  begin
    if ((var_^.flags and CVAR_ARCHIVE) <> 0) then
    begin
      Com_sprintf(buffer, SizeOf(buffer), 'set %s "%s"'#13#10, [var_^.name, var_^.string_]);
      FileWrite(f, buffer, StrLen(buffer)); // Use StrLen for PChar length
    end;
    var_ := var_^.next;
  end;
  FileClose(f);
end;

(*
============
Cvar_List_f

============
*)
procedure Cvar_List_f; cdecl;
var
  var_: cvar_p;
  i: Integer;
begin
  i := 0;
  var_ := cvar_vars;
  while (var_ <> nil) do
  begin
    if (var_^.flags and CVAR_ARCHIVE) <> 0 then
      Com_Printf('*', [])
    else
      Com_Printf(' ', []);
    if (var_^.flags and CVAR_USERINFO) <> 0 then
      Com_Printf('U', [])
    else
      Com_Printf(' ', []);
    if (var_^.flags and CVAR_SERVERINFO) <> 0 then
      Com_Printf('S', [])
    else
      Com_Printf(' ', []);
    if (var_^.flags and CVAR_NOSET) <> 0 then
      Com_Printf('-', [])
    else if (var_^.flags and CVAR_LATCH) <> 0 then
      Com_Printf('L', [])
    else
      Com_Printf(' ', []);
    Com_Printf(' %s "%s"'#10, [var_^.name, var_^.string_]);

    var_ := var_^.next;
    Inc(i);
  end;
  Com_Printf('%d cvars'#10, [i]);
end;


// returns an info string containing all the CVAR_USERINFO cvars
function Cvar_Userinfo_: PChar;
begin
  Result := Cvar_BitInfo(CVAR_USERINFO);
end;

// returns an info string containing all the CVAR_SERVERINFO cvars
function Cvar_Serverinfo_: PChar;
begin
  Result := Cvar_BitInfo(CVAR_SERVERINFO);
end;

(*
============
Cvar_Init
Reads in all archived cvars
============
*)
procedure Cvar_Init;
begin
  Cmd_AddCommand('set', @Cvar_Set_f);
  Cmd_AddCommand('cvarlist', @Cvar_List_f);
end;

end. // End of unit
