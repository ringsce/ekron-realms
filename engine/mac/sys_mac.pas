unit sys_mac;

{$mode objfpc}{$H+} // objfpc mode for classes, {$H+} for strings as AnsiString by default

interface

uses
  // Removed MacOSAll unless specific, deep Cocoa integration is planned.
  // For basic system, SDL2 abstracts most of it, making direct MacOSAll less common.
  // If specific macOS API calls are still needed (e.g., bundle path, alerts beyond SDL),
  // then include relevant sub-units of MacApi (like MacApi.CoreFoundation, MacApi.AppKit).
  // For now, let's keep it minimal.
  SysUtils,   // Core system utilities (for LoadPackage, GetProcAddress, FileExists, etc.)
  Classes,    // For TList and other base classes if needed
  Common in '../game/qcommon/Common.pas', // Assuming this provides Com_Printf, etc.
  q_shared,   // Shared definitions (like qboolean, MAX_OSPATH)
  SDL2 in '../../../../fpc-units/SDL2.pas';       // Crucial: Add SDL2 for cross-platform system functionality

const
  // These memory values are symbolic for historical context but largely irrelevant for modern macOS.
  MINIMUM_MAC_MEMORY = $0A00000;
  MAXIMUM_MAC_MEMORY = $1000000;
  MAX_NUM_ARGVS = 128; // Max command-line arguments count

  // Moved gamename and debugdir to implementation or a more appropriate global config unit
  // if they are not truly constants needed by other units via the interface.
  // For now, keeping them in interface as they were declared.
  gamename: PChar = 'gamex86.dylib';
{$IFNDEF DEBUG}
  debugdir: PChar = 'release';
{$ELSE}
  debugdir: PChar = 'debug';
{$ENDIF}


type
  // TGetGameAPI should be defined where it's used or as a local type within the function.
  // Moved from interface to implementation to reduce interface clutter.
  // TGetGameAPI = function(parms: Pointer): Pointer; cdecl; // Define the function pointer type

  // **TSysMac class definition**
  TSysMac = class
  private
    sys_msg_time: Cardinal; // Current SDL tick time when last event was processed
    sys_frame_time: Cardinal; // Current SDL tick time for frame timing
    starttime: Cardinal; // Time when system was initialized
    game_library: TLibHandle; // Use TLibHandle for cross-platform dynamic library handle

    argc: Integer; // Command line arguments count
    argv: array[0..MAX_NUM_ARGVS - 1] of PChar; // Command line arguments

  public
    constructor Create;
    destructor Destroy; override; // Override destructor for proper cleanup

    procedure Init; // System initialization
    procedure Shutdown; // System shutdown

    // Cross-platform equivalent of WinError (now uses Com_Error or WriteLn)
    procedure DisplaySystemError;

    // Command line parsing (now takes actual command line string)
    procedure ParseCommandLine(CmdLine: String);

    // Timing functions
    function GetMilliseconds: Cardinal;
    procedure Sleep(ms: Cardinal);

    // Console Input/Output (simplified for game console, not true OS console)
    function ConsoleInput: PChar; // Not a true console input, likely for server mode
    procedure ConsoleOutput(aString: PChar);

    // Clipboard Access
    function GetClipboardData: PChar; // Uses SDL clipboard API

    // Application Activation / Focus (now handled by SDL window focus events)
    procedure AppActivate;

    // Game Module (DLL/Shared Library) Loading
    function GetGameAPI(parms: Pointer): Pointer; // Load game logic module

    // CD-ROM Scanning (Highly specific to physical CDs, might be obsolete for modern games)
    function ScanForCD: PChar; // Placeholder - functionality likely needs review

    // Expose argc and argv for common module
    function GetArgc: Integer;
    function GetArgv(idx: Integer): PChar;

  end;

var
  SysMac: TSysMac; // Global instance of the macOS System helper class

// Moved these global variables to implementation or their relevant units if they are only used there.
// If they are truly global and needed by other units, they can stay, but typically a class
// encapsulates such state.
// GetGameAPI_Proc, name_buffer, cwd_buffer, path_ptr, found_path are local to GetGameAPI.
// Removed them as global variables in the interface.

// Global variables that remain (if truly global to the unit's scope and used across methods)
var
  // dedicated: cvar_p; // Still needs to be accessed from cvar unit
  cddir_internal: array[0..MAX_OSPATH - 1] of Char; // Internal buffer for ScanForCD
  done_cdscan: qboolean = False; // Flag for ScanForCD

implementation

uses
  Files, // For FileExists etc.
  cl_main, // For CL_Shutdown
  Cmd, // For Cmd_Argc, Cmd_Argv, Cmd_ExecuteText if needed
  cvar; // For accessing 'dedicated' cvar

{$DEFINE DEBUG} // Define DEBUG for conditional compilation

{ TSysMac }

constructor TSysMac.Create;
begin
  inherited Create; // Call inherited constructor for proper initialization
  sys_msg_time := 0;
  sys_frame_time := 0;
  starttime := SDL_GetTicks; // Initialize starttime with SDL ticks

  // Initialize SDL Subsystems required by the game (Timer and Events).
  // Video and Audio typically initialized by their respective units (vid_macos, snd_mac).
  if SDL_Init(SDL_INIT_TIMER or SDL_INIT_EVENTS) <> 0 then
  begin
    Com_Error(ERR_FATAL, 'Failed to initialize SDL Timer and Events: %s', [SDL_GetError]);
  end;

  game_library := 0; // TLibHandle initializes to 0 or nil
  argc := 0;
  FillChar(argv, SizeOf(argv), 0); // Initialize argv pointers to nil
  FillChar(cddir_internal, SizeOf(cddir_internal), 0);
  done_cdscan := False;

  // No Windows-specific memory or OS version checks needed.
  // Dedicated server console setup would involve checking `dedicated` cvar and
  // potentially redirecting standard I/O streams using SDL functions or basic Pascal.
  // This constructor primarily sets up SDL basics and class fields.
end;

destructor TSysMac.Destroy;
begin
  // Ensure game library is unloaded on shutdown
  if game_library <> 0 then
    UnloadGame;

  // Quit SDL subsystems. This should be done only once when the application truly exits.
  SDL_Quit;

  inherited Destroy;
end;

procedure TSysMac.Init;
begin
  Com_Printf('sys_mac: Initializing system...');

  // The core SDL_Init call is in the constructor. This Init can be for other non-SDL system setups.
  // For now, it mainly updates timing values.
  sys_msg_time := SDL_GetTicks;
  sys_frame_time := SDL_GetTicks;
  starttime := SDL_GetTicks; // Re-initialize starttime if Init can be called multiple times.

  // No Windows-specific memory setup or console handling.
end;

procedure TSysMac.Shutdown;
begin
  Com_Printf('sys_mac: Shutting down system...');

  // Unload game library if it's still loaded
  if game_library <> 0 then
    UnloadGame;

  // Quit SDL subsystems. This must be the *last* SDL call before the program truly exits.
  // Placing it here and in destructor ensures it's called.
  SDL_Quit;
end;

procedure TSysMac.DisplaySystemError;
begin
  Com_Error(ERR_FATAL, 'System Error: %s', [SDL_GetError]); // Use SDL_GetError for last SDL error
  // For a GUI application, consider SDL_ShowSimpleMessageBox for user-friendly errors.
  // Example: SDL_ShowSimpleMessageBox(SDL_MESSAGEBOX_ERROR, 'Quake2 Error', SDL_GetError, nil);
end;

function TSysMac.GetMilliseconds: Cardinal;
begin
  Result := SDL_GetTicks - starttime;
end;

procedure TSysMac.Sleep(ms: Cardinal);
begin
  SDL_Delay(ms);
end;

function TSysMac.ScanForCD: PChar;
// This function is highly legacy and Windows-specific in its original intent.
// On macOS, it would involve enumerating mounted volumes or bundles.
// For modern games, this is usually obsolete.
begin
  Result := nil; // Default to nil if CD scanning is not applicable or fails.
  if done_cdscan then
  begin
    if cddir_internal[0] <> #0 then
      Result := @cddir_internal[0]; // Return previous result if already scanned
    Exit;
  end;

  Com_Printf('sys_mac: Sys_ScanForCD called. This functionality is likely obsolete for macOS.');
  done_cdscan := True;
  cddir_internal[0] := #0; // Indicate no CD found
end;

function TSysMac.ConsoleInput: PChar;
begin
  Result := nil;
  // This function is for dedicated server console input.
  // On macOS/Linux, it would typically read from stdin (e.g., using ReadLn)
  // or use a more sophisticated console library.
  // For game clients, console commands are usually parsed from user input events.
  // If `dedicated` cvar is true, you might implement non-blocking console input here.
  // Requires a way to check if input is available without blocking (e.g., `KeyPressed` from `Crt` unit,
  // but `Crt` is not cross-platform and might conflict with SDL).
  // A proper SDL-based console system would handle this.
end;

procedure TSysMac.ConsoleOutput(aString: PChar);
var
  dedicated_cvar: cvar_t; // Declare local variable for dedicated cvar
begin
  // Access the 'dedicated' cvar from the cvar unit
  dedicated_cvar := Cvar_FindVar('dedicated');

  if (dedicated_cvar = nil) or (dedicated_cvar.value = 0) then
  begin
    // If not dedicated, output to standard game console (Com_Printf)
    Com_Printf('%s', [aString]);
    Exit;
  end;

  // For dedicated server mode, output to actual console/stdout
  WriteLn(aString);
end;

function TSysMac.GetClipboardData: PChar;
var
  ClipboardText: PAnsiChar;
  len: SizeUInt;
begin
  Result := nil;
  ClipboardText := SDL_GetClipboardText; // Get text from clipboard using SDL
  if Assigned(ClipboardText) then
  begin
    len := StrLen(ClipboardText);
    GetMem(Result, len + 1); // Allocate memory for the string + null terminator
    StrCopy(Result, ClipboardText);
    SDL_FreeClipboardText(ClipboardText); // Free memory allocated by SDL
  end;
end;

procedure TSysMac.AppActivate;
begin
  // On macOS, 'activating' usually means bringing the window to the front.
  // This is typically handled by the video layer (e.g., vid_macos) using SDL_RaiseWindow.
  Com_Printf('sys_mac: AppActivate called (delegated to video layer)');
end;

procedure TSysMac.UnloadGame;
begin
  if game_library <> 0 then
  begin
    if not SysUtils.UnloadPackage(game_library) then // Use UnloadPackage for cross-platform
      Com_Error(ERR_FATAL, 'Failed to unload game library: %s', [SysUtils.SysErrorMessage(SysUtils.GetLastError)]);
    game_library := 0;
  end;
end;

function TSysMac.GetGameAPI(parms: Pointer): Pointer;
type
  TGetGameAPI = function(parms: Pointer): Pointer; cdecl; // Define the function pointer type locally
var
  GetGameAPI_Proc: TGetGameAPI;
  name_buffer: array[0..MAX_OSPATH - 1] of Char;
  cwd_buffer: array[0..MAX_OSPATH - 1] of Char;
  path_ptr_local: PChar; // Local variable to avoid conflict with global path_ptr if it existed
  // The global `path_ptr: THandle;` was inappropriate for FS_NextPath, which expects PChar.
  // Changed it to `path_ptr_local: PChar;` for correct usage here.
  found_path: Boolean = False;
begin
  Result := nil; // Default result

  if game_library <> 0 then
    Com_Error(ERR_FATAL, 'Sys_GetGameAPI without Sys_UnloadGame');

  // Attempt to load from current working directory / debugdir first
  if GetCurrentDir(cwd_buffer, SizeOf(cwd_buffer)) then // GetCurrentDir is SysUtils
  begin
    Com_sprintf(name_buffer, SizeOf(name_buffer), '%s/%s/%s', [cwd_buffer, debugdir, gamename]);
    game_library := SysUtils.LoadPackage(name_buffer);
    if game_library <> 0 then
    begin
      Com_DPrintf('Loaded game library from: %s'#10, [name_buffer]);
      found_path := True;
    end
{$IFDEF DEBUG} // Also check CWD directly in debug builds
    else
    begin
      Com_sprintf(name_buffer, SizeOf(name_buffer), '%s/%s', [cwd_buffer, gamename]);
      game_library := SysUtils.LoadPackage(name_buffer);
      if game_library <> 0 then
      begin
        Com_DPrintf('Loaded game library from: %s'#10, [name_buffer]);
        found_path := True;
      end;
    end;
{$ENDIF}
  end;

  // If not found in debug/cwd, try search paths (FS_NextPath is from Files or Common)
  if not found_path then
  begin
    path_ptr_local := nil;
    while True do
    begin
      // FS_NextPath needs to be available and return PChar.
      path_ptr_local := FS_NextPath(path_ptr_local);
      if path_ptr_local = nil then
        Break; // No more paths

      Com_sprintf(name_buffer, SizeOf(name_buffer), '%s/%s', [path_ptr_local, gamename]);
      game_library := SysUtils.LoadPackage(name_buffer);
      if game_library <> 0 then
      begin
        Com_DPrintf('Loaded game library from: %s'#10, [name_buffer]);
        found_path := True;
        Break;
      end;
    end;
  end;

  if not found_path then
  begin
    Com_Error(ERR_FATAL, 'Could not load game library: %s', [gamename]);
    Exit; // Exit function, Result is already nil
  end;

  // Get the 'GetGameAPI' function address from the loaded library
  GetGameAPI_Proc := SysUtils.GetProcAddress(game_library, 'GetGameAPI');
  if not Assigned(GetGameAPI_Proc) then
  begin
    UnloadGame; // Free the library if the function isn't found
    Com_Error(ERR_FATAL, 'Could not find GetGameAPI entry point in game library: %s', [gamename]);
    Exit;
  end;

  Result := GetGameAPI_Proc(parms); // Call the function and return its result
end;


procedure TSysMac.ParseCommandLine(CmdLine: String);
var
  i: Integer;
  current_arg_start: Integer;
  len: Integer;
begin
  // Clear previous arguments
  for i := 0 to argc - 1 do
  begin
    // Assuming argv elements point into the CmdLine string (modified with #0).
    // If they were StrAlloc'd, they'd need to be FreeMem'd here.
  end;
  argc := 0;

  // Add the executable name as the first argument (argv[0])
  // ParamStr(0) typically provides the executable path in Free Pascal.
  // For simplicity, a placeholder 'app' is used, but a real implementation
  // would get the actual executable name.
  argc := 1;
  argv[0] := PChar('app'); // PChar('app') points to a static string.

  current_arg_start := 1; // Start scanning from the first character of CmdLine

  len := Length(CmdLine);
  while (current_arg_start <= len) and (argc < MAX_NUM_ARGVS) do
  begin
    // Skip leading whitespace
    while (current_arg_start <= len) and (CmdLine[current_arg_start] <= ' ') do
      Inc(current_arg_start);

    if current_arg_start > len then
      Break; // End of string

    // Found start of an argument
    argv[argc] := @CmdLine[current_arg_start]; // Point to the start of the argument
    Inc(argc);

    // Find end of argument (next whitespace or end of string)
    while (current_arg_start <= len) and (CmdLine[current_arg_start] > ' ') do
      Inc(current_arg_start);

    // Null-terminate the argument by placing a #0 at the end of it in the CmdLine string buffer.
    // WARNING: This modifies the input `CmdLine` string. `CmdLine` must be a mutable string type.
    if current_arg_start <= len then
    begin
      CmdLine[current_arg_start] := #0; // Terminate current argument
      Inc(current_arg_start); // Move past the null terminator for the next search
    end;
  end;
end;

function TSysMac.GetArgc: Integer;
begin
  Result := argc;
end;

function TSysMac.GetArgv(idx: Integer): PChar;
begin
  if (idx >= 0) and (idx < argc) then
    Result := argv[idx]
  else
    Result := nil;
end;

end. // This `end.` closes the `implementation` block of the `unit sys_mac;`

initialization
  SysMac := TSysMac.Create; // Create the global instance of TSysMac
finalization
  SysMac.Free; // Free the global instance
end.
