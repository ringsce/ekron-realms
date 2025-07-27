unit vid_macos;

{$mode objfpc}{$H+} // objfpc mode for classes, {$H+} for strings as AnsiString by default

interface

{ Main windowed and fullscreen graphics interface module. This module
  is used for both the software and OpenGL rendering versions of the
  Quake refresh engine. }

uses
  SysUtils, // Needed for LoadPackage, GetProcAddress
  SDL2,     // **Crucial: Added SDL2 for cross-platform windowing, input, etc.**

  // Own Quake Units
  ref,      // This unit is crucial for 're' (renderer export)
  keys,
  cvar,
  vid_h,
  Client,   // Provides ActiveApp (ensure ActiveApp is in Client.pas interface var section)
  cl_scrn,
  Common,
  q_shared,
  Console,
  snd_dma;  // Provides S_Activate (ensure S_Activate is in its interface)

// Platform-specific sound and system units, conditionally included
// This ensures the unit remains portable between macOS (Darwin) and Linux.
{$IFDEF LINUX}
snd_sdl in '../linux/snd_sdl.pas', // Keep if still using SDL for sound specifically here
sys_linux in '../linux/sys_linux.pas'; // Keep if still needed for other system calls
{$ENDIF}

{$IFDEF DARWIN}
snd_mac in '../mac/snd_mac.pas', // Keep if still using native macOS sound, or replace with SDL sound
sys_mac in '../mac/sys_mac.pas'; // Keep if still needed for other system calls
{$ENDIF}


type
  vidmode_p = ^vidmode_t;
  vidmode_t = packed record
    description: PChar;
    width, height: Integer;
    mode: Integer;
  end;

// Forward declarations for procedures/functions used in the interface section
procedure VID_CheckChanges;
procedure VID_Init;
procedure VID_Shutdown;

// These are still needed if the refresh DLL calls them back (e.g., for logging/errors)
procedure VID_Printf(print_level: Integer; fmt: PChar; args: array of const); cdecl;
procedure VID_Error(err_level: integer; fmt: PChar; args: array of const); cdecl;

// Procedures for activating/deactivating the application focus, input, audio, etc.
procedure AppActivate(fActive: Boolean; minimize: Boolean); cdecl;
procedure VID_UpdateWindowPosAndSize(x, y: Integer); cdecl;
procedure VID_NewWindow(width, height: Integer); cdecl;
function VID_GetModeInfo(width, height: PInteger; mode: integer): qboolean; cdecl;

const
  MAXPRINTMSG = 4096;

  {──── 128‑entry PC/USB scan‑code → Quake key map ────}
  // This map is still useful for translating raw input (like SDL scancodes) to Quake's internal key codes.
  // Note: SDL_Scancode enum values are generally higher than 127. This array might need to be
  // expanded or `MapKey` adjusted to handle the full range of SDL_Scancode.
  // For now, assuming only the lower 128 values are used or mapped.
  scantokey: array[0..127] of Byte = (
      // 00-0F
      0, 27,  Ord('1'), Ord('2'), Ord('3'), Ord('4'), Ord('5'), Ord('6'),
    Ord('7'), Ord('8'), Ord('9'), Ord('0'), Ord('-'), Ord('='), K_BACKSPACE, 9,
      // 10-1F
    Ord('q'), Ord('w'), Ord('e'), Ord('r'), Ord('t'), Ord('y'), Ord('u'), Ord('i'),
    Ord('o'), Ord('p'), Ord('['), Ord(']'), 13, K_CTRL,  Ord('a'), Ord('s'),
      // 20-2F
    Ord('d'), Ord('f'), Ord('g'), Ord('h'), Ord('j'), Ord('k'), Ord('l'), Ord(';'),
    Ord(''''),Ord('`'), K_SHIFT, Ord('\'), Ord('z'), Ord('x'), Ord('c'), Ord('v'),
      // 30-3F
    Ord('b'), Ord('n'), Ord('m'), Ord(','), Ord('.'), Ord('/'), K_SHIFT, Ord('*'),
    K_ALT,  Ord(' '), 0, K_F1, K_F2, K_F3, K_F4, K_F5,
      // 40-4F
    K_F6, K_F7, K_F8, K_F9, K_F10, K_PAUSE, 0, K_HOME,
    K_UPARROW, K_PGUP, K_KP_MINUS, K_LEFTARROW, K_KP_5, K_RIGHTARROW, K_KP_PLUS, K_END,
      // 50-5F
    K_DOWNARROW, K_PGDN, K_INS, K_DEL, 0, 0, 0, K_F11,
    K_F12, 0, 0, 0, 0, 0, 0, 0,
      // 60-6F
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      // 70-7F
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0
  );

  vid_modes: array[0..10] of vidmode_t =
  (
    (description: 'Mode 0: 320x240'; width: 320; height: 240; mode: 0),
    (description: 'Mode 1: 400x300'; width: 400; height: 300; mode: 1),
    (description: 'Mode 2: 512x384'; width: 512; height: 384; mode: 2),
    (description: 'Mode 3: 640x480'; width: 640; height: 480; mode: 3),
    (description: 'Mode 4: 800x600'; width: 800; height: 600; mode: 4),
    (description: 'Mode 5: 960x720'; width: 960; height: 720; mode: 5),
    (description: 'Mode 6: 1024x768'; width: 1024; height: 768; mode: 6),
    (description: 'Mode 7: 1152x864'; width: 1152; height: 864; mode: 7),
    (description: 'Mode 8: 1280x960'; width: 1280; height: 960; mode: 8),
    (description: 'Mode 9: 1600x1200'; width: 1600; height: 1200; mode: 9),
    (description: 'Mode 10: 2048x1536'; width: 2048; height: 1536; mode: 10)
  );

var
  // Structure containing functions exported from refresh DLL
  re: refexport_t; // 're' is declared here. Ensure refexport_t is visible from 'ref' unit.

  // Console variables that we need to access from this module
  vid_gamma: cvar_p;
  vid_ref: cvar_p;        // Name of Refresh DLL loaded
  vid_xpos: cvar_p;       // X coordinate of window position
  vid_ypos: cvar_p;       // Y coordinate of window position
  vid_fullscreen: cvar_p;

  // Global variables used internally by this module
  viddef: viddef_t;       // global video state; used by other modules

  // Global SDL Window pointer, managed by the main program
  // This will be set by your main program unit (e.g., in Quake2Main.lpr)
  // and passed to relevant functions. It's crucial for SDL2 operations.
  QuakeWindow: PSDL_Window = nil;

const
  VID_NUM_MODES = (sizeof(vid_modes) div sizeof(vid_modes[0]));

implementation

uses
  cl_main,
  Cmd,
  Files,
  CPas, // For DelphiStrFmt
  // Platform-specific system unit (moved from interface to implementation if only used here)
  {$IFDEF LINUX}
  sys_linux, // No longer needs `in` clause if in standard search path
  {$ENDIF}
  {$IFDEF DARWIN}
  sys_mac // No longer needs `in` clause if in standard search path
  {$ENDIF}
  ;

var
  // Static Variables
  reflib_library: TLibHandle;
  reflib_active: qboolean = False;
  vidref_val: Integer; // Added for PGM section

// These are now C-compatible wrappers for the procedures above, used for function pointers
// in refimport_t.
procedure VID_Printf_cdecl(print_level: Integer; fmt: PChar; args: array of const); cdecl;
begin
  VID_Printf(print_level, fmt, args);
end;

procedure VID_Error_cdecl(err_level: integer; fmt: PChar; args: array of const); cdecl;
begin
  VID_Error(err_level, fmt, args);
end;

procedure VID_Printf(print_level: Integer; fmt: PChar; args: array of const);
var
  msg: array[0..MAXPRINTMSG - 1] of char;
begin
  DelphiStrFmt(msg, fmt, args);
  if (print_level = PRINT_ALL) then
    Com_Printf('%s', [msg])
  else if (print_level = PRINT_DEVELOPER) then
    Com_DPrintf('%s', [msg])
  else if (print_level = PRINT_ALERT) then
  begin
    // For a real game, you might want a GUI alert here (e.g., SDL_ShowSimpleMessageBox)
    Com_Printf('ALERT: %s'#10, [msg]); // Fallback to console print
  end;
end;

procedure VID_Error(err_level: integer; fmt: PChar; args: array of const);
var
  msg: array[0..MAXPRINTMSG - 1] of char;
begin
  DelphiStrFmt(msg, fmt, args);
  Com_Error(err_level, '%s', [msg]);
end;

procedure VID_Restart_f; cdecl;
begin
  // Access as pointer
  vid_ref^.modified := True;
end;

procedure VID_Front_f; cdecl;
begin
  if Assigned(QuakeWindow) then
  begin
    SDL_RaiseWindow(QuakeWindow); // Bring the window to front
  end;
end;

function MapKey(key: Integer): Integer;
// Assuming 'key' here is an SDL_Scancode (which is an Integer type in SDL2 bindings).
// The scantokey array currently only supports indices up to 127.
// SDL_Scancode values can be much higher (e.g., SDL_SCANCODE_GRAVE is 52, SDL_SCANCODE_KP_ENTER is 88).
// For a robust solution, you'd need a larger `scantokey` array or a different mapping strategy.
var
  scancode_index: Integer;
begin
  scancode_index := key; // Direct use of SDL_Scancode as index

  if (scancode_index < 0) or (scancode_index >= Length(scantokey)) then // Use Length for array size
  begin
    Result := 0; // Invalid scancode
    Exit;
  end;

  Result := scantokey[scancode_index];

  // Additional handling for specific keys if SDL's default mapping doesn't align
  // with Quake's K_KP_ constants for numpad/extended keys.
  // This part needs careful testing with actual SDL events.
  // Example: SDL_SCANCODE_KP_ENTER could map directly to K_KP_ENTER.
  case Result of
    K_HOME: Result := K_KP_HOME;
    K_UPARROW: Result := K_KP_UPARROW;
    K_PGUP: Result := K_KP_PGUP;
    K_LEFTARROW: Result := K_KP_LEFTARROW;
    K_RIGHTARROW: Result := K_KP_RIGHTARROW;
    K_END: Result := K_KP_END;
    K_DOWNARROW: Result := K_KP_DOWNARROW;
    K_PGDN: Result := K_KP_PGDN;
    K_INS: Result := K_KP_INS;
    K_DEL: Result := K_KP_DEL;
    // Add more specific SDL to Quake key mappings as needed.
  end;
end;

function VID_GetModeInfo(width, height: PInteger; mode: integer): qboolean; cdecl;
begin
  if (mode < 0) or (mode >= VID_NUM_MODES) then
  begin
    Result := False;
    Exit;
  end;

  width^ := vid_modes[mode].width;
  height^ := vid_modes[mode].height;

  Result := True;
end;

procedure AppActivate(fActive, minimize: Boolean); cdecl;
begin
  Minimized := minimize;
  Key_ClearStates;

  // An app is “active” only when not minimised.
  ActiveApp := Ord( fActive and (not Minimized) );

  if ActiveApp = 0 then
  begin
    // Release mouse grab and deactivate input/sound when inactive.
    SDL_SetRelativeMouseMode(SDL_FALSE);
    IN_Activate(False); // Assuming IN_Activate exists in an input unit.
    S_Activate(False);
    // CDAudio_Activate(False); // Explicitly commented out.
  end
  else
  begin
    // Grab mouse and activate input/sound when active.
    SDL_SetRelativeMouseMode(SDL_TRUE);
    IN_Activate(True); // Assuming IN_Activate exists in an input unit.
    S_Activate(True);
    // CDAudio_Activate(True); // Explicitly commented out.
  end;
end;

procedure VID_UpdateWindowPosAndSize(x, y: Integer); cdecl;
begin
  if Assigned(QuakeWindow) then
  begin
    SDL_SetWindowPosition(QuakeWindow, x, y);
  end
  else
  begin
    Com_Printf(PRINT_ALERT, 'VID_UpdateWindowPosAndSize called before SDL window is created.');
  end;
end;

procedure VID_NewWindow(width, height: Integer); cdecl;
begin
  viddef.width := width;
  viddef.height := height;

  if Assigned(QuakeWindow) then
  begin
    SDL_SetWindowSize(QuakeWindow, width, height);
    // Handle fullscreen toggle
    if vid_fullscreen^.value <> 0 then // If fullscreen is desired
    begin
      // SDL_WINDOW_FULLSCREEN_DESKTOP for borderless fullscreen, SDL_WINDOW_FULLSCREEN for true fullscreen.
      SDL_SetWindowFullscreen(QuakeWindow, SDL_WINDOW_FULLSCREEN_DESKTOP);
    end
    else
    begin
      SDL_SetWindowFullscreen(QuakeWindow, 0); // Windowed mode
    end;
  end;

  cl.force_refdef := True; // Can't use a paused refdef.
end;

procedure VID_FreeReflib;
begin
  if (reflib_library <> 0) then
  begin
    SysUtils.UnloadPackage(reflib_library);
  end;

  FillChar(re, SizeOf(re), 0);
  reflib_library := 0;
  reflib_active := False;
end;

function VID_LoadRefresh(name: PChar): qboolean; cdecl;
type
  // Define the type for the GetRefAPI function pointer, as expected by the refresh DLL
  TGetRefAPI = function(imp: refimport_t): refexport_t; cdecl;
var
  ri: refimport_t;
  GetRefAPI_Proc: TGetRefAPI;
begin
  if (reflib_active) then
  begin
    re.Shutdown;
    VID_FreeReflib;
  end;

  Com_Printf('------- Loading %s -------'#10, [name]);

  // Use SysUtils.LoadPackage for cross-platform dynamic library loading
  reflib_library := SysUtils.LoadPackage(name);
  if (reflib_library = 0) then
  begin
    Com_Printf('LoadPackage("%s") failed: %s'#10, [name, SysUtils.SysErrorMessage(SysUtils.GetLastError)]);
    Result := False;
    Exit;
  end;

  // Populate the refimport_t structure with pointers to our functions
  ri.Cmd_AddCommand := Cmd_AddCommand;
  ri.Cmd_RemoveCommand := Cmd_RemoveCommand;
  ri.Cmd_Argc := Cmd_Argc;
  ri.Cmd_Argv := Cmd_Argv;
  ri.Cmd_ExecuteText := Cbuf_ExecuteText;
  ri.Con_Printf := @VID_Printf_cdecl; // Pass address of wrapper procedure
  ri.Sys_Error := @VID_Error_cdecl;    // Pass address of wrapper procedure
  ri.FS_LoadFile := FS_LoadFile;
  ri.FS_FreeFile := FS_FreeFile;
  ri.FS_Gamedir := FS_Gamedir;
  ri.Cvar_Get := Cvar_Get;
  ri.Cvar_Set := Cvar_Set;
  ri.Cvar_SetValue := Cvar_SetValue;
  ri.Vid_GetModeInfo := VID_GetModeInfo;
  ri.Vid_MenuInit := VID_MenuInit;
  ri.Vid_NewWindow := VID_NewWindow;

  // Use SysUtils.GetProcAddress for cross-platform procedure address retrieval
  GetRefAPI_Proc := SysUtils.GetProcAddress(reflib_library, 'GetRefAPI');
  if not Assigned(GetRefAPI_Proc) then
  begin
    Com_Error(ERR_FATAL, 'GetProcAddress failed for "GetRefAPI" in %s: %s', [name, SysUtils.SysErrorMessage(SysUtils.GetLastError)]);
    VID_FreeReflib; // Ensure library is freed on error
    Result := False;
    Exit;
  end;

  // Call the GetRefAPI function from the loaded DLL to get the exported functions
  re := GetRefAPI_Proc(ri);

  // Pass the SDL window and OpenGL context (or nil if managed by renderer) to the renderer's Init function.
  // QuakeWindow should be set by the main program (Quake2Main.lpr) before this.
  if (re.Init(QuakeWindow, nil) = -1) then // nil for GL context if renderer handles it, or if it's a software renderer
  begin
    re.Shutdown;
    VID_FreeReflib;
    Result := False;
    Exit;
  end;

  Com_Printf('------------------------------------'#10, []);
  reflib_active := True;

  // Set vidref_val based on loaded renderer
  if Assigned(vid_ref) then
  begin
    if (StrComp(vid_ref^.string_, 'gl') = 0) then
      vidref_val := VIDREF_GL
    else if (StrComp(vid_ref^.string_, 'soft') = 0) then
      vidref_val := VIDREF_SOFT
    else
      vidref_val := VIDREF_OTHER;
  end else begin
    vidref_val := VIDREF_OTHER; // Default if vid_ref cvar isn't set
  end;

  Result := True;
end;

procedure VID_CheckChanges;
var
  name: array[0..100 - 1] of Char;
begin
  if (vid_ref^.modified) then
  begin
    cl.force_refdef := True;
    S_StopAllSounds;
  end;

  while (vid_ref^.modified) do
  begin
    // Refresh has changed
    vid_ref^.modified := False;
    vid_fullscreen^.modified := True; // Mark fullscreen for update after refresh load
    cl.refresh_prepped := False;
    cls.disable_screen := Integer(True);

    // Use 'dylib' for macOS and 'so' for Linux shared libraries
    {$IFDEF DARWIN}
    Com_sprintf(name, SizeOf(name), 'ref_%s.dylib', [vid_ref^.string_]);
    {$ELSEIF DEF LINUX}
    Com_sprintf(name, SizeOf(name), 'ref_%s.so', [vid_ref^.string_]);
    {$ENDIF}

    if (not VID_LoadRefresh(name)) then
    begin
      if (CompareStr(vid_ref^.string_, 'soft') = 0) then
        Com_Error(ERR_FATAL, 'Couldn''t fall back to software refresh!', []);

      Cvar_Set('vid_ref', 'soft');

      // Drop the console if we fail to load a refresh
      if (cls.key_dest <> key_console) then
        Con_ToggleConsole_f;
    end;

    cls.disable_screen := Integer(False);
  end;

  // Update window position and fullscreen state after refresh is loaded/checked
  if (vid_xpos^.modified or vid_ypos^.modified or vid_fullscreen^.modified) then
  begin
    if Assigned(QuakeWindow) then
    begin
      // Update window position if not in fullscreen
      if (vid_fullscreen^.value = 0) then
      begin
        VID_UpdateWindowPosAndSize(Round(vid_xpos^.value), Round(vid_ypos^.value));
      end;

      // Apply fullscreen mode if changed
      if (vid_fullscreen^.modified) then
      begin
        if (vid_fullscreen^.value <> 0) then
          SDL_SetWindowFullscreen(QuakeWindow, SDL_WINDOW_FULLSCREEN_DESKTOP)
        else
          SDL_SetWindowFullscreen(QuakeWindow, 0); // Windowed mode
        vid_fullscreen^.modified := False;
      end;
    end;

    vid_xpos^.modified := False;
    vid_ypos^.modified := False;
  end;
end;

procedure VID_Init;
begin
  // Create the video variables so we know how to start the graphics drivers
  vid_ref := Cvar_Get('vid_ref', 'soft', CVAR_ARCHIVE);
  vid_xpos := Cvar_Get('vid_xpos', '3', CVAR_ARCHIVE);
  vid_ypos := Cvar_Get('vid_ypos', '22', CVAR_ARCHIVE);
  vid_fullscreen := Cvar_Get('vid_fullscreen', '0', CVAR_ARCHIVE);
  vid_gamma := Cvar_Get('vid_gamma', '1', CVAR_ARCHIVE);

  // Add some console commands that we want to handle
  Cmd_AddCommand('vid_restart', @VID_Restart_f);
  Cmd_AddCommand('vid_front', @VID_Front_f);

  // Start the graphics mode and load refresh DLL
  VID_CheckChanges;
end;

procedure VID_Shutdown;
begin
  if (reflib_active) then
  begin
    re.Shutdown;
    VID_FreeReflib;
  end;
end;

end.
