{----------------------------------------------------------------------------}
{                                                                            }
{ File(s): vid_dll.c                                                         }
{ Content:                                                                   }
{                                                                            }
{ Initial conversion by : Scott Price                                        }
{ Initial conversion on : 12-Jan-2002                                        }
{                                                                            }
{ This File contains part of convertion of Quake2 source to ObjectPascal.    }
{ More information about this project can be found at:                       }
{ http://www.sulaco.co.za/quake2/                                            }
{                                                                            }
{ Copyright (C) 1997-2001 Id Software, Inc.                                  }
{                                                                            }
{ This program is free software; you can redistribute it and/or              }
{ modify it under the terms of the GNU General Public License                }
{ as published by the Free Software Foundation; either version 2             }
{ of the License, or (at your option) any later version.                     }
{                                                                            }
{ This program is distributed in the hope that it will be useful,            }
{ but WITHOUT ANY WARRANTY; without even the implied warranty of             }
{ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                       }
{                                                                            }
{ See the GNU General Public License for more details.                       }
{                                                                            }
{----------------------------------------------------------------------------}

unit vid_macos; // Renamed to accurately reflect its platform focus

interface

{ Main windowed and fullscreen graphics interface module. This module
  is used for both the software and OpenGL rendering versions of the
  Quake refresh engine. }

uses
  { POSIX Standard Units for Linux/macOS }
  {$IFDEF LINUX}
  BaseUnix, Unix,
  {$ENDIF}

  {$IFDEF DARWIN}
  MacOSAll,
  {$ENDIF}

  { Common Pascal Units }
  SysUtils,

  { Own Units }
  ref, // This unit is crucial for 're' (renderer export)
  keys,
  cvar,
  vid_h,

  {$IFDEF LINUX}
  snd_sdl in '../linux/snd_sdl.pas',
  in_linux in '../linux/in_linux.pas',
  sys_linux in '../linux/sys_linux.pas',
  {$ENDIF}

  {$IFDEF DARWIN}
  snd_mac in '../mac/snd_mac.pas',
  // Note: sys_mac is moved to implementation uses if only used there
  {$ENDIF}

  cl_scrn,
  Common,
  q_shared,
  Console,
  snd_dma,
  Client;

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
// MainWndProc and related Windows-specific messaging is removed as it's for Windows GUI
// You will need a platform-specific windowing and event handling mechanism for macOS/Linux.
// This typically involves libraries like SDL, GLFW, or native Cocoa/X11 APIs.

// These are still needed if the refresh DLL calls them back (e.g., for logging/errors)
procedure VID_Printf(print_level: Integer; fmt: PChar; args: array of const);
procedure VID_Error(err_level: integer; fmt: PChar; args: array of const);

const
  MAXPRINTMSG = 4096; // Still relevant for message buffers

var
  // Structure containing functions exported from refresh DLL
  re: refexport_t; // 're' is declared here. Ensure refexport_t is visible from 'ref' unit.

  // Console variables that we need to access from this module
  vid_gamma: cvar_p;
  vid_ref: cvar_p;          // Name of Refresh DLL loaded
  vid_xpos: cvar_p;         // X coordinate of window position
  vid_ypos: cvar_p;         // Y coordinate of window position
  vid_fullscreen: cvar_p;

  // Global variables used internally by this module
  viddef: viddef_t;         // global video state; used by other modules

  // cl_hwnd (HWND) is a Windows-specific handle. This will need to be replaced
  // with a platform-appropriate window handle type (e.g., NSWindow* for Cocoa,
  // SDL_Window* for SDL). For now, it's removed.
  // cl_hwnd: HWND;

  // scantokey and MapKey are Windows-specific keyboard scan code mappings.
  // This will need a new implementation for macOS/Linux input.
  scantokey: array[0..128 - 1] of byte = (
    0, 27, byte('1'), byte('2'), byte('3'), byte('4'), byte('5'), byte('6'),
    byte('7'), byte('8'), byte('9'), byte('0'), byte('-'), byte('='), K_BACKSPACE, 9, // 0
    byte('q'), byte('w'), byte('e'), byte('r'), byte('t'), byte('y'), byte('u'), byte('i'),
    byte('o'), byte('p'), byte('['), byte(']'), 13, K_CTRL, byte('a'), byte('s'), // 1
    byte('d'), byte('f'), byte('g'), byte('h'), byte('j'), byte('k'), byte('l'), byte(';'),
    byte(''''), byte('`'), K_SHIFT, byte('\'), byte('z'), byte('x'), byte('c'), byte('v'), // 2
    byte('b'), byte('n'), byte('m'), byte(','), byte('.'), byte('/'), K_SHIFT, byte('*'),
    K_ALT, byte(' '), 0, K_F1, K_F2, K_F3, K_F4, K_F5, // 3
    K_F6, K_F7, K_F8, K_F9, K_F10, K_PAUSE, 0, K_HOME,
    K_UPARROW, K_PGUP, K_KP_MINUS, K_LEFTARROW, K_KP_5, K_RIGHTARROW, K_KP_PLUS, K_END, //4
    K_DOWNARROW, K_PGDN, K_INS, K_DEL, 0, 0, 0, K_F11,
    K_F12,
    // Fill the remaining 101 elements with zeros to satisfy the array size
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 // <-- No comma after the very last element
  ); // <-- This is the *only* closing parenthesis for the entire array initializer


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

const
  VID_NUM_MODES = (sizeof(vid_modes) div sizeof(vid_modes[0])); // Changed / to div for integer division

implementation

uses
  // Removed cd_win (Windows CD Audio)
  cl_main,
  Cmd,
  Files,
  vid_menu,
  CPas,
  // Platform-specific system unit for macOS/Linux
  {$IFDEF LINUX}
  sys_linux in '../linux/sys_linux.pas',
  {$ENDIF}
  {$IFDEF DARWIN}
  sys_mac in '../mac/sys_mac.pas' // sys_mac is moved here
  {$ENDIF}
  ;

var
  // Static Variables
  // MSH_MOUSEWHEEL: Cardinal; // Windows-specific message ID, removed
  // s_alttab_disabled: qboolean; // Windows-specific flag, removed
  reflib_library: LongWord;    // Handle to refresh DLL/Shared Object
  reflib_active: qboolean = False;
  // win_noalttab: cvar_p; // Windows-specific cvar, removed

// WIN_DisableAltTab and WIN_EnableAltTab are Windows-specific, removed.
// You'll need to implement equivalent platform-specific window management for fullscreen/focus.

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
    // MessageBox is Windows-specific. You need a platform-agnostic way to show alerts.
    // For now, it will just output to debug.
    Com_Printf('ALERT: %s'#10, [msg]); // Fallback to console print
    // OutputDebugString is Windows-specific. You might use SysUtils.OutputDebugString or a custom debug log.
  end;
end;

procedure VID_Error(err_level: integer; fmt: PChar; args: array of const);
var
  msg: array[0..MAXPRINTMSG - 1] of char;
begin
  DelphiStrFmt(msg, fmt, args);
  Com_Error(err_level, '%s', [msg]);
end;

(* ============
VID_Restart_f

Console command to re-start the video mode and refresh DLL. We do this
simply by setting the modified flag for the vid_ref variable, which will
cause the entire video mode and refresh DLL to be reset on the next frame.
============ *)

procedure VID_Restart_f; cdecl;
begin
  vid_ref^.modified := True; // Access as pointer
end;

procedure VID_Front_f; cdecl;
begin
  // SetWindowLong and SetForegroundWindow are Windows-specific.
  // You'll need to use platform-specific APIs for bringing the window to front.
  // For example, in Cocoa (macOS), you'd interact with NSWindow.
end;

(* =======
MapKey

Map from windows to quake keynums
======= *)

function MapKey(key: Integer): Integer;
var
  iResult: Integer;
  modified: Integer;
  is_extended: qboolean;
begin
  // This logic is highly dependent on Windows WM_KEYDOWN/WM_SYSKEYDOWN lParam.
  // For macOS/Linux, you'll get different key codes (e.g., from SDL, Cocoa, X11).
  // You will need to rewrite this function based on the input system you use.
  // For now, it's kept as-is, but be aware it won't work correctly without input system changes.

  modified := (key shr 16) and 255;
  is_extended := False;

  if (modified > 127) then
  begin
    Result := 0;
    Exit;
  end;

  if (key and (1 shl 24) <> 0) then
    is_extended := True;

  iResult := scantokey[modified];

  if (not is_extended) then
  begin
    case iResult of
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
    else
      Result := iResult;
    end;
  end
  else
  begin
    case iResult of
      $0D: Result := K_KP_ENTER;
      $2F: Result := K_KP_SLASH;
      $AF: Result := K_KP_PLUS;
    else
      Result := iResult;
    end;
  end;
end;

procedure AppActivate(fActive: Boolean; minimize: Boolean);
var
  Minimized: Boolean;
begin
  Minimized := minimize;

  Key_ClearStates;

  // we don't want to act like we're active if we're minimized
  if (fActive and (not Minimized)) then
    ActiveApp := Integer(True)
  else
    ActiveApp := Integer(False);

  // minimize/restore mouse-capture on demand
  if (ActiveApp = 0) then
  begin
    IN_Activate(False);
    CDAudio_Activate(False); // CDAudio_Activate might need platform-specific implementation
    S_Activate(False);

    // win_noalttab cvar and its functions were Windows-specific, removed.
  end
  else
  begin
    IN_Activate(True);
    CDAudio_Activate(True);
    S_Activate(True);

    // win_noalttab cvar and its functions were Windows-specific, removed.
  end;
end;

(* ====================
MainWndProc

main window procedure
==================== *)

// This function is entirely Windows-specific due to HWND, uMsg, wParam, lParam,
// and Windows messages like WM_MOUSEWHEEL, WM_HOTKEY, WM_CREATE, etc.
// It MUST be rewritten for macOS/Linux using platform-native windowing APIs
// (e.g., Cocoa for macOS, X11 for Linux, or a cross-platform library like SDL).
// For now, I'm removing it entirely, as it won't compile or function on non-Windows.

// function MainWndProc(h_Wnd: HWND; uMsg: Cardinal; wParam: WPARAM; lParam: LPARAM): LongInt;
// begin
//   // ... (Removed Windows-specific message handling)
//   Result := 0; // Placeholder
// end;


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

(*** VID_UpdateWindowPosAndSize ***)

procedure VID_UpdateWindowPosAndSize(x, y: Integer); cdecl;
var
  // TRECT, GetWindowLong, AdjustWindowRect, MoveWindow are Windows-specific.
  // You'll need to use platform-specific equivalents for window positioning and sizing.
  // r: TRECT;
  // style, w, h: Integer;
begin
  // Implementation for macOS/Linux window sizing and positioning
  // This will require calls to Cocoa/X11 or SDL APIs
  // For now, the Windows-specific calls are commented out/removed.
  // r.left := 0;
  // r.top := 0;
  // r.right := viddef.width;
  // r.bottom := viddef.height;

  // style := GetWindowLong(cl_hwnd, GWL_STYLE);
  // AdjustWindowRect(r, style, FALSE);

  // w := (r.right - r.left);
  // h := (r.bottom - r.top);

  // MoveWindow(cl_hwnd, Round(vid_xpos^.value), Round(vid_ypos^.value), w, h, TRUE);
end;

(*** VID_NewWindow ***)

procedure VID_NewWindow(width, height: Integer); cdecl;
begin
  viddef.width := width;
  viddef.height := height;

  cl.force_refdef := True; // can't use a paused refdef
end;

procedure VID_FreeReflib;
begin
  // FreeLibrary is Windows-specific. For macOS/Linux, you'll use dlclose for shared libraries.
  // However, Free Pascal's DynLibs unit provides platform-agnostic functions.
  // Assuming 'reflib_library' is a handle obtained via LoadLibrary equivalent (e.g., LoadLibrary_ or fpdlopen).
  if (not FreeLibrary(reflib_library)) then // Assuming FreeLibrary is a cross-platform wrapper or placeholder
    Com_Error(ERR_FATAL, 'Reflib FreeLibrary failed', []);

  FillChar(re, SizeOf(re), 0);

  reflib_library := 0;
  reflib_active := False;
end;

(* ==============
VID_LoadRefresh
============== *)

function VID_LoadRefresh(name: PChar): qboolean; cdecl;
var
  ri: refimport_t;
  GetRefAPI: GetRefAPI_t;
begin
  if (reflib_active) then
  begin
    re.Shutdown;
    VID_FreeReflib;
  end;

  Com_Printf('------- Loading %s -------'#10, [name]);

  // LoadLibrary is Windows-specific. Replace with a platform-agnostic
  // (e.g., from DynLibs unit) or platform-specific equivalent (dlopen on POSIX).
  // Free Pascal's SysUtils.LoadLibrary can often handle this cross-platform.
  reflib_library := LoadLibrary(name);
  if (reflib_library = 0) then
  begin
    Com_Printf('LoadLibrary("%s") failed'#10, [name]);

    Result := False;
    Exit;
  end;

  ri.Cmd_AddCommand := Cmd_AddCommand;
  ri.Cmd_RemoveCommand := Cmd_RemoveCommand;
  ri.Cmd_Argc := Cmd_Argc;
  ri.Cmd_Argv := Cmd_Argv;
  ri.Cmd_ExecuteText := Cbuf_ExecuteText;
  ri.Con_Printf := VID_Printf_cdecl; // Ensure VID_Printf_cdecl is defined as cdecl
  ri.Sys_Error := VID_Error_cdecl;   // Ensure VID_Error_cdecl is defined as cdecl
  ri.FS_LoadFile := FS_LoadFile;
  ri.FS_FreeFile := FS_FreeFile;
  ri.FS_Gamedir := FS_Gamedir;
  ri.Cvar_Get := Cvar_Get;
  ri.Cvar_Set := Cvar_Set;
  ri.Cvar_SetValue := Cvar_SetValue;
  ri.Vid_GetModeInfo := VID_GetModeInfo;
  ri.Vid_MenuInit := VID_MenuInit;
  ri.Vid_NewWindow := VID_NewWindow;

  // GetProcAddress is Windows-specific. Use GetProcedureAddress (from DynLibs)
  // or dlsym (on POSIX) or a cross-platform wrapper.
  GetRefApi := GetProcAddress(reflib_library, 'GetRefAPI'); // Assuming GetProcAddress is a cross-platform wrapper
  if not Assigned(GetRefApi) then
    Com_Error(ERR_FATAL, 'GetProcAddress failed on %s', [name]);

  // global_hInstance and MainWndProc are Windows-specific.
  // You'll need to adapt 're.Init' to use platform-agnostic handles or
  // an SDL window pointer, or remove them if the renderer doesn't need them.
  // For now, replaced with nil and a placeholder MainWndProc.
  // If 're.Init' absolutely requires a window handle, you'll need to create one
  // via SDL, GLFW, or native APIs before calling this.
  if (re.Init(nil, nil) = -1) then // Pass nil for HWND and MainWndProc placeholder
  begin
    re.Shutdown;
    VID_FreeReflib;
    Result := False;
    Exit;
  end;

  Com_Printf('------------------------------------'#10, []);
  reflib_active := True;

  //======
  //PGM
  vidref_val := VIDREF_OTHER;
  if Assigned(vid_ref) then
  begin
    if (StrComp(vid_ref^.string_, 'gl') = 0) then
      vidref_val := VIDREF_GL
    else if (StrComp(vid_ref^.string_, 'soft') = 0) then
      vidref_val := VIDREF_SOFT;
  end;
  //PGM
  //======

  Result := True;
end;

{*
============
VID_CheckChanges

This function gets called once just before drawing each frame, and it's sole purpose in life
is to check to see if any of the video mode parameters have changed, and if they have to
update the rendering DLL and/or video mode to match.
============
*}

procedure VID_CheckChanges;
var
  name: array[0..100 - 1] of Char;
begin
  // win_noalttab and WIN_DisableAltTab/WIN_EnableAltTab are Windows-specific, removed.
  // if (win_noalttab^.modified) then
  // begin
  //   if (win_noalttab^.value <> 0) then
  //     WIN_DisableAltTab
  //   else
  //     WIN_EnableAltTab;
  //   win_noalttab^.modified := False;
  // end;

  if (vid_ref^.modified) then // Access as pointer
  begin
    cl.force_refdef := True;  // can't use a paused refdef
    S_StopAllSounds;
  end;

  while (vid_ref^.modified) do // Access as pointer
  begin
    // refresh has changed
    vid_ref^.modified := False; // Access as pointer
    vid_fullscreen^.modified := True; // Access as pointer
    cl.refresh_prepped := False;
    cls.disable_screen := Integer(True);

    // Use 'lib' extension for shared libraries on Unix-like systems
    {$IFDEF LINUX}
    Com_sprintf(name, SizeOf(name), 'ref_%s.so', [vid_ref^.string_]);
    {$ELSEIF DEF DARWIN}
    Com_sprintf(name, SizeOf(name), 'ref_%s.dylib', [vid_ref^.string_]);
    {$ELSE}
    // Fallback if no specific OS defined, though this unit is macOS/Linux focused
    Com_sprintf(name, SizeOf(name), 'ref_%s.so', [vid_ref^.string_]);
    {$ENDIF}

    if (not VID_LoadRefresh(name)) then
    begin
      if (CompareStr(vid_ref^.string_, 'soft') = 0) then // Access as pointer
        Com_Error(ERR_FATAL, 'Couldn''t fall back to software refresh!', []);

      Cvar_Set('vid_ref', 'soft');

      // drop the console if we fail to load a refresh
      if (cls.key_dest <> key_console) then
        Con_ToggleConsole_f;
    end;

    cls.disable_screen := Integer(False);
  end;

  // update our window position
  if (vid_xpos^.modified or vid_ypos^.modified) then // Access as pointer
  begin
    if (vid_fullscreen^.value = 0) then // Access as pointer
      VID_UpdateWindowPosAndSize(Round(vid_xpos^.value), Round(vid_ypos^.value)); // Access as pointer

    vid_xpos^.modified := False; // Access as pointer
    vid_ypos^.modified := False; // Access as pointer
  end;
end;

(* ============
VID_Init
============ *)

procedure VID_Init;
begin
  // Create the video variables so we know how to start the graphics drivers
  vid_ref := Cvar_Get('vid_ref', 'soft', CVAR_ARCHIVE);
  vid_xpos := Cvar_Get('vid_xpos', '3', CVAR_ARCHIVE);
  vid_ypos := Cvar_Get('vid_ypos', '22', CVAR_ARCHIVE);
  vid_fullscreen := Cvar_Get('vid_fullscreen', '0', CVAR_ARCHIVE);
  vid_gamma := Cvar_Get('vid_gamma', '1', CVAR_ARCHIVE);
  // win_noalttab: cvar_p; // Windows-specific, removed
  // win_noalttab := Cvar_Get('win_noalttab', '0', CVAR_ARCHIVE);

  // Add some console commands that we want to handle
  Cmd_AddCommand('vid_restart', @VID_Restart_f);
  Cmd_AddCommand('vid_front', @VID_Front_f);

  // Disable the 3Dfx splash screen (Windows-specific environment variable)
  // Windows.SetEnvironmentVariable('FX_GLIDE_NO_SPLASH', '0'); // Removed

  // Start the graphics mode and load refresh DLL
  VID_CheckChanges;
end;

(* ============
VID_Shutdown
============ *)

procedure VID_Shutdown;
begin
  if (reflib_active) then
  begin
    re.Shutdown;
    VID_FreeReflib;
  end;
end;

end.
