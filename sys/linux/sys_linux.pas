{----------------------------------------------------------------------------}
{                                                                            }
{ File(s): sys_win.c                                                         }
{                                                                            }
{ Initial conversion by : Softland (softland_gh@ureach.com)                  }
{ Initial conversion on : 07-Jan-2002                                        }
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
{ Updated on : 03-jun-2002                                                   }
{ Updated by : Juha Hartikainen (juha@linearteam.org)                        }
{ - Fixed uses clause                                                        }
{----------------------------------------------------------------------------}

unit sys_linux;

interface

uses
  BaseUnix, Unix, UnixType, // Linux-specific API
  SysUtils, Math,
  Files in '../game/Files.pas',
  Common,
  cl_main,
  q_shared,
  // The fpGetInputReady function is in the Unix unit on some FPC versions
  // It is used for non-blocking console input.
  TermIOS; // for Terminal I/O functions

implementation

const
  MINIMUM_SYS_MEMORY = $0A00000;
  MAXIMUM_SYS_MEMORY = $1000000;
  MAX_NUM_ARGVS = 128;

var
  sys_frame_time: Cardinal;
  starttime: TUnixTime; // Use a Linux-specific time type
  argc: Integer;
  argv: array[0..MAX_NUM_ARGVS - 1] of PChar;
  console_text: array[0..255] of Char;
  console_textlen: Integer;

  game_library: Pointer;
  global_hInstance: Pointer; // Retained for compatibility but will be null

procedure Sys_Error(error: PChar; args: array of const);
procedure Sys_Quit;
function Sys_ScanForCD: PChar;
procedure Sys_CopyProtect;
procedure Sys_Init;
function Sys_ConsoleInput: PChar;
procedure Sys_ConsoleOutput(aString: PChar);
procedure Sys_SendKeyEvents;
function Sys_GetClipboardData: PChar;
procedure Sys_AppActivate;
procedure Sys_Unloadgame;
function Sys_GetGameAPI(parms: Pointer): Pointer;

procedure SysError;
procedure ParseCommandLine(cmdLine: PChar);
function Sys_Main(argc: Integer; argv: PPChar): Integer;

var
  cddir: array[0..MAX_OSPATH - 1] of Char;
  done: qboolean;

procedure Sys_Error(error: PChar; args: array of const);
var
  text: string;
begin
  CL_Shutdown;
  Qcommon_Shutdown;

  // Report error
  text := Format(error, args);

  {$IFDEF MSWINDOWS}
    MessageBox(0, PChar(text), 'Error', MB_OK or MB_ICONERROR);
  {$ELSE}
    // On Linux/macOS, just print to stderr
    Writeln(StdErr, 'ERROR: ', text);
  {$ENDIF}

  {$IFDEF MSWINDOWS}
    if qwclsemaphore <> 0 then
      CloseHandle(qwclsemaphore);
    DeinitConProc;
  {$ENDIF}

  Halt(1);
end;

procedure Sys_Quit;
begin
  {$IFDEF MSWINDOWS}
  timeEndPeriod(1);
  CloseHandle(qwclsemaphore);
  if (dedicated <> nil) and (dedicated.value <> 0) then
    FreeConsole;
  DeinitConProc();
  {$ENDIF}

  {$IFDEF LINUX}
  // No timeEndPeriod on Linux
  // If you want similar behavior for timer cleanup, use setitimer() or clock_nanosleep()
  // No FreeConsole or ConProc — just rely on normal stdout/stderr cleanup
  {$ENDIF}

  {$IFDEF DARWIN}
  // macOS uses same cleanup as Linux
  {$ENDIF}

  CL_Shutdown;
  Qcommon_Shutdown;

  Halt(0);
end;

procedure SysError;
var
  errnum: cint;
  errstr: string;
begin
  {$IFDEF MSWINDOWS}
    // Windows version
    var lpMsgBuf: PChar;
    FormatMessage(
      FORMAT_MESSAGE_ALLOCATE_BUFFER or FORMAT_MESSAGE_FROM_SYSTEM,
      nil,
      GetLastError,
      (SUBLANG_DEFAULT shl 10) or LANG_NEUTRAL,
      PChar(@lpMsgBuf),
      0,
      nil);
    MessageBox(0, lpMsgBuf, 'GetLastError', MB_OK or MB_ICONINFORMATION);
    LocalFree(HLOCAL(lpMsgBuf));
  {$ELSE}
    // Linux / macOS version
    errnum := fpgeterrno;  // gets the last errno
    errstr := StrError(errnum); // returns error string for errno
    Writeln(StdErr, 'System Error ', errnum, ': ', errstr);
  {$ENDIF}
end;

function Sys_ScanForCD: PChar;

{$IFNDEF DEMO}

var
  path: string;
  drive: string;

{$ENDIF}

begin

  {$IFNDEF DEMO}
  function Sys_ScanForCD: PChar;
  var
    mountPoints: array[0..4] of string = (
      '/media/',
      '/mnt/',
      '/run/media/',
      '/Volumes/',  // macOS and some Linux distros
      '/cdrom/'     // older Linux distros
    );
    mountPath: string;
    i: Integer;
    exePath: string;
  begin
    // Don't re-check
    if done then
    begin
      Result := cddir;
      Exit;
    end;

    done := True;

    // Scan known CD/DVD mount points
    for i := Low(mountPoints) to High(mountPoints) do
    begin
      mountPath := mountPoints[i] + 'install/data';
      exePath := IncludeTrailingPathDelimiter(mountPath) + 'quake2.exe';

      // We can't check drive type like on Windows, so we assume CD if the file exists
      if FileExists(exePath) then
      begin
        StrPCopy(cddir, mountPath);
        Result := cddir;
        Exit;
      end;
    end;
  {$ENDIF}

    cddir[0] := #0;
    Result := nil;
  end;

procedure Sys_CopyProtect;
var
  DataFileExists: Boolean;
begin
  // For a modern port, you could check for the presence of the game data file.
  // This is a simple example and may not be the best approach for a full engine.
  DataFileExists := FileExists('baseq2/pak0.pak');

  if not DataFileExists then
  begin
    // Com_Error is likely defined elsewhere in the engine.
    Com_Error(ERR_FATAL, 'Quake2 data files not found. Make sure "baseq2/pak0.pak" is in the correct directory.');
  end;
end;


procedure Sys_Init;
begin
  //
  // Mutex and Semaphore logic is not needed for a standard
  // Linux port as modern systems handle process management differently.
  // The original Quake 2 used this to prevent multiple instances
  // and for inter-process communication with a front-end launcher.
  // Modern ports typically don't require this.
  //
  // The original version check is also removed. Linux kernels
  // don't have a "version 4 or greater" dependency like Windows,
  // and the platform IDs are Windows-specific.
  //

  Randomize; // Initializes the random number generator.

  //
  // timeBeginPeriod(1) is a Windows-specific function to
  // increase timer resolution. On Linux, this is handled
  // by the kernel or other libraries (like SDL) that a
  // modern port would use for timing. So, we remove it.
  //

  // The original code checked for dedicated server mode to
  // allocate a console. On Linux, a console is always available
  // when the program is run from a terminal. The equivalent
  // of AllocConsole is not needed. The standard I/O handles
  // are always available.
  if dedicated.value <> 0 then
  begin
    // For a dedicated server, we can simply initialize the
    // console procedure with the standard I/O streams.
    // The hinput and houtput variables are no longer needed
    // as we don't have to create a new console.
    InitConProc(argc, @argv[0]);
  end;
end;

function Sys_ConsoleInput: PChar;
var
  ch: Char;
  bytesRead: Integer;
begin
  Result := nil;
  // This check for 'dedicated' is still valid, as it determines
  // if the game is a dedicated server and needs console input.
  if (dedicated = nil) or (dedicated.value = 0) then
    Exit;

  // The original loop for reading console events is replaced with a simple
  // check to see if there's any data waiting on standard input (stdin).
  // This is a much simpler and more direct approach on Linux.

  // Check if there is data on stdin without blocking.
  if fpGetInputReady(stdin) then
  begin
    // Read a single character from stdin.
    bytesRead := Read(stdin, ch, 1);
    if bytesRead > 0 then
    begin
      case Ord(ch) of
        13: // Enter key (or carriage return)
        begin
          // On Linux, a newline is typically a single #10.
          // We can use WriteLn, which is a simpler way to handle this.
          WriteLn;
          if console_textlen <> 0 then
          begin
            console_text[console_textlen] := #0;
            console_textlen := 0;
            Result := console_text;
            Exit;
          end;
        end;

        8, 127: // Backspace key (ASCII 8) or Delete key (ASCII 127)
        begin
          if console_textlen > 0 then
          begin
            Dec(console_textlen);
            // We use standard terminal escape sequences to move the
            // cursor back, print a space, and move back again.
            Write(Chr(8), ' ', Chr(8));
          end;
        end;

        else // Any other printable character
          if (Ord(ch) >= 32) then
          begin
            if console_textlen < (SizeOf(console_text) - 2) then
            begin
              Write(ch);
              console_text[console_textlen] := ch;
              Inc(console_textlen);
            end;
          end;
      end;
    end;
  end;
end;

procedure Sys_ConsoleOutput(aString: PChar);
var
  i: Integer;
begin
  if (dedicated = nil) or (dedicated.value = 0) then
    Exit;

  // On Linux, the standard way to handle this is to first save the cursor position,
  // then output the new text, and finally restore the cursor.
  // We use standard terminal escape codes for this.

  // 1. Clear the current command line by moving the cursor back to the start of the line.
  //    and clearing from the cursor to the end of the line.
  Write(Chr(13)); // Carriage return, moves cursor to the beginning of the current line.
  Write(Chr(27), '[K'); // ANSI escape code to clear line from cursor to end.

  // 2. Output the new string.
  Write(aString);

  // 3. Re-print the console input line that was cleared.
  if console_textlen <> 0 then
  begin
    // We move the cursor to the new line before re-printing the prompt.
    WriteLn;

    // We can directly write the console_text, as it is a null-terminated string.
    Write(console_text);
  end;

  // We should also print a newline after the output string to ensure it's
  // on a separate line from the input prompt.
  WriteLn;
end;

procedure Sys_SendKeyEvents;
var
  aMsg: TMSG;
begin
  while PeekMessage(aMsg, 0, 0, 0, PM_NOREMOVE) do
  begin
    if not GetMessage(aMsg, 0, 0, 0) then
      Sys_Quit;
    sys_msg_time := aMsg.time;
    TranslateMessage(aMsg);
    DispatchMessage(aMsg);
  end;
  sys_frame_time := timeGetTime;
end;

function Sys_GetClipboardData: PChar;
var
  data: PChar;
  cliptext: PChar;
  hClipboardData: THandle;
begin
  data := nil;
  if OpenClipboard(0) then
  begin
    hClipboardData := GetClipboardData(CF_TEXT);
    if hClipboardData <> 0 then
    begin
      cliptext := GlobalLock(hClipboardData);
      if cliptext <> nil then
      begin
        GetMem(data, GlobalSize(hClipboardData) + 1);
        StrCopy(data, cliptext);
        GlobalUnlock(hClipboardData);
      end;
    end;
    CloseClipboard;
  end;
  Result := data;
end;

procedure Sys_AppActivate;
begin
  ShowWindow(cl_hwnd, SW_RESTORE);
  SetForegroundWindow(cl_hwnd);
end;

procedure Sys_Unloadgame;
begin
  if not FreeLibrary(game_library) then
    Com_Error(ERR_FATAL, 'FreeLibrary failed for game library');
  game_library := 0;
end;

function Sys_GetGameAPI(parms: Pointer): Pointer;
var
  GetGameAPI: function(parms: Pointer): Pointer; cdecl;
  name: array[0..MAX_OSPATH - 1] of Char;
  cwd: array[0..MAX_OSPATH - 1] of Char;
  path: PChar;

const
  gamename: PChar = 'gamex86.dll';
{$IFNDEF DEBUG}
  debugdir: PChar = 'release';
{$ELSE}
  debugdir: PChar = 'debug';
{$ENDIF}

begin
  if game_library <> 0 then
    Com_Error(ERR_FATAL, 'Sys_GetGameAPI without Sys_UnloadingGame');

  // Check the current debug directory first, for development purposes.
  GetCurrentDirectory(SizeOf(cwd), @cwd);
  Com_sprintf(name, SizeOf(name), '%s/%s/%s', [cwd, debugdir, gamename]);
  game_library := LoadLibrary(name);
  if game_library <> 0 then
    Com_DPrintf('LoadLibrary (%s)'#10, [name])
  else
  begin

{$IFDEF DEBUG}

    // Not found? Check the current directory, for other development purposes
    Com_sprintf(name, SizeOf(name), '%s/%s', [cwd, gamename]);
    game_library := LoadLibrary(name);
    if game_library <> 0 then
      Com_DPrintf('LoadLibrary (%s)'#10, [name])
    else

{$ENDIF}

    begin

      // Still not found? Run through the search paths
      path := nil;
      while True do
      begin
        path := FS_NextPath(path);
        if path = nil then
        begin
          Result := nil;
          Exit;
        end;
        Com_sprintf(name, SizeOf(name), '%s/%s', [path, gamename]);
        game_library := LoadLibrary(name);
        if game_library <> 0 then
        begin
          Com_DPrintf('LoadLibrary (%s)'#10, [name]);
          Break;
        end;
      end;

    end;

  end;

  // Gets here, if the library was found.
  GetGameAPI := GetProcAddress(game_library, 'GetGameAPI');
  if not Assigned(GetGameAPI) then
  begin
    Sys_Unloadgame;
    Result := nil;
    Exit;
  end;

  Result := GetGameAPI(parms);
end;

procedure ParseCommandLine(lpCmdLine: LPSTR);
begin
  argc := 1;
  argv[0] := 'exe';

  // This is to break the command line string down to one or more sub-strings,
  // so that each sub-string contains one argument.
  // In addition, it calculates the number of the arguments.
  while (lpCmdLine^ <> #0) and (argc < MAX_NUM_ARGVS) do
  begin
    // Skip "white-space" to the first/next argument.
    while (lpCmdLine^ <> #0) and ((Ord(lpCmdLine^) <= 32) or (Ord(lpCmdLine^) > 126)) do
      Inc(lpCmdLine);

    // Check to see if it's the end of the command line.
    if lpCmdLine^ <> #0 then
    begin
      // Keep a new reference (pointer) to the argument
      argv[argc] := lpCmdLine;
      Inc(argc);

      // Skip the argument to the next white space.
      while (lpCmdLine^ <> #0) and ((Ord(lpCmdLine^) > 32) and (Ord(lpCmdLine^) <= 126)) do
        Inc(lpCmdLine);

      // Split the command line between the arguments,
      // by placing a zero after the end of each argument.
      if lpCmdLine^ <> #0 then
      begin
        lpCmdLine^ := #0;
        Inc(lpCmdLine);
      end;
    end;
  end;
end;

function WinMain(hInstance, hPrevInstance: HINST; lpCmdLine: LPSTR; nCmdShow: Integer): Integer; stdcall;
var
  aMsg: TMSG;
  time, oldtime, newtime: Integer;
  cddir: PChar;
  i: Integer;
begin

  // This is to make sure that previous instances do not exist.
  if hPrevInstance <> 0 then
  begin
    Result := 0;
    Exit;
  end;

  global_hInstance := hInstance;
  ParseCommandLine(lpCmdLine);
  cddir := Sys_ScanForCD;

  if (cddir <> nil) and (argc < MAX_NUM_ARGVS - 3) then
  begin
    i := 0;

    // Search for "cddir" in the command line.
    while i < argc do
    begin
      if StrComp(argv[i], 'cddir') = 0 then
        Break;
      i := i + 1;
    end;

    // If "cddir" is not in the command line,
    // add the following arguments: "+set", "cddir" and the value of cddir.
    if i = argc then
    begin
      argv[argc] := '+set';
      Inc(argc);
      argv[argc] := 'cddir';
      Inc(argc);
      argv[argc] := cddir;
      Inc(argc);
    end;
  end;

  // Initialize Quake2.
  Qcommon_Init(argc, @argv[0]);
  oldtime := Sys_Milliseconds;

  // The main window message loop.
  while True do
  begin
    if Minimized or ((dedicated <> nil) and (dedicated.value <> 0)) then
      Sleep(1);

    // Process messages.
    while PeekMessage(aMsg, 0, 0, 0, PM_NOREMOVE) do
    begin
      if not GetMessage(aMsg, 0, 0, 0) then
        Com_Quit;
      sys_msg_time := aMsg.time;
      TranslateMessage(aMsg);
      DispatchMessage(aMsg);
    end;

    // Wait more than 1 ms.
    repeat
      newtime := Sys_Milliseconds;
      time := newtime - oldtime;
    until time >= 1;

    {
    Con_Printf('time:%5.2f - %5.2f = %5.2f'#10, [newtime, oldtime, time]);
    SetExceptionMask([exDenormalized, exOverflow, exUnderflow, exPrecision]);
    }

    SetPrecisionMode(pmExtended);
    Qcommon_Frame(time);

    oldtime := newtime;
  end;

  Result := 1;
end;

end.
