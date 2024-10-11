program erd;

{$IFDEF FPC}
  {$MODE ObjFPC}
{$ENDIF}

uses
  SysUtils,
  {$IFDEF WIN32}
  sys_win   in '..\win32\sys_win.pas',
  vid_dll   in '..\win32\vid_dll.pas',
  snd_win   in '..\win32\snd_win.pas',
  // More Windows-specific units
  {$ENDIF}

  {$IFDEF LINUX}
  sys_linux   in '../linux/sys_linux.pas',
  vid_so      in '../linux/vid_so.pas',
  // More Linux-specific units
  {$ENDIF}

  {$IFDEF DARWIN}
  sys_mac     in '../macos/sys_mac.pas',
  vid_mac     in '../macos/vid_mac.pas',
  snd_mac     in '../macos/snd_mac.pas',
  // More macOS-specific units
  {$ENDIF}

  qfiles      in '../qcommon/qfiles.pas',
  crc         in '../qcommon/crc.pas',
  // Common units for all platforms
  CPas        in '../qcommon/CPas.pas',
  cmd         in '../qcommon/cmd.pas',
  Common      in '../qcommon/Common.pas',
  // Add any more common units.

{$R *.res}

{$IFDEF WIN32}
{$INCLUDE ..\Jedi.inc}
{$ELSE}
{$IFDEF DARWIN}
{$INCLUDE ../macos/Jedi.inc}
{$ELSE}
{$INCLUDE ../Jedi.inc}
{$ENDIF}
{$ENDIF}

var
  Saved8087CW: Word;
  CommandLine: String;
  i: Integer;
begin
  { Save the current FPU state and then disable FPU exceptions }
  {$IFDEF WIN32}
  Saved8087CW := Default8087CW;
  Set8087CW($133f); { Disable all FPU exceptions }
  {$ENDIF}

  { Command line parsing }
  for i := 1 to ParamCount do
    CommandLine := CommandLine + ' ' + ParamStr(i);
  CommandLine := Trim(CommandLine);

  {$IFDEF WIN32}
  WinMain(hInstance, hPrevInst, PChar(CommandLine), CmdShow);
  {$ELSE}
  Main(i, PChar(CommandLine));
  {$ENDIF}

  { Reset FPU to the previous state }
  {$IFDEF WIN32}
  Set8087CW(Saved8087CW);
  {$ENDIF}
end.

