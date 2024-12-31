program realms;

{$IFDEF FPC}
  {$MODE ObjFPC}
{$ENDIF}

uses
  SysUtils,

  {$IFDEF WIN32}
  Windows, // Required for Windows API declarations
  sys_win   in '..\win32\sys_win.pas',
  vid_dll   in '..\win32\vid_dll.pas',
  snd_win   in '..\win32\snd_win.pas',
  {$ENDIF}

  {$IFDEF LINUX}
  sys_linux   in '../linux/sys_linux.pas',
  vid_so      in '../linux/vid_so.pas',
  snd_linux   in '../linux/snd_linux.pas',
  {$ENDIF}

  {$IFDEF DARWIN}
  sys_mac     in '../macos/sys_mac.pas',
  vid_mac     in '../macos/vid_mac.pas',
  snd_mac     in '../macos/snd_mac.pas',
  {$ENDIF}

  {$IFDEF IOS}
  sys_ios     in '../ios/sys_ios.pas', // iOS-specific system code
  vid_ios     in '../ios/vid_ios.pas', // iOS-specific video code
  snd_ios     in '../ios/snd_ios.pas', // iOS-specific sound code
  {$ENDIF}

  {$IFDEF ANDROID}
  sys_android   in '../android/sys_android.pas',
  vid_android   in '../android/vid_android.pas',
  snd_android   in '../android/snd_android.pas',
  {$ENDIF}

  qfiles      in '../qcommon/qfiles.pas',
  crc         in '../qcommon/crc.pas',
  CPas        in '../qcommon/CPas.pas',
  cmd         in '../qcommon/cmd.pas',
  Common      in '../qcommon/Common.pas';

{$R *.res}

{$IFDEF WIN32}
{$INCLUDE ..\Jedi.inc}
{$ELSE}
{$IFDEF DARWIN}
{$INCLUDE ../macos/Jedi.inc}
{$ELSE}
{$IFDEF IOS}
{$INCLUDE ../ios/Jedi.inc}
{$ELSE}
{$IFDEF ANDROID}
{$INCLUDE ../android/Jedi.inc}
{$ELSE}
{$INCLUDE ../Jedi.inc}
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}

var
  Saved8087CW: Word;
  CommandLine: String;
  i: Integer;

begin
  {$IFDEF WIN32}
  Saved8087CW := Default8087CW;
  Set8087CW($133f); { Disable all FPU exceptions }
  {$ENDIF}

  CommandLine := '';
  for i := 1 to ParamCount do
    CommandLine := CommandLine + ' ' + ParamStr(i);
  CommandLine := Trim(CommandLine);

  {$IFDEF WIN32}
  WinMain(GetModuleHandle(nil), 0, PChar(CommandLine), SW_SHOWNORMAL);

  {$ELSE}
  {$IFDEF LINUX}
  Main(i, PChar(CommandLine));

  {$ELSE}
  {$IFDEF ANDROID}
  AndroidMain(i, PChar(CommandLine));

  {$ELSE}
  {$IFDEF DARWIN}
  Main(i, PChar(CommandLine)); // macOS-specific entry point

  {$ELSE}
  {$IFDEF IOS}
  IOSMain(i, PChar(CommandLine)); // iOS-specific entry point

  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}

  {$IFDEF WIN32}
  Set8087CW(Saved8087CW);
  {$ENDIF}
end.

